;;; lsp-installer-utils.el --- Utility functions for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains utility functions used by lsp-installer.

;;; Code:

(require 'cl-lib)

(defcustom lsp-installer-pip-executable
  (if (executable-find "pip3")
      "pip3"
    "pip")
  "Path to pip executable (e.g. 'pip', 'pip3')."
  :type 'string
  :group 'lsp-installer)

;;; Variables

(defvar lsp-installer--npm-executable "npm"
  "Path to npm executable.")

(defvar lsp-installer--curl-executable "curl"
  "Path to curl executable.")

(defvar lsp-installer--wget-executable "wget"
  "Path to wget executable.")

(defvar lsp-installer--tar-executable "tar"
  "Path to tar executable.")

(defvar lsp-installer--unzip-executable "unzip"
  "Path to unzip executable.")

(defvar lsp-installer--dotnet-executable "dotnet"
  "Path to dotnet executable.")

;;; Utility functions

(defun lsp-installer--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun lsp-installer--message (format-string &rest args)
  "Display a message with LSP-INSTALLER prefix."
  (message "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--error (format-string &rest args)
  "Signal an error with LSP-INSTALLER prefix."
  (error "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--executable-find (command)
  "Find executable COMMAND in PATH."
  (executable-find command))

(defun lsp-installer--download-file (url target-file)
  "Download file from URL to TARGET-FILE."
  (lsp-installer--message "Downloading %s..." url)
  (cond
   ((lsp-installer--executable-find lsp-installer--curl-executable)
    (let ((exit-code
           (call-process lsp-installer--curl-executable
                         nil
                         "*lsp-installer-download*"
                         t
                         "-L"
                         "-o"
                         target-file
                         url)))
      (when (/= exit-code 0)
        (lsp-installer--error
         "Failed to download %s with curl (exit code: %d)"
         url exit-code))))
   ((lsp-installer--executable-find lsp-installer--wget-executable)
    (let ((exit-code
           (call-process lsp-installer--wget-executable
                         nil
                         "*lsp-installer-download*"
                         t
                         "-O"
                         target-file
                         url)))
      (when (/= exit-code 0)
        (lsp-installer--error
         "Failed to download %s with wget (exit code: %d)"
         url exit-code))))
   (t
    (lsp-installer--error "Neither curl nor wget found in PATH"))))

(defun lsp-installer--extract-archive
    (archive-file target-dir &optional strip-components)
  "Extract ARCHIVE-FILE to TARGET-DIR with optional STRIP-COMPONENTS."
  (lsp-installer--ensure-directory target-dir)
  (let ((file-ext (file-name-extension archive-file)))
    (cond
     ((string-match-p "tar\\.gz\\|tgz" archive-file)
      (let ((args `("-xzf" ,archive-file "-C" ,target-dir)))
        (when strip-components
          (setq args
                (append
                 args
                 (list
                  "--strip-components"
                  (number-to-string strip-components)))))
        (let ((exit-code
               (apply #'call-process
                      lsp-installer--tar-executable
                      nil
                      "*lsp-installer-extract*"
                      t
                      args)))
          (when (/= exit-code 0)
            (lsp-installer--error
             "Failed to extract tar.gz archive (exit code: %d)"
             exit-code)))))
     ((string-match-p "tar\\.xz" archive-file)
      (let ((args `("-xJf" ,archive-file "-C" ,target-dir)))
        (when strip-components
          (setq args
                (append
                 args
                 (list
                  "--strip-components"
                  (number-to-string strip-components)))))
        (let ((exit-code
               (apply #'call-process
                      lsp-installer--tar-executable
                      nil
                      "*lsp-installer-extract*"
                      t
                      args)))
          (when (/= exit-code 0)
            (lsp-installer--error
             "Failed to extract tar.xz archive (exit code: %d)"
             exit-code)))))
     ((string= file-ext "zip")
      (let ((exit-code
             (call-process lsp-installer--unzip-executable
                           nil
                           "*lsp-installer-extract*"
                           t
                           "-o"
                           archive-file
                           "-d"
                           target-dir)))
        (when (/= exit-code 0)
          (lsp-installer--error
           "Failed to extract zip archive (exit code: %d)"
           exit-code))))
     (t
      (lsp-installer--error "Unsupported archive format: %s"
                           archive-file)))))

(defun lsp-installer--make-executable (file-path)
  "Make FILE-PATH executable."
  (when (file-exists-p file-path)
    (set-file-modes file-path (logior (file-modes file-path) #o111))))

(defun lsp-installer--get-platform ()
  "Get current platform string."
  (cond
   ((eq system-type 'darwin)
    "darwin")
   ((eq system-type 'gnu/linux)
    "linux")
   ((eq system-type 'windows-nt)
    "windows")
   (t
    (symbol-name system-type))))

(defun lsp-installer--get-arch ()
  "Get current architecture string."
  (cond
   ((string-match "x86_64\\|amd64" system-configuration)
    "x86_64")
   ((string-match "aarch64\\|arm64" system-configuration)
    "aarch64")
   ((string-match "arm" system-configuration)
    "arm")
   (t
    "x86_64")))

(defun lsp-installer--get-server-install-dir (server-name)
  "Get installation directory for SERVER-NAME."
  (expand-file-name server-name lsp-installer-install-dir))

(defun lsp-installer--add-to-exec-path (server-name)
  "Add SERVER-NAME's bin directories to exec-path."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (npm-bin-dir
          (expand-file-name "node_modules/.bin" server-dir)))
    (when (file-directory-p bin-dir)
      (add-to-list 'exec-path bin-dir))
    (when (file-directory-p npm-bin-dir)
      (add-to-list 'exec-path npm-bin-dir))))

(defun lsp-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed."
  (let ((server-dir
         (lsp-installer--get-server-install-dir server-name)))
    (and (file-directory-p server-dir)
         (not (directory-empty-p server-dir)))))

(defun lsp-installer--list-installed-servers ()
  "List all installed servers."
  (when (file-directory-p lsp-installer-install-dir)
    (cl-remove-if-not
     (lambda (dir)
       (and (file-directory-p
             (expand-file-name dir lsp-installer-install-dir))
            (not (member dir '("." "..")))))
     (directory-files lsp-installer-install-dir))))

(provide 'lsp-installer-utils)

;;; lsp-installer-utils.el ends here
