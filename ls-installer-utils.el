;;; ls-installer-utils.el --- Utility functions for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains utility functions used by ls-installer.

;;; Code:

(require 'cl-lib)

(defcustom ls-installer-pip-executable
  (if (executable-find "pip3")
      "pip3"
    "pip")
  "Path to pip executable (e.g. 'pip', 'pip3')."
  :type 'string
  :group 'ls-installer)

;;; Variables

(defvar ls-installer--npm-executable "npm"
  "Path to npm executable.")

(defvar ls-installer--curl-executable "curl"
  "Path to curl executable.")

(defvar ls-installer--wget-executable "wget"
  "Path to wget executable.")

(defvar ls-installer--tar-executable "tar"
  "Path to tar executable.")

(defvar ls-installer--unzip-executable "unzip"
  "Path to unzip executable.")

(defvar ls-installer--dotnet-executable "dotnet"
  "Path to dotnet executable.")

;;; Utility functions

(defun ls-installer--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun ls-installer--message (format-string &rest args)
  "Display a message with LS-INSTALLER prefix."
  (message "[LS-INSTALLER] %s" (apply #'format format-string args)))

(defun ls-installer--error (format-string &rest args)
  "Signal an error with LS-INSTALLER prefix."
  (error "[LS-INSTALLER] %s" (apply #'format format-string args)))

(defun ls-installer--executable-find (command)
  "Find executable COMMAND in PATH."
  (executable-find command))

(defun ls-installer--download-file (url target-file)
  "Download file from URL to TARGET-FILE."
  (ls-installer--message "Downloading %s..." url)
  (cond
   ((ls-installer--executable-find ls-installer--curl-executable)
    (let ((exit-code
           (call-process ls-installer--curl-executable
                         nil
                         "*ls-installer-download*"
                         t
                         "-L"
                         "-o"
                         target-file
                         url)))
      (when (/= exit-code 0)
        (ls-installer--error
         "Failed to download %s with curl (exit code: %d)"
         url exit-code))))
   ((ls-installer--executable-find ls-installer--wget-executable)
    (let ((exit-code
           (call-process ls-installer--wget-executable
                         nil
                         "*ls-installer-download*"
                         t
                         "-O"
                         target-file
                         url)))
      (when (/= exit-code 0)
        (ls-installer--error
         "Failed to download %s with wget (exit code: %d)"
         url exit-code))))
   (t
    (ls-installer--error "Neither curl nor wget found in PATH"))))

(defun ls-installer--extract-archive
    (archive-file target-dir &optional strip-components)
  "Extract ARCHIVE-FILE to TARGET-DIR with optional STRIP-COMPONENTS."
  (ls-installer--ensure-directory target-dir)
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
                      ls-installer--tar-executable
                      nil
                      "*ls-installer-extract*"
                      t
                      args)))
          (when (/= exit-code 0)
            (ls-installer--error
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
                      ls-installer--tar-executable
                      nil
                      "*ls-installer-extract*"
                      t
                      args)))
          (when (/= exit-code 0)
            (ls-installer--error
             "Failed to extract tar.xz archive (exit code: %d)"
             exit-code)))))
     ((string= file-ext "zip")
      (let ((exit-code
             (call-process ls-installer--unzip-executable
                           nil
                           "*ls-installer-extract*"
                           t
                           "-o"
                           archive-file
                           "-d"
                           target-dir)))
        (when (/= exit-code 0)
          (ls-installer--error
           "Failed to extract zip archive (exit code: %d)"
           exit-code))))
     (t
      (ls-installer--error "Unsupported archive format: %s"
                           archive-file)))))

(defun ls-installer--make-executable (file-path)
  "Make FILE-PATH executable."
  (when (file-exists-p file-path)
    (set-file-modes file-path (logior (file-modes file-path) #o111))))

(defun ls-installer--get-platform ()
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

(defun ls-installer--get-arch ()
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

(defun ls-installer--get-server-install-dir (server-name)
  "Get installation directory for SERVER-NAME."
  (expand-file-name server-name ls-installer-install-dir))

(defun ls-installer--add-to-exec-path (server-name)
  "Add SERVER-NAME's bin directory to exec-path."
  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir)))
    (when (file-directory-p bin-dir)
      (add-to-list 'exec-path bin-dir))))

(defun ls-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed."
  (let ((server-dir
         (ls-installer--get-server-install-dir server-name)))
    (and (file-directory-p server-dir)
         (not (directory-empty-p server-dir)))))

(defun ls-installer--list-installed-servers ()
  "List all installed servers."
  (when (file-directory-p ls-installer-install-dir)
    (cl-remove-if-not
     (lambda (dir)
       (and (file-directory-p
             (expand-file-name dir ls-installer-install-dir))
            (not (member dir '("." "..")))))
     (directory-files ls-installer-install-dir))))

(provide 'ls-installer-utils)

;;; ls-installer-utils.el ends here
