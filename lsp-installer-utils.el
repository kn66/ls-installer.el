;;; lsp-installer-utils.el --- Utility functions for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains utility functions used by lsp-installer.

;;; Code:

(require 'cl-lib)

;;; Executable Configuration

(defcustom lsp-installer-pip-executable
  (if (executable-find "pip3")
      "pip3"
    "pip")
  "Path to pip executable (e.g. pip3, pip)."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-npm-executable "npm"
  "Path to npm executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-curl-executable "curl"
  "Path to curl executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-wget-executable "wget"
  "Path to wget executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-tar-executable "tar"
  "Path to tar executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-unzip-executable "unzip"
  "Path to unzip executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-dotnet-executable "dotnet"
  "Path to dotnet executable."
  :type 'string
  :group 'lsp-installer)

(defcustom lsp-installer-go-executable "go"
  "Path to go executable."
  :type 'string
  :group 'lsp-installer)

;;; Utility functions

(defun lsp-installer--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun lsp-installer--message (format-string &rest args)
  "Display a message with LSP-INSTALLER prefix.

FORMAT-STRING and ARGS are passed to `format' to create the message."
  (message "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--error (format-string &rest args)
  "Signal an error with LSP-INSTALLER prefix.

FORMAT-STRING and ARGS are passed to `format' to create the error message."
  (error "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--warning (format-string &rest args)
  "Display a warning with LSP-INSTALLER prefix.

FORMAT-STRING and ARGS are passed to `format' to create the warning message."
  (display-warning 'lsp-installer
                   (apply #'format format-string args)
                   :warning))

(defun lsp-installer--executable-find (command)
  "Find executable COMMAND in PATH.

Return the full path to COMMAND if found, nil otherwise."
  (or (executable-find command)
      (when (file-executable-p command)
        command)))

(defun lsp-installer--require-executable (command &optional custom-path)
  "Ensure COMMAND is available, with optional CUSTOM-PATH.

Signal an error if the executable is not found.
CUSTOM-PATH can be used to specify a custom executable path."
  (let ((exe-path (or custom-path (lsp-installer--executable-find command))))
    (unless exe-path
      (lsp-installer--error "Required executable not found: %s" command))
    exe-path))

;;; Download functions

(defun lsp-installer--download-file (url target-file)
  "Download file from URL to TARGET-FILE.

Uses curl or wget, whichever is available. Signals an error if download fails."
  (lsp-installer--message "Downloading %s..." url)

  ;; Ensure target directory exists
  (lsp-installer--ensure-directory (file-name-directory target-file))

  (condition-case err
      (cond
       ((lsp-installer--executable-find lsp-installer-curl-executable)
        (lsp-installer--download-with-curl url target-file))
       ((lsp-installer--executable-find lsp-installer-wget-executable)
        (lsp-installer--download-with-wget url target-file))
       (t
        (lsp-installer--error "Neither curl nor wget found in PATH")))
    (error
     (when (file-exists-p target-file)
       (delete-file target-file))
     (signal (car err) (cdr err))))

  (unless (file-exists-p target-file)
    (lsp-installer--error "Download failed: %s" target-file))

  (lsp-installer--message "Downloaded %s" target-file))

(defun lsp-installer--download-with-curl (url target-file)
  "Download URL to TARGET-FILE using curl."
  (let ((exit-code
         (call-process lsp-installer-curl-executable
                       nil "*lsp-installer-download*" t
                       "-L" "-f" "--create-dirs"
                       "-o" target-file
                       url)))
    (unless (= exit-code 0)
      (lsp-installer--error "Failed to download %s with curl (exit code: %d)"
                           url exit-code))))

(defun lsp-installer--download-with-wget (url target-file)
  "Download URL to TARGET-FILE using wget."
  (let ((exit-code
         (call-process lsp-installer-wget-executable
                       nil "*lsp-installer-download*" t
                       "-O" target-file
                       url)))
    (unless (= exit-code 0)
      (lsp-installer--error "Failed to download %s with wget (exit code: %d)"
                           url exit-code))))

;;; Archive extraction

(defun lsp-installer--extract-archive (archive-file target-dir &optional strip-components)
  "Extract ARCHIVE-FILE to TARGET-DIR with optional STRIP-COMPONENTS.

Supports tar.gz, tar.xz, tgz, and zip formats.
STRIP-COMPONENTS specifies how many leading path components to strip."
  (lsp-installer--ensure-directory target-dir)

  (unless (file-exists-p archive-file)
    (lsp-installer--error "Archive file does not exist: %s" archive-file))

  (lsp-installer--message "Extracting %s to %s..."
                         (file-name-nondirectory archive-file)
                         target-dir)

  (let ((archive-type (lsp-installer--detect-archive-type archive-file)))
    (condition-case err
        (pcase archive-type
          ('tar-gz (lsp-installer--extract-tar archive-file target-dir "z" strip-components))
          ('tar-xz (lsp-installer--extract-tar archive-file target-dir "J" strip-components))
          ('zip (lsp-installer--extract-zip archive-file target-dir))
          (_ (lsp-installer--error "Unsupported archive format: %s" archive-file)))
      (error
       (lsp-installer--error "Failed to extract %s: %s"
                            archive-file (error-message-string err))))

  (lsp-installer--message "Extraction completed")))

(defun lsp-installer--detect-archive-type (archive-file)
  "Detect the type of ARCHIVE-FILE based on extension.

Return symbol indicating archive type: 'tar-gz, 'tar-xz, or 'zip."
  (cond
   ((string-match-p "\\.tar\\.gz\\|\\.tgz" archive-file) 'tar-gz)
   ((string-match-p "\\.tar\\.xz" archive-file) 'tar-xz)
   ((string-match-p "\\.zip" archive-file) 'zip)
   (t nil)))

(defun lsp-installer--extract-tar (archive-file target-dir compression &optional strip-components)
  "Extract tar ARCHIVE-FILE to TARGET-DIR with COMPRESSION type.

COMPRESSION should be 'z' for gzip or 'J' for xz.
STRIP-COMPONENTS specifies path components to strip."
  (let ((args `("-x" ,(concat compression "f") ,archive-file "-C" ,target-dir)))
    (when strip-components
      (setq args (append args (list "--strip-components"
                                   (number-to-string strip-components)))))

    (let ((exit-code (apply #'call-process lsp-installer-tar-executable
                           nil "*lsp-installer-extract*" t args)))
      (unless (= exit-code 0)
        (lsp-installer--error "Failed to extract tar archive (exit code: %d)" exit-code)))))

(defun lsp-installer--extract-zip (archive-file target-dir)
  "Extract ZIP ARCHIVE-FILE to TARGET-DIR."
  (let ((exit-code (call-process lsp-installer-unzip-executable
                                nil "*lsp-installer-extract*" t
                                "-o" archive-file "-d" target-dir)))
    (unless (= exit-code 0)
      (lsp-installer--error "Failed to extract zip archive (exit code: %d)" exit-code))))

;;; File utilities

(defun lsp-installer--make-executable (file-path)
  "Make FILE-PATH executable."
  (when (file-exists-p file-path)
    (set-file-modes file-path (logior (file-modes file-path) #o111))))

;;; Platform detection

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

;;; Server management

(defun lsp-installer--get-server-install-dir (server-name)
  "Get installation directory for SERVER-NAME."
  (expand-file-name server-name lsp-installer-install-dir))

(defun lsp-installer--add-to-exec-path (server-name)
  "Add SERVER-NAME's bin directories to exec-path.

Adds both standard bin/ directory and npm-specific node_modules/.bin
if they exist and are not already in exec-path."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-dirs (lsp-installer--get-server-bin-dirs server-dir))
         (added-count 0))

    (dolist (bin-dir bin-dirs)
      (when (and (file-directory-p bin-dir)
                 (not (member bin-dir exec-path)))
        (add-to-list 'exec-path bin-dir)
        (cl-incf added-count)))

    (when (> added-count 0)
      (lsp-installer--message "Added %d path(s) for %s" added-count server-name))))

(defun lsp-installer--get-server-bin-dirs (server-dir)
  "Get list of potential binary directories for SERVER-DIR.

Return a list of directories that might contain executables."
  (let ((candidates (list
                    (expand-file-name "bin" server-dir)
                    (expand-file-name "node_modules/.bin" server-dir)
                    server-dir))) ; Sometimes executables are in the root
    (cl-remove-if-not #'file-directory-p candidates)))

(defun lsp-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed.

Return t if server directory exists and is not empty, nil otherwise."
  (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
    (and (file-directory-p server-dir)
         (not (lsp-installer--directory-empty-p server-dir)))))

(defun lsp-installer--directory-empty-p (directory)
  "Check if DIRECTORY is empty.

Return t if directory is empty (contains only . and ..), nil otherwise."
  (when (file-directory-p directory)
    (let ((files (directory-files directory nil "^[^.]")))
      (null files))))

(defun lsp-installer--list-installed-servers ()
  "List all installed servers.

Return a list of server names that are currently installed.
Only returns servers with non-empty installation directories."
  (when (file-directory-p lsp-installer-install-dir)
    (cl-remove-if-not
     (lambda (dir)
       (and (not (member dir '("." "..")))
            (let ((full-path (expand-file-name dir lsp-installer-install-dir)))
              (and (file-directory-p full-path)
                   (not (lsp-installer--directory-empty-p full-path))))))
     (directory-files lsp-installer-install-dir nil "^[^.]")))) ; Exclude hidden files

;;; Process utilities

(defun lsp-installer--run-command (command args &optional buffer-name)
  "Run COMMAND with ARGS, optionally in BUFFER-NAME.

Return the exit code. Signal an error if command is not found."
  (let ((executable (lsp-installer--require-executable command))
        (buffer (or buffer-name "*lsp-installer-process*")))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (let ((exit-code (apply #'call-process executable nil t t args)))
        (unless (= exit-code 0)
          (lsp-installer--warning "Command failed (exit %d): %s %s\nOutput:\n%s"
                                 exit-code command
                                 (string-join args " ")
                                 (buffer-string)))
        exit-code))))

(defun lsp-installer--run-command-successfully (command args &optional buffer-name)
  "Run COMMAND with ARGS and ensure it succeeds.

Signal an error if the command fails (non-zero exit code)."
  (let ((exit-code (lsp-installer--run-command command args buffer-name)))
    (unless (= exit-code 0)
      (lsp-installer--error "Command failed: %s %s" command (string-join args " ")))
    t))

(provide 'lsp-installer-utils)

;;; lsp-installer-utils.el ends here
