;;; lsp-installer.el --- Language server installer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, lsp
;; URL: https://github.com/kn66/lsp-installer

;;; Commentary:

;; This package provides functionality to install, update, and manage
;; language servers for Emacs, similar to vim-lsp-settings.
;;
;; Language servers are installed to ~/.emacs.d/language-servers/
;; and their paths are automatically added to exec-path.
;;
;; Features:
;; - Unified configuration format for all servers
;; - Multiple installation methods: npm, pip, go, github, binary, dotnet
;; - Robust error handling and validation
;; - Interactive commands with completion
;; - Automatic path management
;;
;; Usage:
;; (require 'lsp-installer)
;; (lsp-installer-setup)  ; Add installed servers to exec-path
;;
;; Interactive commands:
;; - `lsp-installer-install-server'   - Install a server
;; - `lsp-installer-uninstall-server' - Remove a server
;; - `lsp-installer-update-server'    - Update a server
;; - `lsp-installer-list-servers'     - Show available/installed servers

;;; Code:

(require 'cl-lib)

;; Load sub-modules
(require 'lsp-installer-utils)
(require 'lsp-installer-config)
(require 'lsp-installer-npm)
(require 'lsp-installer-pip)
(require 'lsp-installer-go)
(require 'lsp-installer-dotnet)
(require 'lsp-installer-binary)
(require 'lsp-installer-jdtls)

;;; Customization

(defgroup lsp-installer nil
  "Language server installer for Emacs."
  :group 'tools
  :prefix "lsp-installer-")

(defcustom lsp-installer-install-dir
  (expand-file-name "language-servers" user-emacs-directory)
  "Directory where language servers will be installed."
  :type 'directory
  :group 'lsp-installer)

(defcustom lsp-installer-servers-file
  (expand-file-name "servers.eld"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "File containing server configuration data."
  :type 'file
  :group 'lsp-installer)

;;; Interactive commands

(defun lsp-installer--validate-server-config (server-name config)
  "Validate server configuration CONFIG for SERVER-NAME.

Return non-nil if valid, otherwise signal an error."
  (unless config
    (lsp-installer--error "Server %s not found in configuration" server-name))

  (let ((install-method (plist-get config :install-method))
        (source (plist-get config :source))
        (executable (plist-get config :executable)))

    (unless install-method
      (lsp-installer--error "Server %s: :install-method not specified" server-name))

    (unless (member install-method '("npm" "pip" "go" "dotnet" "binary" "github"))
      (lsp-installer--error "Server %s: unsupported install method: %s"
                           server-name install-method))

    (unless source
      (lsp-installer--error "Server %s: :source not specified" server-name))

    (unless executable
      (lsp-installer--error "Server %s: :executable not specified" server-name))

    t))

(defun lsp-installer--confirm-reinstall (server-name)
  "Confirm reinstallation of SERVER-NAME if already installed.

Return t to proceed, nil to cancel."
  (if (lsp-installer--server-installed-p server-name)
      (y-or-n-p (format "Server %s is already installed. Reinstall? " server-name))
    t))

(defun lsp-installer--extract-install-options (config)
  "Extract installation options from CONFIG.

Return a plist with extracted options."
  (let* ((options (plist-get config :options))
         (asset-pattern (and options (plist-get options :asset-pattern)))
         (target-subdir (and options (plist-get options :target-subdir)))
         (strip-components (and options (plist-get options :strip-components))))
    (list :asset-pattern asset-pattern
          :target-subdir target-subdir
          :strip-components strip-components)))

(defun lsp-installer--dispatch-installation (server-name config)
  "Dispatch installation for SERVER-NAME based on CONFIG."
  (let* ((install-method (plist-get config :install-method))
         (source (plist-get config :source))
         (executable (plist-get config :executable))
         (version (plist-get config :version))
         (install-options (lsp-installer--extract-install-options config))
         (asset-pattern (plist-get install-options :asset-pattern))
         (target-subdir (plist-get install-options :target-subdir))
         (strip-components (plist-get install-options :strip-components)))

    (lsp-installer--message "Installing %s via %s..." server-name install-method)

    (condition-case err
        (cond
         ((string= install-method "npm")
          (lsp-installer--install-npm-package server-name source version))
         ((string= install-method "pip")
          (lsp-installer--install-pip-package server-name source version))
         ((string= install-method "go")
          (lsp-installer--install-go-binary server-name source))
         ((string= install-method "dotnet")
          (lsp-installer--install-dotnet-tool server-name source version))
         ((string= install-method "binary")
          (if (string= server-name "jdtls")
              (lsp-installer--install-jdtls server-name source target-subdir)
            (lsp-installer--install-binary server-name source executable
                                          target-subdir strip-components)))
         ((string= install-method "github")
          (lsp-installer--install-github-release server-name source asset-pattern
                                                executable target-subdir strip-components))
         (t
          (lsp-installer--error "Unsupported install method: %s" install-method)))
      (error
       (lsp-installer--error "Failed to install %s: %s" server-name (error-message-string err))))

    (lsp-installer--message "Successfully installed %s" server-name)))

;;;###autoload
(defun lsp-installer-install-server (server-name)
  "Install language server SERVER-NAME.

Interactively prompts for server name from available servers.
Validates configuration and handles reinstallation confirmation."
  (interactive (list
                (completing-read
                 "Install server: "
                 (lsp-installer--list-available-servers)
                 nil t)))

  (let ((config (lsp-installer--get-server-config server-name)))
    ;; Validate configuration
    (lsp-installer--validate-server-config server-name config)

    ;; Confirm reinstallation if needed
    (unless (lsp-installer--confirm-reinstall server-name)
      (lsp-installer--message "Installation cancelled")
      (return))

    ;; Perform installation
    (lsp-installer--dispatch-installation server-name config)))

;;;###autoload
(defun lsp-installer-uninstall-server (server-name)
  "Uninstall language server SERVER-NAME.

Interactively prompts for server name from installed servers.
Requires confirmation before removal."
  (interactive (list
                (completing-read
                 "Uninstall server: "
                 (lsp-installer--list-installed-servers)
                 nil t)))

  (unless (lsp-installer--server-installed-p server-name)
    (lsp-installer--error "Server %s is not installed" server-name))

  (when (y-or-n-p (format "Really uninstall server %s? " server-name))
    (condition-case err
        (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
          (when (file-directory-p server-dir)
            (delete-directory server-dir t)
            (lsp-installer--message "Successfully uninstalled %s" server-name)))
      (error
       (lsp-installer--error "Failed to uninstall %s: %s"
                            server-name (error-message-string err))))))

;;;###autoload
(defun lsp-installer-update-server (server-name)
  "Update language server SERVER-NAME by uninstalling and reinstalling.

Interactively prompts for server name from installed servers.
Validates configuration before attempting update."
  (interactive (list
                (completing-read
                 "Update server: "
                 (lsp-installer--list-installed-servers)
                 nil t)))

  (let ((config (lsp-installer--get-server-config server-name)))
    ;; Validate configuration exists
    (lsp-installer--validate-server-config server-name config)

    (unless (lsp-installer--server-installed-p server-name)
      (lsp-installer--error "Server %s is not installed" server-name))

    (lsp-installer--message "Updating %s..." server-name)

    ;; Perform update
    (condition-case err
        (progn
          (lsp-installer--message "Removing old version...")
          (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
            (when (file-directory-p server-dir)
              (delete-directory server-dir t)))

          (lsp-installer--message "Installing new version...")
          (lsp-installer--dispatch-installation server-name config)

          (lsp-installer--message "Successfully updated %s" server-name))
      (error
       (lsp-installer--error "Failed to update %s: %s"
                            server-name (error-message-string err))))))

;;;###autoload
(defun lsp-installer-list-servers ()
  "List all available and installed language servers."
  (interactive)
  (let ((available (lsp-installer--list-available-servers))
        (installed (lsp-installer--list-installed-servers)))

    (with-current-buffer (get-buffer-create "*LSP Installer*")
      (erase-buffer)
      (insert "Language Server Status\n")
      (insert "======================\n\n")

      (insert "Available servers:\n")
      (dolist (server available)
        (let ((status
               (if (member server installed)
                   " [INSTALLED]"
                 " [NOT INSTALLED]")))
          (insert (format "  %s%s\n" server status))))

      (insert "\nInstalled servers:\n")
      (if installed
          (dolist (server installed)
            (insert (format "  %s\n" server)))
        (insert "  None\n"))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun lsp-installer-setup ()
  "Setup language server paths in exec-path.

Adds all installed language server binary directories to Emacs exec-path.
This should be called after installation or in your Emacs configuration."
  (interactive)

  (let ((installed-servers (lsp-installer--list-installed-servers)))
    (if installed-servers
        (progn
          (lsp-installer--message "Setting up paths for %d servers..."
                                 (length installed-servers))
          (dolist (server installed-servers)
            (lsp-installer--add-to-exec-path server)
            (lsp-installer--message "Added %s to exec-path" server))
          (lsp-installer--message "Language server paths setup complete"))
      (lsp-installer--message "No language servers installed"))))

;;; Backward compatibility

;;;###autoload
(define-obsolete-function-alias 'ls-installer-setup 'lsp-installer-setup "0.2.0"
  "Use `lsp-installer-setup' instead.")

;;; Health check functions

(defun lsp-installer-check-dependencies ()
  "Check if required system dependencies are available.

Return a list of missing dependencies."
  (interactive)

  (let ((required-tools '(("curl" . "Download files from URLs")
                         ("wget" . "Alternative download tool")
                         ("tar" . "Extract tar archives")
                         ("unzip" . "Extract zip archives")))
        (missing '())
        (found-downloader nil))

    ;; Check for at least one downloader
    (when (or (executable-find "curl") (executable-find "wget"))
      (setq found-downloader t))

    ;; Check required tools
    (dolist (tool required-tools)
      (let ((cmd (car tool))
            (desc (cdr tool)))
        (unless (or (executable-find cmd)
                   (and (member cmd '("curl" "wget")) found-downloader))
          (push (cons cmd desc) missing))))

    (if (called-interactively-p 'interactive)
        (if missing
            (lsp-installer--warning
             "Missing dependencies:\n%s"
             (mapconcat (lambda (dep)
                         (format "  %s - %s" (car dep) (cdr dep)))
                       missing "\n"))
          (lsp-installer--message "All dependencies are available"))
      missing)))

(defun lsp-installer-diagnose ()
  "Run diagnostic checks for lsp-installer.

Check configuration, dependencies, and installation status."
  (interactive)

  (lsp-installer--message "Running lsp-installer diagnostics...")

  ;; Check configuration file
  (if (file-exists-p lsp-installer-servers-file)
      (lsp-installer--message "✓ Configuration file found: %s" lsp-installer-servers-file)
    (lsp-installer--warning "✗ Configuration file not found: %s" lsp-installer-servers-file))

  ;; Check install directory
  (if (file-directory-p lsp-installer-install-dir)
      (lsp-installer--message "✓ Install directory exists: %s" lsp-installer-install-dir)
    (lsp-installer--message "ⓘ Install directory will be created: %s" lsp-installer-install-dir))

  ;; Check dependencies
  (let ((missing-deps (lsp-installer-check-dependencies)))
    (if missing-deps
        (lsp-installer--warning "✗ Missing dependencies: %s"
                               (mapconcat #'car missing-deps ", "))
      (lsp-installer--message "✓ All dependencies available")))

  ;; Show installed servers
  (let ((installed (lsp-installer--list-installed-servers)))
    (lsp-installer--message "📦 Installed servers: %s"
                           (if installed
                               (string-join installed ", ")
                             "none")))

  (lsp-installer--message "Diagnostics complete"))

;;; Integration helpers

(defun lsp-installer-get-server-executable-path (server-name)
  "Get the full path to SERVER-NAME's executable.

Return nil if the server is not installed or executable not found."
  (when (lsp-installer--server-installed-p server-name)
    (let* ((config (lsp-installer--get-server-config server-name))
           (executable (and config (plist-get config :executable)))
           (server-dir (lsp-installer--get-server-install-dir server-name))
           (bin-dirs (lsp-installer--get-server-bin-dirs server-dir)))

      (when executable
        (or
         ;; Try in all bin directories
         (cl-some (lambda (bin-dir)
                   (let ((full-path (expand-file-name executable bin-dir)))
                     (when (file-executable-p full-path)
                       full-path)))
                 bin-dirs)
         ;; Try as absolute path from server directory
         (let ((full-path (expand-file-name executable server-dir)))
           (when (file-executable-p full-path)
             full-path))
         ;; Try to find in PATH (if already added)
         (executable-find executable))))))

(provide 'lsp-installer)

;;; lsp-installer.el ends here
