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
;; Supports installation of Eclipse JDT Language Server (jdtls) for Java development.

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

;;;###autoload
(defun lsp-installer-install-server (server-name)
  "Install language server SERVER-NAME."
  (interactive (list
                (completing-read
                 "Install server: "
                 (lsp-installer--list-available-servers)
                 nil
                 t)))
  (let ((config (lsp-installer--get-server-config server-name)))
    (unless config
      (lsp-installer--error
       "Server %s not found in configuration" server-name))

    (when (lsp-installer--server-installed-p server-name)
      (when (not
             (y-or-n-p
              (format "Server %s is already installed. Reinstall? "
                      server-name)))
        (lsp-installer--message "Installation cancelled")
        (return-from lsp-installer-install-server)))

    (let ((install-script (plist-get config :install-script))
          (package-name
           (or (plist-get config :package-name) server-name))
          (version (plist-get config :version))
          (binary-url (plist-get config :binary-url))
          (github-repo (plist-get config :github-repo))
          (asset-pattern (plist-get config :asset-pattern))
          (executable-name (plist-get config :executable-name))
          (target-subdir (plist-get config :target-subdir))
          (strip-components (plist-get config :strip-components)))

      (cond
       ((eq install-script 'ls--install-npm-package)
        (lsp-installer--install-npm-package
         server-name package-name version))
       ((eq install-script 'ls--install-pip-package)
        (lsp-installer--install-pip-package
         server-name package-name version))
       ((eq install-script 'ls--install-go-binary)
        (unless package-name
          (lsp-installer--error
           "package-name not specified for Go binary installation"))
        (lsp-installer--install-go-binary server-name package-name))
       ((eq install-script 'ls--install-dotnet-tool)
        (lsp-installer--install-dotnet-tool
         server-name package-name version))
       ((eq install-script 'ls--install-binary)
        (if (string= server-name "jdtls")
            ;; Special handling for JDT LS
            (lsp-installer--install-jdtls
             server-name binary-url target-subdir)
          ;; Standard binary installation
          (progn
            (unless binary-url
              (lsp-installer--error
               "binary-url not specified for binary installation"))
            (lsp-installer--install-binary
             server-name
             binary-url
             executable-name
             target-subdir
             strip-components))))
       ((eq install-script 'ls--install-github-release)
        (unless github-repo
          (lsp-installer--error
           "github-repo not specified for GitHub release installation"))
        (lsp-installer--install-github-release
         server-name
         github-repo
         asset-pattern
         executable-name
         target-subdir
         strip-components))
       (t
        (lsp-installer--error
         "Unsupported install script: %s" install-script))))))

;;;###autoload
(defun lsp-installer-uninstall-server (server-name)
  "Uninstall language server SERVER-NAME."
  (interactive (list
                (completing-read
                 "Uninstall server: "
                 (lsp-installer--list-installed-servers)
                 nil
                 t)))
  (when (y-or-n-p (format "Really uninstall server %s? " server-name))
    (let ((server-dir
           (lsp-installer--get-server-install-dir server-name)))
      (when (file-directory-p server-dir)
        (delete-directory server-dir t)
        (lsp-installer--message
         "Successfully uninstalled %s" server-name)))))

;;;###autoload
(defun lsp-installer-update-server (server-name)
  "Update language server SERVER-NAME by uninstalling and reinstalling."
  (interactive (list
                (completing-read
                 "Update server: "
                 (lsp-installer--list-installed-servers)
                 nil
                 t)))
  (let ((config (lsp-installer--get-server-config server-name)))
    (unless config
      (lsp-installer--error
       "Server %s not found in configuration" server-name))
    (lsp-installer-uninstall-server server-name)
    (lsp-installer-install-server server-name)))

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
  "Setup language server paths in exec-path."
  (interactive)
  (lsp-installer--message "Setting up language server paths...")
  (dolist (server (lsp-installer--list-installed-servers))
    (lsp-installer--add-to-exec-path server))
  (lsp-installer--message "Language server paths setup complete"))

;;;###autoload
(define-obsolete-function-alias 'ls-installer-setup 'lsp-installer-setup "0.2.0")

(provide 'lsp-installer)

;;; lsp-installer.el ends here
