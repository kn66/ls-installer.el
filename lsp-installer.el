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

    (let* ((install-method (plist-get config :install-method))
           (source (plist-get config :source))
           (executable (plist-get config :executable))
           (version (plist-get config :version))
           (options (plist-get config :options))
           ;; Extract options safely
           (asset-pattern (and options (plist-get options :asset-pattern)))
           (target-subdir (and options (plist-get options :target-subdir)))
           (strip-components (and options (plist-get options :strip-components))))

      (cond
       ((string= install-method "npm")
        (lsp-installer--install-npm-package
         server-name source version))
       ((string= install-method "pip")
        (lsp-installer--install-pip-package
         server-name source version))
       ((string= install-method "go")
        (unless source
          (lsp-installer--error
           "source not specified for Go binary installation"))
        (lsp-installer--install-go-binary server-name source))
       ((string= install-method "dotnet")
        (lsp-installer--install-dotnet-tool
         server-name source version))
       ((string= install-method "binary")
        (if (string= server-name "jdtls")
            ;; Special handling for JDT LS
            (lsp-installer--install-jdtls
             server-name source target-subdir)
          ;; Standard binary installation
          (progn
            (unless source
              (lsp-installer--error
               "source not specified for binary installation"))
            (lsp-installer--install-binary
             server-name
             source
             executable
             target-subdir
             strip-components))))
       ((string= install-method "github")
        (unless source
          (lsp-installer--error
           "source not specified for GitHub release installation"))
        (lsp-installer--install-github-release
         server-name
         source
         asset-pattern
         executable
         target-subdir
         strip-components))
       (t
        (lsp-installer--error
         "Unsupported install method: %s" install-method))))))

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
