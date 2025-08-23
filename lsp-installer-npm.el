;;; lsp-installer-npm.el --- NPM package installation for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains NPM package installation functions for lsp-installer.

;;; Code:

(require 'json)

;;; NPM package installation functions

(defun lsp-installer--install-npm-package (server-name package-name &optional version)
  "Install npm package PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (lsp-installer--executable-find lsp-installer-npm-executable)
    (lsp-installer--error "npm not found in PATH"))

  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (package-spec
          (if version
              (format "%s@%s" package-name version)
            package-name)))

    (lsp-installer--ensure-directory server-dir)
    (lsp-installer--message
     "Installing npm package %s..." package-spec)

    (let ((default-directory server-dir))
      ;; Initialize package.json if it doesn't exist
      (unless (file-exists-p "package.json")
        (with-temp-file "package.json"
          (insert
           (json-encode
            '((name . "lsp-server") (version . "1.0.0"))))))

      ;; Install the package
      (let ((exit-code
             (call-process lsp-installer-npm-executable
                           nil
                           "*lsp-installer-npm*"
                           t
                           "install"
                           package-spec)))
        (if (= exit-code 0)
            (progn
              (lsp-installer--message
               "Successfully installed %s" package-spec)
              (lsp-installer--add-to-exec-path server-name)
              t)
          (lsp-installer--error
           "Failed to install %s (exit code: %d)"
           package-spec
           exit-code))))))

(defun lsp-installer--uninstall-npm-package (server-name package-name)
  "Uninstall npm package PACKAGE-NAME for SERVER-NAME."
  (let ((server-dir
         (lsp-installer--get-server-install-dir server-name)))
    (when (file-directory-p server-dir)
      (lsp-installer--message
       "Uninstalling npm package %s..." package-name)
      (let ((default-directory server-dir))
        (let ((exit-code
               (call-process lsp-installer-npm-executable
                             nil
                             "*lsp-installer-npm*"
                             t
                             "uninstall"
                             package-name)))
          (if (= exit-code 0)
              (lsp-installer--message
               "Successfully uninstalled %s" package-name)
            (lsp-installer--error
             "Failed to uninstall %s" package-name)))))))

(provide 'lsp-installer-npm)

;;; lsp-installer-npm.el ends here
