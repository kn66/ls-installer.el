;;; ls-installer-npm.el --- NPM package installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains NPM package installation functions for ls-installer.

;;; Code:

(require 'json)

;;; NPM package installation functions

(defun ls-installer--install-npm-package
    (server-name package-name &optional version)
  "Install npm package PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (ls-installer--executable-find ls-installer--npm-executable)
    (ls-installer--error "npm not found in PATH"))

  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (package-spec
          (if version
              (format "%s@%s" package-name version)
            package-name)))

    (ls-installer--ensure-directory server-dir)
    (ls-installer--message
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
             (call-process ls-installer--npm-executable
                           nil
                           "*ls-installer-npm*"
                           t
                           "install"
                           package-spec)))
        (if (= exit-code 0)
            (progn
              (ls-installer--message
               "Successfully installed %s" package-spec)
              (ls-installer--add-to-exec-path server-name)
              t)
          (ls-installer--error
           "Failed to install %s (exit code: %d)"
           package-spec
           exit-code))))))

(defun ls-installer--uninstall-npm-package (server-name package-name)
  "Uninstall npm package PACKAGE-NAME for SERVER-NAME."
  (let ((server-dir
         (ls-installer--get-server-install-dir server-name)))
    (when (file-directory-p server-dir)
      (ls-installer--message
       "Uninstalling npm package %s..." package-name)
      (let ((default-directory server-dir))
        (let ((exit-code
               (call-process ls-installer--npm-executable
                             nil
                             "*ls-installer-npm*"
                             t
                             "uninstall"
                             package-name)))
          (if (= exit-code 0)
              (ls-installer--message
               "Successfully uninstalled %s" package-name)
            (ls-installer--error
             "Failed to uninstall %s" package-name)))))))

(provide 'ls-installer-npm)

;;; ls-installer-npm.el ends here
