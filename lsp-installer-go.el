;;; lsp-installer-go.el --- Go package installation for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains Go package installation functions for lsp-installer.

;;; Code:

;;; Go package installation functions

(defun lsp-installer--install-go-binary (server-name package-name)
  "Install Go binary using 'go install' command for SERVER-NAME.
PACKAGE-NAME is the Go package to install."
  (unless (lsp-installer--executable-find "go")
    (lsp-installer--error "Go compiler not found in PATH"))

  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (gopath (expand-file-name "go" server-dir))
         (gobin bin-dir))

    (lsp-installer--ensure-directory server-dir)
    (lsp-installer--ensure-directory bin-dir)
    (lsp-installer--ensure-directory gopath)
    (lsp-installer--message "Installing Go package %s..." package-name)

    ;; Set environment variables for Go installation
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "GOPATH" gopath)
      (setenv "GOBIN" gobin)

      ;; Install the Go package
      (let ((exit-code
             (call-process "go"
                           nil
                           "*lsp-installer-go*"
                           t
                           "install"
                           package-name)))
        (if (= exit-code 0)
            (progn
              (lsp-installer--message
               "Successfully installed Go package %s" package-name)
              (lsp-installer--add-to-exec-path server-name)
              t)
          (lsp-installer--error
           "Failed to install Go package %s (exit code: %d)"
           package-name
           exit-code))))))

(provide 'lsp-installer-go)

;;; lsp-installer-go.el ends here
