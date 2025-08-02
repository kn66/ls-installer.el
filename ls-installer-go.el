;;; ls-installer-go.el --- Go package installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains Go package installation functions for ls-installer.

;;; Code:

;;; Go package installation functions

(defun ls-installer--install-go-binary (server-name package-name)
  "Install Go binary using 'go install' command for SERVER-NAME.
PACKAGE-NAME is the Go package to install."
  (unless (ls-installer--executable-find "go")
    (ls-installer--error "Go compiler not found in PATH"))

  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (gopath (expand-file-name "go" server-dir))
         (gobin bin-dir))

    (ls-installer--ensure-directory server-dir)
    (ls-installer--ensure-directory bin-dir)
    (ls-installer--ensure-directory gopath)
    (ls-installer--message "Installing Go package %s..." package-name)

    ;; Set environment variables for Go installation
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "GOPATH" gopath)
      (setenv "GOBIN" gobin)

      ;; Install the Go package
      (let ((exit-code
             (call-process "go"
                           nil
                           "*ls-installer-go*"
                           t
                           "install"
                           package-name)))
        (if (= exit-code 0)
            (progn
              (ls-installer--message
               "Successfully installed Go package %s" package-name)
              (ls-installer--add-to-exec-path server-name)
              t)
          (ls-installer--error
           "Failed to install Go package %s (exit code: %d)"
           package-name
           exit-code))))))

(provide 'ls-installer-go)

;;; ls-installer-go.el ends here
