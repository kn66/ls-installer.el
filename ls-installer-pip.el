;;; ls-installer-pip.el --- Python package installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains Python package installation functions for ls-installer.

;;; Code:

;;; Python package installation functions

(defun ls-installer--install-pip-package
    (server-name package-name &optional version)
  "Install pip package PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (ls-installer--executable-find ls-installer--pip-executable)
    (ls-installer--error "pip not found in PATH"))

  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (package-spec
          (if version
              (format "%s==%s" package-name version)
            package-name))
         (target-dir
          (expand-file-name "lib/python/site-packages" server-dir))
         (bin-dir (expand-file-name "bin" server-dir)))

    (ls-installer--ensure-directory target-dir)
    (ls-installer--ensure-directory bin-dir)
    (ls-installer--message
     "Installing pip package %s..." package-spec)

    ;; Install the package to specific directory
    (let ((exit-code
           (call-process ls-installer--pip-executable
                         nil
                         "*ls-installer-pip*"
                         t
                         "install"
                         "--target"
                         target-dir
                         "--install-option"
                         (format "--install-scripts=%s" bin-dir)
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
         exit-code)))))

(defun ls-installer--uninstall-pip-package (server-name package-name)
  "Uninstall pip package PACKAGE-NAME for SERVER-NAME."
  (ls-installer--message
   "Uninstalling pip package %s..." package-name)
  (let ((exit-code
         (call-process ls-installer--pip-executable
                       nil
                       "*ls-installer-pip*"
                       t
                       "uninstall"
                       "-y"
                       package-name)))
    (if (= exit-code 0)
        (ls-installer--message
         "Successfully uninstalled %s" package-name)
      (ls-installer--error "Failed to uninstall %s" package-name))))

(provide 'ls-installer-pip)

;;; ls-installer-pip.el ends here
