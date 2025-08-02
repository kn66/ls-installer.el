;;; ls-installer-dotnet.el --- .NET tool installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains .NET tool installation functions for ls-installer.

;;; Code:

;;; .NET tool installation functions

(defun ls-installer--install-dotnet-tool
    (server-name package-name &optional version)
  "Install .NET tool PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (ls-installer--executable-find
           ls-installer--dotnet-executable)
    (ls-installer--error ".NET SDK not found in PATH"))

  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir))
         (package-spec
          (if version
              (format "%s::%s" package-name version)
            package-name)))

    (ls-installer--ensure-directory server-dir)
    (ls-installer--ensure-directory tools-dir)
    (ls-installer--message "Installing .NET tool %s..." package-spec)

    ;; Install the .NET tool to a specific directory
    (let ((exit-code
           (call-process ls-installer--dotnet-executable
                         nil
                         "*ls-installer-dotnet*"
                         t
                         "tool"
                         "install"
                         package-name
                         "--tool-path"
                         tools-dir)))
      (if (= exit-code 0)
          (progn
            (ls-installer--message
             "Successfully installed .NET tool %s" package-spec)
            ;; Add tools directory to exec-path
            (add-to-list 'exec-path tools-dir)
            t)
        (ls-installer--error
         "Failed to install .NET tool %s (exit code: %d)"
         package-spec
         exit-code)))))

(defun ls-installer--uninstall-dotnet-tool (server-name package-name)
  "Uninstall .NET tool PACKAGE-NAME for SERVER-NAME."
  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir)))
    (when (file-directory-p tools-dir)
      (ls-installer--message
       "Uninstalling .NET tool %s..." package-name)
      (let ((exit-code
             (call-process ls-installer--dotnet-executable
                           nil
                           "*ls-installer-dotnet*"
                           t
                           "tool"
                           "uninstall"
                           package-name
                           "--tool-path"
                           tools-dir)))
        (if (= exit-code 0)
            (ls-installer--message
             "Successfully uninstalled .NET tool %s" package-name)
          (ls-installer--error
           "Failed to uninstall .NET tool %s" package-name))))))

(provide 'ls-installer-dotnet)

;;; ls-installer-dotnet.el ends here
