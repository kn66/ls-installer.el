;;; lsp-installer-dotnet.el --- .NET tool installation for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains .NET tool installation functions for lsp-installer.

;;; Code:

;;; .NET tool installation functions

(defun lsp-installer--install-dotnet-tool
    (server-name package-name &optional version)
  "Install .NET tool PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (lsp-installer--executable-find
           lsp-installer-dotnet-executable)
    (lsp-installer--error ".NET SDK not found in PATH"))

  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir))
         (package-spec
          (if version
              (format "%s::%s" package-name version)
            package-name)))

    (lsp-installer--ensure-directory server-dir)
    (lsp-installer--ensure-directory tools-dir)
    (lsp-installer--message "Installing .NET tool %s..." package-spec)

    ;; Install the .NET tool to a specific directory
    (let ((exit-code
           (call-process lsp-installer-dotnet-executable
                         nil
                         "*lsp-installer-dotnet*"
                         t
                         "tool"
                         "install"
                         package-name
                         "--tool-path"
                         tools-dir)))
      (if (= exit-code 0)
          (progn
            (lsp-installer--message
             "Successfully installed .NET tool %s" package-spec)
            ;; Add tools directory to exec-path
            (add-to-list 'exec-path tools-dir)
            t)
        (lsp-installer--error
         "Failed to install .NET tool %s (exit code: %d)"
         package-spec
         exit-code)))))

(defun lsp-installer--uninstall-dotnet-tool (server-name package-name)
  "Uninstall .NET tool PACKAGE-NAME for SERVER-NAME."
  (let* ((server-dir
          (lsp-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir)))
    (when (file-directory-p tools-dir)
      (lsp-installer--message
       "Uninstalling .NET tool %s..." package-name)
      (let ((exit-code
             (call-process lsp-installer-dotnet-executable
                           nil
                           "*lsp-installer-dotnet*"
                           t
                           "tool"
                           "uninstall"
                           package-name
                           "--tool-path"
                           tools-dir)))
        (if (= exit-code 0)
            (lsp-installer--message
             "Successfully uninstalled .NET tool %s" package-name)
          (lsp-installer--error
           "Failed to uninstall .NET tool %s" package-name))))))

(provide 'lsp-installer-dotnet)

;;; lsp-installer-dotnet.el ends here
