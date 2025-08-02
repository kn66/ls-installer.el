;;; ls-installer-pip.el --- Python package installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains Python package installation functions for ls-installer.

;;; Code:

;;; Python package installation functions

(defun ls-installer--install-pip-package
    (server-name package-name &optional version)
  "Install pip package PACKAGE-NAME for SERVER-NAME with optional VERSION."
  (unless (ls-installer--executable-find ls-installer-pip-executable)
    (ls-installer--error "pip not found in PATH"))

  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (package-spec
          (if version
              (format "%s==%s" package-name version)
            package-name))
         (venv-dir (expand-file-name "venv" server-dir))
         (bin-dir (expand-file-name "bin" server-dir)))

    (ls-installer--ensure-directory server-dir)
    (ls-installer--ensure-directory bin-dir)
    (ls-installer--message
     "Installing pip package %s..." package-spec)

    ;; Create virtual environment
    (let ((exit-code
           (call-process (or (executable-find "python3")
                             (executable-find "python")
                             "python")
                         nil
                         "*ls-installer-pip*"
                         t
                         "-m"
                         "venv"
                         venv-dir)))
      (when (/= exit-code 0)
        (ls-installer--error
         "Failed to create virtual environment (exit code: %d)"
         exit-code)))

    ;; Install package in virtual environment
    (let* ((python-exe
            (expand-file-name (if (eq system-type 'windows-nt)
                                  "Scripts/python.exe"
                                "bin/python")
                              venv-dir))
           (pip-exe
            (expand-file-name (if (eq system-type 'windows-nt)
                                  "Scripts/pip.exe"
                                "bin/pip")
                              venv-dir))
           (exit-code
            (call-process pip-exe
                          nil
                          "*ls-installer-pip*"
                          t
                          "install"
                          package-spec)))
      (if (= exit-code 0)
          (progn
            ;; Create wrapper scripts in bin directory
            (ls-installer--create-pip-wrappers
             server-name package-name venv-dir)
            (ls-installer--message
             "Successfully installed %s" package-spec)
            (ls-installer--add-to-exec-path server-name)
            t)
        (ls-installer--error
         "Failed to install %s (exit code: %d)"
         package-spec
         exit-code)))))

(defun ls-installer--create-pip-wrappers
    (server-name package-name venv-dir)
  "Create wrapper scripts for Python executables."
  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (venv-bin-dir
          (expand-file-name (if (eq system-type 'windows-nt)
                                "Scripts"
                              "bin")
                            venv-dir)))

    ;; Find executables in venv and create wrappers
    (when (file-directory-p venv-bin-dir)
      (dolist (file (directory-files venv-bin-dir))
        (let ((full-path (expand-file-name file venv-bin-dir)))
          (when (and (file-executable-p full-path)
                     (not (member file '("." "..")))
                     (not
                      (string-match-p
                       "python\\|pip\\|activate" file)))
            ;; Create wrapper script
            (let ((wrapper-path (expand-file-name file bin-dir)))
              (with-temp-file wrapper-path
                (insert "#!/bin/bash\n")
                (insert (format "exec \"%s\" \"$@\"\n" full-path)))
              (ls-installer--make-executable wrapper-path)

              ;; For Windows, also create .bat file
              (when (eq system-type 'windows-nt)
                (let ((bat-path (concat wrapper-path ".bat")))
                  (with-temp-file bat-path
                    (insert "@echo off\n")
                    (insert
                     (format "\"%s\" %%*\n" full-path))))))))))))

(defun ls-installer--uninstall-pip-package (server-name package-name)
  "Uninstall pip package PACKAGE-NAME for SERVER-NAME."
  (let ((server-dir
         (ls-installer--get-server-install-dir server-name)))
    (when (file-directory-p server-dir)
      (ls-installer--message
       "Uninstalling pip package %s..." package-name)
      (delete-directory server-dir t)
      (ls-installer--message
       "Successfully uninstalled %s" package-name))))

(provide 'ls-installer-pip)

;;; ls-installer-pip.el ends here
