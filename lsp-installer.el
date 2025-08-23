;;; lsp-installer.el --- Language server installer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 0.3.0
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
;; Features:
;; - Unified configuration format for all servers
;; - Multiple installation methods: npm, pip, go, github, binary, dotnet
;; - Simple error handling and validation
;; - Interactive commands with completion
;; - Automatic path management
;;
;; Usage:
;; (require 'lsp-installer)
;; (lsp-installer-setup)  ; Add installed servers to exec-path
;;
;; Interactive commands:
;; - `lsp-installer-install-server'   - Install a server
;; - `lsp-installer-uninstall-server' - Remove a server
;; - `lsp-installer-update-server'    - Update a server
;; - `lsp-installer-list-servers'     - Show available/installed servers

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

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

;; Executable paths - simplified configuration
(defvar lsp-installer--executables
  '((npm . "npm")
    (pip . "pip3")
    (go . "go")
    (dotnet . "dotnet")
    (curl . "curl")
    (wget . "wget")
    (tar . "tar")
    (unzip . "unzip")
    (java . "java"))
  "Alist of executable names.")

;;; Variables

(defvar lsp-installer--servers-cache nil
  "Cache for loaded server configurations.")

;;; Utility functions

(defun lsp-installer--message (format-string &rest args)
  "Display a message with LSP-INSTALLER prefix."
  (message "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--error (format-string &rest args)
  "Signal an error with LSP-INSTALLER prefix."
  (error "[LSP-INSTALLER] %s" (apply #'format format-string args)))

(defun lsp-installer--executable-find (exe-key)
  "Find executable for EXE-KEY from our configuration."
  (let ((exe-name (cdr (assq exe-key lsp-installer--executables))))
    (or (executable-find exe-name)
        (when (file-executable-p exe-name) exe-name))))

(defun lsp-installer--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun lsp-installer--make-executable (file-path)
  "Make FILE-PATH executable."
  (when (file-exists-p file-path)
    (set-file-modes file-path (logior (file-modes file-path) #o111))))

(defun lsp-installer--get-server-install-dir (server-name)
  "Get installation directory for SERVER-NAME."
  (expand-file-name server-name lsp-installer-install-dir))

(defun lsp-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed."
  (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
    (and (file-directory-p server-dir)
         (not (lsp-installer--directory-empty-p server-dir)))))

(defun lsp-installer--directory-empty-p (directory)
  "Check if DIRECTORY is empty."
  (when (file-directory-p directory)
    (let ((files (directory-files directory nil "^[^.]")))
      (null files))))

;;; Configuration management

(defun lsp-installer--load-config ()
  "Load server configurations from file."
  (unless lsp-installer--servers-cache
    (when (file-exists-p lsp-installer-servers-file)
      (with-temp-buffer
        (insert-file-contents lsp-installer-servers-file)
        (goto-char (point-min))
        (setq lsp-installer--servers-cache (read (current-buffer))))))
  lsp-installer--servers-cache)

(defun lsp-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME."
  (let ((servers (lsp-installer--load-config)))
    (cl-find-if (lambda (server)
                  (string= (plist-get server :name) server-name))
                servers)))

(defun lsp-installer--list-available-servers ()
  "List all available servers from configuration."
  (let ((servers (lsp-installer--load-config)))
    (mapcar (lambda (server) (plist-get server :name)) servers)))

(defun lsp-installer--list-installed-servers ()
  "List all installed servers."
  (when (file-directory-p lsp-installer-install-dir)
    (cl-remove-if-not
     (lambda (dir)
       (let ((full-path (expand-file-name dir lsp-installer-install-dir)))
         (and (file-directory-p full-path)
              (not (lsp-installer--directory-empty-p full-path)))))
     (directory-files lsp-installer-install-dir nil "^[^.]"))))

(defun lsp-installer--validate-config (server-name config)
  "Validate server configuration CONFIG for SERVER-NAME."
  (unless config
    (lsp-installer--error "Server %s not found" server-name))
  (let ((method (plist-get config :install-method))
        (source (plist-get config :source))
        (executable (plist-get config :executable)))
    (unless (and method source executable)
      (lsp-installer--error "Server %s: incomplete configuration" server-name))
    (unless (member method '("npm" "pip" "go" "dotnet" "binary" "github"))
      (lsp-installer--error "Server %s: unsupported method %s" server-name method))
    t))

;;; Unified error handling wrapper

(defmacro lsp-installer--with-error-handling (server-name &rest body)
  "Execute BODY with unified error handling for SERVER-NAME."
  `(condition-case err
       (progn ,@body)
     (error
      (lsp-installer--error "Failed to install %s: %s"
                           ,server-name (error-message-string err)))))

;;; Path management

(defun lsp-installer--add-to-exec-path (server-name)
  "Add SERVER-NAME's bin directories to exec-path."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-paths (list (expand-file-name "bin" server-dir)
                         (expand-file-name "node_modules/.bin" server-dir)
                         (expand-file-name "tools" server-dir)))
         (added 0))
    (dolist (path bin-paths)
      (when (and (file-directory-p path)
                 (not (member path exec-path)))
        (add-to-list 'exec-path path)
        (cl-incf added)))
    (when (> added 0)
      (lsp-installer--message "Added %d path(s) for %s" added server-name))))

;;; Installation methods - consolidated and simplified

(defun lsp-installer--install-npm (server-name source version)
  "Install npm package SOURCE for SERVER-NAME with optional VERSION."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (package-spec (if version (format "%s@%s" source version) source))
         (npm-exe (lsp-installer--executable-find 'npm)))
    (unless npm-exe
      (lsp-installer--error "npm not found in PATH"))
    (lsp-installer--ensure-directory server-dir)
    (let ((default-directory server-dir))
      ;; Create package.json if needed
      (unless (file-exists-p "package.json")
        (with-temp-file "package.json"
          (insert (json-encode '((name . "lsp-server") (version . "1.0.0"))))))
      ;; Install package
      (let ((exit-code (call-process npm-exe nil "*lsp-installer*" t "install" package-spec)))
        (unless (= exit-code 0)
          (lsp-installer--error "npm install failed (exit code: %d)" exit-code))))))

(defun lsp-installer--install-pip (server-name source version)
  "Install pip package SOURCE for SERVER-NAME with optional VERSION."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (package-spec (if version (format "%s==%s" source version) source))
         (pip-exe (lsp-installer--executable-find 'pip))
         (python-exe (or (executable-find "python3") (executable-find "python")))
         (venv-dir (expand-file-name "venv" server-dir))
         (bin-dir (expand-file-name "bin" server-dir)))
    (unless (and pip-exe python-exe)
      (lsp-installer--error "pip/python not found in PATH"))
    (lsp-installer--ensure-directory server-dir)
    (lsp-installer--ensure-directory bin-dir)
    ;; Create virtual environment
    (let ((exit-code (call-process python-exe nil "*lsp-installer*" t "-m" "venv" venv-dir)))
      (unless (= exit-code 0)
        (lsp-installer--error "Failed to create virtual environment (exit code: %d)" exit-code)))
    ;; Install package in venv
    (let* ((venv-pip (expand-file-name (if (eq system-type 'windows-nt) "Scripts/pip.exe" "bin/pip") venv-dir))
           (exit-code (call-process venv-pip nil "*lsp-installer*" t "install" package-spec)))
      (unless (= exit-code 0)
        (lsp-installer--error "pip install failed (exit code: %d)" exit-code)))
    ;; Create wrapper scripts
    (lsp-installer--create-pip-wrappers server-name venv-dir)))

(defun lsp-installer--create-pip-wrappers (server-name venv-dir)
  "Create wrapper scripts for Python executables in VENV-DIR."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (venv-bin (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") venv-dir)))
    (when (file-directory-p venv-bin)
      (dolist (file (directory-files venv-bin))
        (let ((full-path (expand-file-name file venv-bin)))
          (when (and (file-executable-p full-path)
                     (not (member file '("." ".." "python" "pip" "activate")))
                     (not (string-match-p "python\\|pip" file)))
            (let ((wrapper (expand-file-name file bin-dir)))
              (with-temp-file wrapper
                (insert "#!/bin/bash\nexec \"" full-path "\" \"$@\"\n"))
              (lsp-installer--make-executable wrapper))))))))

(defun lsp-installer--install-go (server-name source)
  "Install Go binary SOURCE for SERVER-NAME."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (go-exe (lsp-installer--executable-find 'go)))
    (unless go-exe
      (lsp-installer--error "Go not found in PATH"))
    (lsp-installer--ensure-directory bin-dir)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "GOPATH" (expand-file-name "go" server-dir))
      (setenv "GOBIN" bin-dir)
      (let ((exit-code (call-process go-exe nil "*lsp-installer*" t "install" source)))
        (unless (= exit-code 0)
          (lsp-installer--error "go install failed (exit code: %d)" exit-code))))))

(defun lsp-installer--install-dotnet (server-name source version)
  "Install .NET tool SOURCE for SERVER-NAME with optional VERSION."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (tools-dir (expand-file-name "tools" server-dir))
         (dotnet-exe (lsp-installer--executable-find 'dotnet)))
    (unless dotnet-exe
      (lsp-installer--error ".NET SDK not found in PATH"))
    (lsp-installer--ensure-directory tools-dir)
    (let ((exit-code (call-process dotnet-exe nil "*lsp-installer*" t "tool" "install"
                                  source "--tool-path" tools-dir)))
      (unless (= exit-code 0)
        (lsp-installer--error "dotnet tool install failed (exit code: %d)" exit-code)))))

;;; Download and extraction utilities

(defun lsp-installer--download-file (url target-file)
  "Download file from URL to TARGET-FILE."
  (lsp-installer--ensure-directory (file-name-directory target-file))
  (let ((curl-exe (lsp-installer--executable-find 'curl))
        (wget-exe (lsp-installer--executable-find 'wget)))
    (cond
     (curl-exe
      (let ((exit-code (call-process curl-exe nil "*lsp-installer*" t "-L" "-f"
                                    "--create-dirs" "-o" target-file url)))
        (unless (= exit-code 0)
          (lsp-installer--error "curl download failed (exit code: %d)" exit-code))))
     (wget-exe
      (let ((exit-code (call-process wget-exe nil "*lsp-installer*" t "-O" target-file url)))
        (unless (= exit-code 0)
          (lsp-installer--error "wget download failed (exit code: %d)" exit-code))))
     (t (lsp-installer--error "Neither curl nor wget found")))))

(defun lsp-installer--extract-archive (archive target-dir &optional strip-components)
  "Extract ARCHIVE to TARGET-DIR with optional STRIP-COMPONENTS."
  (lsp-installer--ensure-directory target-dir)
  (cond
   ;; TAR archives
   ((string-match-p "\\.tar\\.(gz|xz)\\|tgz" archive)
    (let* ((tar-exe (lsp-installer--executable-find 'tar))
           (compression (if (string-match-p "xz" archive) "J" "z"))
           (args `("-x" ,(concat compression "f") ,archive "-C" ,target-dir)))
      (unless tar-exe (lsp-installer--error "tar not found"))
      (when strip-components
        (setq args (append args (list "--strip-components" (number-to-string strip-components)))))
      (let ((exit-code (apply #'call-process tar-exe nil "*lsp-installer*" t args)))
        (unless (= exit-code 0)
          (lsp-installer--error "tar extraction failed (exit code: %d)" exit-code)))))
   ;; ZIP archives
   ((string-match-p "\\.zip" archive)
    (let ((unzip-exe (lsp-installer--executable-find 'unzip)))
      (unless unzip-exe (lsp-installer--error "unzip not found"))
      (let ((exit-code (call-process unzip-exe nil "*lsp-installer*" t "-o" archive "-d" target-dir)))
        (unless (= exit-code 0)
          (lsp-installer--error "unzip failed (exit code: %d)" exit-code)))))
   (t (lsp-installer--error "Unsupported archive format: %s" archive))))

;;; Binary installation with simplified selection

(defun lsp-installer--score-asset (asset-name server-name)
  "Score ASSET-NAME for SERVER-NAME based on platform compatibility."
  (let ((score 0)
        (name (downcase asset-name)))
    ;; OS scoring
    (cond
     ((eq system-type 'windows-nt)
      (if (string-match-p "win\\|windows" name) (cl-incf score 10)))
     ((eq system-type 'darwin)
      (if (string-match-p "osx\\|darwin\\|mac" name) (cl-incf score 10)))
     ((eq system-type 'gnu/linux)
      (if (string-match-p "linux" name) (cl-incf score 10))))
    ;; Architecture scoring
    (cond
     ((string-match "x86_64\\|amd64" system-configuration)
      (if (string-match-p "x64\\|x86_64\\|amd64" name) (cl-incf score 5)))
     ((string-match "aarch64\\|arm64" system-configuration)
      (if (string-match-p "arm64\\|aarch64" name) (cl-incf score 5))))
    ;; Avoid unwanted files
    (when (string-match-p "source\\|debug\\|symbols" name) (cl-decf score 20))
    ;; Server-specific scoring
    (when (string= server-name "omnisharp")
      (when (string-match-p "http\\|mono" name) (cl-decf score 15)))
    score))

(defun lsp-installer--install-github (server-name repo-path executable &optional options)
  "Install binary from GitHub release for SERVER-NAME."
  (let* ((api-url (format "https://api.github.com/repos/%s/releases/latest" repo-path))
         (temp-buffer (url-retrieve-synchronously api-url t)))
    (unless temp-buffer
      (lsp-installer--error "Failed to fetch GitHub API for %s" repo-path))
    (with-current-buffer temp-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (let* ((release-data (json-read))
             (assets (cdr (assq 'assets release-data)))
             ;; Find best asset using scoring
             (best-asset
              (cl-reduce (lambda (a b)
                          (if (> (lsp-installer--score-asset (cdr (assq 'name a)) server-name)
                                 (lsp-installer--score-asset (cdr (assq 'name b)) server-name))
                              a b))
                        (append assets nil))))
        (kill-buffer)
        (unless best-asset
          (lsp-installer--error "No suitable asset found for %s" server-name))
        (lsp-installer--message "Selected asset: %s" (cdr (assq 'name best-asset)))
        (lsp-installer--install-binary server-name
                                      (cdr (assq 'browser_download_url best-asset))
                                      executable options)))))

(defun lsp-installer--install-binary (server-name url executable &optional options)
  "Install binary from URL for SERVER-NAME."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (temp-dir (make-temp-file "lsp-installer-" t))
         (filename (file-name-nondirectory (car (split-string url "?"))))
         (temp-file (expand-file-name filename temp-dir))
         (target-subdir (plist-get options :target-subdir))
         (strip-components (plist-get options :strip-components))
         (extract-dir (if target-subdir
                         (expand-file-name target-subdir server-dir)
                       server-dir)))
    (unwind-protect
        (progn
          (lsp-installer--ensure-directory server-dir)
          (lsp-installer--ensure-directory bin-dir)
          (lsp-installer--download-file url temp-file)
          (if (string-match-p "\\.(?:tar\\.|zip)" filename)
              ;; Archive - extract it
              (progn
                (lsp-installer--extract-archive temp-file extract-dir strip-components)
                ;; Handle executable setup
                (when executable
                  (let ((exec-path (expand-file-name executable extract-dir)))
                    (when (file-exists-p exec-path)
                      (lsp-installer--make-executable exec-path)
                      ;; Create symlink in bin if needed
                      (let ((bin-path (expand-file-name (file-name-nondirectory executable) bin-dir)))
                        (unless (file-exists-p bin-path)
                          (make-symbolic-link exec-path bin-path)))))))
            ;; Single file - copy to bin
            (let ((target-file (expand-file-name
                               (or (file-name-nondirectory executable) filename)
                               bin-dir)))
              (copy-file temp-file target-file t)
              (lsp-installer--make-executable target-file))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; JDTLS special handling

(defun lsp-installer--install-jdtls (server-name url options)
  "Install Eclipse JDT Language Server with proper setup."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (target-subdir (plist-get options :target-subdir)))
    ;; Install as binary first
    (lsp-installer--install-binary server-name url "" options)
    ;; Create wrapper script
    (lsp-installer--create-jdtls-wrapper server-name
                                         (if target-subdir
                                             (expand-file-name target-subdir server-dir)
                                           server-dir))))

(defun lsp-installer--create-jdtls-wrapper (server-name jdtls-dir)
  "Create wrapper script for JDTLS in SERVER-NAME installation."
  (let* ((server-dir (lsp-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (wrapper (expand-file-name "jdtls" bin-dir))
         (config-dir (cond
                     ((eq system-type 'darwin) "config_mac")
                     ((eq system-type 'gnu/linux) "config_linux")
                     ((eq system-type 'windows-nt) "config_win")
                     (t "config_linux")))
         (launcher-jars (file-expand-wildcards
                        (expand-file-name "plugins/org.eclipse.equinox.launcher_*.jar" jdtls-dir))))
    (unless launcher-jars
      (lsp-installer--error "Could not find Eclipse launcher jar"))
    (with-temp-file wrapper
      (insert "#!/bin/bash\n")
      (insert "JAVA=java\n")
      (insert "[[ -n \"$JAVA_HOME\" ]] && JAVA=\"$JAVA_HOME/bin/java\"\n")
      (insert "WORKSPACE=\"${1:-$PWD}\"\n")
      (insert "shift\n")
      (insert (format "exec \"$JAVA\" -Xmx1G --add-modules=ALL-SYSTEM \\\n"))
      (insert "  --add-opens java.base/java.util=ALL-UNNAMED \\\n")
      (insert "  --add-opens java.base/java.lang=ALL-UNNAMED \\\n")
      (insert "  -Declipse.application=org.eclipse.jdt.ls.core.id1 \\\n")
      (insert "  -Dosgi.bundles.defaultStartLevel=4 \\\n")
      (insert "  -Declipse.product=org.eclipse.jdt.ls.core.product \\\n")
      (insert (format "  -jar \"%s\" \\\n" (car launcher-jars)))
      (insert (format "  -configuration \"%s\" \\\n" (expand-file-name config-dir jdtls-dir)))
      (insert "  -data \"$HOME/.cache/jdtls-workspace\" \\\n")
      (insert "  \"$@\"\n"))
    (lsp-installer--make-executable wrapper)))

;;; Main installation dispatcher

(defun lsp-installer--dispatch-installation (server-name config)
  "Dispatch installation for SERVER-NAME based on CONFIG."
  (let* ((method (plist-get config :install-method))
         (source (plist-get config :source))
         (executable (plist-get config :executable))
         (version (plist-get config :version))
         (options (plist-get config :options)))
    (lsp-installer--message "Installing %s via %s..." server-name method)
    (lsp-installer--with-error-handling server-name
      (cond
       ((string= method "npm")
        (lsp-installer--install-npm server-name source version))
       ((string= method "pip")
        (lsp-installer--install-pip server-name source version))
       ((string= method "go")
        (lsp-installer--install-go server-name source))
       ((string= method "dotnet")
        (lsp-installer--install-dotnet server-name source version))
       ((string= method "github")
        (lsp-installer--install-github server-name source executable options))
       ((string= method "binary")
        (if (string= server-name "jdtls")
            (lsp-installer--install-jdtls server-name source options)
          (lsp-installer--install-binary server-name source executable options)))
       (t (lsp-installer--error "Unsupported install method: %s" method)))
      (lsp-installer--add-to-exec-path server-name)
      (lsp-installer--message "Successfully installed %s" server-name))))

;;; Interactive commands

;;;###autoload
(defun lsp-installer-install-server (server-name)
  "Install language server SERVER-NAME."
  (interactive (list (completing-read "Install server: "
                                     (lsp-installer--list-available-servers) nil t)))
  (let ((config (lsp-installer--get-server-config server-name)))
    (lsp-installer--validate-config server-name config)
    (when (and (lsp-installer--server-installed-p server-name)
               (not (y-or-n-p (format "Server %s already installed. Reinstall? " server-name))))
      (lsp-installer--message "Installation cancelled")
      (return))
    (lsp-installer--dispatch-installation server-name config)))

;;;###autoload
(defun lsp-installer-uninstall-server (server-name)
  "Uninstall language server SERVER-NAME."
  (interactive (list (completing-read "Uninstall server: "
                                     (lsp-installer--list-installed-servers) nil t)))
  (unless (lsp-installer--server-installed-p server-name)
    (lsp-installer--error "Server %s is not installed" server-name))
  (when (y-or-n-p (format "Really uninstall server %s? " server-name))
    (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
      (when (file-directory-p server-dir)
        (delete-directory server-dir t)
        (lsp-installer--message "Successfully uninstalled %s" server-name)))))

;;;###autoload
(defun lsp-installer-update-server (server-name)
  "Update language server SERVER-NAME by reinstalling."
  (interactive (list (completing-read "Update server: "
                                     (lsp-installer--list-installed-servers) nil t)))
  (let ((config (lsp-installer--get-server-config server-name)))
    (lsp-installer--validate-config server-name config)
    (unless (lsp-installer--server-installed-p server-name)
      (lsp-installer--error "Server %s is not installed" server-name))
    (lsp-installer--message "Updating %s..." server-name)
    (let ((server-dir (lsp-installer--get-server-install-dir server-name)))
      (when (file-directory-p server-dir)
        (delete-directory server-dir t)))
    (lsp-installer--dispatch-installation server-name config)
    (lsp-installer--message "Successfully updated %s" server-name)))

;;;###autoload
(defun lsp-installer-list-servers ()
  "List all available and installed language servers."
  (interactive)
  (let ((available (lsp-installer--list-available-servers))
        (installed (lsp-installer--list-installed-servers)))
    (with-current-buffer (get-buffer-create "*LSP Installer*")
      (erase-buffer)
      (insert "Language Server Status\n======================\n\n")
      (insert "Available servers:\n")
      (dolist (server available)
        (insert (format "  %s%s\n" server
                       (if (member server installed) " [INSTALLED]" ""))))
      (when installed
        (insert "\nInstalled servers:\n")
        (dolist (server installed)
          (insert (format "  %s\n" server))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun lsp-installer-setup ()
  "Setup language server paths in exec-path."
  (interactive)
  (let ((installed (lsp-installer--list-installed-servers)))
    (if installed
        (progn
          (lsp-installer--message "Setting up paths for %d servers..." (length installed))
          (dolist (server installed)
            (lsp-installer--add-to-exec-path server))
          (lsp-installer--message "Language server paths setup complete"))
      (lsp-installer--message "No language servers installed"))))

;;; Backward compatibility

;;;###autoload
(define-obsolete-function-alias 'ls-installer-setup 'lsp-installer-setup "0.2.0"
  "Use `lsp-installer-setup' instead.")

;;; Integration helpers

(defun lsp-installer-get-server-executable-path (server-name)
  "Get the full path to SERVER-NAME's executable."
  (when (lsp-installer--server-installed-p server-name)
    (let* ((config (lsp-installer--get-server-config server-name))
           (executable (plist-get config :executable))
           (server-dir (lsp-installer--get-server-install-dir server-name))
           (bin-paths (list (expand-file-name "bin" server-dir)
                           (expand-file-name "node_modules/.bin" server-dir)
                           (expand-file-name "tools" server-dir))))
      (when executable
        (or (cl-some (lambda (path)
                      (let ((full-path (expand-file-name executable path)))
                        (when (file-executable-p full-path) full-path)))
                    bin-paths)
            (let ((full-path (expand-file-name executable server-dir)))
              (when (file-executable-p full-path) full-path))
            (executable-find executable))))))

(provide 'lsp-installer)

;;; lsp-installer.el ends here