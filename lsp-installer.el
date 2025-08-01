;;; lsp-installer.el --- Automatic language server installation with exec-path integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Assistant
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tools, lsp
;; URL: https://github.com/example/lsp-installer

;;; Commentary:

;; This package provides automatic installation and configuration of language
;; servers with exec-path based approach for better system integration.
;;
;; Features:
;; - Automatic detection and installation of language servers
;; - Platform-specific binary downloads
;; - Integration with package managers (npm, cargo, pip, go, gem)
;; - exec-path based server discovery (no hardcoded paths)
;; - ELD-based configuration for flexibility and type safety
;; - Works with any LSP client, not limited to specific implementations
;;
;; Usage:
;; M-x lsp-installer-install-server
;; M-x lsp-installer-install-all
;; M-x lsp-installer-update-server

;;; Code:

(require 'url)
(require 'json)
(require 'tar-mode)

(defgroup lsp-installer nil
  "Language server installer."
  :group 'tools
  :prefix "lsp-installer-")

(defcustom lsp-installer-dir
  (expand-file-name "language-servers" user-emacs-directory)
  "Directory to install language servers."
  :type 'directory
  :group 'lsp-installer)

(defcustom lsp-installer-confirm-downloads t
  "Confirm before downloading language servers."
  :type 'boolean
  :group 'lsp-installer)

(defcustom lsp-installer-update-exec-path t
  "Automatically update exec-path when installing servers."
  :type 'boolean
  :group 'lsp-installer)

(defvar lsp-installer--servers-file
  (expand-file-name "servers.eld"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the server definitions file.")

(defvar lsp-installer--added-paths nil
  "List of paths added to exec-path by this package.")

(defvar lsp-installer--server-definitions nil
  "Cache of server definitions.")

;;; Utility macros and functions

(defmacro lsp-installer--with-error-handling (server-name &rest body)
  "Execute BODY with error handling for SERVER-NAME operations."
  (declare (indent 1))
  `(condition-case err
       (progn
         ,@body)
     (error
      (message "Failed to process %s: %s"
               ,server-name
               (error-message-string err))
      (signal (car err) (cdr err)))))

(defmacro lsp-installer--define-installer
    (name args docstring &rest body)
  "Define an installer function NAME with ARGS, DOCSTRING and BODY.
Automatically handles common error patterns and directory creation."
  (declare (indent 3) (doc-string 3))
  (let ((func-name
         (intern (format "lsp-installer--install-via-%s" name))))
    `(defun ,func-name ,args
       ,docstring
       ,@body)))

(defun lsp-installer--ensure-directory (dir)
  "Ensure DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun lsp-installer--system-info ()
  "Return enhanced system information as a plist."
  (let* ((system-type-mapped
          (pcase system-type
            ('darwin 'macos)
            ('windows-nt 'windows)
            ('gnu/linux 'linux)
            (_ 'unix)))
         (arch-mapped
          (pcase system-configuration
            ((pred (string-match-p "x86_64")) 'x86_64)
            ((pred (string-match-p "aarch64\\|arm64")) 'arm64)
            ((pred (string-match-p "i[3-6]86")) 'x86)
            (_ 'unknown))))
    (list
     :system system-type-mapped
     :arch arch-mapped
     :exe-suffix
     (if (eq system-type 'windows-nt)
         ".exe"
       "")
     ;; Special mapping for URLs
     :system-url
     (pcase system-type-mapped
       ('macos "apple-darwin")
       ('linux "unknown-linux-gnu")
       ('windows "pc-windows-msvc")
       (_ (symbol-name system-type-mapped)))
     :arch-url
     (pcase arch-mapped
       ('x86_64 "x86_64")
       ('arm64 "aarch64")
       (_ (symbol-name arch-mapped))))))

(defun lsp-installer--setup-jdtls (exe-file config)
  "Setup JDTLS after installation - now properly integrated."
  (let* ((jdtls-bin-dir (file-name-directory exe-file))
         ;; 修正: 親ディレクトリを正しく取得
         (jdtls-root-dir (file-name-directory (directory-file-name jdtls-bin-dir)))
         (system-info (lsp-installer--system-info))
         (system-type (plist-get system-info :system))
         (config-dir
          (pcase system-type
            ('linux "config_linux")
            ('macos "config_mac")
            ('windows "config_win")
            (_ "config_linux")))
         ;; jdtlsルートディレクトリ直下のpluginsディレクトリを指定
         (plugins-dir (expand-file-name "plugins" jdtls-root-dir)))

    (message "exe-file: %s" exe-file)
    (message "jdtls-bin-dir: %s" jdtls-bin-dir)
    (message "jdtls-root-dir: %s" jdtls-root-dir)
    (message "plugins-dir: %s" plugins-dir)
    
    ;; Make the startup script executable
    (lsp-installer--make-executable exe-file)

    ;; Check for the existence of the plugins directory
    (unless (file-directory-p plugins-dir)
      (error "JDTLS plugins directory not found: %s" plugins-dir))

    ;; Create a platform-specific wrapper script
    (let ((wrapper-script (expand-file-name "jdtls" jdtls-bin-dir))
          (launcher-jar
           (car
            (directory-files
             plugins-dir
             t "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))))
      (unless launcher-jar
        (error "JDTLS launcher jar not found in %s" plugins-dir))

      (with-temp-file wrapper-script
        (insert "#!/bin/bash\n")
        (insert
         (format
          "exec java -jar %s -configuration %s/%s -data \"$@\"\n"
          launcher-jar 
          jdtls-root-dir
          config-dir)))
      (lsp-installer--make-executable wrapper-script))))

(defun lsp-installer--expand-template (template server-config)
  "Expand TEMPLATE with values from SERVER-CONFIG and system info."
  (let ((system-info (lsp-installer--system-info)))
    (replace-regexp-in-string
     "{\([^}]+\)}"
     (lambda (match)
       (let* ((key (intern (match-string 1 match)))
              (value
               (or (plist-get system-info key)
                   (plist-get server-config key))))
         (if value
             (format "%s" value)
           match)))
     template)))

(defun lsp-installer--add-to-exec-path (directory)
  "Add DIRECTORY to exec-path if not already present."
  (when (file-directory-p directory)
    (let ((normalized-dir (file-truename directory)))
      (unless (member normalized-dir exec-path)
        (add-to-list 'exec-path normalized-dir)
        (add-to-list 'lsp-installer--added-paths normalized-dir)
        (message "Added %s to exec-path" normalized-dir)))))

(defun lsp-installer--get-bin-directory (server-name installer)
  "Get the bin directory for SERVER-NAME based on INSTALLER type."
  (let ((base-dir
         (expand-file-name (symbol-name server-name)
                           lsp-installer-dir)))
    (pcase installer
      ('npm (expand-file-name "node_modules/.bin" base-dir))
      ((or 'cargo 'go 'gem 'dotnet 'coursier 'cabal)
       (expand-file-name "bin" base-dir))
      ('composer (expand-file-name "vendor/bin" base-dir))
      ('luarocks (expand-file-name "bin" base-dir))
      ('nimble (expand-file-name "bin" base-dir))
      ('opam (expand-file-name "bin" base-dir))
      ('pip base-dir)
      ('binary base-dir)
      (_ base-dir))))

;;; Server definition management

(defun lsp-installer--load-servers ()
  "Load server definitions from ELD file."
  (when (and lsp-installer--servers-file
             (file-exists-p lsp-installer--servers-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents lsp-installer--servers-file)
          (read (current-buffer)))
      (error
       (message "Warning: Failed to load server definitions: %s"
                (error-message-string err))
       nil))))

(defun lsp-installer--get-servers ()
  "Get server definitions, loading if necessary."
  (unless lsp-installer--server-definitions
    (setq lsp-installer--server-definitions
          (lsp-installer--load-servers)))
  lsp-installer--server-definitions)

(defun lsp-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME."
  (alist-get server-name (lsp-installer--get-servers)))

(defun lsp-installer--available-servers ()
  "Get list of available server names."
  (mapcar #'car (lsp-installer--get-servers)))

(defun lsp-installer--servers-for-mode (mode)
  "Get servers available for MODE."
  (cl-loop
   for (server-name . config) in (lsp-installer--get-servers) when
   (let ((modes (plist-get config :modes)))
     (or (eq modes t) (memq mode modes)))
   collect server-name))

;;; Installation status management

(defun lsp-installer--get-package-name (server-name config)
  "Get package name for SERVER-NAME, fallback to server name if not specified."
  (or (plist-get config :package-name) (symbol-name server-name)))

(defun lsp-installer--server-installed-p (server-name)
  "Check if SERVER-NAME is installed by checking if its directory exists."
  (file-directory-p
   (expand-file-name (symbol-name server-name) lsp-installer-dir)))

(defun lsp-installer--get-installed-servers ()
  "Get list of installed servers by checking directories."
  (when (file-directory-p lsp-installer-dir)
    (cl-loop
     for
     dir
     in
     (directory-files lsp-installer-dir nil "^[^.]")
     when
     (file-directory-p (expand-file-name dir lsp-installer-dir))
     collect
     (intern dir))))

;;; Download and decompression utilities

(defmacro lsp-installer--with-cleanup (cleanup-form &rest body)
  "Execute BODY with CLEANUP-FORM executed even if error occurs."
  (declare (indent 1))
  (let ((success-var (gensym "success")))
    `(let ((,success-var nil))
       (unwind-protect
           (prog1 (progn
                    ,@body)
             (setq ,success-var t))
         (unless ,success-var
           ,cleanup-form)))))

(defun lsp-installer--download-file (url dest-file)
  "Download URL to DEST-FILE using Emacs's url-copy-file (handles redirects robustly)."
  (lsp-installer--with-cleanup (when (file-exists-p dest-file)
                                 (delete-file dest-file))
                               (url-copy-file url dest-file t)))

(defun lsp-installer--decompress-file (file dest-dir method)
  "Decompress FILE to DEST-DIR using METHOD."
  (unless (file-exists-p file)
    (error "File does not exist: %s" file))
  (lsp-installer--ensure-directory dest-dir)
  (pcase method
    ('gzip
     (let ((dest-file
            (expand-file-name (file-name-sans-extension
                               (file-name-nondirectory file))
                              dest-dir)))
       (unless (zerop (call-process "gunzip" file t nil "-c"))
         (error "Failed to decompress gzip file: %s" file))
       dest-file))
    ('zip
     (unless (zerop
              (call-process "unzip"
                            nil
                            nil
                            nil
                            "-q"
                            file
                            "-d"
                            dest-dir))
       (error "Failed to decompress zip file: %s" file)))
    ('tar.gz
     (unless (zerop
              (call-process "tar"
                            nil
                            nil
                            nil
                            "-xzf"
                            file
                            "-C"
                            dest-dir))
       (error "Failed to decompress tar.gz file: %s" file)))
    ('tar.xz
     (unless (zerop
              (call-process "tar"
                            nil
                            nil
                            nil
                            "-xJf"
                            file
                            "-C"
                            dest-dir))
       (error "Failed to decompress tar.xz file: %s" file)))
    (_ (error "Unsupported compression method: %s" method))))

(defun lsp-installer--make-executable (file)
  "Make FILE executable."
  (when (file-exists-p file)
    (set-file-modes file (logior (file-modes file) #o111))))

;;; Package manager installers

(lsp-installer--define-installer npm
    (package-name dest-dir &optional executable-name)
    "Install PACKAGE-NAME via npm to DEST-DIR."
  (let ((default-directory dest-dir)
        (bin-dir (expand-file-name "node_modules/.bin" dest-dir)))
    (lsp-installer--ensure-directory dest-dir)
    (unless (zerop
             (call-process "npm" nil nil nil "install" package-name))
      (error "Failed to install %s via npm" package-name))
    bin-dir))

(lsp-installer--define-installer cargo (package-name dest-dir)
                                 "Install PACKAGE-NAME via cargo to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (unless (zerop
             (call-process "cargo"
                           nil
                           nil
                           nil
                           "install"
                           "--root"
                           dest-dir
                           package-name))
      (error "Failed to install %s via cargo" package-name))
    bin-dir))

(lsp-installer--define-installer pip (package-name dest-dir)
                                 "Install PACKAGE-NAME via pip to DEST-DIR."
  (lsp-installer--ensure-directory dest-dir)
  (unless (zerop
           (call-process "pip"
                         nil
                         nil
                         nil
                         "install"
                         "--target"
                         dest-dir
                         package-name))
    (error "Failed to install %s via pip" package-name))
  dest-dir)

(lsp-installer--define-installer go (package-name dest-dir)
                                 "Install PACKAGE-NAME via go install to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (let ((process-environment
           (cons (format "GOBIN=%s" bin-dir) process-environment)))
      (unless (zerop
               (call-process "go" nil nil nil "install" package-name))
        (error "Failed to install %s via go install" package-name))
      bin-dir)))

(lsp-installer--define-installer gem (package-name dest-dir)
                                 "Install PACKAGE-NAME via gem to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory dest-dir)
    (unless (zerop
             (call-process "gem"
                           nil
                           nil
                           nil
                           "install"
                           package-name
                           "--install-dir"
                           dest-dir
                           "--bindir"
                           bin-dir))
      (error "Failed to install %s via gem" package-name))
    bin-dir))

(lsp-installer--define-installer dotnet (package-name dest-dir)
                                 "Install PACKAGE-NAME via dotnet tool to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (unless (zerop
             (call-process "dotnet"
                           nil
                           nil
                           nil
                           "tool"
                           "install"
                           package-name
                           "--tool-path"
                           bin-dir))
      (error
       "Failed to install %s via dotnet tool install" package-name))
    bin-dir))

(lsp-installer--define-installer composer (package-name dest-dir)
                                 "Install PACKAGE-NAME via composer to DEST-DIR."
  (let ((bin-dir (expand-file-name "vendor/bin" dest-dir)))
    (lsp-installer--ensure-directory dest-dir)
    (let ((default-directory dest-dir))
      (unless (zerop
               (call-process "composer"
                             nil
                             nil
                             nil
                             "require"
                             package-name))
        (error "Failed to install %s via composer" package-name)))
    bin-dir))

(lsp-installer--define-installer coursier (package-name dest-dir)
                                 "Install PACKAGE-NAME via coursier to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (unless (zerop
             (call-process "coursier"
                           nil
                           nil
                           nil
                           "install"
                           "--install-dir"
                           bin-dir
                           package-name))
      (error "Failed to install %s via coursier" package-name))
    bin-dir))

(lsp-installer--define-installer luarocks (package-name dest-dir)
                                 "Install PACKAGE-NAME via luarocks to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory dest-dir)
    (unless (zerop
             (call-process "luarocks"
                           nil
                           nil
                           nil
                           "install"
                           "--tree"
                           dest-dir
                           package-name))
      (error "Failed to install %s via luarocks" package-name))
    bin-dir))

(lsp-installer--define-installer nimble (package-name dest-dir)
                                 "Install PACKAGE-NAME via nimble to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (let ((process-environment
           (cons
            (format "NIMBLE_DIR=%s" dest-dir) process-environment)))
      (unless (zerop
               (call-process "nimble"
                             nil
                             nil
                             nil
                             "install"
                             "--accept"
                             package-name))
        (error "Failed to install %s via nimble" package-name)))
    bin-dir))

(lsp-installer--define-installer opam (package-name dest-dir)
                                 "Install PACKAGE-NAME via opam to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory dest-dir)
    (unless (zerop
             (call-process "opam"
                           nil
                           nil
                           nil
                           "install"
                           "--destdir"
                           dest-dir
                           package-name))
      (error "Failed to install %s via opam" package-name))
    bin-dir))

(lsp-installer--define-installer cabal (package-name dest-dir)
                                 "Install PACKAGE-NAME via cabal to DEST-DIR."
  (let ((bin-dir (expand-file-name "bin" dest-dir)))
    (lsp-installer--ensure-directory bin-dir)
    (unless (zerop
             (call-process "cabal"
                           nil
                           nil
                           nil
                           "install"
                           "--installdir"
                           bin-dir
                           package-name))
      (error "Failed to install %s via cabal" package-name))
    bin-dir))

;;; Binary installer - 修正：適切なbin-directory戻り値

(defun lsp-installer--install-binary (server-name config)
  "Install binary for SERVER-NAME using CONFIG."
  (let* ((url
          (lsp-installer--expand-template
           (plist-get config :download-url) config))
         (dest-dir
          (expand-file-name (symbol-name server-name)
                            lsp-installer-dir))
         (temp-file (make-temp-file "lsp-installer-"))
         (executable-name
          (or (plist-get config :executable-name)
              (symbol-name server-name)))
         (system-info (lsp-installer--system-info))
         (exe-file
          (expand-file-name (concat
                             executable-name
                             (plist-get system-info :exe-suffix))
                            dest-dir)))

    (lsp-installer--ensure-directory dest-dir)
    (message "Downloading %s from %s..." server-name url)
    (lsp-installer--download-file url temp-file)

    (if-let ((method (plist-get config :decompress)))
        (progn
          (message "Decompressing %s..." server-name)
          (lsp-installer--decompress-file temp-file dest-dir method)
          (delete-file temp-file))
      (rename-file temp-file exe-file))

    (lsp-installer--make-executable exe-file)

    (when-let ((hook (plist-get config :post-install-hook)))
      (cond
       ((eq hook 'jdtls-setup)
        (lsp-installer--setup-jdtls exe-file config))
       ((functionp hook)
        (funcall hook exe-file config))
       (t
        (message "Warning: Unknown post-install-hook: %s" hook))))

    (lsp-installer--get-bin-directory server-name 'binary)))

;;; Main installation function

(defun lsp-installer--check-platform-support (config)
  "Check if current platform is supported by CONFIG."
  (let ((platforms (plist-get config :platforms))
        (current-platform
         (plist-get (lsp-installer--system-info) :system)))
    (or (null platforms) ; No platform restriction
        (memq current-platform platforms))))

(defun lsp-installer--install-server-internal (server-name)
  "Internal function to install SERVER-NAME."
  (let* ((config (lsp-installer--get-server-config server-name))
         (installer (plist-get config :installer)))

    (unless config
      (error "Unknown server: %s" server-name))
    (unless installer
      (error
       "Server %s has no :installer defined in its config"
       server-name))

    (unless (lsp-installer--check-platform-support config)
      (error
       "Server %s is not supported on %s"
       server-name
       (plist-get (lsp-installer--system-info) :system)))

    (when-let ((condition (plist-get config :condition)))
      (unless (funcall condition)
        (error "Condition not met for server: %s" server-name)))

    (let* ((package-name
            (lsp-installer--get-package-name server-name config))
           (dest-dir
            (expand-file-name (symbol-name server-name)
                              lsp-installer-dir))
           ;; 修正：統一されたbin-directory処理
           (bin-directory
            (pcase installer
              ('npm
               (lsp-installer--install-via-npm
                package-name
                dest-dir
                (plist-get config :executable-name)))
              ('cargo
               (lsp-installer--install-via-cargo
                package-name dest-dir))
              ('pip
               (lsp-installer--install-via-pip package-name dest-dir))
              ('go
               (lsp-installer--install-via-go package-name dest-dir))
              ('gem
               (lsp-installer--install-via-gem package-name dest-dir))
              ('dotnet
               (lsp-installer--install-via-dotnet
                package-name dest-dir))
              ('composer
               (lsp-installer--install-via-composer
                package-name dest-dir))
              ('coursier
               (lsp-installer--install-via-coursier
                package-name dest-dir))
              ('luarocks
               (lsp-installer--install-via-luarocks
                package-name dest-dir))
              ('nimble
               (lsp-installer--install-via-nimble
                package-name dest-dir))
              ('opam
               (lsp-installer--install-via-opam
                package-name dest-dir))
              ('cabal
               (lsp-installer--install-via-cabal
                package-name dest-dir))
              ('binary
               (lsp-installer--install-binary server-name config))
              ('system
               (error "System installer doesn't need installation"))
              (_ (error "Unknown installer type: %s" installer)))))

      ;; Add bin directory to exec-path
      (when (and lsp-installer-update-exec-path
                 bin-directory
                 (file-directory-p bin-directory))
        (lsp-installer--add-to-exec-path bin-directory))

      (message "Successfully installed %s, added %s to exec-path"
               server-name
               bin-directory)
      bin-directory)))

;;; Interactive commands

(defmacro lsp-installer--define-command
    (name args docstring &rest body)
  "Define an interactive command NAME with ARGS, DOCSTRING and BODY."
  (declare (indent 3) (doc-string 3))
  `(defun ,(intern (format "lsp-installer-%s" name)) ,args
     ,docstring
     (interactive)
     ,@body))

;;;###autoload
(defun lsp-installer-install-server (server-name)
  "Install language server SERVER-NAME."
  (interactive (list
                (intern
                 (completing-read "Install server: "
                                  (lsp-installer--available-servers)
                                  nil
                                  t))))
  (when (or (not lsp-installer-confirm-downloads)
            (y-or-n-p
             (format "Install language server %s? " server-name)))
    (lsp-installer--with-error-handling server-name
      (lsp-installer--install-server-internal server-name))))

;;;###autoload
(defun lsp-installer-install-for-mode (&optional mode)
  "Install language servers for MODE (defaults to current major mode)."
  (interactive)
  (let* ((target-mode (or mode major-mode))
         (servers (lsp-installer--servers-for-mode target-mode)))
    (cond
     ((null servers)
      (message "No servers available for mode: %s" target-mode))
     ((= (length servers) 1)
      (lsp-installer-install-server (car servers)))
     (t
      (let ((server
             (intern
              (completing-read
               (format "Install server for %s: " target-mode) servers
               nil t))))
        (lsp-installer-install-server server))))))

;;;###autoload
(defun lsp-installer-install-for-current-mode ()
  "Install language servers for current major mode."
  (interactive)
  (lsp-installer-install-for-mode major-mode))

;;;###autoload
(defun lsp-installer-install-all ()
  "Install all available language servers."
  (interactive)
  (when
      (y-or-n-p
       "Install all available language servers? This may take a while.")
    (let ((servers (lsp-installer--available-servers))
          (failed nil)
          (succeeded nil))
      (dolist (server servers)
        (message "Installing %s..." server)
        (condition-case err
            (progn
              (lsp-installer--install-server-internal server)
              (push server succeeded))
          (error
           (message "Failed to install %s: %s"
                    server
                    (error-message-string err))
           (push server failed))))
      (message "Installation complete. Succeeded: %d, Failed: %d"
               (length succeeded)
               (length failed))
      (when failed
        (message "Failed servers: %s" failed)))))

;;;###autoload
(defun lsp-installer-update-server (server-name)
  "Update language server SERVER-NAME."
  (interactive (list
                (intern
                 (completing-read
                  "Update server: "
                  (lsp-installer--get-installed-servers)
                  nil
                  t))))
  (let ((server-dir
         (expand-file-name (symbol-name server-name)
                           lsp-installer-dir)))
    (when (file-directory-p server-dir)
      (delete-directory server-dir t))
    (lsp-installer-install-server server-name)))

;;;###autoload
(defun lsp-installer-update-all ()
  "Update all installed language servers."
  (interactive)
  (let ((installed-servers (lsp-installer--get-installed-servers)))
    (if (null installed-servers)
        (message "No servers are currently installed")
      (when
          (y-or-n-p
           (format
            "Update all %d installed servers? This may take a while."
            (length installed-servers)))
        (let ((failed nil)
              (updated 0))
          (dolist (server installed-servers)
            (message "Updating %s..." server)
            (condition-case err
                (progn
                  (let ((server-dir
                         (expand-file-name (symbol-name server)
                                           lsp-installer-dir)))
                    (when (file-directory-p server-dir)
                      (delete-directory server-dir t))
                    (lsp-installer--install-server-internal server))
                  (setq updated (1+ updated)))
              (error
               (message "Failed to update %s: %s"
                        server
                        (error-message-string err))
               (push server failed))))
          (message "Update complete. Updated: %d, Failed: %d"
                   updated
                   (length failed))
          (when failed
            (message "Failed servers: %s" failed)))))))

;;;###autoload
(defun lsp-installer-uninstall-server (server-name)
  "Uninstall language server SERVER-NAME."
  (interactive (list
                (intern
                 (completing-read
                  "Uninstall server: "
                  (lsp-installer--get-installed-servers)
                  nil
                  t))))
  (let* ((server-dir
          (expand-file-name (symbol-name server-name)
                            lsp-installer-dir))
         (config (lsp-installer--get-server-config server-name))
         (installer (and config (plist-get config :installer)))
         (bin-dir
          (when installer
            (lsp-installer--get-bin-directory
             server-name installer))))
    (when (and (file-directory-p server-dir)
               (y-or-n-p (format "Delete %s? " server-dir)))
      (delete-directory server-dir t)
      ;; Remove from exec-path if we added it
      (when (and bin-dir (member bin-dir lsp-installer--added-paths))
        (setq exec-path (delete bin-dir exec-path))
        (setq lsp-installer--added-paths
              (delete bin-dir lsp-installer--added-paths)))
      (message "Uninstalled %s%s"
               server-name
               (if bin-dir
                   " and removed from exec-path"
                 "")))))

;;;###autoload
(defun lsp-installer-list-servers ()
  "List all available language servers."
  (interactive)
  (let ((servers (lsp-installer--get-servers)))
    (with-help-window "*Language Server Installer Servers*"
      (princ "Available Language Servers:\n\n")
      (dolist (server-def servers)
        (let* ((name (car server-def))
               (config (cdr server-def))
               (modes (plist-get config :modes))
               (installer (plist-get config :installer))
               (installed-p (lsp-installer--server-installed-p name))
               (executable-name
                (or (plist-get config :executable-name)
                    (symbol-name name)))
               (available-p (executable-find executable-name)))
          (princ (format "• %s" name))
          (cond
           (installed-p
            (princ " [INSTALLED]"))
           (available-p
            (princ " [AVAILABLE IN PATH]")))
          (princ "\n")
          (when modes
            (princ (format "  Modes: %s\n" modes)))
          (when installer
            (princ (format "  Installer: %s\n" installer)))
          (princ (format "  Executable: %s\n" executable-name))
          (princ "\n"))))))

;;;###autoload
(defun lsp-installer-show-exec-path ()
  "Show current exec-path and highlight paths added by lsp-installer."
  (interactive)
  (with-help-window "*Language Server Installer exec-path*"
    (princ "Current exec-path:\n\n")
    (dolist (path exec-path)
      (if (member path lsp-installer--added-paths)
          (princ (format "• %s [ADDED BY LSP-INSTALLER]\n" path))
        (princ (format "• %s\n" path))))))

;;;###autoload
(defun lsp-installer-add-installed-bins-to-exec-path ()
  "Add all installed server bin directories under lsp-installer-dir to exec-path."
  (interactive)
  (let ((servers (lsp-installer--available-servers))
        (added-count 0))
    (dolist (server servers)
      (when (lsp-installer--server-installed-p server)
        (let* ((config (lsp-installer--get-server-config server))
               (installer (and config (plist-get config :installer)))
               (bin-dir
                (when installer
                  (lsp-installer--get-bin-directory
                   server installer))))
          (when (and bin-dir (file-directory-p bin-dir))
            (lsp-installer--add-to-exec-path bin-dir)
            (setq added-count (1+ added-count))))))
    (message "Added %d server bin directories to exec-path"
             added-count)))

(provide 'lsp-installer)

;;; lsp-installer.el ends here
