;;; lsp-installer-config.el --- Configuration management for lsp-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains configuration management functions for lsp-installer.

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Variables

(defvar lsp-installer--servers-cache nil
  "Cache for loaded server configurations.")

;;; Configuration loading functions

(defun lsp-installer--load-servers-config (&optional force-reload)
  "Load server configurations from servers.eld file.

If FORCE-RELOAD is non-nil, reload even if cache exists.
Return the loaded configuration or cached version."
  (when (or force-reload
            (and (not lsp-installer--servers-cache)
                 (file-exists-p lsp-installer-servers-file)))

    (unless (file-exists-p lsp-installer-servers-file)
      (lsp-installer--error "Server configuration file not found: %s"
                           lsp-installer-servers-file))

    (condition-case err
        (with-temp-buffer
          (insert-file-contents lsp-installer-servers-file)
          (goto-char (point-min))
          (let ((config (read (current-buffer))))
            ;; Validate configuration format
            (unless (lsp-installer--validate-config-format config)
              (lsp-installer--error "Invalid configuration format in %s"
                                   lsp-installer-servers-file))

            (setq lsp-installer--servers-cache config)
            (when force-reload
              (lsp-installer--message "Reloaded server configurations from %s"
                                     lsp-installer-servers-file))))
      (error
       (lsp-installer--error "Failed to load configuration: %s"
                            (error-message-string err)))))
  lsp-installer--servers-cache)

(defun lsp-installer--validate-config-format (config)
  "Validate that CONFIG has the expected format.

Return t if valid, nil otherwise."
  (and (listp config)
       (or (null config) ; Empty list is valid
           (and (listp (car config))
                (plist-get (car config) :name)))))

(defun lsp-installer-reload-servers-config ()
  "Reload server configurations from servers.eld file.
This clears the cache and forces a fresh read of the configuration file."
  (interactive)
  (setq lsp-installer--servers-cache nil)
  (lsp-installer--load-servers-config t)
  (lsp-installer--message
   "Server configurations reloaded successfully"))

(defun lsp-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME.

Return the configuration plist for the server, or nil if not found."
  (unless (stringp server-name)
    (lsp-installer--error "Server name must be a string: %s" server-name))

  (let ((servers (lsp-installer--load-servers-config)))
    (when servers
      (cl-find-if
       (lambda (server)
         (and (listp server)
              (string= (plist-get server :name) server-name)))
       servers))))

(defun lsp-installer--list-available-servers ()
  "List all available servers from configuration.

Return a list of server names (strings) that are defined in the configuration."
  (let ((servers (lsp-installer--load-servers-config)))
    (if servers
        (cl-remove-duplicates
         (cl-remove-if-not
          #'stringp
          (mapcar (lambda (server)
                    (when (listp server)
                      (plist-get server :name)))
                  servers))
         :test #'string=)
      '())))

(provide 'lsp-installer-config)

;;; lsp-installer-config.el ends here
