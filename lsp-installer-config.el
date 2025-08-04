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
If FORCE-RELOAD is non-nil, reload even if cache exists."
  (when (or force-reload
            (and (not lsp-installer--servers-cache)
                 (file-exists-p lsp-installer-servers-file)))
    (if (file-exists-p lsp-installer-servers-file)
        (with-temp-buffer
          (insert-file-contents lsp-installer-servers-file)
          (goto-char (point-min))
          (setq lsp-installer--servers-cache (read (current-buffer)))
          (when force-reload
            (lsp-installer--message
             "Reloaded server configurations from %s"
             lsp-installer-servers-file)))
      (lsp-installer--error
       "Server configuration file not found: %s"
       lsp-installer-servers-file)))
  lsp-installer--servers-cache)

(defun lsp-installer-reload-servers-config ()
  "Reload server configurations from servers.eld file.
This clears the cache and forces a fresh read of the configuration file."
  (interactive)
  (setq lsp-installer--servers-cache nil)
  (lsp-installer--load-servers-config t)
  (lsp-installer--message
   "Server configurations reloaded successfully"))

(defun lsp-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME."
  (let ((servers (lsp-installer--load-servers-config)))
    (cond
     ;; Handle list format (first example in servers.eld)
     ((and (listp servers) (listp (car servers)))
      (cl-find-if
       (lambda (server)
         (string= (plist-get server :name) server-name))
       servers))
     ;; Handle hash format (other examples)
     ((plist-get servers :servers)
      (plist-get (plist-get servers :servers) (intern server-name)))
     (t
      nil))))

(defun lsp-installer--list-available-servers ()
  "List all available servers from configuration."
  (let ((servers (lsp-installer--load-servers-config)))
    (cond
     ;; Handle list format
     ((and (listp servers) (listp (car servers)))
      (mapcar (lambda (server) (plist-get server :name)) servers))
     ;; Handle hash format
     ((plist-get servers :servers)
      (mapcar #'symbol-name (map-keys (plist-get servers :servers))))
     (t
      '()))))

(provide 'lsp-installer-config)

;;; lsp-installer-config.el ends here
