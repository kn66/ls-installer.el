;;; ls-installer-config.el --- Configuration management for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains configuration management functions for ls-installer.

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Variables

(defvar ls-installer--servers-cache nil
  "Cache for loaded server configurations.")

;;; Configuration loading functions

(defun ls-installer--load-servers-config (&optional force-reload)
  "Load server configurations from servers.eld file.
If FORCE-RELOAD is non-nil, reload even if cache exists."
  (when (or force-reload
            (and (not ls-installer--servers-cache)
                 (file-exists-p ls-installer-servers-file)))
    (if (file-exists-p ls-installer-servers-file)
        (with-temp-buffer
          (insert-file-contents ls-installer-servers-file)
          (goto-char (point-min))
          (setq ls-installer--servers-cache (read (current-buffer)))
          (when force-reload
            (ls-installer--message
             "Reloaded server configurations from %s"
             ls-installer-servers-file)))
      (ls-installer--error
       "Server configuration file not found: %s"
       ls-installer-servers-file)))
  ls-installer--servers-cache)

(defun ls-installer-reload-servers-config ()
  "Reload server configurations from servers.eld file.
This clears the cache and forces a fresh read of the configuration file."
  (interactive)
  (setq ls-installer--servers-cache nil)
  (ls-installer--load-servers-config t)
  (ls-installer--message
   "Server configurations reloaded successfully"))

(defun ls-installer--get-server-config (server-name)
  "Get configuration for SERVER-NAME."
  (let ((servers (ls-installer--load-servers-config)))
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

(defun ls-installer--list-available-servers ()
  "List all available servers from configuration."
  (let ((servers (ls-installer--load-servers-config)))
    (cond
     ;; Handle list format
     ((and (listp servers) (listp (car servers)))
      (mapcar (lambda (server) (plist-get server :name)) servers))
     ;; Handle hash format
     ((plist-get servers :servers)
      (mapcar #'symbol-name (map-keys (plist-get servers :servers))))
     (t
      '()))))

(provide 'ls-installer-config)

;;; ls-installer-config.el ends here
