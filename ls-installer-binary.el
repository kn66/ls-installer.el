;;; ls-installer-binary.el --- Binary installation for ls-installer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains binary installation functions for ls-installer.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'cl-lib)

;;; Binary installation functions

(defun ls-installer--install-binary
    (server-name
     url &optional executable-name target-subdir strip-components)
  "Install binary from URL for SERVER-NAME.
EXECUTABLE-NAME is the name of the executable after extraction.
TARGET-SUBDIR is optional subdirectory to extract to.
STRIP-COMPONENTS is number of path components to strip during extraction."
  (let* ((server-dir
          (ls-installer--get-server-install-dir server-name))
         (bin-dir (expand-file-name "bin" server-dir))
         (temp-dir (make-temp-file "ls-installer-" t))
         (filename
          (file-name-nondirectory (car (split-string url "?"))))
         (temp-file (expand-file-name filename temp-dir))
         (extract-dir
          (if target-subdir
              (expand-file-name target-subdir server-dir)
            server-dir)))

    (unwind-protect
        (progn
          (ls-installer--ensure-directory server-dir)
          (ls-installer--ensure-directory bin-dir)
          (ls-installer--message
           "Installing binary %s from %s..." server-name url)

          ;; Download the file
          (ls-installer--download-file url temp-file)

          ;; Extract if it's an archive, otherwise copy directly
          (if (string-match-p
               "\\.(tar\\.gz\\|tgz\\|tar\\.xz\\|zip)$" filename)
              (progn
                (ls-installer--extract-archive
                 temp-file extract-dir strip-components)
                ;; Find and make executable files executable
                (when executable-name
                  (let ((exec-path
                         (expand-file-name executable-name
                                           extract-dir)))
                    (when (file-exists-p exec-path)
                      (ls-installer--make-executable exec-path)
                      ;; Create symlink in bin directory if not already there
                      (let ((bin-path
                             (expand-file-name
                              (file-name-nondirectory executable-name)
                              bin-dir)))
                        (unless (file-exists-p bin-path)
                          (make-symbolic-link
                           exec-path bin-path)))))))
            ;; For single binary files
            (let ((target-file
                   (expand-file-name
                    (or executable-name
                        (file-name-nondirectory filename))
                    bin-dir)))
              (copy-file temp-file target-file t)
              (ls-installer--make-executable target-file)))

          (ls-installer--message
           "Successfully installed %s" server-name)
          (ls-installer--add-to-exec-path server-name)
          t)

      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(defun ls-installer--install-github-release
    (server-name
     repo-path
     &optional
     asset-pattern
     executable-name
     target-subdir
     strip-components)
  "Install binary from GitHub release.
REPO-PATH is the GitHub repository path (e.g., 'rust-lang/rust-analyzer').
ASSET-PATTERN is a regex to match the asset name for current platform.
EXECUTABLE-NAME is the name of the executable after extraction.
TARGET-SUBDIR is optional subdirectory to extract to.
STRIP-COMPONENTS is number of path components to strip during extraction."
  (let* ((api-url
          (format "https://api.github.com/repos/%s/releases/latest"
                  repo-path))
         (temp-buffer (url-retrieve-synchronously api-url t))
         release-data
         download-url)

    (unless temp-buffer
      (ls-installer--error
       "Failed to fetch GitHub API for %s" repo-path))

    (with-current-buffer temp-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq release-data (json-read))
      (kill-buffer))

    (let* ((assets (cdr (assq 'assets release-data)))
           (platform (ls-installer--get-platform))
           (arch (ls-installer--get-arch))
           (pattern
            (or asset-pattern (format "%s.*%s" platform arch))))

      ;; Find matching asset
      (cl-loop
       for
       asset
       across
       assets
       for
       name
       =
       (cdr (assq 'name asset))
       when
       (string-match-p pattern name)
       do
       (setq download-url (cdr (assq 'browser_download_url asset)))
       and
       return
       t)

      (unless download-url
        (ls-installer--error
         "No suitable binary found for %s %s in %s"
         platform
         arch
         repo-path))

      (ls-installer--install-binary server-name download-url
                                    executable-name
                                    target-subdir
                                    strip-components))))

(provide 'ls-installer-binary)

;;; ls-installer-binary.el ends here
