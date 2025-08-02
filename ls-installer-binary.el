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
                ;; It's an archive - extract it
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
                          (when (file-exists-p exec-path)
                            (make-symbolic-link
                             exec-path bin-path))))))))
            ;; For single binary files (not archives)
            (let ((target-file
                   (expand-file-name
                    (or (and executable-name
                             (file-name-nondirectory executable-name))
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
  "Install binary from GitHub release."
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
           ;; OS判定
           (os-patterns
            (cond
             ((eq system-type 'windows-nt)
              '("win" "windows"))
             ((eq system-type 'darwin)
              '("osx" "darwin" "mac" "macos"))
             ((eq system-type 'gnu/linux)
              '("linux"))
             (t
              '("linux"))))
           ;; アーキテクチャ判定（64ビット優先、デフォルトパターンを追加）
           (arch-patterns
            (cond
             ((string-match "aarch64\\|arm64" system-configuration)
              '("arm64" "aarch64" "x64" "x86_64" "amd64" ""))
             ((string-match "x86_64\\|amd64" system-configuration)
              '("x64" "x86_64" "amd64" "ia32" "x86" ""))
             (t
              '("x64" "x86_64" "amd64" "ia32" "x86" ""))))
           ;; Windowsの場合は.exe拡張子を追加
           (platform-executable-name
            (if (and executable-name (eq system-type 'windows-nt))
                (if (string-suffix-p ".exe" executable-name)
                    executable-name
                  (concat executable-name ".exe"))
              executable-name)))

      ;; 1段階目: OS別にassetをフィルタリング
      (let ((os-filtered-assets
             (cl-remove-if-not
              (lambda (asset)
                (let ((name (cdr (assq 'name asset))))
                  (and
                   (not
                    (string-match-p
                     "submodules\\|source\\|debug-symbols\\|indexing"
                     name))
                   ;; omnisharpの場合は特別な除外ルールを追加
                   (if (string= server-name "omnisharp")
                       (not
                        (string-match-p
                         "\\.http-\\|omnisharp\\.http\\|-mono" name))
                     t)
                   (cl-some
                    (lambda (pattern)
                      (string-match-p pattern name))
                    os-patterns))))
              (append assets nil))))

        (unless os-filtered-assets
          (ls-installer--error
           "No assets found for OS %s in %s. Available assets: %s"
           (car os-patterns)
           repo-path
           (mapcar
            (lambda (asset)
              (cdr (assq 'name asset)))
            (append assets nil))))

        ;; 2段階目: アーキテクチャ別に優先順位をつけて選択
        (let ((selected-asset
               (cl-loop
                for arch-pattern in arch-patterns for matching-asset =
                (cl-find-if
                 (lambda (asset)
                   (let ((name (cdr (assq 'name asset))))
                     (and
                      (if (string= arch-pattern "")
                          ;; 空文字の場合は常にマッチ（デフォルト）
                          t
                        ;; 通常のパターンマッチ
                        (string-match-p arch-pattern name))
                      ;; asset-patternが指定されている場合はそれもチェック
                      (or (not asset-pattern)
                          (string-match-p asset-pattern name))
                      ;; omnisharpの場合は.NET 6.0版を優先
                      (if (string= server-name "omnisharp")
                          (not (string-match-p "-net6\\.0" name))
                        t))))
                 os-filtered-assets)
                when matching-asset return matching-asset)))

          (unless selected-asset
            (ls-installer--error
             "No suitable binary found for %s %s in %s. OS-filtered assets: %s"
             (car os-patterns)
             (car arch-patterns)
             repo-path
             (mapcar
              (lambda (asset)
                (cdr (assq 'name asset)))
              os-filtered-assets)))

          (setq download-url
                (cdr (assq 'browser_download_url selected-asset)))

          (ls-installer--message
           "Selected asset: %s" (cdr (assq 'name selected-asset)))

          (ls-installer--install-binary server-name download-url
                                        platform-executable-name
                                        target-subdir
                                        strip-components))))))

(provide 'ls-installer-binary)

;;; ls-installer-binary.el ends here
