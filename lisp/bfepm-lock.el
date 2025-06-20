;;; bfepm-lock.el --- BFEPM Lock file management -*- lexical-binding: t -*-

;;; Commentary:

;; Lock file functionality for BFEPM to ensure reproducible package installations.
;; Uses S-expression format for efficient parsing and processing.

;;; Code:

(require 'bfepm-core)
(require 'bfepm-utils)
(require 'cl-lib)

;; Declare external functions to avoid compilation warnings
(declare-function bfepm-package-install "bfepm-package")
(declare-function bfepm-package--parse-metadata "bfepm-package")
;; bfepm-config is optional - structures are defined in bfepm-core

(defvar bfepm-lock-file-name "bfepm.lock"
  "Name of the lock file.")

(defvar bfepm-lock-file-version "1.0.0"
  "Version of the lock file format.")

;; Lock file data structure as S-expression:
;; (:meta (:version "1.0.0" :generated "2025-06-19T09:00:00Z" :bfepm-version "0.1.0")
;;  :packages ((:name "package1" :version "1.2.3" :source "melpa" :checksum "abc123" :dependencies ("dep1" "dep2"))
;;             (:name "package2" :version "2.0.0" :source "gnu" :checksum "def456" :dependencies ()))
;;  :resolution (:strategy "conservative" :resolved-at "2025-06-19T09:00:00Z"))

(defun bfepm-lock-generate ()
  "Generate lock file from current installed packages using S-expression format."
  (let* ((installed-packages (bfepm-core-get-installed-packages))
         (lock-packages (bfepm-lock--build-package-entries installed-packages))
         (lock-data (bfepm-lock--create-lock-data lock-packages)))
    
    (bfepm-lock-save lock-data)
    (bfepm-utils-message "Lock file generated successfully with %d packages" (length lock-packages))))

(defun bfepm-lock--build-package-entries (package-names)
  "Build lock file entries for PACKAGE-NAMES."
  (mapcar #'bfepm-lock--build-package-entry package-names))

(defun bfepm-lock--build-package-entry (package-name)
  "Build a lock file entry for PACKAGE-NAME in S-expression format."
  (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
         (version (bfepm-lock--detect-version package-name package-dir))
         (source (bfepm-lock--detect-source package-name package-dir))
         (checksum (bfepm-lock--calculate-package-checksum package-dir))
         (dependencies (bfepm-lock--detect-dependencies package-name package-dir)))
    
    (list :name package-name
          :version version
          :source source
          :checksum checksum
          :dependencies dependencies
          :installed-at (bfepm-lock--get-install-time package-dir))))

(defun bfepm-lock--detect-version (package-name package-dir)
  "Detect version of PACKAGE-NAME in PACKAGE-DIR."
  (or
   ;; Try bfepm version file first
   (bfepm-lock--read-bfepm-version package-dir)
   ;; Try package metadata cache
   (bfepm-lock--read-cached-version package-name package-dir)
   ;; Try -pkg.el file
   (bfepm-lock--read-pkg-version package-name package-dir)
   ;; Try main .el file headers
   (bfepm-lock--read-header-version package-name package-dir)
   ;; Fallback
   "unknown"))

(defun bfepm-lock--read-bfepm-version (package-dir)
  "Read version from .bfepm-version file in PACKAGE-DIR."
  (let ((version-file (expand-file-name ".bfepm-version" package-dir)))
    (when (file-exists-p version-file)
      (with-temp-buffer
        (insert-file-contents version-file)
        (string-trim (buffer-string))))))

(defun bfepm-lock--read-cached-version (_package-name package-dir)
  "Read version from cached metadata in PACKAGE-DIR for PACKAGE-NAME."
  (let ((metadata-file (expand-file-name ".bfepm-metadata" package-dir)))
    (when (file-exists-p metadata-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (let ((metadata (read (current-buffer))))
              (cdr (assoc 'version metadata))))
        (error nil)))))

(defun bfepm-lock--read-pkg-version (package-name package-dir)
  "Read version from -pkg.el file for PACKAGE-NAME in PACKAGE-DIR."
  (let ((pkg-file (expand-file-name (concat package-name "-pkg.el") package-dir)))
    (when (file-exists-p pkg-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents pkg-file)
            (goto-char (point-min))
            (when (re-search-forward "\"\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\"" nil t)
              (match-string 1)))
        (error nil)))))

(defun bfepm-lock--read-header-version (package-name package-dir)
  "Read version from package header in main .el file for PACKAGE-NAME in PACKAGE-DIR."
  (let ((main-file (expand-file-name (concat package-name ".el") package-dir)))
    (when (file-exists-p main-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Version:[[:space:]]*\\(.+\\)$" nil t)
              (string-trim (match-string 1))))
        (error nil)))))

(defun bfepm-lock--calculate-package-checksum (package-dir)
  "Calculate checksum for package in PACKAGE-DIR."
  (let ((files (directory-files-recursively package-dir "\\.el$")))
    (with-temp-buffer
      (dolist (file files)
        (insert-file-contents file)
        (goto-char (point-max)))
      (secure-hash 'sha256 (current-buffer)))))

(defun bfepm-lock--detect-dependencies (package-name package-dir)
  "Detect dependencies for PACKAGE-NAME in PACKAGE-DIR."
  (or
   ;; Try cached metadata first
   (bfepm-lock--read-cached-dependencies package-name package-dir)
   ;; Try parsing main .el file
   (bfepm-lock--parse-dependencies-from-file package-name package-dir)
   ;; Fallback to empty list
   '()))

(defun bfepm-lock--read-cached-dependencies (_package-name package-dir)
  "Read dependencies from cached metadata in PACKAGE-DIR."
  (let ((metadata-file (expand-file-name ".bfepm-metadata" package-dir)))
    (when (file-exists-p metadata-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (let ((metadata (read (current-buffer))))
              (cdr (assoc 'requires metadata))))
        (error nil)))))

(defun bfepm-lock--parse-dependencies-from-file (package-name package-dir)
  "Parse dependencies from main .el file for PACKAGE-NAME in PACKAGE-DIR."
  (let ((main-file (expand-file-name (concat package-name ".el") package-dir)))
    (when (file-exists-p main-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Package-Requires:[[:space:]]*\\(.+\\)$" nil t)
              (let ((requires-str (match-string 1)))
                (ignore-errors (read requires-str)))))
        (error nil)))))

(defun bfepm-lock--detect-source (package-name package-dir)
  "Detect package source for PACKAGE-NAME in PACKAGE-DIR."
  (or
   ;; Try to determine from installation metadata
   (bfepm-lock--read-source-from-metadata package-name package-dir)
   ;; Default fallback
   "unknown"))

(defun bfepm-lock--read-source-from-metadata (_package-name _package-dir)
  "Read source information from package metadata."
  ;; For now, default to melpa - could be enhanced to track actual source
  "melpa")

(defun bfepm-lock--get-install-time (package-dir)
  "Get installation time for package in PACKAGE-DIR."
  (let ((version-file (expand-file-name ".bfepm-version" package-dir)))
    (if (file-exists-p version-file)
        (format-time-string "%Y-%m-%dT%H:%M:%SZ" (file-attribute-modification-time (file-attributes version-file)))
      (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))))

(defun bfepm-lock--create-lock-data (packages)
  "Create complete lock data structure for PACKAGES in S-expression format."
  (list :meta (bfepm-lock--create-meta)
        :packages packages
        :resolution (bfepm-lock--create-resolution packages)))

(defun bfepm-lock--create-meta ()
  "Create meta section for lock file in S-expression format."
  (list :version bfepm-lock-file-version
        :generated (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time))
        :bfepm-version "0.1.0"
        :emacs-version emacs-version
        :system-type (symbol-name system-type)))

(defun bfepm-lock--create-resolution (packages)
  "Create resolution section for lock file with PACKAGES."
  (list :strategy "conservative"
        :resolved-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time))
        :total-packages (length packages)
        :conflicts '()
        :warnings '()))

(defun bfepm-lock-save (lock-data &optional file)
  "Save LOCK-DATA to FILE in S-expression format."
  (let ((lock-file (or file (expand-file-name bfepm-lock-file-name user-emacs-directory))))
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; coding: utf-8; -*-\n")
      (insert ";; BFEPM Lock File\n")
      (insert ";; This file is auto-generated. Do not edit manually.\n")
      (insert ";; Generated at: " (plist-get (plist-get lock-data :meta) :generated) "\n\n")
      (pp lock-data (current-buffer))
      (write-file lock-file))
    (bfepm-utils-message "Lock file saved to %s" lock-file)))

(defun bfepm-lock-load (&optional file)
  "Load lock file from FILE (default: bfepm.lock in `user-emacs-directory')."
  (let ((lock-file (or file (expand-file-name bfepm-lock-file-name user-emacs-directory))))
    (if (file-exists-p lock-file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents lock-file)
              (goto-char (point-min))
              ;; Skip comment lines
              (while (looking-at "^;;")
                (forward-line 1))
              (read (current-buffer)))
          (error
           (bfepm-utils-message "Failed to load lock file %s: %s" lock-file (error-message-string err))
           nil))
      (bfepm-utils-message "Lock file not found: %s" lock-file)
      nil)))

(defun bfepm-lock-verify (&optional file)
  "Verify that installed packages match the lock FILE."
  (let ((lock-data (bfepm-lock-load file)))
    (if lock-data
        (bfepm-lock--verify-packages (plist-get lock-data :packages))
      (bfepm-utils-message "Cannot verify: lock file not available"))))

(defun bfepm-lock--verify-packages (locked-packages)
  "Verify that installed packages match LOCKED-PACKAGES."
  (let ((installed-packages (bfepm-core-get-installed-packages))
        (mismatches '())
        (missing '())
        (extra '()))
    
    ;; Check for missing and mismatched packages
    (dolist (locked-pkg locked-packages)
      (let* ((name (plist-get locked-pkg :name))
             (expected-version (plist-get locked-pkg :version))
             (expected-checksum (plist-get locked-pkg :checksum)))
        (if (member name installed-packages)
            ;; Package is installed, verify version and checksum
            (let* ((package-dir (expand-file-name name (bfepm-core-get-packages-directory)))
                   (actual-version (bfepm-lock--detect-version name package-dir))
                   (actual-checksum (bfepm-lock--calculate-package-checksum package-dir)))
              (unless (and (bfepm-lock--version-equal-p expected-version actual-version)
                          (string= expected-checksum actual-checksum))
                (push (list :name name
                           :expected-version expected-version
                           :actual-version actual-version
                           :expected-checksum expected-checksum
                           :actual-checksum actual-checksum)
                      mismatches)))
          ;; Package is missing
          (push name missing))))
    
    ;; Check for extra packages
    (dolist (installed-pkg installed-packages)
      (unless (cl-find-if (lambda (locked-pkg)
                           (string= (plist-get locked-pkg :name) installed-pkg))
                         locked-packages)
        (push installed-pkg extra)))
    
    ;; Report results
    (bfepm-lock--report-verification-results missing mismatches extra)))

(defun bfepm-lock--report-verification-results (missing mismatches extra)
  "Report verification results for MISSING, MISMATCHES, and EXTRA packages."
  (if (and (null missing) (null mismatches) (null extra))
      (bfepm-utils-message "✅ Lock file verification passed: all packages match")
    (progn
      (when missing
        (bfepm-utils-message "❌ Missing packages: %s" (mapconcat 'identity missing ", ")))
      (when mismatches
        (bfepm-utils-message "❌ Version/checksum mismatches:")
        (dolist (mismatch mismatches)
          (bfepm-utils-message "  - %s: expected %s, got %s"
                             (plist-get mismatch :name)
                             (plist-get mismatch :expected-version)
                             (plist-get mismatch :actual-version))))
      (when extra
        (bfepm-utils-message "⚠️  Extra packages not in lock file: %s" (mapconcat 'identity extra ", ")))
      (bfepm-utils-message "🔒 Run 'bfepm-lock-install' to sync with lock file"))))

(defun bfepm-lock-install (&optional file)
  "Install packages from lock file FILE to match exact versions."
  (let ((lock-data (bfepm-lock-load file)))
    (if lock-data
        (let ((packages (plist-get lock-data :packages)))
          (bfepm-utils-message "Installing %d packages from lock file..." (length packages))
          (bfepm-lock--install-packages packages))
      (bfepm-utils-message "Cannot install: lock file not available"))))

(defun bfepm-lock--install-packages (locked-packages)
  "Install packages from LOCKED-PACKAGES list."
  (let ((success-count 0)
        (failure-count 0))
    (dolist (locked-pkg locked-packages)
      (let* ((name (plist-get locked-pkg :name))
             (version (plist-get locked-pkg :version)))
        (bfepm-utils-message "Installing %s version %s..." name version)
        (condition-case err
            (progn
              ;; Install specific version from lock file
              (require 'bfepm-package)
              (if (fboundp 'bfepm-package-install-version)
                  (bfepm-package-install-version name version)
                (progn
                  (bfepm-utils-message "Warning: Installing latest version of %s (specific version installation not implemented)" name)
                  (bfepm-package-install name)))
              (setq success-count (1+ success-count)))
          (error
           (bfepm-utils-message "Failed to install %s: %s" name (error-message-string err))
           (setq failure-count (1+ failure-count))))))
    (bfepm-utils-message "Lock install complete: %d succeeded, %d failed" success-count failure-count)))

;; Interactive commands
(defun bfepm-lock-generate-interactive ()
  "Interactively generate lock file."
  (interactive)
  (bfepm-lock-generate))

(defun bfepm-lock-verify-interactive ()
  "Interactively verify lock file."
  (interactive)
  (bfepm-lock-verify))

(defun bfepm-lock-install-interactive ()
  "Interactively install from lock file."
  (interactive)
  (bfepm-lock-install))

;; Utility functions
(defun bfepm-lock--version-equal-p (version1 version2)
  "Compare VERSION1 and VERSION2 for equality.
Handle \\='unknown\\=' versions and normalize version strings."
  (cond
   ((or (string= version1 "unknown") (string= version2 "unknown"))
    ;; If either version is unknown, only match if both are unknown
    (and (string= version1 "unknown") (string= version2 "unknown")))
   (t
    ;; Normal string comparison for now - could be enhanced with semantic versioning
    (string= version1 version2))))

;; Package installation integration
(defun bfepm-lock-update-on-install (package-name)
  "Update lock file when PACKAGE-NAME is installed."
  (when (file-exists-p (expand-file-name bfepm-lock-file-name user-emacs-directory))
    (bfepm-utils-message "Updating lock file after installing %s..." package-name)
    (bfepm-lock-generate)))

(provide 'bfepm-lock)

;;; bfepm-lock.el ends here
