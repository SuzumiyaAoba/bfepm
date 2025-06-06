;;; epm-lock.el --- EPM Lock file management -*- lexical-binding: t -*-

;;; Commentary:

;; Lock file functionality for EPM to ensure reproducible package installations.

;;; Code:

(require 'epm-core)
(require 'epm-utils)
;; epm-config is optional - structures are defined in epm-core

(defvar epm-lock-file-name "epm.lock"
  "Name of the lock file.")

(cl-defstruct epm-lock
  "Structure representing a lock file."
  meta
  packages
  resolution)

(cl-defstruct epm-lock-package
  "Structure representing a locked package."
  name
  version
  source
  checksum
  dependencies)

(defun epm-lock-generate ()
  "Generate lock file from current installed packages."
  (let* ((config (epm-core-get-config))
         (installed-packages (epm-core-get-installed-packages))
         (lock-packages (epm-lock--build-package-entries installed-packages))
         (lock (make-epm-lock
                :meta (epm-lock--create-meta)
                :packages lock-packages
                :resolution (epm-lock--create-resolution lock-packages))))
    
    (epm-lock-save lock)
    (epm-utils-message "Lock file generated successfully")))

(defun epm-lock--build-package-entries (package-names)
  "Build lock file entries for PACKAGE-NAMES."
  (mapcar #'epm-lock--build-package-entry package-names))

(defun epm-lock--build-package-entry (package-name)
  "Build a lock file entry for PACKAGE-NAME."
  (let* ((package-dir (expand-file-name package-name (epm-core-get-packages-directory)))
         (version (epm-lock--detect-version package-dir))
         (checksum (epm-lock--calculate-package-checksum package-dir))
         (dependencies (epm-lock--detect-dependencies package-dir)))
    
    (make-epm-lock-package
     :name package-name
     :version version
     :source "melpa"  ; Would need to track actual source
     :checksum checksum
     :dependencies dependencies)))

(defun epm-lock--detect-version (package-dir)
  "Detect version of package in PACKAGE-DIR."
  ;; This is a simplified version - real implementation would parse package files
  (let ((pkg-file (expand-file-name (concat (file-name-nondirectory package-dir) "-pkg.el") 
                                    package-dir)))
    (if (file-exists-p pkg-file)
        (with-temp-buffer
          (insert-file-contents pkg-file)
          (goto-char (point-min))
          (when (re-search-forward "\"\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\"" nil t)
            (match-string 1)))
      "unknown")))

(defun epm-lock--calculate-package-checksum (package-dir)
  "Calculate checksum for package in PACKAGE-DIR."
  (let ((files (directory-files-recursively package-dir "\\.[el]$")))
    (with-temp-buffer
      (dolist (file files)
        (insert-file-contents file)
        (goto-char (point-max)))
      (secure-hash 'sha256 (current-buffer)))))

(defun epm-lock--detect-dependencies (package-dir)
  "Detect dependencies for package in PACKAGE-DIR."
  ;; Simplified - would parse package files for actual dependencies
  '())

(defun epm-lock--create-meta ()
  "Create meta section for lock file."
  `((version . "1.0.0")
    (generated . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))
    (epm-version . "0.1.0")))

(defun epm-lock--create-resolution (packages)
  "Create resolution section for lock file."
  `((strategy . "conservative")
    (conflicts . ())
    (warnings . ())))

(defun epm-lock-save (lock &optional file)
  "Save LOCK to FILE (default: epm.lock in user-emacs-directory)."
  (let ((lock-file (or file (expand-file-name epm-lock-file-name user-emacs-directory))))
    (with-temp-buffer
      (insert (epm-lock--to-toml lock))
      (write-file lock-file))))

(defun epm-lock--to-toml (lock)
  "Convert LOCK structure to TOML format."
  (concat
   "[meta]\n"
   (epm-lock--encode-section (epm-lock-meta lock))
   "\n\n"
   (mapconcat (lambda (package)
                (format "[packages.%s]\n%s"
                        (epm-lock-package-name package)
                        (epm-lock--encode-package package)))
              (epm-lock-packages lock)
              "\n\n")
   "\n\n[resolution]\n"
   (epm-lock--encode-section (epm-lock-resolution lock))))

(defun epm-lock--encode-section (section)
  "Encode a TOML section."
  (mapconcat (lambda (entry)
               (format "%s = %s"
                       (symbol-name (car entry))
                       (epm-lock--encode-value (cdr entry))))
             section
             "\n"))

(defun epm-lock--encode-package (package)
  "Encode a package entry for TOML."
  (epm-lock--encode-section
   `((version . ,(epm-lock-package-version package))
     (source . ,(epm-lock-package-source package))
     (checksum . ,(epm-lock-package-checksum package)))))

(defun epm-lock--encode-value (value)
  "Encode a value for TOML."
  (cond
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   ((symbolp value) (format "\"%s\"" (symbol-name value)))
   ((listp value) "[]")  ; Simplified
   (t (format "\"%s\"" value))))

(defun epm-lock-load (&optional file)
  "Load lock file from FILE (default: epm.lock in user-emacs-directory)."
  (let ((lock-file (or file (expand-file-name epm-lock-file-name user-emacs-directory))))
    (if (file-exists-p lock-file)
        (condition-case err
            (epm-lock--parse-toml-file lock-file)
          (error
           (epm-utils-error "Failed to load lock file %s: %s" lock-file err)))
      nil)))

(defun epm-lock--parse-toml-file (file)
  "Parse TOML lock FILE and return epm-lock structure."
  ;; This would use a proper TOML parser
  ;; For now, just return a basic structure
  (make-epm-lock
   :meta '((version . "1.0.0"))
   :packages '()
   :resolution '((strategy . "conservative"))))

(defun epm-lock-verify ()
  "Verify that installed packages match lock file."
  (let ((lock (epm-lock-load)))
    (if lock
        (epm-lock--verify-packages (epm-lock-packages lock))
      (epm-utils-message "No lock file found"))))

(defun epm-lock--verify-packages (lock-packages)
  "Verify that LOCK-PACKAGES match installed packages."
  (let ((mismatches '()))
    (dolist (lock-package lock-packages)
      (let ((package-name (epm-lock-package-name lock-package))
            (expected-checksum (epm-lock-package-checksum lock-package)))
        (if (epm-core-package-installed-p package-name)
            (let* ((package-dir (expand-file-name package-name (epm-core-get-packages-directory)))
                   (actual-checksum (epm-lock--calculate-package-checksum package-dir)))
              (unless (string= expected-checksum actual-checksum)
                (push (format "%s: checksum mismatch" package-name) mismatches)))
          (push (format "%s: not installed" package-name) mismatches))))
    
    (if mismatches
        (progn
          (epm-utils-message "Lock file verification failed:")
          (dolist (mismatch mismatches)
            (message "  - %s" mismatch)))
      (epm-utils-message "Lock file verification passed"))))

(defun epm-lock-install ()
  "Install packages from lock file."
  (let ((lock (epm-lock-load)))
    (if lock
        (progn
          (epm-utils-message "Installing packages from lock file...")
          (dolist (lock-package (epm-lock-packages lock))
            (let ((package-name (epm-lock-package-name lock-package)))
              (unless (epm-core-package-installed-p package-name)
                (epm-package-install package-name)))))
      (epm-utils-message "No lock file found"))))

(provide 'epm-lock)

;;; epm-lock.el ends here
