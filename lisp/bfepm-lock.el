;;; bfepm-lock.el --- BFEPM Lock file management -*- lexical-binding: t -*-

;;; Commentary:

;; Lock file functionality for BFEPM to ensure reproducible package installations.

;;; Code:

(require 'bfepm-core)
(require 'bfepm-utils)

;; Declare external functions to avoid compilation warnings
(declare-function bfepm-package-install "bfepm-package")
;; bfepm-config is optional - structures are defined in bfepm-core

(defvar bfepm-lock-file-name "bfepm.lock"
  "Name of the lock file.")

(cl-defstruct bfepm-lock
  "Structure representing a lock file."
  meta
  packages
  resolution)

(cl-defstruct bfepm-lock-package
  "Structure representing a locked package."
  name
  version
  source
  checksum
  dependencies)

(defun bfepm-lock-generate ()
  "Generate lock file from current installed packages."
  (let* ((_config (bfepm-core-get-config))
         (installed-packages (bfepm-core-get-installed-packages))
         (lock-packages (bfepm-lock--build-package-entries installed-packages))
         (lock (make-bfepm-lock
                :meta (bfepm-lock--create-meta)
                :packages lock-packages
                :resolution (bfepm-lock--create-resolution lock-packages))))
    
    (bfepm-lock-save lock)
    (bfepm-utils-message "Lock file generated successfully")))

(defun bfepm-lock--build-package-entries (package-names)
  "Build lock file entries for PACKAGE-NAMES."
  (mapcar #'bfepm-lock--build-package-entry package-names))

(defun bfepm-lock--build-package-entry (package-name)
  "Build a lock file entry for PACKAGE-NAME."
  (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
         (version (bfepm-lock--detect-version package-dir))
         (checksum (bfepm-lock--calculate-package-checksum package-dir))
         (dependencies (bfepm-lock--detect-dependencies package-dir)))
    
    (make-bfepm-lock-package
     :name package-name
     :version version
     :source "melpa"  ; Would need to track actual source
     :checksum checksum
     :dependencies dependencies)))

(defun bfepm-lock--detect-version (package-dir)
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

(defun bfepm-lock--calculate-package-checksum (package-dir)
  "Calculate checksum for package in PACKAGE-DIR."
  (let ((files (directory-files-recursively package-dir "\\.[el]$")))
    (with-temp-buffer
      (dolist (file files)
        (insert-file-contents file)
        (goto-char (point-max)))
      (secure-hash 'sha256 (current-buffer)))))

(defun bfepm-lock--detect-dependencies (_package-dir)
  "Detect dependencies for package in PACKAGE-DIR."
  ;; Simplified - would parse package files for actual dependencies
  '())

(defun bfepm-lock--create-meta ()
  "Create meta section for lock file."
  `((version . "1.0.0")
    (generated . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))
    (bfepm-version . "0.1.0")))

(defun bfepm-lock--create-resolution (_packages)
  "Create resolution section for lock file."
  `((strategy . "conservative")
    (conflicts . ())
    (warnings . ())))

(defun bfepm-lock-save (lock &optional file)
  "Save LOCK to FILE (default: bfepm.lock in \='user-emacs-directory\=')."
  (let ((lock-file (or file (expand-file-name bfepm-lock-file-name user-emacs-directory))))
    (with-temp-buffer
      (insert (bfepm-lock--to-toml lock))
      (write-file lock-file))))

(defun bfepm-lock--to-toml (lock)
  "Convert LOCK structure to TOML format."
  (concat
   "[meta]\n"
   (bfepm-lock--encode-section (bfepm-lock-meta lock))
   "\n\n"
   (mapconcat (lambda (package)
                (format "[packages.%s]\n%s"
                        (bfepm-lock-package-name package)
                        (bfepm-lock--encode-package package)))
              (bfepm-lock-packages lock)
              "\n\n")
   "\n\n[resolution]\n"
   (bfepm-lock--encode-section (bfepm-lock-resolution lock))))

(defun bfepm-lock--encode-section (section)
  "Encode a TOML SECTION."
  (mapconcat (lambda (entry)
               (format "%s = %s"
                       (symbol-name (car entry))
                       (bfepm-lock--encode-value (cdr entry))))
             section
             "\n"))

(defun bfepm-lock--encode-package (package)
  "Encode a PACKAGE entry for TOML."
  (bfepm-lock--encode-section
   `((version . ,(bfepm-lock-package-version package))
     (source . ,(bfepm-lock-package-source package))
     (checksum . ,(bfepm-lock-package-checksum package)))))

(defun bfepm-lock--encode-value (value)
  "Encode a VALUE for TOML."
  (cond
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   ((symbolp value) (format "\"%s\"" (symbol-name value)))
   ((listp value) "[]")  ; Simplified
   (t (format "\"%s\"" value))))

(defun bfepm-lock-load (&optional file)
  "Load lock file from FILE (default: bfepm.lock in \='user-emacs-directory\=')."
  (let ((lock-file (or file (expand-file-name bfepm-lock-file-name user-emacs-directory))))
    (if (file-exists-p lock-file)
        (condition-case err
            (bfepm-lock--parse-toml-file lock-file)
          (error
           (bfepm-utils-error "Failed to load lock file %s: %s" lock-file err)))
      nil)))

(defun bfepm-lock--parse-toml-file (_file)
  "Parse TOML lock FILE and return bfepm-lock structure."
  ;; This would use a proper TOML parser
  ;; For now, just return a basic structure
  (make-bfepm-lock
   :meta '((version . "1.0.0"))
   :packages '()
   :resolution '((strategy . "conservative"))))

(defun bfepm-lock-verify ()
  "Verify that installed packages match lock file."
  (let ((lock (bfepm-lock-load)))
    (if lock
        (bfepm-lock--verify-packages (bfepm-lock-packages lock))
      (bfepm-utils-message "No lock file found"))))

(defun bfepm-lock--verify-packages (lock-packages)
  "Verify that LOCK-PACKAGES match installed packages."
  (let ((mismatches '()))
    (dolist (lock-package lock-packages)
      (let ((package-name (bfepm-lock-package-name lock-package))
            (expected-checksum (bfepm-lock-package-checksum lock-package)))
        (if (bfepm-core-package-installed-p package-name)
            (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
                   (actual-checksum (bfepm-lock--calculate-package-checksum package-dir)))
              (unless (string= expected-checksum actual-checksum)
                (push (format "%s: checksum mismatch" package-name) mismatches)))
          (push (format "%s: not installed" package-name) mismatches))))
    
    (if mismatches
        (progn
          (bfepm-utils-message "Lock file verification failed:")
          (dolist (mismatch mismatches)
            (message "  - %s" mismatch)))
      (bfepm-utils-message "Lock file verification passed"))))

(defun bfepm-lock-install ()
  "Install packages from lock file."
  (let ((lock (bfepm-lock-load)))
    (if lock
        (progn
          (bfepm-utils-message "Installing packages from lock file...")
          (dolist (lock-package (bfepm-lock-packages lock))
            (let ((package-name (bfepm-lock-package-name lock-package)))
              (unless (bfepm-core-package-installed-p package-name)
                (bfepm-package-install package-name)))))
      (bfepm-utils-message "No lock file found"))))

(provide 'bfepm-lock)

;;; bfepm-lock.el ends here
