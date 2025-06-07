;;; bfepm-utils.el --- BFEPM Utility functions -*- lexical-binding: t -*-

;;; Commentary:

;; Utility functions for BFEPM.

;;; Code:

(require 'url)
(require 'json)

(defun bfepm-utils-message (format-string &rest args)
  "Display a formatted message with BFEPM prefix."
  (message "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-error (format-string &rest args)
  "Signal an error with BFEPM prefix."
  (error "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-version-compare (v1 v2)
  "Compare version strings V1 and V2.
Return 1 if V1 > V2, -1 if V1 < V2, 0 if equal."
  (if (and (bfepm-utils--is-melpa-date-version-p v1) 
           (bfepm-utils--is-melpa-date-version-p v2))
      ;; Both are MELPA date versions
      (let ((num1 (string-to-number (replace-regexp-in-string "\\." "" v1)))
            (num2 (string-to-number (replace-regexp-in-string "\\." "" v2))))
        (cond ((> num1 num2) 1)
              ((< num1 num2) -1)
              (t 0)))
    ;; Standard semantic version handling
    (let ((parts1 (mapcar #'string-to-number (split-string v1 "\\.")))
          (parts2 (mapcar #'string-to-number (split-string v2 "\\."))))
      (bfepm-utils--version-compare-parts parts1 parts2))))

(defun bfepm-utils--version-compare-parts (parts1 parts2)
  "Compare version parts lists PARTS1 and PARTS2."
  (cond
   ((and (null parts1) (null parts2)) 0)
   ((null parts1) -1)
   ((null parts2) 1)
   ((> (car parts1) (car parts2)) 1)
   ((< (car parts1) (car parts2)) -1)
   (t (bfepm-utils--version-compare-parts (cdr parts1) (cdr parts2)))))

(defun bfepm-utils-version-satisfies-p (version requirement)
  "Check if VERSION satisfies REQUIREMENT."
  (cond
   ((string= requirement "latest") t)
   ((string-prefix-p "^" requirement)
    (bfepm-utils--version-satisfies-caret-p version (substring requirement 1)))
   ((string-prefix-p "~" requirement)
    (bfepm-utils--version-satisfies-tilde-p version (substring requirement 1)))
   (t (string= version requirement))))

(defun bfepm-utils--is-melpa-date-version-p (version)
  "Check if VERSION is a MELPA date version (YYYYMMDD.HHMM format)."
  (string-match-p "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$" version))

(defun bfepm-utils--version-satisfies-caret-p (version requirement)
  "Check if VERSION satisfies caret REQUIREMENT."
  (if (bfepm-utils--is-melpa-date-version-p version)
      ;; MELPA date version handling (YYYYMMDD.HHMM)
      (if (bfepm-utils--is-melpa-date-version-p requirement)
          ;; Both are MELPA date versions - same year
          (let ((ver-date (car (split-string version "\\.")))
                (req-date (car (split-string requirement "\\."))))
            (and (>= (bfepm-utils-version-compare version requirement) 0)
                 (string= (substring ver-date 0 4) (substring req-date 0 4))))
        ;; Version is MELPA date but requirement is not - assume requirement is date prefix
        (let ((ver-date (car (split-string version "\\."))))
          (and (>= (string-to-number ver-date) (string-to-number requirement))
               (string= (substring ver-date 0 4) (substring requirement 0 4)))))
    ;; Standard semantic version handling
    (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
          (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
      (and (>= (bfepm-utils-version-compare version requirement) 0)
           (< (car ver-parts) (1+ (car req-parts)))))))

(defun bfepm-utils--version-satisfies-tilde-p (version requirement)
  "Check if VERSION satisfies tilde REQUIREMENT."
  (if (bfepm-utils--is-melpa-date-version-p version)
      ;; MELPA date version handling - same day
      (if (bfepm-utils--is-melpa-date-version-p requirement)
          ;; Both are MELPA date versions - same day
          (let ((ver-date (car (split-string version "\\.")))
                (req-date (car (split-string requirement "\\."))))
            (and (>= (bfepm-utils-version-compare version requirement) 0)
                 (string= ver-date req-date)))
        ;; Version is MELPA date but requirement is not - check day prefix match
        (let ((ver-date (car (split-string version "\\."))))
          (and (>= (string-to-number ver-date) (string-to-number requirement))
               (string-prefix-p requirement ver-date))))
    ;; Standard semantic version handling
    (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
          (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
      (and (>= (bfepm-utils-version-compare version requirement) 0)
           (= (car ver-parts) (car req-parts))
           (= (cadr ver-parts) (cadr req-parts))))))

(defun bfepm-utils-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-utils-download-file (url local-file)
  "Download file from URL to LOCAL-FILE."
  (bfepm-utils-message "Downloading from %s..." url)
  (condition-case err
      (progn
        (url-copy-file url local-file t)
        (bfepm-utils-message "Downloaded to %s" local-file))
    (error
     (bfepm-utils-error "Failed to download %s: %s" url (error-message-string err)))))

(defun bfepm-utils-extract-tar (tar-file target-dir)
  "Extract TAR-FILE to TARGET-DIR."
  (bfepm-utils-message "Extracting %s..." tar-file)
  (let ((default-directory target-dir))
    (with-temp-buffer
      (call-process "tar" nil t nil "-xf" tar-file)
      (when (> (buffer-size) 0)
        (bfepm-utils-message "Extraction output: %s" (buffer-string))))))

(defun bfepm-utils-copy-file (source target)
  "Copy SOURCE file to TARGET."
  (copy-file source target t))

(defun bfepm-utils-file-checksum (file)
  "Calculate SHA256 checksum of FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(provide 'bfepm-utils)

;;; bfepm-utils.el ends here