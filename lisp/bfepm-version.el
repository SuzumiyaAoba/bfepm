;;; bfepm-version.el --- Version constraint engine for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides version comparison and constraint satisfaction
;; functionality for BFEPM. It handles both semantic versions and
;; MELPA date-based versions with support for caret (^) and tilde (~)
;; constraint operators.
;;
;; Key Features:
;; - Semantic version comparison (e.g., "1.2.3")
;; - MELPA date version support (e.g., "20250618.1234")
;; - Constraint satisfaction with operators:
;;   - "latest" - matches any version
;;   - "^1.2.3" - compatible versions (same major)
;;   - "~1.2.3" - patch-level changes only
;; - Best version matching from multiple candidates
;; - Robust error handling and validation
;;
;; Version Format Support:
;; - Semantic: "MAJOR.MINOR.PATCH" (e.g., "1.2.3")
;; - MELPA Date: "YYYYMMDD.HHMM" (e.g., "20250618.1234")
;; - Mixed comparisons between different formats

;;; Code:

(require 'cl-lib)

;; Try to load version-constraint-engine from lib directory
(condition-case nil
    (require 'version-constraint-engine)
  (error
   (message "Warning: version-constraint-engine not available, using built-in version handling")))

;; Global version constraint engine
(defvar bfepm-version--engine nil
  "Version constraint engine instance for BFEPM.")

;; Forward declare VCE functions to avoid warnings
(declare-function vce-create-engine "version-constraint-engine")
(declare-function vce-compare-versions "version-constraint-engine")
(declare-function vce-satisfies-p "version-constraint-engine")
(declare-function vce-find-best-match "version-constraint-engine")
(declare-function vce-sort-versions "version-constraint-engine")

;; Constants for version pattern matching (fallback)
(defconst bfepm-version--melpa-date-pattern "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$"
  "Pattern for MELPA date versions (YYYYMMDD.HHMM format).")

(defconst bfepm-version--date-prefix-pattern "^[0-9]\\{4,8\\}$"
  "Pattern for date prefix requirements (YYYY to YYYYMMDD).")

(defconst bfepm-version--year-length 4
  "Length of year component in date versions.")

(defun bfepm-version--ensure-engine ()
  "Ensure version constraint engine is initialized."
  (unless bfepm-version--engine
    (if (fboundp 'vce-create-engine)
        (setq bfepm-version--engine
              (vce-create-engine :name "bfepm-version"))
      ;; Fallback: use a simple marker to indicate fallback mode
      (setq bfepm-version--engine 'fallback))))

(defun bfepm-version-compare (v1 v2)
  "Compare version strings V1 and V2.
Return 1 if V1 > V2, -1 if V1 < V2, 0 if equal.

Supports both semantic versions (e.g., '1.2.3') and MELPA date versions
(e.g., '20250426.1319')."
  (bfepm-version--ensure-engine)
  (if (and (not (eq bfepm-version--engine 'fallback))
           (fboundp 'vce-compare-versions))
      ;; Use version-constraint-engine if available
      (vce-compare-versions bfepm-version--engine v1 v2)
    ;; Fallback implementation
    (bfepm-version--compare-fallback v1 v2)))

(defun bfepm-version--compare-fallback (v1 v2)
  "Fallback version comparison implementation."
  (if (and (bfepm-version--is-melpa-date-version-p v1)
           (bfepm-version--is-melpa-date-version-p v2))
      ;; Both are MELPA date versions
      (let ((num1 (string-to-number (replace-regexp-in-string "\\." "" v1)))
            (num2 (string-to-number (replace-regexp-in-string "\\." "" v2))))
        (cond ((> num1 num2) 1)
              ((< num1 num2) -1)
              (t 0)))
    ;; Standard semantic version handling
    (let ((parts1 (mapcar #'string-to-number (split-string v1 "\\.")))
          (parts2 (mapcar #'string-to-number (split-string v2 "\\."))))
      (bfepm-version--compare-parts parts1 parts2))))

(defun bfepm-version--compare-parts (parts1 parts2)
  "Compare version parts lists PARTS1 and PARTS2."
  (cond
   ((and (null parts1) (null parts2)) 0)
   ((null parts1) -1)
   ((null parts2) 1)
   ((> (car parts1) (car parts2)) 1)
   ((< (car parts1) (car parts2)) -1)
   (t (bfepm-version--compare-parts (cdr parts1) (cdr parts2)))))

(defun bfepm-version-satisfies-p (version requirement)
  "Check if VERSION satisfies REQUIREMENT.

REQUIREMENT can be:
- \\='latest\\=' - matches any version
- Exact version string (e.g., \\='1.2.3\\=')
- Caret constraint (e.g., \\='^1.2.3\\=') - compatible version
- Tilde constraint (e.g., \\='~1.2.3\\=') - patch level changes only

Returns t if VERSION satisfies REQUIREMENT, nil otherwise."
  (bfepm-version--ensure-engine)
  (if (and (not (eq bfepm-version--engine 'fallback))
           (fboundp 'vce-satisfies-p))
      ;; Use version-constraint-engine if available
      (vce-satisfies-p bfepm-version--engine version requirement)
    ;; Fallback implementation
    (bfepm-version--satisfies-p-fallback version requirement)))

(defun bfepm-version--is-melpa-date-version-p (version)
  "Check if VERSION is a MELPA date version (YYYYMMDD.HHMM format)."
  (string-match-p bfepm-version--melpa-date-pattern version))

(defun bfepm-version--satisfies-p-fallback (version requirement)
  "Fallback implementation for version constraint satisfaction."
  (cond
   ((string= requirement "latest") t)
   ((string-prefix-p "^" requirement)
    (bfepm-version--satisfies-caret-p version (substring requirement 1)))
   ((string-prefix-p "~" requirement)
    (bfepm-version--satisfies-tilde-p version (substring requirement 1)))
   (t (string= version requirement))))

(defun bfepm-version--satisfies-caret-p (version requirement)
  "Check if VERSION satisfies caret REQUIREMENT (^).
Caret constraints allow compatible changes within the same major version."
  (if (bfepm-version--is-melpa-date-version-p version)
      (bfepm-version--satisfies-caret-melpa-p version requirement)
    (bfepm-version--satisfies-caret-semantic-p version requirement)))

(defun bfepm-version--satisfies-caret-melpa-p (version requirement)
  "Check if MELPA date VERSION satisfies caret REQUIREMENT (^).
For MELPA date versions, caret allows any version within the same year.
Supports YYYYMMDD.HHMM format or date prefix."
  (if (bfepm-version--is-melpa-date-version-p requirement)
      ;; Both are MELPA date versions - same year
      (let ((ver-date (car (split-string version "\\.")))
            (req-date (car (split-string requirement "\\."))))
        (and (>= (bfepm-version-compare version requirement) 0)
             (string= (substring ver-date 0 bfepm-version--year-length) 
                      (substring req-date 0 bfepm-version--year-length))))
    ;; Version is MELPA date but requirement is not - assume requirement is date prefix
    (let ((ver-date (car (split-string version "\\."))))
      (unless (string-match-p bfepm-version--date-prefix-pattern requirement)
        (error "[BFEPM Version] Invalid date prefix requirement: %s" requirement))
      (and (>= (string-to-number ver-date) (string-to-number requirement))
           (string= (substring ver-date 0 bfepm-version--year-length) 
                    (substring requirement 0 bfepm-version--year-length))))))

(defun bfepm-version--satisfies-caret-semantic-p (version requirement)
  "Check if semantic VERSION satisfies caret REQUIREMENT (^).
For semantic versions, caret allows compatible changes within same major.
VERSION must be >= REQUIREMENT and < next major version."
  (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
        (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
    (and (>= (bfepm-version-compare version requirement) 0)
         (< (car ver-parts) (1+ (car req-parts))))))

(defun bfepm-version--satisfies-tilde-p (version requirement)
  "Check if VERSION satisfies tilde REQUIREMENT (~).
Tilde constraints allow patch-level changes only."
  (if (bfepm-version--is-melpa-date-version-p version)
      (bfepm-version--satisfies-tilde-melpa-p version requirement)
    (bfepm-version--satisfies-tilde-semantic-p version requirement)))

(defun bfepm-version--satisfies-tilde-melpa-p (version requirement)
  "Check if MELPA date VERSION satisfies tilde REQUIREMENT (~).
For MELPA date versions, tilde allows versions within same day/prefix.
This provides more restrictive matching than caret constraints."
  (if (bfepm-version--is-melpa-date-version-p requirement)
      ;; Both are MELPA date versions - same day
      (let ((ver-date (car (split-string version "\\.")))
            (req-date (car (split-string requirement "\\."))))
        (and (>= (bfepm-version-compare version requirement) 0)
             (string= ver-date req-date)))
    ;; Version is MELPA date but requirement is not - check day prefix match
    (let ((ver-date (car (split-string version "\\."))))
      (unless (string-match-p bfepm-version--date-prefix-pattern requirement)
        (error "[BFEPM Version] Invalid date prefix requirement: %s" requirement))
      (and (>= (string-to-number ver-date) (string-to-number requirement))
           (string-prefix-p requirement ver-date)))))

(defun bfepm-version--satisfies-tilde-semantic-p (version requirement)
  "Check if semantic VERSION satisfies tilde REQUIREMENT (~).
For semantic versions, tilde allows patch-level changes only.
VERSION must be >= REQUIREMENT with same major.minor versions."
  (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
        (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
    (and (>= (bfepm-version-compare version requirement) 0)
         (= (car ver-parts) (car req-parts))
         (= (cadr ver-parts) (cadr req-parts)))))

(defun bfepm-version-normalize (version)
  "Normalize VERSION string to a consistent format.
Handles various version formats and returns a canonical string representation."
  (cond
   ((stringp version) version)
   ((numberp version) (number-to-string version))
   ((and (listp version) version)
    ;; Handle list format like (20250426 1319)
    (cond
     ((= (length version) 1) (number-to-string (car version)))
     ((= (length version) 2) (format "%s.%s" (car version) (cadr version)))
     (t (mapconcat #'number-to-string version "."))))
   (t "unknown")))

(defun bfepm-version-parse-constraint (constraint)
  "Parse a version CONSTRAINT string into its operator and version parts.
Returns a list (OPERATOR VERSION) where OPERATOR is one of:
\\='exact, \\='caret, \\='tilde, or \\='latest."
  (cond
   ((string= constraint "latest") '(latest nil))
   ((string-prefix-p "^" constraint) `(caret ,(substring constraint 1)))
   ((string-prefix-p "~" constraint) `(tilde ,(substring constraint 1)))
   (t `(exact ,constraint))))

(defun bfepm-version-constraint-matches-p (version constraint)
  "Check if VERSION matches CONSTRAINT using parsed constraint format.
CONSTRAINT should be the result of `bfepm-version-parse-constraint'."
  (let ((operator (car constraint))
        (required-version (cadr constraint)))
    (pcase operator
      ('latest t)
      ('exact (string= version required-version))
      ('caret (bfepm-version--satisfies-caret-p version required-version))
      ('tilde (bfepm-version--satisfies-tilde-p version required-version))
      (_ nil))))

(defun bfepm-version-find-best-match (available-versions constraints)
  "Find the best version from AVAILABLE-VERSIONS that satisfies CONSTRAINTS.
AVAILABLE-VERSIONS is a list of version strings.
CONSTRAINTS is a list of constraint strings.
Returns the highest version that satisfies all constraints, or nil if none."
  (bfepm-version--ensure-engine)
  (if (and (not (eq bfepm-version--engine 'fallback))
           (fboundp 'vce-satisfies-p)
           (fboundp 'vce-sort-versions))
      ;; Use version-constraint-engine for all cases
      (let ((matching-versions
             (cl-remove-if-not
              (lambda (version)
                (cl-every (lambda (constraint)
                            (vce-satisfies-p bfepm-version--engine version constraint))
                          constraints))
              available-versions)))
        (when matching-versions
          ;; Sort descending to get the highest version first
          (car (vce-sort-versions bfepm-version--engine matching-versions t))))
    ;; Fallback implementation
    (bfepm-version--find-best-match-fallback available-versions constraints)))

(defun bfepm-version--find-best-match-fallback (available-versions constraints)
  "Fallback implementation for finding best matching version."
  (let ((matching-versions
         (cl-remove-if-not
          (lambda (version)
            (cl-every (lambda (constraint)
                        (bfepm-version-satisfies-p version constraint))
                      constraints))
          available-versions)))
    ;; Return the highest matching version
    (when matching-versions
      (car (sort matching-versions
                 (lambda (a b) (> (bfepm-version-compare a b) 0)))))))

(provide 'bfepm-version)

;;; bfepm-version.el ends here