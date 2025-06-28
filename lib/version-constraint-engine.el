;;; version-constraint-engine.el --- Generic Version Constraint System -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: version constraints semver comparison
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This library provides a generic version constraint satisfaction system
;; that can handle multiple version formats and constraint operators.
;; 
;; Supported Version Formats:
;; - Semantic Versioning (semver): "1.2.3", "2.0.0-alpha.1"
;; - Date-based versions: "20240618.1234" (MELPA style)
;; - Integer sequences: "1.2.3.4.5"
;; - Custom formats via pluggable parsers
;;
;; Supported Constraint Operators:
;; - Exact: "1.2.3" or "=1.2.3"
;; - Caret: "^1.2.3" (compatible within major version)
;; - Tilde: "~1.2.3" (compatible within minor version)
;; - Range: ">=1.2.0 <2.0.0"
;; - Latest: "latest" (any version)
;; - Custom operators via pluggable handlers
;;
;; Usage:
;;   (setq engine (vce-create-engine :name "semver"))
;;   (vce-add-format engine 'semver #'parse-semver #'compare-semver)
;;   (vce-satisfies-p engine "1.2.5" "^1.2.0")  ; => t
;;   (vce-find-best-match engine '("1.1.0" "1.2.0" "2.0.0") "^1.0.0")  ; => "1.2.0"

;;; Code:

(require 'cl-lib)

;;; Core Data Structures

(cl-defstruct (vce-engine (:constructor make-vce-engine)
                          (:copier nil))
  "Version constraint satisfaction engine."
  name                   ; String: engine identifier
  version-formats        ; Alist: (format-name . format-spec)
  constraint-operators   ; Alist: (operator . handler-function)
  default-format         ; Symbol: default version format
  strict-mode-p          ; Boolean: strict parsing vs permissive
  cache                  ; Hash table: memoization cache
  hooks)                 ; Alist: lifecycle hooks

(cl-defstruct (vce-version-format (:constructor make-vce-version-format)
                                  (:copier nil))
  "Version format specification."
  name                   ; Symbol: format identifier (semver, date, etc.)
  parser                 ; Function: (version-string) -> parsed-version
  comparator             ; Function: (version1 version2) -> {-1, 0, 1}
  normalizer             ; Function: (version) -> normalized-string
  validator              ; Function: (version-string) -> valid-p
  pattern                ; Regexp: version pattern for quick detection
  constraint-handlers    ; Alist: (operator . handler-function)
  metadata)              ; Plist: format-specific metadata

(cl-defstruct (vce-parsed-version (:constructor make-vce-parsed-version)
                                  (:copier nil))
  "Parsed version representation."
  original               ; String: original version string
  format                 ; Symbol: detected/specified format
  components             ; List: version components (numbers, strings)
  prerelease             ; List: prerelease components (for semver)
  build-metadata         ; String: build metadata (for semver)
  normalized             ; String: normalized representation
  metadata)              ; Plist: version-specific metadata

(cl-defstruct (vce-constraint (:constructor make-vce-constraint)
                              (:copier nil))
  "Version constraint representation."
  original               ; String: original constraint string
  operator               ; Symbol: constraint operator (exact, caret, tilde, etc.)
  version                ; vce-parsed-version: target version
  parameters             ; List: additional constraint parameters
  metadata)              ; Plist: constraint-specific metadata

;;; Version Format Implementations

;; Semantic Versioning (semver) support
(defconst vce--semver-pattern
  "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\(?:-\\([^+]+\\)\\)?\\(?:\\+\\(.+\\)\\)?$"
  "Regular expression for semantic version format.")

(defun vce--parse-semver (version-string)
  "Parse semantic version VERSION-STRING."
  (if (string-match vce--semver-pattern version-string)
      (let ((major (string-to-number (match-string 1 version-string)))
            (minor (string-to-number (match-string 2 version-string)))
            (patch (string-to-number (match-string 3 version-string)))
            (prerelease (match-string 4 version-string))
            (build (match-string 5 version-string)))
        (make-vce-parsed-version
         :original version-string
         :format 'semver
         :components (list major minor patch)
         :prerelease (when prerelease (split-string prerelease "\\."))
         :build-metadata build
         :normalized (format "%d.%d.%d" major minor patch)))
    (error "Invalid semantic version: %s" version-string)))

(defun vce--compare-semver (v1 v2)
  "Compare semantic versions V1 and V2."
  (let ((c1 (vce-parsed-version-components v1))
        (c2 (vce-parsed-version-components v2)))
    ;; Compare major.minor.patch
    (let ((result (vce--compare-version-components c1 c2)))
      (if (= result 0)
          ;; Equal base versions, compare prerelease
          (vce--compare-prerelease v1 v2)
        result))))

(defun vce--compare-prerelease (v1 v2)
  "Compare prerelease components of versions V1 and V2."
  (let ((p1 (vce-parsed-version-prerelease v1))
        (p2 (vce-parsed-version-prerelease v2)))
    (cond
     ((and (null p1) (null p2)) 0)  ; Both are releases
     ((null p1) 1)                  ; Release > prerelease
     ((null p2) -1)                 ; Prerelease < release
     (t (vce--compare-prerelease-components p1 p2)))))

(defun vce--compare-prerelease-components (p1 p2)
  "Compare prerelease component lists P1 and P2."
  (let ((len1 (length p1))
        (len2 (length p2))
        (i 0)
        (result 0))
    (while (and (= result 0) (< i (min len1 len2)))
      (let ((c1 (nth i p1))
            (c2 (nth i p2)))
        (setq result (vce--compare-prerelease-component c1 c2))
        (setq i (1+ i))))
    (if (= result 0)
        (- len1 len2)  ; Longer prerelease > shorter
      result)))

(defun vce--compare-prerelease-component (c1 c2)
  "Compare individual prerelease components C1 and C2."
  (let ((n1 (string-to-number c1))
        (n2 (string-to-number c2)))
    (cond
     ((and (= n1 0) (not (string= c1 "0"))
           (= n2 0) (not (string= c2 "0")))
      ;; Both are non-numeric strings
      (if (string< c1 c2) -1 (if (string> c1 c2) 1 0)))
     ((and (> n1 0) (> n2 0))
      ;; Both are numeric
      (- n1 n2))
     ((> n1 0)
      ;; c1 is numeric, c2 is string - numeric < string
      -1)
     (t
      ;; c1 is string, c2 is numeric - string > numeric
      1))))

;; Date version support (MELPA style)
(defconst vce--date-version-pattern "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$"
  "Regular expression for date version format (YYYYMMDD.HHMM).")

(defun vce--parse-date-version (version-string)
  "Parse date version VERSION-STRING (YYYYMMDD.HHMM format)."
  (if (string-match vce--date-version-pattern version-string)
      (let* ((parts (split-string version-string "\\."))
             (date-part (car parts))
             (time-part (cadr parts))
             (year (string-to-number (substring date-part 0 4)))
             (month (string-to-number (substring date-part 4 6)))
             (day (string-to-number (substring date-part 6 8)))
             (hour (string-to-number (substring time-part 0 2)))
             (minute (string-to-number (substring time-part 2 4))))
        (make-vce-parsed-version
         :original version-string
         :format 'date
         :components (list year month day hour minute)
         :normalized version-string))
    (error "Invalid date version: %s" version-string)))

(defun vce--compare-date-version (v1 v2)
  "Compare date versions V1 and V2."
  (let ((num1 (string-to-number (replace-regexp-in-string "\\." "" 
                                                         (vce-parsed-version-original v1))))
        (num2 (string-to-number (replace-regexp-in-string "\\." "" 
                                                         (vce-parsed-version-original v2)))))
    (cond ((> num1 num2) 1)
          ((< num1 num2) -1)
          (t 0))))

;; Generic version component comparison
(defun vce--compare-version-components (c1 c2)
  "Compare version component lists C1 and C2."
  (let ((len1 (length c1))
        (len2 (length c2))
        (i 0)
        (result 0))
    (while (and (= result 0) (< i (max len1 len2)))
      (let ((comp1 (if (< i len1) (nth i c1) 0))
            (comp2 (if (< i len2) (nth i c2) 0)))
        (setq result (- comp1 comp2))
        (setq i (1+ i))))
    (if (> result 0) 1 (if (< result 0) -1 0))))

;;; Constraint Operators

(defun vce--handle-exact-constraint (version constraint)
  "Check if VERSION satisfies exact CONSTRAINT."
  (= 0 (vce--compare-versions version (vce-constraint-version constraint))))

(defun vce--handle-caret-constraint (version constraint)
  "Check if VERSION satisfies caret CONSTRAINT (^)."
  (let ((target (vce-constraint-version constraint)))
    (when (eq (vce-parsed-version-format version) (vce-parsed-version-format target))
      (let ((v-comp (vce-parsed-version-components version))
            (t-comp (vce-parsed-version-components target)))
        (and (>= (vce--compare-versions version target) 0)
             (= (car v-comp) (car t-comp)))))))  ; Same major version

(defun vce--handle-tilde-constraint (version constraint)
  "Check if VERSION satisfies tilde CONSTRAINT (~)."
  (let ((target (vce-constraint-version constraint)))
    (when (eq (vce-parsed-version-format version) (vce-parsed-version-format target))
      (let ((v-comp (vce-parsed-version-components version))
            (t-comp (vce-parsed-version-components target)))
        (and (>= (vce--compare-versions version target) 0)
             (>= (length v-comp) 2)             ; Ensure at least major.minor
             (>= (length t-comp) 2)             ; Ensure at least major.minor
             (= (car v-comp) (car t-comp))      ; Same major
             (= (cadr v-comp) (cadr t-comp))))))) ; Same minor

(defun vce--handle-latest-constraint (_version _constraint)
  "Check if VERSION satisfies latest CONSTRAINT (any version)."
  t)

;;; Engine Management

(defvar *current-engine* nil
  "Current version constraint engine.")

(defun vce-create-engine (&rest args)
  "Create version constraint engine with ARGS.
ARGS is a plist with keys matching vce-engine structure slots."
  (let ((engine (apply #'make-vce-engine
                      :cache (make-hash-table :test 'equal)
                      args)))
    ;; Register default version formats
    (vce-register-format engine 'semver
                        (make-vce-version-format
                         :name 'semver
                         :parser #'vce--parse-semver
                         :comparator #'vce--compare-semver
                         :normalizer (lambda (v) (vce-parsed-version-normalized v))
                         :validator (lambda (s) (string-match-p vce--semver-pattern s))
                         :pattern vce--semver-pattern))
    
    (vce-register-format engine 'date
                        (make-vce-version-format
                         :name 'date
                         :parser #'vce--parse-date-version
                         :comparator #'vce--compare-date-version
                         :normalizer (lambda (v) (vce-parsed-version-normalized v))
                         :validator (lambda (s) (string-match-p vce--date-version-pattern s))
                         :pattern vce--date-version-pattern))
    
    ;; Register default constraint operators
    (vce-register-operator engine 'exact #'vce--handle-exact-constraint)
    (vce-register-operator engine 'caret #'vce--handle-caret-constraint)
    (vce-register-operator engine 'tilde #'vce--handle-tilde-constraint)
    (vce-register-operator engine 'latest #'vce--handle-latest-constraint)
    
    engine))

(defun vce-register-format (engine name format-spec)
  "Register version format FORMAT-SPEC with NAME in ENGINE."
  (let ((formats (vce-engine-version-formats engine)))
    (setf (vce-engine-version-formats engine)
          (cons (cons name format-spec)
                (cl-remove name formats :key #'car)))))

(defun vce-register-operator (engine operator handler)
  "Register constraint OPERATOR with HANDLER function in ENGINE."
  (let ((operators (vce-engine-constraint-operators engine)))
    (setf (vce-engine-constraint-operators engine)
          (cons (cons operator handler)
                (cl-remove operator operators :key #'car)))))

;;; Version Parsing and Comparison

(defun vce-parse-version (engine version-string &optional format)
  "Parse VERSION-STRING using ENGINE, optionally specifying FORMAT."
  (if format
      (let ((format-spec (cdr (assoc format (vce-engine-version-formats engine)))))
        (when format-spec
          (funcall (vce-version-format-parser format-spec) version-string)))
    ;; Auto-detect format
    (dolist (format-entry (vce-engine-version-formats engine))
      (let ((format-spec (cdr format-entry)))
        (when (funcall (vce-version-format-validator format-spec) version-string)
          (cl-return-from vce-parse-version (funcall (vce-version-format-parser format-spec) version-string)))))
    (error "Unable to parse version: %s" version-string)))

(defun vce-compare-versions (engine v1 v2)
  "Compare versions V1 and V2 using ENGINE."
  (let ((parsed-v1 (if (vce-parsed-version-p v1) v1 (vce-parse-version engine v1)))
        (parsed-v2 (if (vce-parsed-version-p v2) v2 (vce-parse-version engine v2))))
    (vce--compare-versions parsed-v1 parsed-v2)))

(defun vce--compare-versions (v1 v2)
  "Internal function to compare parsed versions V1 and V2."
  (if (eq (vce-parsed-version-format v1) (vce-parsed-version-format v2))
      ;; Same format, use format-specific comparator
      (let* ((format (vce-parsed-version-format v1))
             (format-spec (cdr (assoc format (vce-engine-version-formats *current-engine*))))
             (comparator (vce-version-format-comparator format-spec)))
        (funcall comparator v1 v2))
    ;; Different formats, fall back to string comparison of normalized versions
    (let ((n1 (vce-parsed-version-normalized v1))
          (n2 (vce-parsed-version-normalized v2)))
      (cond ((string< n1 n2) -1)
            ((string> n1 n2) 1)
            (t 0)))))

;;; Constraint Parsing and Satisfaction

(defun vce-parse-constraint (engine constraint-string)
  "Parse CONSTRAINT-STRING using ENGINE."
  (cond
   ((string= constraint-string "latest")
    (make-vce-constraint
     :original constraint-string
     :operator 'latest
     :version nil))
   
   ((string-prefix-p "^" constraint-string)
    (let ((version-part (substring constraint-string 1)))
      (make-vce-constraint
       :original constraint-string
       :operator 'caret
       :version (vce-parse-version engine version-part))))
   
   ((string-prefix-p "~" constraint-string)
    (let ((version-part (substring constraint-string 1)))
      (make-vce-constraint
       :original constraint-string
       :operator 'tilde
       :version (vce-parse-version engine version-part))))
   
   ((string-prefix-p "=" constraint-string)
    (let ((version-part (substring constraint-string 1)))
      (make-vce-constraint
       :original constraint-string
       :operator 'exact
       :version (vce-parse-version engine version-part))))
   
   (t
    ;; Default to exact match
    (make-vce-constraint
     :original constraint-string
     :operator 'exact
     :version (vce-parse-version engine constraint-string)))))

(defun vce-satisfies-p (engine version constraint)
  "Check if VERSION satisfies CONSTRAINT using ENGINE."
  (let ((parsed-version (if (vce-parsed-version-p version)
                           version
                         (vce-parse-version engine version)))
        (parsed-constraint (if (vce-constraint-p constraint)
                             constraint
                           (vce-parse-constraint engine constraint))))
    
    (let* ((operator (vce-constraint-operator parsed-constraint))
           (handler (cdr (assoc operator (vce-engine-constraint-operators engine)))))
      (if handler
          (let ((*current-engine* engine))  ; Dynamic binding for internal functions
            (funcall handler parsed-version parsed-constraint))
        (error "Unsupported constraint operator: %s" operator)))))

;;; Version Selection

(defun vce-find-best-match (engine versions constraint)
  "Find best version from VERSIONS that satisfies CONSTRAINT using ENGINE."
  (let ((matching-versions (cl-remove-if-not
                           (lambda (version)
                             (vce-satisfies-p engine version constraint))
                           versions)))
    (when matching-versions
      (car (sort matching-versions
                (lambda (v1 v2)
                  (> (vce-compare-versions engine v1 v2) 0)))))))

(defun vce-sort-versions (engine versions &optional descending-p)
  "Sort VERSIONS using ENGINE. If DESCENDING-P is t, sort in descending order."
  (sort (copy-sequence versions)
        (if descending-p
            (lambda (v1 v2) (> (vce-compare-versions engine v1 v2) 0))
          (lambda (v1 v2) (< (vce-compare-versions engine v1 v2) 0)))))

;;; Normalization

(defun vce-normalize-version (engine version)
  "Normalize VERSION using ENGINE."
  (let ((parsed (vce-parse-version engine version)))
    (vce-parsed-version-normalized parsed)))

;;; Public API Convenience Functions

(defun vce-version< (engine v1 v2)
  "Return t if V1 is less than V2 according to ENGINE."
  (< (vce-compare-versions engine v1 v2) 0))

(defun vce-version> (engine v1 v2)
  "Return t if V1 is greater than V2 according to ENGINE."
  (> (vce-compare-versions engine v1 v2) 0))

(defun vce-version= (engine v1 v2)
  "Return t if V1 equals V2 according to ENGINE."
  (= (vce-compare-versions engine v1 v2) 0))

;;; Error Handling

(define-error 'vce-error "Version Constraint Engine Error")
(define-error 'vce-parse-error "Version Parse Error" 'vce-error)
(define-error 'vce-constraint-error "Constraint Error" 'vce-error)

(defun vce-error (format-string &rest args)
  "Signal version constraint engine error with formatted message."
  (signal 'vce-error (list (apply #'format format-string args))))

;;; Engine Discovery and Registration

(defvar vce--registered-engines (make-hash-table :test 'equal)
  "Registry of named version constraint engines.")

(defun vce-register-engine (name engine)
  "Register ENGINE with NAME for global access."
  (puthash name engine vce--registered-engines))

(defun vce-get-engine (name)
  "Get registered engine by NAME."
  (gethash name vce--registered-engines))

(defun vce-list-engines ()
  "List all registered engine names."
  (hash-table-keys vce--registered-engines))

(provide 'version-constraint-engine)

;;; version-constraint-engine.el ends here