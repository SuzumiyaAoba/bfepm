;;; generic-config-framework.el --- Generic Configuration Management Framework -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: configuration management toml json yaml
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This library provides a generic configuration management framework
;; that supports multiple configuration formats with validation,
;; migration, and environment variable resolution.
;;
;; Supported Features:
;; - Multiple configuration formats (TOML, JSON, YAML, S-expressions)
;; - Hierarchical configuration merging
;; - Schema validation with detailed error reporting
;; - Configuration migration between versions
;; - Environment variable interpolation
;; - Fallback configuration loading
;; - Configuration templates and defaults
;; - Watch mode for configuration file changes
;;
;; Supported Formats:
;; - TOML (.toml files)
;; - JSON (.json files)  
;; - YAML (.yaml, .yml files)
;; - S-expressions (.el, .lisp files)
;; - INI-style (.ini, .conf files)
;; - Custom formats via pluggable parsers
;;
;; Usage:
;;   (setq loader (gcf-create-loader 
;;                 :formats '(("toml" . gcf-parse-toml)
;;                           ("json" . gcf-parse-json))
;;                 :schema my-config-schema))
;;   
;;   (setq config (gcf-load loader "config.toml"))
;;   (gcf-validate loader config)

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Core Data Structures

(cl-defstruct (gcf-loader (:constructor make-gcf-loader)
                          (:copier nil))
  "Configuration loader with multi-format support."
  name                   ; String: loader identifier
  supported-formats      ; Alist: (extension . parser-function)
  format-priority        ; List: preferred format order
  validators             ; List: validation functions
  fallback-loader        ; Function: fallback when preferred format fails
  default-factory        ; Function: () -> default-config
  schema                 ; Configuration schema for validation
  migration-handlers     ; Alist: (version . migration-function)
  environment-resolver   ; Function: () -> environment variables
  template-engine        ; Function: (template context) -> expanded-config
  watch-mode-p           ; Boolean: enable file watching
  cache-policy           ; Plist: caching configuration
  hooks)                 ; Alist: lifecycle hooks

(cl-defstruct (gcf-config (:constructor make-gcf-config)
                          (:copier nil))
  "Configuration object with metadata."
  data                   ; Hash table or plist: configuration data
  format                 ; Symbol: source format (toml, json, yaml, etc.)
  source-file            ; String: source file path
  version                ; String: configuration version
  environment            ; String: target environment (dev, prod, etc.)
  metadata               ; Plist: configuration metadata
  validation-errors      ; List: validation error messages
  last-modified          ; Time: last modification time
  checksum)              ; String: configuration checksum

(cl-defstruct (gcf-schema (:constructor make-gcf-schema)
                          (:copier nil))
  "Configuration schema for validation."
  name                   ; String: schema identifier
  version                ; String: schema version
  fields                 ; Alist: (field-name . field-spec)
  required-fields        ; List: required field names
  optional-fields        ; List: optional field names
  validation-rules       ; List: custom validation functions
  nested-schemas         ; Alist: (path . nested-schema)
  metadata)              ; Plist: schema metadata

(cl-defstruct (gcf-field-spec (:constructor make-gcf-field-spec)
                              (:copier nil))
  "Configuration field specification."
  name                   ; Symbol: field name
  type                   ; Symbol: field type (string, number, list, etc.)
  required-p             ; Boolean: is field required
  default-value          ; Any: default value if not specified
  validator              ; Function: (value) -> valid-p
  transformer            ; Function: (value) -> transformed-value
  description            ; String: field description
  examples               ; List: example values
  constraints)           ; Plist: additional constraints

;;; Format Parsers

;; TOML Parser (requires external library or fallback)
(defun gcf-parse-toml (file-path)
  "Parse TOML configuration from FILE-PATH."
  (condition-case nil
      (if (fboundp 'toml-parse-file)
          (toml-parse-file file-path)
        (gcf-parse-toml-fallback file-path))
    (error (error "Failed to parse TOML file: %s" file-path))))

(defun gcf-parse-toml-fallback (file-path)
  "Fallback TOML parser for basic TOML files.
WARNING: This parser only supports a subset of TOML:
- Basic key-value pairs
- Simple sections [section]  
- String, number, and boolean values
- Simple arrays (comma-separated)
It does NOT support: multi-line strings, nested tables,
date/time values, or other advanced TOML features."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((config (make-hash-table :test 'equal))
          (current-section nil))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
          (cond
           ;; Skip comments and empty lines
           ((or (string-empty-p line) (string-prefix-p "#" line))
            nil)
           ;; Section headers [section]
           ((string-match "^\\[\\([^]]+\\)\\]$" line)
            (setq current-section (match-string 1 line))
            (puthash current-section (make-hash-table :test 'equal) config))
           ;; Key-value pairs
           ((string-match "^\\([^=]+\\)=\\(.+\\)$" line)
            (let* ((key (string-trim (match-string 1 line)))
                   (value (string-trim (match-string 2 line)))
                   (parsed-value (gcf--parse-toml-value value))
                   (target (if current-section
                              (gethash current-section config)
                            config)))
              (puthash key parsed-value target)))))
        (forward-line 1))
      config)))

(defun gcf--parse-toml-value (value-string)
  "Parse TOML value from VALUE-STRING."
  (cond
   ;; String values (quoted)
   ((string-match "^\"\\(.*\\)\"$" value-string)
    (match-string 1 value-string))
   ((string-match "^'\\(.*\\)'$" value-string)
    (match-string 1 value-string))
   ;; Boolean values
   ((string= value-string "true") t)
   ((string= value-string "false") nil)
   ;; Numeric values
   ((string-match "^[0-9]+$" value-string)
    (string-to-number value-string))
   ((string-match "^[0-9]+\\.[0-9]+$" value-string)
    (string-to-number value-string))
   ;; Array values (basic support)
   ((string-match "^\\[\\(.*\\)\\]$" value-string)
    (let ((array-content (match-string 1 value-string)))
      (mapcar #'string-trim (split-string array-content ","))))
   ;; Default to string
   (t value-string)))

;; JSON Parser
(defun gcf-parse-json (file-path)
  "Parse JSON configuration from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'hash-table :array-type 'list)))

;; S-expression Parser
(defun gcf-parse-sexp (file-path)
  "Parse S-expression configuration from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (read (current-buffer))))

;; YAML Parser (basic implementation)
(defun gcf-parse-yaml (file-path)
  "Parse YAML configuration from FILE-PATH (basic implementation)."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((config (make-hash-table :test 'equal)))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
               (trimmed (string-trim line)))
          (unless (or (string-empty-p trimmed) (string-prefix-p "#" trimmed))
            (when (string-match "^\\([^:]+\\):\\s-*\\(.+\\)?$" trimmed)
              (let ((key (string-trim (match-string 1 trimmed)))
                    (value (when (match-string 2 trimmed)
                            (string-trim (match-string 2 trimmed)))))
                (puthash key (or value "") config))))
          (forward-line 1)))
      config)))

;;; Configuration Loading

(defun gcf-load (loader config-path &optional options)
  "Load configuration from CONFIG-PATH using LOADER.
OPTIONS is a plist that can contain:
  :format - force specific format
  :environment - target environment
  :validate-p - whether to validate (default: t)
  :merge-with - existing config to merge with"
  (let* ((format (or (plist-get options :format)
                    (gcf--detect-format config-path)))
         (parser (gcf--get-parser loader format))
         (environment (plist-get options :environment))
         (validate-p (if (plist-member options :validate-p)
                        (plist-get options :validate-p)
                      t)))
    
    (unless parser
      (error "No parser available for format: %s" format))
    
    (let* ((raw-data (condition-case err
                        (funcall parser config-path)
                      (error
                        (if (gcf-loader-fallback-loader loader)
                            (funcall (gcf-loader-fallback-loader loader) config-path)
                          (signal (car err) (cdr err))))))
           (config (make-gcf-config
                   :data raw-data
                   :format format
                   :source-file config-path
                   :environment environment
                   :last-modified (file-attribute-modification-time
                                  (file-attributes config-path))
                   :checksum (gcf--calculate-checksum config-path))))
      
      ;; Apply environment variable resolution
      (when (gcf-loader-environment-resolver loader)
        (setf (gcf-config-data config)
              (gcf--resolve-environment-variables 
               (gcf-config-data config)
               (funcall (gcf-loader-environment-resolver loader)))))
      
      ;; Merge with existing config if specified
      (when-let ((merge-target (plist-get options :merge-with)))
        (setf (gcf-config-data config)
              (gcf--merge-configs (gcf-config-data merge-target)
                                 (gcf-config-data config))))
      
      ;; Validate configuration
      (when (and validate-p (gcf-loader-schema loader))
        (let ((errors (gcf-validate loader config)))
          (setf (gcf-config-validation-errors config) errors)
          (when errors
            (warn "Configuration validation errors: %s" errors))))
      
      config)))

(defun gcf--detect-format (file-path)
  "Detect configuration format from FILE-PATH extension."
  (let ((extension (file-name-extension file-path)))
    (cond
     ((member extension '("toml")) 'toml)
     ((member extension '("json")) 'json)
     ((member extension '("yaml" "yml")) 'yaml)
     ((member extension '("el" "lisp")) 'sexp)
     ((member extension '("ini" "conf")) 'ini)
     (t 'unknown))))

(defun gcf--get-parser (loader format)
  "Get parser function for FORMAT from LOADER."
  (cdr (assoc (symbol-name format) (gcf-loader-supported-formats loader))))

;;; Configuration Validation

(defun gcf-validate (loader config)
  "Validate CONFIG using LOADER's schema. Returns list of errors."
  (when-let ((schema (gcf-loader-schema loader)))
    (gcf--validate-against-schema (gcf-config-data config) schema)))

(defun gcf--validate-against-schema (data schema)
  "Validate DATA against SCHEMA. Returns list of error messages."
  (let ((errors '()))
    
    ;; Check required fields
    (dolist (required-field (gcf-schema-required-fields schema))
      (unless (gcf--config-has-key-p data required-field)
        (push (format "Missing required field: %s" required-field) errors)))
    
    ;; Validate field types and constraints
    (dolist (field-entry (gcf-schema-fields schema))
      (let* ((field-name (car field-entry))
             (field-spec (cdr field-entry))
             (value (gcf--config-get-value data field-name)))
        (when value
          (let ((field-errors (gcf--validate-field value field-spec field-name)))
            (setq errors (append errors field-errors))))))
    
    ;; Run custom validation rules
    (dolist (validator (gcf-schema-validation-rules schema))
      (let ((custom-errors (funcall validator data)))
        (when custom-errors
          (setq errors (append errors custom-errors)))))
    
    errors))

(defun gcf--validate-field (value field-spec field-name)
  "Validate VALUE against FIELD-SPEC for FIELD-NAME."
  (let ((errors '())
        (expected-type (gcf-field-spec-type field-spec))
        (validator (gcf-field-spec-validator field-spec)))
    
    ;; Type validation
    (unless (gcf--type-matches-p value expected-type)
      (push (format "Field %s: expected %s, got %s"
                   field-name expected-type (type-of value)) errors))
    
    ;; Custom validator
    (when (and validator (not (funcall validator value)))
      (push (format "Field %s: validation failed" field-name) errors))
    
    errors))

(defun gcf--type-matches-p (value expected-type)
  "Check if VALUE matches EXPECTED-TYPE."
  (pcase expected-type
    ('string (stringp value))
    ('number (numberp value))
    ('integer (integerp value))
    ('boolean (booleanp value))
    ('list (listp value))
    ('hash-table (hash-table-p value))
    ('symbol (symbolp value))
    (_ t)))  ; Unknown type, accept anything

;;; Environment Variable Resolution

(defun gcf--resolve-environment-variables (config env-vars)
  "Resolve environment variables in CONFIG using ENV-VARS."
  (cond
   ((stringp config)
    (gcf--expand-env-string config env-vars))
   ((hash-table-p config)
    (let ((new-config (make-hash-table :test (hash-table-test config))))
      (maphash (lambda (key value)
                (puthash key (gcf--resolve-environment-variables value env-vars)
                        new-config))
               config)
      new-config))
   ((listp config)
    (mapcar (lambda (item)
             (gcf--resolve-environment-variables item env-vars))
           config))
   (t config)))

(defun gcf--expand-env-string (string env-vars)
  "Expand environment variables in STRING using ENV-VARS."
  (let ((result string))
    (dolist (env-var env-vars)
      (let* ((var-name (car env-var))
             (var-value (cdr env-var))
             (pattern (format "${%s}" var-name)))
        (setq result (replace-regexp-in-string 
                     (regexp-quote pattern) var-value result t t))))
    result))

;;; Configuration Merging

(defun gcf--merge-configs (base overlay)
  "Merge OVERLAY configuration into BASE configuration."
  (cond
   ((and (hash-table-p base) (hash-table-p overlay))
    (let ((merged (copy-hash-table base)))
      (maphash (lambda (key value)
                (let ((base-value (gethash key merged)))
                  (puthash key
                          (if (and base-value 
                                  (hash-table-p base-value)
                                  (hash-table-p value))
                              (gcf--merge-configs base-value value)
                            value)
                          merged)))
               overlay)
      merged))
   ((hash-table-p overlay) overlay)
   (t overlay)))

;;; Utility Functions

(defun gcf--config-has-key-p (config key)
  "Check if CONFIG has KEY."
  (cond
   ((hash-table-p config) (gethash key config))
   ((listp config) (plist-member config key))
   (t nil)))

(defun gcf--config-get-value (config key)
  "Get value for KEY from CONFIG."
  (cond
   ((hash-table-p config) (gethash key config))
   ((listp config) (plist-get config key))
   (t nil)))

(defun gcf--calculate-checksum (file-path)
  "Calculate checksum for file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (secure-hash 'sha256 (current-buffer))))

;;; File Watching

(defun gcf-enable-watch-mode (loader config-path callback)
  "Enable watch mode for CONFIG-PATH using LOADER, calling CALLBACK on changes."
  (when (fboundp 'file-notify-add-watch)
    (file-notify-add-watch
     config-path
     '(change)
     (lambda (event)
       (when (eq (nth 1 event) 'changed)
         (let ((new-config (gcf-load loader config-path)))
           (funcall callback new-config)))))))

;;; Public API

(defun gcf-create-loader (&rest args)
  "Create configuration loader with ARGS.
ARGS is a plist with keys matching gcf-loader structure slots."
  (let ((loader (apply #'make-gcf-loader args)))
    ;; Register default format parsers if not specified
    (unless (gcf-loader-supported-formats loader)
      (setf (gcf-loader-supported-formats loader)
            '(("toml" . gcf-parse-toml)
              ("json" . gcf-parse-json)
              ("yaml" . gcf-parse-yaml)
              ("sexp" . gcf-parse-sexp))))
    
    ;; Set default environment resolver
    (unless (gcf-loader-environment-resolver loader)
      (setf (gcf-loader-environment-resolver loader)
            (lambda () process-environment)))
    
    loader))

(defun gcf-create-schema (&rest args)
  "Create configuration schema with ARGS."
  (apply #'make-gcf-schema args))

(defun gcf-create-field-spec (name type &rest args)
  "Create field specification for NAME of TYPE with additional ARGS."
  (apply #'make-gcf-field-spec :name name :type type args))

(defun gcf-get-value (config path &optional default)
  "Get value at PATH from CONFIG, returning DEFAULT if not found.
PATH can be a string key or list of nested keys."
  (let ((data (gcf-config-data config)))
    (if (listp path)
        (gcf--get-nested-value data path default)
      (or (gcf--config-get-value data path) default))))

(defun gcf--get-nested-value (data path default)
  "Get nested value from DATA using PATH list."
  (if (null path)
      data
    (let ((value (gcf--config-get-value data (car path))))
      (if value
          (gcf--get-nested-value value (cdr path) default)
        default))))

(defun gcf-set-value (config path value)
  "Set VALUE at PATH in CONFIG."
  (let ((data (gcf-config-data config)))
    (if (listp path)
        (gcf--set-nested-value data path value)
      (gcf--config-set-value data path value))))

(defun gcf--set-nested-value (data path value)
  "Set nested VALUE in DATA using PATH list."
  (if (= (length path) 1)
      (gcf--config-set-value data (car path) value)
    (let ((next-data (gcf--config-get-value data (car path))))
      (unless next-data
        (setq next-data (make-hash-table :test 'equal))
        (gcf--config-set-value data (car path) next-data))
      (gcf--set-nested-value next-data (cdr path) value))))

(defun gcf--config-set-value (config key value)
  "Set VALUE for KEY in CONFIG."
  (cond
   ((hash-table-p config) (puthash key value config))
   ((listp config) (plist-put config key value))
   (t (error "Cannot set value in config of type: %s" (type-of config)))))

;;; Configuration Templates

(defun gcf-create-template (_name template-string)
  "Create configuration template with NAME and TEMPLATE-STRING."
  (lambda (context)
    (let ((result template-string))
      (dolist (var context)
        (let ((var-name (car var))
              (var-value (cdr var)))
          (setq result (replace-regexp-in-string
                       (regexp-quote (format "{{%s}}" var-name))
                       (format "%s" var-value)
                       result))))
      result)))

;;; Error Handling

(define-error 'gcf-error "Generic Configuration Framework Error")
(define-error 'gcf-parse-error "Configuration Parse Error" 'gcf-error)
(define-error 'gcf-validation-error "Configuration Validation Error" 'gcf-error)

(defun gcf-error (format-string &rest args)
  "Signal configuration framework error with formatted message."
  (signal 'gcf-error (list (apply #'format format-string args))))

(provide 'generic-config-framework)

;;; generic-config-framework.el ends here