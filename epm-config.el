;;; epm-config.el --- EPM Configuration management -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration file handling for EPM.
;; Supports TOML format for the main configuration.

;;; Code:

(require 'epm-core)
(require 'epm-utils)

;; TOML support is optional
(defvar epm-config--toml-available nil
  "Whether TOML parsing is available.")

(condition-case nil
    (progn
      (require 'toml)
      (setq epm-config--toml-available t))
  (error 
   (message "Warning: TOML parser not available - TOML config files will not be supported")
   (setq epm-config--toml-available nil)))

(defvar epm-config--default-sources
  '(("melpa" . ((url . "https://melpa.org/packages/")
                (type . "elpa")
                (priority . 10)))
    ("gnu" . ((url . "https://elpa.gnu.org/packages/")
              (type . "elpa")
              (priority . 5)))
    ("melpa-stable" . ((url . "https://stable.melpa.org/packages/")
                       (type . "elpa")
                       (priority . 7))))
  "Default package sources.")

(defun epm-config-load (file)
  "Load EPM configuration from TOML FILE."
  (if (file-exists-p file)
      (condition-case err
          (epm-config--parse-toml-file file)
        (error
         (epm-utils-error "Failed to load config file %s: %s" file err)))
    (epm-utils-error "Config file not found: %s" file)))

(defun epm-config--parse-toml-file (file)
  "Parse TOML configuration FILE and return epm-config structure."
  (if epm-config--toml-available
      (let* ((toml-data (toml:read-from-file file))
             (packages (epm-config--parse-packages (alist-get 'packages toml-data)))
             (sources (epm-config--parse-sources (alist-get 'sources toml-data))))
        
        (make-epm-config
         :packages packages
         :sources (or sources (epm-config--default-sources))))
    (error "TOML parser not available - cannot parse config file")))

(defun epm-config--parse-packages (packages-data)
  "Parse packages section from TOML data."
  (when packages-data
    (mapcar (lambda (entry)
              (let ((name (symbol-name (car entry)))
                    (spec (cdr entry)))
                (epm-config--parse-package-spec name spec)))
            packages-data)))

(defun epm-config--parse-package-spec (name spec)
  "Parse a single package specification."
  (cond
   ((stringp spec)
    ;; Simple version specification: "latest" or "1.2.3"
    (make-epm-package :name name :version spec))
   
   ((listp spec)
    ;; Complex specification with version, source, etc.
    (let ((version (or (alist-get 'version spec) "latest"))
          (optional (alist-get 'optional spec))
          (source (alist-get 'source spec)))
      (make-epm-package
       :name name
       :version version
       :source source
       :status (if optional 'optional 'required))))
   
   (t (epm-utils-error "Invalid package specification for %s: %s" name spec))))

(defun epm-config--parse-sources (sources-data)
  "Parse sources section from TOML data."
  (when sources-data
    (mapcar (lambda (entry)
              (let ((name (symbol-name (car entry)))
                    (spec (cdr entry)))
                (cons name (epm-config--parse-source-spec spec))))
            sources-data)))

(defun epm-config--parse-source-spec (spec)
  "Parse a single source specification."
  (make-epm-source
   :name (alist-get 'name spec)
   :url (alist-get 'url spec)
   :type (or (alist-get 'type spec) "elpa")
   :priority (or (alist-get 'priority spec) 10)))

(defun epm-config-create-default ()
  "Create a default EPM configuration."
  (make-epm-config
   :packages nil
   :sources (epm-config--default-sources)))

(defun epm-config-save (config file)
  "Save EPM CONFIG to TOML FILE."
  (let ((toml-data (epm-config--to-toml config)))
    (with-temp-buffer
      (insert (epm-config--toml-encode toml-data))
      (write-file file))))

(defun epm-config--to-toml (config)
  "Convert epm-config structure to TOML-compatible alist."
  (let ((result '()))
    
    ;; Add meta section
    (push (cons 'meta
                `((version . "1.0.0")
                  (created . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))
                  (updated . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))))
          result)
    
    ;; Add sources
    (when (epm-config-sources config)
      (push (cons 'sources (epm-config--sources-to-toml (epm-config-sources config)))
            result))
    
    ;; Add packages
    (when (epm-config-packages config)
      (push (cons 'packages (epm-config--packages-to-toml (epm-config-packages config)))
            result))
    
    (nreverse result)))

(defun epm-config--sources-to-toml (sources)
  "Convert sources list to TOML format."
  (mapcar (lambda (source)
            (cons (intern (car source))
                  `((url . ,(epm-source-url (cdr source)))
                    (type . ,(epm-source-type (cdr source)))
                    (priority . ,(epm-source-priority (cdr source))))))
          sources))

(defun epm-config--packages-to-toml (packages)
  "Convert packages list to TOML format."
  (mapcar (lambda (package)
            (cons (intern (epm-package-name package))
                  (epm-package-version package)))
          packages))

(defun epm-config--toml-encode (data)
  "Encode DATA as TOML string."
  ;; Simple TOML encoder - for now just create a basic format
  ;; In a real implementation, you'd use a proper TOML library
  (mapconcat (lambda (section)
               (let ((name (car section))
                     (content (cdr section)))
                 (concat "[" (symbol-name name) "]\n"
                         (epm-config--encode-section content))))
             data "\n\n"))

(defun epm-config--encode-section (section)
  "Encode a TOML section."
  (mapconcat (lambda (entry)
               (let ((key (car entry))
                     (value (cdr entry)))
                 (format "%s = %s" 
                         (symbol-name key)
                         (epm-config--encode-value value))))
             section "\n"))

(defun epm-config--encode-value (value)
  "Encode a value for TOML."
  (cond
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   ((symbolp value) (format "\"%s\"" (symbol-name value)))
   ((listp value) (format "[%s]" 
                          (mapconcat #'epm-config--encode-value value ", ")))
   (t (format "\"%s\"" value))))

(defun epm-config-validate (config)
  "Validate EPM configuration structure."
  (unless (epm-config-p config)
    (epm-utils-error "Invalid configuration structure"))
  
  ;; Validate required fields
  (unless (epm-config-sources config)
    (epm-utils-error "No package sources defined"))
  
  ;; Validate package specifications
  (dolist (package (epm-config-packages config))
    (unless (epm-package-name package)
      (epm-utils-error "Package missing name"))
    (unless (epm-package-version package)
      (epm-utils-error "Package %s missing version" (epm-package-name package))))
  
  t)

(defun epm-config-get-package (config package-name)
  "Get package specification for PACKAGE-NAME from CONFIG."
  (cl-find package-name (epm-config-packages config)
           :key #'epm-package-name :test #'string=))

(defun epm-config-get-source (config source-name)
  "Get source specification for SOURCE-NAME from CONFIG."
  (alist-get source-name (epm-config-sources config) nil nil #'string=))

(provide 'epm-config)

;;; epm-config.el ends here
