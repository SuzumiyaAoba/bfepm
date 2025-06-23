;;; bfepm-config.el --- BFEPM Configuration management -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration file handling for BFEPM.
;; Supports TOML format for the main configuration.

;;; Code:

(require 'bfepm-core)
(require 'bfepm-utils)

;; TOML support is optional
(defvar bfepm-config--toml-available nil
  "Whether TOML parsing is available.")

(condition-case nil
    (progn
      (require 'toml)
      (setq bfepm-config--toml-available t))
  (error
   (message "Warning: TOML parser not available - TOML config files will not be supported")
   (setq bfepm-config--toml-available nil)))

;; Declare external functions to avoid compilation warnings
(declare-function toml:read-from-file "toml")

(defvar bfepm-config--default-sources
  `(("melpa" . ,(make-bfepm-source
                 :name "melpa"
                 :url "https://melpa.org/packages/"
                 :type "elpa"
                 :priority 10))
    ("gnu" . ,(make-bfepm-source
               :name "gnu"
               :url "https://elpa.gnu.org/packages/"
               :type "elpa"
               :priority 5))
    ("melpa-stable" . ,(make-bfepm-source
                        :name "melpa-stable"
                        :url "https://stable.melpa.org/packages/"
                        :type "elpa"
                        :priority 7)))
  "Default package sources.")

(defun bfepm-config-load (file)
  "Load BFEPM configuration from TOML FILE."
  (if (file-exists-p file)
      (condition-case err
          (bfepm-config--parse-toml-file file)
        (error
         (bfepm-utils-error "Failed to load config file %s: %s" file err)))
    (bfepm-utils-error "Config file not found: %s" file)))

(defun bfepm-config--parse-toml-file (file)
  "Parse TOML configuration FILE and return bfepm-config structure."
  (if bfepm-config--toml-available
      (let* ((toml-data (toml:read-from-file file))
             (packages (bfepm-config--parse-packages (alist-get 'packages toml-data)))
             (sources (bfepm-config--parse-sources (alist-get 'sources toml-data)))
             (profiles (bfepm-config--parse-profiles (alist-get 'profiles toml-data))))
        
        (make-bfepm-config
         :packages packages
         :sources (or sources bfepm-config--default-sources)
         :profiles profiles))
    (error "TOML parser not available - cannot parse config file")))

(defun bfepm-config--parse-packages (packages-data)
  "Parse packages section from TOML data.
PACKAGES-DATA is the raw package data from TOML parsing."
  (when packages-data
    (mapcar (lambda (entry)
              (let ((name (symbol-name (car entry)))
                    (spec (cdr entry)))
                (bfepm-config--parse-package-spec name spec)))
            packages-data)))

(defun bfepm-config--parse-package-spec (name spec)
  "Parse a single package specification.
NAME is the package name and SPEC is the specification data."
  (cond
   ((stringp spec)
    ;; Simple version specification: "latest" or "1.2.3"
    (make-bfepm-package :name name :version spec))
   
   ((listp spec)
    ;; Check if this is a git package specification
    (let ((git-url (alist-get 'git spec)))
      (if git-url
          ;; Git package specification
          (let ((branch (alist-get 'branch spec))
                (tag (alist-get 'tag spec))
                (ref (alist-get 'ref spec)))
            ;; Create git source configuration
            (let ((git-source (list :url git-url :type "git")))
              (when branch (setq git-source (plist-put git-source :ref branch)))
              (when tag (setq git-source (plist-put git-source :ref tag)))
              (when ref (setq git-source (plist-put git-source :ref ref)))
              (make-bfepm-package
               :name name
               :version (or branch tag ref "latest")
               :source git-source
               :status 'required)))
        ;; Regular complex specification with version, source, etc.
        (let ((version (or (alist-get 'version spec) "latest"))
              (optional (alist-get 'optional spec))
              (source (alist-get 'source spec)))
          (make-bfepm-package
           :name name
           :version version
           :source source
           :status (if optional 'optional 'required))))))
   
   (t (bfepm-utils-error "Invalid package specification for %s: %s" name spec))))

(defun bfepm-config--parse-sources (sources-data)
  "Parse sources section from TOML data.
SOURCES-DATA is the raw sources data from TOML parsing."
  (when sources-data
    (mapcar (lambda (entry)
              (let ((name (symbol-name (car entry)))
                    (spec (cdr entry)))
                (cons name (bfepm-config--parse-source-spec spec))))
            sources-data)))

(defun bfepm-config--parse-source-spec (spec)
  "Parse a single source specification.
SPEC is the source specification data."
  (make-bfepm-source
   :name (alist-get 'name spec)
   :url (alist-get 'url spec)
   :type (or (alist-get 'type spec) "elpa")
   :priority (or (alist-get 'priority spec) 10)))

(defun bfepm-config--parse-profiles (profiles-data)
  "Parse profiles section from TOML data.
PROFILES-DATA is the raw profile data from TOML parsing."
  (when profiles-data
    (let ((profiles '()))
      (dolist (entry profiles-data)
        (let ((profile-name (symbol-name (car entry)))
              (profile-spec (cdr entry)))
          (cond
           ;; Profile definition like: development = ["base", "lsp"]
           ((and (listp profile-spec) (stringp (car profile-spec)))
            (push (cons profile-name profile-spec) profiles))
           ;; Profile with packages like: [profiles.development.packages]
           ((and (listp profile-spec) (alist-get 'packages profile-spec))
            (let ((includes (alist-get 'includes profile-spec))
                  (packages (alist-get 'packages profile-spec)))
              (push (cons profile-name
                          (list :includes includes :packages packages))
                    profiles)))
           ;; Simple profile list
           ((listp profile-spec)
            (push (cons profile-name profile-spec) profiles)))))
      (nreverse profiles))))

(defun bfepm-config-create-default ()
  "Create a default BFEPM configuration."
  (make-bfepm-config
   :packages nil
   :sources bfepm-config--default-sources
   :profiles nil))

(defun bfepm-config-save (config file)
  "Save BFEPM CONFIG to TOML FILE."
  (let ((toml-data (bfepm-config--to-toml config)))
    (with-temp-buffer
      (insert (bfepm-config--toml-encode toml-data))
      (write-file file))))

(defun bfepm-config--to-toml (config)
  "Convert bfepm-config structure to TOML-compatible alist.
CONFIG is the bfepm-config structure to convert."
  (let ((result '()))
    
    ;; Add meta section
    (push (cons 'meta
                `((version . "1.0.0")
                  (created . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))
                  (updated . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))))
          result)
    
    ;; Add sources
    (when (bfepm-config-sources config)
      (push (cons 'sources (bfepm-config--sources-to-toml (bfepm-config-sources config)))
            result))
    
    ;; Add packages
    (when (bfepm-config-packages config)
      (push (cons 'packages (bfepm-config--packages-to-toml (bfepm-config-packages config)))
            result))
    
    (nreverse result)))

(defun bfepm-config--sources-to-toml (sources)
  "Convert SOURCES list to TOML format."
  (mapcar (lambda (source)
            (cons (intern (car source))
                  `((url . ,(bfepm-source-url (cdr source)))
                    (type . ,(bfepm-source-type (cdr source)))
                    (priority . ,(bfepm-source-priority (cdr source))))))
          sources))

(defun bfepm-config--packages-to-toml (packages)
  "Convert PACKAGES list to TOML format."
  (mapcar (lambda (package)
            (cons (intern (bfepm-package-name package))
                  (bfepm-package-version package)))
          packages))

(defun bfepm-config--toml-encode (data)
  "Encode DATA as TOML string."
  ;; Simple TOML encoder - for now just create a basic format
  ;; In a real implementation, you'd use a proper TOML library
  (mapconcat (lambda (section)
               (let ((name (car section))
                     (content (cdr section)))
                 (concat "[" (symbol-name name) "]\n"
                         (bfepm-config--encode-section content))))
             data "\n\n"))

(defun bfepm-config--encode-section (section)
  "Encode a TOML SECTION."
  (mapconcat (lambda (entry)
               (let ((key (car entry))
                     (value (cdr entry)))
                 (format "%s = %s"
                         (symbol-name key)
                         (bfepm-config--encode-value value))))
             section "\n"))

(defun bfepm-config--encode-value (value)
  "Encode a VALUE for TOML."
  (cond
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   ((symbolp value) (format "\"%s\"" (symbol-name value)))
   ((listp value) (format "[%s]"
                          (mapconcat #'bfepm-config--encode-value value ", ")))
   (t (format "\"%s\"" value))))

(defun bfepm-config-validate (config)
  "Validate BFEPM configuration structure.
CONFIG is the configuration structure to validate."
  (unless (bfepm-config-p config)
    (bfepm-utils-error "Invalid configuration structure"))
  
  ;; Validate required fields
  (unless (bfepm-config-sources config)
    (bfepm-utils-error "No package sources defined"))
  
  ;; Validate package specifications
  (dolist (package (bfepm-config-packages config))
    (unless (bfepm-package-name package)
      (bfepm-utils-error "Package missing name"))
    (unless (bfepm-package-version package)
      (bfepm-utils-error "Package %s missing version" (bfepm-package-name package))))
  
  t)

(defun bfepm-config-get-package (config package-name)
  "Get package specification for PACKAGE-NAME from CONFIG."
  (cl-find package-name (bfepm-config-packages config)
           :key #'bfepm-package-name :test #'string=))

(defun bfepm-config-get-source (config source-name)
  "Get source specification for SOURCE-NAME from CONFIG."
  (alist-get source-name (bfepm-config-sources config) nil nil #'string=))

(provide 'bfepm-config)

;;; bfepm-config.el ends here
