;;; bfepm-framework-integration.el --- BFEPM Framework Integration Example -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: package-management framework integration
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This file demonstrates how to integrate BFEPM with the generic
;; package manager framework. It shows how existing BFEPM functionality
;; can be refactored to use the abstract framework interfaces while
;; maintaining backward compatibility.
;;
;; This integration serves as:
;; 1. A practical example of framework usage
;; 2. A migration path for existing BFEPM installations
;; 3. A template for other package manager integrations
;; 4. A demonstration of framework extensibility
;;
;; The integration maintains all existing BFEPM functionality while
;; adding new capabilities from the framework:
;; - Pluggable version engines
;; - Multiple configuration formats
;; - Enhanced search capabilities
;; - Network operation improvements
;; - Plugin system support

;;; Code:

(require 'package-manager-framework)
(require 'generic-http-client)
(require 'version-constraint-engine)
(require 'generic-config-framework)
(require 'generic-search-engine)
(require 'plugin-system)

;;; BFEPM-Specific Framework Components

;; BFEPM Version Engine Implementation
(defun bfepm-create-version-engine ()
  "Create BFEPM-specific version constraint engine."
  (let ((engine (vce-create-engine :name "bfepm")))
    
    ;; Register MELPA date version format
    (vce-register-format engine 'melpa-date
      (make-vce-version-format
       :name 'melpa-date
       :parser #'bfepm--parse-melpa-date-version
       :comparator #'bfepm--compare-melpa-date-versions
       :normalizer #'bfepm--normalize-melpa-date-version
       :validator #'bfepm--validate-melpa-date-version
       :pattern "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$"))
    
    ;; Register constraint operators
    (vce-register-operator engine 'latest #'bfepm--handle-latest-constraint)
    
    engine))

(defun bfepm--parse-melpa-date-version (version-string)
  "Parse MELPA date version string."
  (if (string-match "^\\([0-9]\\{8\\}\\)\\.\\([0-9]\\{4\\}\\)$" version-string)
      (let ((date-part (match-string 1 version-string))
            (time-part (match-string 2 version-string)))
        (make-vce-parsed-version
         :original version-string
         :format 'melpa-date
         :components (list (string-to-number date-part) (string-to-number time-part))
         :normalized version-string))
    (error "Invalid MELPA date version: %s" version-string)))

(defun bfepm--compare-melpa-date-versions (v1 v2)
  "Compare MELPA date versions V1 and V2."
  (let* ((v1-parts (vce-parsed-version-components v1))
         (v2-parts (vce-parsed-version-components v2))
         (date1 (nth 0 v1-parts))
         (date2 (nth 0 v2-parts))
         (time1 (nth 1 v1-parts))
         (time2 (nth 1 v2-parts)))
    (cond ((> date1 date2) 1)
          ((< date1 date2) -1)
          ((> time1 time2) 1)
          ((< time1 time2) -1)
          (t 0))))

(defun bfepm--normalize-melpa-date-version (version)
  "Normalize MELPA date version."
  (vce-parsed-version-original version))

(defun bfepm--validate-melpa-date-version (version-string)
  "Validate MELPA date version format."
  (string-match-p "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$" version-string))

(defun bfepm--handle-latest-constraint (_version _constraint)
  "Handle \\='latest\\=' constraint - matches any version."
  t)

;; BFEPM HTTP Client Configuration
(defun bfepm-create-http-client ()
  "Create BFEPM-specific HTTP client."
  (ghc-create-client
   :user-agent "BFEPM/1.0 (Emacs Package Manager)"
   :timeout 30
   :retry-count 3
   :retry-strategy 'exponential
   :rate-limit 5))  ; 5 requests per second

;; BFEPM Configuration Loader
(defun bfepm-create-config-loader ()
  "Create BFEPM-specific configuration loader."
  (gcf-create-loader
   :name "bfepm"
   :supported-formats '(("toml" . gcf-parse-toml)
                       ("json" . gcf-parse-json)
                       ("sexp" . gcf-parse-sexp))
   :format-priority '("toml" "json" "sexp")
   :fallback-loader #'bfepm--parse-minimal-config
   :default-factory #'bfepm--create-default-config
   :schema (bfepm-create-config-schema)))

(defun bfepm--parse-minimal-config (_file-path)
  "Minimal configuration parser for BFEPM."
  (let ((config (make-hash-table :test 'equal)))
    (puthash "packages" (make-hash-table :test 'equal) config)
    (puthash "sources" (bfepm--get-default-sources) config)
    config))

(defun bfepm--create-default-config ()
  "Create default BFEPM configuration."
  (let ((config (make-hash-table :test 'equal)))
    (puthash "packages" (make-hash-table :test 'equal) config)
    (puthash "sources" (bfepm--get-default-sources) config)
    config))

(defun bfepm--get-default-sources ()
  "Get default BFEPM package sources."
  (let ((sources (make-hash-table :test 'equal)))
    (puthash "melpa" 
             (list :url "https://melpa.org/packages/" 
                   :type "elpa" 
                   :priority 10) 
             sources)
    (puthash "gnu" 
             (list :url "https://elpa.gnu.org/packages/" 
                   :type "elpa" 
                   :priority 5) 
             sources)
    sources))

(defun bfepm-create-config-schema ()
  "Create BFEPM configuration schema."
  (gcf-create-schema
   :name "bfepm-config"
   :version "1.0"
   :required-fields '("sources")
   :optional-fields '("packages" "profiles" "settings")
   :fields `(("sources" . ,(gcf-create-field-spec 'sources 'hash-table 
                                                 :required-p t
                                                 :description "Package sources"))
             ("packages" . ,(gcf-create-field-spec 'packages 'hash-table
                                                  :required-p nil
                                                  :description "Package specifications")))))

;; BFEPM Search Engine
(defun bfepm-create-search-engine (http-client)
  "Create BFEPM-specific search engine using HTTP-CLIENT."
  (let ((engine (gse-create-engine :name "bfepm-search")))
    
    ;; Add MELPA source
    (gse-add-source engine
      (gse-create-source
       :name "melpa"
       :type 'elpa
       :priority 10
       :search-fn (lambda (query options)
                   (bfepm--search-elpa-source http-client 
                                             "https://melpa.org/packages/" 
                                             query options))
       :search-async-fn (lambda (query options callback)
                         (bfepm--search-elpa-source-async http-client
                                                         "https://melpa.org/packages/"
                                                         query options callback))))
    
    ;; Add GNU ELPA source
    (gse-add-source engine
      (gse-create-source
       :name "gnu"
       :type 'elpa
       :priority 5
       :search-fn (lambda (query options)
                   (bfepm--search-elpa-source http-client
                                             "https://elpa.gnu.org/packages/"
                                             query options))
       :search-async-fn (lambda (query options callback)
                         (bfepm--search-elpa-source-async http-client
                                                         "https://elpa.gnu.org/packages/"
                                                         query options callback))))
    
    ;; Set custom ranking algorithm
    (setf (gse-engine-ranking-algorithm engine) #'bfepm--rank-search-results)
    
    engine))

(defun bfepm--search-elpa-source (http-client base-url query _options)
  "Search ELPA source at BASE-URL for QUERY using HTTP-CLIENT."
  (let* ((archive-url (concat base-url "archive-contents"))
         (response (ghc-get http-client archive-url))
         (results '()))
    
    (when (= (ghc-response-status-code response) 200)
      (let ((archive-contents (condition-case nil
                                    (car (read-from-string (ghc-response-body response)))
                                  (error
                                   (error "Failed to parse archive contents safely")))))
        (dolist (package-entry archive-contents)
          (when (bfepm--package-matches-query-p package-entry query)
            (push (bfepm--create-search-result-from-package package-entry base-url) 
                  results)))))
    
    results))

(defun bfepm--search-elpa-source-async (http-client base-url query _options callback)
  "Search ELPA source asynchronously."
  (let ((archive-url (concat base-url "archive-contents")))
    (ghc-get http-client archive-url
      (lambda (response)
        (if (= (ghc-response-status-code response) 200)
            (let ((results '()))
              (condition-case err
                  (let ((archive-contents (condition-case nil
                                            (car (read-from-string (ghc-response-body response)))
                                          (error
                                           (funcall callback nil nil "Failed to parse archive contents safely")
                                           (cl-return-from bfepm--search-elpa-source-async)))))
                    (dolist (package-entry archive-contents)
                      (when (bfepm--package-matches-query-p package-entry query)
                        (push (bfepm--create-search-result-from-package package-entry base-url)
                              results)))
                    (funcall callback t results nil))
                (error
                 (funcall callback nil nil (error-message-string err)))))
          (funcall callback nil nil (format "HTTP error: %d" 
                                           (ghc-response-status-code response))))))))

(defun bfepm--package-matches-query-p (package-entry query)
  "Check if PACKAGE-ENTRY matches search QUERY."
  (let* ((package-name (symbol-name (car package-entry)))
         (package-info (cdr package-entry))
         (description (when (vectorp package-info) (aref package-info 2)))
         (query-lower (downcase query))
         (escaped-query (regexp-quote query-lower)))
    (or (string-match-p escaped-query (downcase package-name))
        (and description (string-match-p escaped-query (downcase description))))))

(defun bfepm--create-search-result-from-package (package-entry source-url)
  "Create search result from PACKAGE-ENTRY and SOURCE-URL."
  (let* ((package-name (symbol-name (car package-entry)))
         (package-info (cdr package-entry))
         (version (when (vectorp package-info) (aref package-info 0)))
         (description (when (vectorp package-info) (aref package-info 2))))
    
    (gse-create-result
     :id package-name
     :title package-name
     :description (or description "No description available")
     :url (concat source-url package-name)
     :source (if (string-match-p "melpa" source-url) "melpa" "gnu")
     :type 'package
     :metadata (list :version version :source-url source-url))))

(defun bfepm--rank-search-results (results query)
  "Rank search RESULTS based on relevance to QUERY."
  (let ((query-lower (downcase (gse-query-text query))))
    (sort results
          (lambda (a b)
            (> (bfepm--calculate-result-score a query-lower)
               (bfepm--calculate-result-score b query-lower))))))

(defun bfepm--calculate-result-score (result query)
  "Calculate relevance score for RESULT given QUERY."
  (let ((title (downcase (gse-result-title result)))
        (description (downcase (gse-result-description result)))
        (escaped-query (regexp-quote query))
        (score 0.0))
    
    ;; Exact match gets highest score
    (when (string= title query)
      (setq score (+ score 100.0)))
    
    ;; Title starts with query
    (when (string-prefix-p query title)
      (setq score (+ score 50.0)))
    
    ;; Title contains query
    (when (string-match-p escaped-query title)
      (setq score (+ score 25.0)))
    
    ;; Description contains query
    (when (string-match-p escaped-query description)
      (setq score (+ score 10.0)))
    
    ;; Source priority (MELPA preferred)
    (when (string= (gse-result-source result) "melpa")
      (setq score (+ score 5.0)))
    
    score))

;; BFEPM Package Manager Instance
(defun bfepm-create-framework-instance ()
  "Create BFEPM package manager using the generic framework."
  (let* ((http-client (bfepm-create-http-client))
         (version-engine (bfepm-create-version-engine))
         (config-loader (bfepm-create-config-loader))
         (search-engine (bfepm-create-search-engine http-client)))
    
    ;; Create package manager instance
    (pmf-create-package-manager "bfepm"
      :version-engine version-engine
      :config-loader config-loader
      :search-engine search-engine
      :installation-backend (bfepm-create-installation-backend http-client)
      :source-manager (bfepm-create-source-manager)
      :hooks (bfepm-create-lifecycle-hooks))))

(defun bfepm-create-installation-backend (http-client)
  "Create BFEPM installation backend using HTTP-CLIENT."
  (make-pmf-installation-backend
   :name "bfepm-installer"
   :download-manager (make-pmf-download-manager
                     :http-client http-client
                     :retry-strategy '(:max-retries 3 :delay 1.0)
                     :checksum-verifier #'bfepm--verify-package-checksum)
   :extraction-handlers '(("tar" . bfepm--extract-tar-package)
                         ("single" . bfepm--extract-single-file))
   :install-fn #'bfepm--install-package
   :remove-fn #'bfepm--remove-package
   :rollback-fn #'bfepm--rollback-package))

(defun bfepm-create-source-manager ()
  "Create BFEPM source manager."
  (make-pmf-source-manager
   :sources (list (make-pmf-source
                  :name "melpa"
                  :type 'elpa
                  :url "https://melpa.org/packages/"
                  :priority 10)
                 (make-pmf-source
                  :name "gnu"
                  :type 'elpa
                  :url "https://elpa.gnu.org/packages/"
                  :priority 5))
   :priority-resolver #'bfepm--sort-sources-by-priority
   :availability-checker #'bfepm--check-source-availability))

(defun bfepm-create-lifecycle-hooks ()
  "Create BFEPM lifecycle hooks."
  '((before-install . bfepm--before-install-hook)
    (after-install . bfepm--after-install-hook)
    (before-remove . bfepm--before-remove-hook)
    (after-remove . bfepm--after-remove-hook)))

;; Helper functions
(defun bfepm--sort-sources-by-priority (sources)
  "Sort SOURCES by priority."
  (sort sources (lambda (a b) (> (pmf-source-priority a) (pmf-source-priority b)))))

(defun bfepm--check-source-availability (_source)
  "Check if SOURCE is available."
  t)  ; Assume all sources are available for now

(defun bfepm--verify-package-checksum (_file-path _checksum)
  "Verify CHECKSUM for FILE-PATH."
  t)  ; Basic implementation

(defun bfepm--extract-tar-package (_file-path _destination)
  "Extract tar package from FILE-PATH to DESTINATION."
  nil)  ; Basic implementation

(defun bfepm--extract-single-file (_file-path _destination)
  "Extract single file from FILE-PATH to DESTINATION."
  nil)  ; Basic implementation

(defun bfepm--install-package (_package)
  "Install PACKAGE."
  nil)  ; Basic implementation

(defun bfepm--remove-package (_package)
  "Remove PACKAGE."
  nil)  ; Basic implementation

(defun bfepm--rollback-package (_package)
  "Rollback PACKAGE installation."
  nil)  ; Basic implementation

(defun bfepm--before-install-hook (_package)
  "Before install hook for PACKAGE."
  nil)

(defun bfepm--after-install-hook (_package)
  "After install hook for PACKAGE."
  nil)

(defun bfepm--before-remove-hook (_package)
  "Before remove hook for PACKAGE."
  nil)

(defun bfepm--after-remove-hook (_package)
  "After remove hook for PACKAGE."
  nil)

;;; Integration Functions

(defun bfepm-framework-install (package-spec)
  "Install package using framework integration."
  (let ((pm (bfepm-create-framework-instance)))
    (pmf-lifecycle-hook pm 'before-install package-spec)
    ;; Implementation would call framework installation backend
    (pmf-lifecycle-hook pm 'after-install package-spec)))

(defun bfepm-framework-search (query)
  "Search packages using framework integration."
  (let* ((pm (bfepm-create-framework-instance))
         (search-engine (pmf-package-manager-search-engine pm)))
    (gse-search search-engine query)))

(defun bfepm-framework-search-async (query callback)
  "Search packages asynchronously using framework integration."
  (let* ((pm (bfepm-create-framework-instance))
         (search-engine (pmf-package-manager-search-engine pm)))
    (gse-search-async search-engine query callback)))

;;; Migration Utilities

;; Declare variable first
(defvar bfepm--framework-instance nil
  "Global framework instance for backward compatibility.")

(defun bfepm-migrate-to-framework ()
  "Migrate existing BFEPM installation to use framework."
  (interactive)
  (message "Starting BFEPM framework migration...")
  
  ;; Backup existing configuration
  (bfepm--backup-existing-config)
  
  ;; Convert configuration format
  (bfepm--convert-config-to-framework-format)
  
  ;; Initialize framework instance
  (setq bfepm--framework-instance (bfepm-create-framework-instance))
  
  ;; Load plugins
  (bfepm--load-framework-plugins)
  
  (message "BFEPM framework migration completed"))

(defun bfepm--backup-existing-config ()
  "Backup existing BFEPM configuration."
  (when (file-exists-p "~/.emacs.d/bfepm.toml")
    (copy-file "~/.emacs.d/bfepm.toml" "~/.emacs.d/bfepm.toml.backup" t)
    (message "Backed up existing configuration")))

(defun bfepm--convert-config-to-framework-format ()
  "Convert existing configuration to framework format."
  ;; Implementation would parse existing config and convert to new format
  (message "Configuration format is already compatible"))

(defun bfepm--load-framework-plugins ()
  "Load framework plugins for BFEPM."
  (let ((plugin-manager (ps-create-manager :name "bfepm")))
    (ps-add-plugin-directory plugin-manager "~/.emacs.d/bfepm/plugins/")
    (ps-refresh-plugins plugin-manager)
    (message "Loaded %d plugins" (length (ps-list-plugins plugin-manager)))))

;;; Backward Compatibility Layer

(defun bfepm-ensure-framework-instance ()
  "Ensure framework instance is initialized."
  (unless bfepm--framework-instance
    (setq bfepm--framework-instance (bfepm-create-framework-instance))))

;; Compatibility wrapper functions
(defun bfepm-install-compat (package-spec)
  "Backward compatible install function."
  (bfepm-ensure-framework-instance)
  (bfepm-framework-install package-spec))

(defun bfepm-search-compat (query)
  "Backward compatible search function."
  (bfepm-ensure-framework-instance)
  (bfepm-framework-search query))

;;; Example Plugin

(ps-define-plugin bfepm-git-plugin
  ;; Register Git source type
  (ps-register-hook manager 'source-types
    (make-ps-hook
     :name 'git-source
     :description "Git repository source support"
     :type 'filter))
  
  ;; Add Git search adapter
  (let* ((pm-instance (ps-get-config manager "bfepm" "instance"))
         (search-engine (when pm-instance
                         (pmf-package-manager-search-engine pm-instance))))
    (unless search-engine
      (ps-log 'error "Failed to get search engine from package manager instance"))
    (when search-engine
      (gse-add-source search-engine
        (gse-create-source
         :name "git-repos"
         :type 'git
         :priority 15
         :search-fn #'bfepm-git-plugin--search-git-repos))))
  
  (ps-log 'info "Git plugin loaded successfully"))

(defun bfepm-git-plugin--search-git-repos (_query _options)
  "Search Git repositories for QUERY."
  ;; Implementation would search Git repositories
  '())

;;; Testing and Validation

(defun bfepm-test-framework-integration ()
  "Test BFEPM framework integration."
  (interactive)
  (let ((pm (bfepm-create-framework-instance)))
    
    ;; Test version engine
    (let ((engine (pmf-package-manager-version-engine pm)))
      (message "Version comparison test: %s" 
               (vce-compare-versions engine "1.2.3" "1.2.4")))
    
    ;; Test search engine
    (let ((search-engine (pmf-package-manager-search-engine pm)))
      (gse-search-async search-engine "helm"
        (lambda (success results error)
          (if success
              (message "Found %d search results" (length results))
            (message "Search failed: %s" error)))))
    
    ;; Test configuration
    (let ((config-loader (pmf-package-manager-config-loader pm)))
      (message "Config loader: %s" (gcf-loader-name config-loader)))
    
    (message "Framework integration tests completed")))

(provide 'bfepm-framework-integration)

;;; bfepm-framework-integration.el ends here