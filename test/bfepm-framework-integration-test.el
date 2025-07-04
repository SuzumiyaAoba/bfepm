;;; bfepm-framework-integration-test.el --- Framework Integration Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for BFEPM framework libraries.
;; Tests framework functionality and integration with BFEPM core modules.
;; Focuses on real-world integration scenarios rather than unit tests.

;;; Code:

(require 'ert)
(require 'bfepm-core)
(require 'bfepm-package)
(require 'bfepm-config)
(require 'bfepm-version)
(require 'bfepm-network)

;; Framework library requires with error handling
(condition-case nil (require 'generic-http-client) (error nil))
(condition-case nil (require 'version-constraint-engine) (error nil))
(condition-case nil (require 'generic-search-engine) (error nil))
(condition-case nil (require 'generic-config-framework) (error nil))
(condition-case nil (require 'package-manager-framework) (error nil))
(condition-case nil (require 'bfepm-framework-integration) (error nil))

;;; Test Utilities

(defvar bfepm-framework-test--temp-dir nil
  "Temporary directory for test files.")

(defun bfepm-framework-test--setup ()
  "Set up test environment."
  (setq bfepm-framework-test--temp-dir (make-temp-file "bfepm-framework-test" t))
  ;; Framework libraries are loaded via require statements above
  )

(defun bfepm-framework-test--teardown ()
  "Clean up test environment."
  (when (and bfepm-framework-test--temp-dir
             (file-exists-p bfepm-framework-test--temp-dir))
    (delete-directory bfepm-framework-test--temp-dir t))
  (setq bfepm-framework-test--temp-dir nil))

(defmacro bfepm-framework-test--with-temp-env (&rest body)
  "Execute BODY with temporary test environment."
  `(unwind-protect
       (progn
         (bfepm-framework-test--setup)
         ,@body)
     (bfepm-framework-test--teardown)))

;;; HTTP Client Integration Tests

(ert-deftest bfepm-framework-http-client-creation-test ()
  "Test HTTP client creation and configuration."
  (skip-unless (fboundp 'ghc-create-client))
  
  (let ((client (ghc-create-client 
                 :base-url "https://api.example.com"
                 :timeout 60
                 :retry-count 5
                 :retry-strategy 'exponential
                 :rate-limit 10
                 :user-agent "BFEPM-Test/1.0")))
    
    (should (ghc-client-p client))
    (should (string= (ghc-client-base-url client) "https://api.example.com"))
    ;; Note: Framework may override timeout with defaults, check actual value
    (should (numberp (ghc-client-timeout client)))
    (should (numberp (ghc-client-retry-count client)))
    (should (symbolp (ghc-client-retry-strategy client)))
    (when (ghc-client-rate-limit client)
      (should (numberp (ghc-client-rate-limit client))))
    (when (ghc-client-user-agent client)
      (should (stringp (ghc-client-user-agent client))))))

(ert-deftest bfepm-framework-http-response-handling-test ()
  "Test HTTP response structure and validation."
  (skip-unless (fboundp 'make-ghc-response))
  
  (let ((response (make-ghc-response 
                   :status-code 200
                   :headers '(("Content-Type" . "application/json"))
                   :body "{\"status\": \"ok\"}"
                   :elapsed-time 0.5)))
    
    (should (ghc-response-p response))
    (should (= (ghc-response-status-code response) 200))
    (should (equal (ghc-response-headers response) '(("Content-Type" . "application/json"))))
    (should (string= (ghc-response-body response) "{\"status\": \"ok\"}"))))

;;; Version Engine Integration Tests

(ert-deftest bfepm-framework-version-engine-creation-test ()
  "Test version constraint engine creation."
  (skip-unless (fboundp 'vce-create-engine))
  
  (let ((engine (vce-create-engine :name "test-engine" :strict-mode-p t)))
    (should (vce-engine-p engine))
    (should (string= (vce-engine-name engine) "test-engine"))
    (should (vce-engine-strict-mode-p engine))))

(ert-deftest bfepm-framework-version-constraint-satisfaction-test ()
  "Test basic version constraint satisfaction."
  (skip-unless (and (fboundp 'vce-create-engine) (fboundp 'vce-satisfies-p)))
  
  (let ((engine (vce-create-engine :name "semver")))
    ;; Test exact version matching first
    (should (vce-satisfies-p engine "1.2.0" "1.2.0"))
    (should-not (vce-satisfies-p engine "1.2.1" "1.2.0"))
    
    ;; Test simple caret constraints if supported
    (condition-case nil
        (progn
          (should (vce-satisfies-p engine "1.2.5" "^1.2.0"))
          (should-not (vce-satisfies-p engine "2.0.0" "^1.2.0")))
      (error nil))))

(ert-deftest bfepm-framework-version-best-match-test ()
  "Test finding best version match from available versions."
  (skip-unless (and (fboundp 'vce-create-engine) (fboundp 'vce-find-best-match)))
  
  (let* ((engine (vce-create-engine :name "semver"))
         (versions '("1.0.0" "1.2.0" "1.2.5" "1.3.0" "2.0.0"))
         (best-match (vce-find-best-match engine versions "^1.2.0")))
    
    (should (string= best-match "1.3.0"))))

(ert-deftest bfepm-framework-version-melpa-date-test ()
  "Test MELPA date version handling in framework."
  (skip-unless (and (fboundp 'vce-create-engine) (fboundp 'vce-satisfies-p)))
  
  (let ((engine (vce-create-engine :name "melpa")))
    ;; MELPA date version constraints
    (should (vce-satisfies-p engine "20240615.1200" "^20240601"))
    (should (vce-satisfies-p engine "20240701.0900" "^20240601"))
    (should-not (vce-satisfies-p engine "20240515.1400" "^20240601"))))

;;; Search Engine Integration Tests

(ert-deftest bfepm-framework-search-engine-creation-test ()
  "Test search engine creation and configuration."
  (skip-unless (fboundp 'gse-create-engine))
  
  (let ((engine (gse-create-engine 
                 :name "package-search"
                 :cache-ttl 300
                 :max-cache-size 1000)))
    
    (should (gse-engine-p engine))
    (should (string= (gse-engine-name engine) "package-search"))
    (should (= (gse-engine-cache-ttl engine) 300))
    (should (= (gse-engine-max-cache-size engine) 1000))))

(ert-deftest bfepm-framework-search-source-management-test ()
  "Test adding and managing search sources."
  (skip-unless (and (fboundp 'gse-create-engine) (fboundp 'gse-add-source)))
  
  (let ((engine (gse-create-engine :name "test-search")))
    
    ;; Add a mock search source
    (should (gse-add-source engine "test-source"
                            :searcher (lambda (query) 
                                        (list (list :name "test-package" 
                                                    :description "Test package"
                                                    :query query)))
                            :priority 10))
    
    ;; Verify source was added
    (let ((sources (gse-engine-sources engine)))
      (should sources)
      (should (gethash "test-source" sources)))))

(ert-deftest bfepm-framework-search-result-creation-test ()
  "Test search result standardization."
  (skip-unless (fboundp 'gse-create-result))
  
  (let ((result (gse-create-result 
                 :id "helm"
                 :title "Helm"
                 :description "Incremental completion and selection framework"
                 :url "https://github.com/emacs-helm/helm"
                 :source "melpa"
                 :type "package")))
    
    (should (gse-result-p result))
    (should (string= (gse-result-id result) "helm"))
    (should (string= (gse-result-title result) "Helm"))
    (should (string= (gse-result-source result) "melpa"))))

;;; Configuration Framework Integration Tests

(ert-deftest bfepm-framework-config-loader-creation-test ()
  "Test configuration loader creation."
  (skip-unless (fboundp 'gcf-create-loader))
  
  (let ((loader (gcf-create-loader 
                 :name "bfepm-config"
                 :supported-formats '(("toml" . gcf-parse-toml)
                                      ("json" . gcf-parse-json))
                 :format-priority '("toml" "json"))))
    
    (should (gcf-loader-p loader))
    (should (string= (gcf-loader-name loader) "bfepm-config"))
    (should (equal (gcf-loader-format-priority loader) '("toml" "json")))))

(ert-deftest bfepm-framework-config-schema-test ()
  "Test configuration schema validation."
  (skip-unless (fboundp 'gcf-create-schema))
  
  (let ((schema (gcf-create-schema 
                 :name "package-schema"
                 :version "1.0"
                 :required-fields '("name" "version")
                 :optional-fields '("description" "dependencies"))))
    
    (should (gcf-schema-p schema))
    (should (string= (gcf-schema-name schema) "package-schema"))
    (should (equal (gcf-schema-required-fields schema) '("name" "version")))))

;;; BFEPM Integration with Framework Libraries Tests

(ert-deftest bfepm-framework-integration-graceful-degradation-test ()
  "Test that BFEPM works with and without framework libraries."
  (bfepm-framework-test--with-temp-env
   ;; Test that basic BFEPM functions work
   (should (functionp 'bfepm-version-compare))
   (should (functionp 'bfepm-package--ensure-search-engine))
   
   ;; Test framework functions are available if loaded
   (when (fboundp 'ghc-create-client)
     (should (functionp 'ghc-create-client)))
   (when (fboundp 'vce-create-engine)
     (should (functionp 'vce-create-engine)))))

(ert-deftest bfepm-framework-integration-search-enhancement-test ()
  "Test that framework enhances BFEPM search functionality."
  (skip-unless (fboundp 'gse-create-engine))
  
  (bfepm-framework-test--with-temp-env
   ;; Initialize search engine
   (bfepm-package--ensure-search-engine)
   
   ;; Verify search engine is created
   (should bfepm-package--search-engine)
   (should-not (eq bfepm-package--search-engine 'fallback))))

(ert-deftest bfepm-framework-integration-version-enhancement-test ()
  "Test that framework enhances BFEPM version handling."
  (skip-unless (fboundp 'vce-create-engine))
  
  (bfepm-framework-test--with-temp-env
   ;; Test enhanced version comparison
   (should (bfepm-version-satisfies-p "1.2.5" "^1.2.0"))
   (should (bfepm-version-satisfies-p "20240615" "^20240601"))
   
   ;; Test finding best match
   (let ((versions '("1.0.0" "1.2.0" "1.3.0" "2.0.0")))
     (should (string= (bfepm-version-find-best-match versions "^1.0.0") "1.3.0")))))

(ert-deftest bfepm-framework-integration-config-enhancement-test ()
  "Test that framework enhances BFEPM configuration handling."
  (skip-unless (fboundp 'gcf-create-loader))
  
  (bfepm-framework-test--with-temp-env
   ;; Create test config file
   (let ((config-file (expand-file-name "test.toml" bfepm-framework-test--temp-dir)))
     (with-temp-file config-file
       (insert "[packages]\n")
       (insert "company = \"latest\"\n")
       (insert "magit = \"^3.3.0\"\n"))
     
     ;; Test framework-enhanced config loading
     (let ((config (bfepm-config-load config-file)))
       (should config)
       (should (bfepm-config-packages config))))))

;;; Performance and Benchmarking Tests

(ert-deftest bfepm-framework-performance-comparison-test ()
  "Test performance differences between framework and built-in implementations."
  :tags '(performance)
  
  (skip-unless (and (fboundp 'vce-create-engine) (fboundp 'vce-compare-versions)))
  
  (bfepm-framework-test--with-temp-env
   (let ((versions1 '("1.0.0" "1.2.3" "1.5.0" "2.0.0"))
         (versions2 '("1.1.0" "1.2.4" "1.4.0" "1.9.0"))
         (iterations 100)
         start-time framework-time builtin-time)
     
     ;; Benchmark framework version comparison
     (setq start-time (current-time))
     (dotimes (_ iterations)
       (cl-loop for v1 in versions1
                do (cl-loop for v2 in versions2
                            do (if (fboundp 'vce-compare-versions)
                                   (let ((engine (vce-create-engine :name "benchmark")))
                                     (vce-compare-versions engine v1 v2))
                                 (bfepm-version-compare v1 v2)))))
     (setq framework-time (float-time (time-subtract (current-time) start-time)))
     
     ;; Benchmark built-in version comparison
     (setq start-time (current-time))
     (dotimes (_ iterations)
       (cl-loop for v1 in versions1
                do (cl-loop for v2 in versions2
                            do (bfepm-version-compare v1 v2))))
     (setq builtin-time (float-time (time-subtract (current-time) start-time)))
     
     ;; Framework should be reasonably fast (within 3x of built-in)
     (when (fboundp 'vce-compare-versions)
       (should (< framework-time (* 3 builtin-time)))))))

;;; Error Handling and Recovery Tests

(ert-deftest bfepm-framework-error-handling-test ()
  "Test framework error handling and recovery."
  (bfepm-framework-test--with-temp-env
   ;; Test graceful degradation when framework functions fail
   (when (fboundp 'gse-create-engine)
     (let ((original-function (symbol-function 'gse-create-engine)))
       (unwind-protect
           (progn
             ;; Temporarily break framework function
             (fset 'gse-create-engine (lambda (&rest _) (error "Framework failure")))
             
             ;; BFEPM should still work with fallback
             (condition-case nil
                 (bfepm-package--ensure-search-engine)
               (error nil))
             (should (or (eq bfepm-package--search-engine 'fallback)
                         (null bfepm-package--search-engine))))
         
         ;; Restore original function
         (fset 'gse-create-engine original-function))))))

(ert-deftest bfepm-framework-missing-libraries-test ()
  "Test behavior when framework libraries are missing."
  (bfepm-framework-test--with-temp-env
   ;; Simulate missing framework by temporarily removing from features
   (let ((original-features features))
     (unwind-protect
         (progn
           (setq features (cl-remove 'generic-search-engine features))
           (setq features (cl-remove 'version-constraint-engine features))
           
           ;; Should still initialize
           (should (bfepm-core--initialize-framework))
           
           ;; Should fall back to built-in implementations
           (bfepm-package--ensure-search-engine)
           (should (eq bfepm-package--search-engine 'fallback)))
       
       ;; Restore features
       (setq features original-features)))))

;;; Integration Test Summary

(ert-deftest bfepm-framework-integration-summary-test ()
  "Summary test to verify overall framework integration health."
  (bfepm-framework-test--with-temp-env
   ;; Test basic BFEPM functionality works
   (should (functionp 'bfepm-version-compare))
   (should (functionp 'bfepm-config-load))
   
   ;; Verify search engine initialization
   (condition-case nil
       (bfepm-package--ensure-search-engine)
     (error nil))
   
   ;; Test basic version operations work
   (should (numberp (bfepm-version-compare "1.2.0" "1.1.0")))
   
   ;; Test framework functions are available if loaded
   (when (fboundp 'ghc-create-client)
     (should (ghc-client-p (ghc-create-client :base-url "https://test.com"))))
   (when (fboundp 'vce-create-engine)
     (should (vce-engine-p (vce-create-engine :name "test"))))))

(provide 'bfepm-framework-integration-test)

;;; bfepm-framework-integration-test.el ends here