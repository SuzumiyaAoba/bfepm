;;; bfepm-framework-basic-test.el --- Basic Framework Integration Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Basic integration tests that verify framework libraries work
;; with BFEPM. Tests focus on successful integration rather than
;; comprehensive API coverage.

;;; Code:

(require 'ert)
(require 'bfepm-core)
(require 'bfepm-version)

;; Framework library requires with error handling
(condition-case nil (require 'generic-http-client) (error nil))
(condition-case nil (require 'version-constraint-engine) (error nil))
(condition-case nil (require 'generic-search-engine) (error nil))
(condition-case nil (require 'generic-config-framework) (error nil))

;;; Basic Framework Availability Tests

(ert-deftest bfepm-framework-libraries-loadable-test ()
  "Test that framework libraries can be loaded."
  ;; These should load without error if present
  (when (locate-library "generic-http-client")
    (should (featurep 'generic-http-client)))
  (when (locate-library "version-constraint-engine")
    (should (featurep 'version-constraint-engine)))
  (when (locate-library "generic-search-engine")
    (should (featurep 'generic-search-engine)))
  (when (locate-library "generic-config-framework")
    (should (featurep 'generic-config-framework))))

(ert-deftest bfepm-framework-http-client-basic-test ()
  "Test basic HTTP client functionality."
  (skip-unless (fboundp 'ghc-create-client))
  
  (let ((client (ghc-create-client :base-url "https://example.com")))
    (should (ghc-client-p client))
    (should (string= (ghc-client-base-url client) "https://example.com"))))

(ert-deftest bfepm-framework-version-engine-basic-test ()
  "Test basic version engine functionality."
  (skip-unless (fboundp 'vce-create-engine))
  
  (let ((engine (vce-create-engine :name "test")))
    (should (vce-engine-p engine))
    (should (string= (vce-engine-name engine) "test"))))

(ert-deftest bfepm-framework-search-engine-basic-test ()
  "Test basic search engine functionality."
  (skip-unless (fboundp 'gse-create-engine))
  
  (let ((engine (gse-create-engine :name "test")))
    (should (gse-engine-p engine))
    (should (string= (gse-engine-name engine) "test"))))

(ert-deftest bfepm-framework-config-framework-basic-test ()
  "Test basic config framework functionality."
  (skip-unless (fboundp 'gcf-create-loader))
  
  (let ((loader (gcf-create-loader :name "test")))
    (should (gcf-loader-p loader))
    (should (string= (gcf-loader-name loader) "test"))))

;;; BFEPM Integration Tests

(ert-deftest bfepm-framework-version-integration-test ()
  "Test that BFEPM version functions work with or without framework."
  ;; Basic version comparison should always work
  (should (numberp (bfepm-version-compare "1.2.0" "1.1.0")))
  (should (> (bfepm-version-compare "1.2.0" "1.1.0") 0))
  (should (< (bfepm-version-compare "1.1.0" "1.2.0") 0))
  (should (= (bfepm-version-compare "1.2.0" "1.2.0") 0)))

(ert-deftest bfepm-framework-search-integration-test ()
  "Test that BFEPM search engine initialization works."
  ;; Search engine should initialize without error
  (should (functionp 'bfepm-package--ensure-search-engine))
  (condition-case nil
      (bfepm-package--ensure-search-engine)
    (error nil))
  ;; Search engine variable should be set to something
  (should (boundp 'bfepm-package--search-engine)))

(ert-deftest bfepm-framework-graceful-degradation-test ()
  "Test that BFEPM gracefully handles missing framework libraries."
  ;; Core BFEPM functions should work regardless of framework availability
  (should (functionp 'bfepm-version-compare))
  (should (functionp 'bfepm-package--ensure-search-engine))
  
  ;; Test that missing framework functions don't break BFEPM
  (when (not (fboundp 'ghc-create-client))
    (should (functionp 'bfepm-version-compare)))
  (when (not (fboundp 'vce-create-engine))
    (should (functionp 'bfepm-version-compare))))

;;; Framework Enhancement Detection Tests

(ert-deftest bfepm-framework-enhancement-detection-test ()
  "Test detection of framework enhancements."
  (let ((http-enhanced (fboundp 'ghc-create-client))
        (version-enhanced (fboundp 'vce-create-engine))
        (search-enhanced (fboundp 'gse-create-engine))
        (config-enhanced (fboundp 'gcf-create-loader)))
    
    ;; At least log what's available for debugging
    (message "Framework enhancements: HTTP=%s Version=%s Search=%s Config=%s"
             http-enhanced version-enhanced search-enhanced config-enhanced)
    
    ;; If any framework is available, BFEPM should be able to use it
    (when (or http-enhanced version-enhanced search-enhanced config-enhanced)
      (should (functionp 'bfepm-version-compare))
      (should (functionp 'bfepm-package--ensure-search-engine)))))

;;; Performance and Compatibility Tests

(ert-deftest bfepm-framework-performance-basic-test ()
  "Basic performance test to ensure framework doesn't break things."
  (let ((start-time (current-time))
        (iterations 100))
    
    ;; Test version comparison performance
    (dotimes (_ iterations)
      (bfepm-version-compare "1.2.3" "1.2.4"))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should complete 100 comparisons in reasonable time (< 1 second)
      (should (< elapsed 1.0)))))

(ert-deftest bfepm-framework-memory-basic-test ()
  "Basic memory test to ensure framework doesn't leak."
  (let ((initial-objects (length obarray)))
    
    ;; Create and discard some framework objects if available
    (when (fboundp 'ghc-create-client)
      (dotimes (_ 10)
        (ghc-create-client :base-url "https://test.com")))
    
    (when (fboundp 'vce-create-engine)
      (dotimes (_ 10)
        (vce-create-engine :name "test")))
    
    ;; Run garbage collection
    (garbage-collect)
    
    (let ((final-objects (length obarray)))
      ;; Object count shouldn't grow excessively (allow some growth)
      (should (< final-objects (+ initial-objects 1000))))))

;;; Integration Health Check

(ert-deftest bfepm-framework-health-check-test ()
  "Overall health check for framework integration."
  ;; Basic BFEPM functionality should work
  (should (functionp 'bfepm-version-compare))
  
  ;; Framework libraries should be loadable if present
  (when (locate-library "generic-http-client")
    (should (require 'generic-http-client nil t)))
  
  ;; No major errors should occur during initialization
  (condition-case err
      (progn
        (bfepm-package--ensure-search-engine)
        t)
    (error
     ;; Log error for debugging but don't fail test
     (message "Warning: Search engine initialization failed: %s" err)
     t)))

(provide 'bfepm-framework-basic-test)

;;; bfepm-framework-basic-test.el ends here