;;; bfepm-search-test.el --- Tests for bfepm-search -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the BFEPM package search functionality.

;;; Code:

(require 'ert)
(require 'bfepm-search)
(require 'bfepm-core)
(require 'bfepm-network)

;;; Test Data

(defconst bfepm-search-test--mock-melpa-archive
  '((package-a . [(1 2 3) nil "Description for package A" tar])
    (package-b . [(0 5 0) nil "A useful utility package" single])
    (search-test . [(2 1 0) nil "Test package for search functionality" tar])
    (utility-pkg . [(1 0 0) nil "General utility functions" single])
    (emacs-helper . [(3 2 1) nil "Helper functions for Emacs" tar]))
  "Mock MELPA archive contents for testing.")

(defconst bfepm-search-test--mock-gnu-archive
  '((gnu-package . [(1 0 0) nil "GNU ELPA package" tar])
    (old-package . [(0 1 0) nil "Legacy package from GNU ELPA" single])
    (search-test . [(1 0 0) nil "Duplicate package in GNU ELPA" single]))
  "Mock GNU ELPA archive contents for testing.")

;;; Helper Functions

(defun bfepm-search-test--mock-fetch-archive-contents (url)
  "Mock function to return test archive contents for URL."
  (cond
   ((string-match-p "melpa" url) bfepm-search-test--mock-melpa-archive)
   ((string-match-p "gnu" url) bfepm-search-test--mock-gnu-archive)
   (t nil)))

(defmacro bfepm-search-test--with-mock-network (&rest body)
  "Execute BODY with mocked network functions."
  `(cl-letf (((symbol-function 'bfepm-search--fetch-archive-contents-sync)
              #'bfepm-search-test--mock-fetch-archive-contents)
             ((symbol-function 'bfepm-network-fetch-archive-contents-async)
              (lambda (url callback &optional _rate-limit-delay)
                (let ((contents (bfepm-search-test--mock-fetch-archive-contents url)))
                  (if contents
                      (funcall callback t contents nil)
                    (funcall callback nil nil "Archive not found"))))))
     ,@body))

;;; Search Result Structure Tests

(ert-deftest bfepm-search-test-result-structure ()
  "Test bfepm-search-result structure creation and access."
  (let ((result (make-bfepm-search-result
                 :name "test-package"
                 :version "1.2.3"
                 :description "Test package description"
                 :source "melpa"
                 :dependencies '((dep1 "1.0") (dep2 "2.0"))
                 :kind 'tar
                 :installed-p nil)))
    
    (should (bfepm-search-result-p result))
    (should (string= (bfepm-search-result-name result) "test-package"))
    (should (string= (bfepm-search-result-version result) "1.2.3"))
    (should (string= (bfepm-search-result-description result) "Test package description"))
    (should (string= (bfepm-search-result-source result) "melpa"))
    (should (equal (bfepm-search-result-dependencies result) '((dep1 "1.0") (dep2 "2.0"))))
    (should (eq (bfepm-search-result-kind result) 'tar))
    (should-not (bfepm-search-result-installed-p result))))

;;; Query Processing Tests

(ert-deftest bfepm-search-test-build-query-regex ()
  "Test query regex building for different search terms."
  ;; Single term
  (should (string= (bfepm-search--build-query-regex "test")
                   "test"))
  
  ;; Multiple terms (AND logic)
  (should (string= (bfepm-search--build-query-regex "test package")
                   "test.*package"))
  
  ;; Three terms
  (should (string= (bfepm-search--build-query-regex "emacs test utility")
                   "emacs.*test.*utility"))
  
  ;; Case handling
  (should (string= (bfepm-search--build-query-regex "Test Package")
                   "test.*package")))

(ert-deftest bfepm-search-test-matches-query ()
  "Test query matching against search results."
  (let ((result (make-bfepm-search-result
                 :name "emacs-helper"
                 :description "Helper functions for Emacs development"
                 :version "1.0.0"
                 :source "melpa"
                 :dependencies nil
                 :kind 'tar
                 :installed-p nil)))
    
    ;; Name match
    (should (bfepm-search--matches-query-p result "emacs"))
    (should (bfepm-search--matches-query-p result "helper"))
    
    ;; Description match
    (should (bfepm-search--matches-query-p result "functions"))
    (should (bfepm-search--matches-query-p result "development"))
    
    ;; Multiple terms (AND logic)
    (should (bfepm-search--matches-query-p result "helper.*emacs"))
    
    ;; Case insensitive
    (should (bfepm-search--matches-query-p result "EMACS"))
    (should (bfepm-search--matches-query-p result "Helper"))
    
    ;; No match
    (should-not (bfepm-search--matches-query-p result "nonexistent"))))

;;; Synchronous Search Tests

(ert-deftest bfepm-search-test-sync-single-source ()
  "Test synchronous search in a single source."
  (bfepm-search-test--with-mock-network
   (let ((results (bfepm-search--search-source-sync 
                   "package"
                   "melpa"
                   "https://melpa.org/packages/")))
     (should (> (length results) 0))
     
     ;; Should find package-a and package-b
     (let ((names (mapcar #'bfepm-search-result-name results)))
       (should (member "package-a" names))
       (should (member "package-b" names))
       (should-not (member "emacs-helper" names))))))

(ert-deftest bfepm-search-test-sync-search ()
  "Test complete synchronous search across sources."
  (bfepm-search-test--with-mock-network
   (let ((results (bfepm-search--search-sync "test" nil)))
     (should (> (length results) 0))
     
     ;; Should find search-test from both sources
     (let ((search-results (cl-remove-if-not 
                           (lambda (r) (string= (bfepm-search-result-name r) "search-test"))
                           results)))
       (should (= (length search-results) 2)) ; One from each source
       
       ;; Check sources
       (let ((sources (mapcar #'bfepm-search-result-source search-results)))
         (should (member "melpa" sources))
         (should (member "gnu" sources)))))))

(ert-deftest bfepm-search-test-empty-query ()
  "Test handling of empty search query."
  (let ((results (bfepm-search "")))
    (should (null results))))

(ert-deftest bfepm-search-test-no-results ()
  "Test search with query that returns no results."
  (bfepm-search-test--with-mock-network
   (let ((results (bfepm-search--search-sync "nonexistent-package" nil)))
     (should (null results)))))

;;; Asynchronous Search Tests

(ert-deftest bfepm-search-test-async-search ()
  "Test asynchronous search functionality."
  (bfepm-search-test--with-mock-network
   (let ((callback-called nil)
         (callback-success nil)
         (callback-results nil)
         (callback-error nil))
     
     ;; Set up callback
     (bfepm-search--search-async 
      "helper"
      '(("melpa" . "https://melpa.org/packages/"))
      (lambda (success results error-msg)
        (setq callback-called t
              callback-success success
              callback-results results
              callback-error error-msg)))
     
     ;; Wait for async operation (simulate)
     (while (not callback-called)
       (sleep-for 0.01))
     
     (should callback-called)
     (should callback-success)
     (should-not callback-error)
     (should (> (length callback-results) 0))
     
     ;; Verify result
     (let ((result (car callback-results)))
       (should (string= (bfepm-search-result-name result) "emacs-helper"))
       (should (string= (bfepm-search-result-source result) "melpa"))))))

(ert-deftest bfepm-search-test-async-error-handling ()
  "Test async search error handling."
  (cl-letf (((symbol-function 'bfepm-network-fetch-archive-contents-async)
             (lambda (_url callback &optional _rate-limit-delay)
               (funcall callback nil nil "Network error"))))
    
    (let ((callback-called nil)
          (callback-success nil)
          (async-results nil)
          (callback-error nil))
      
      (bfepm-search--search-async 
       "test"
       '(("test" . "https://test.example/"))
       (lambda (success results error-msg)
         (setq callback-called t
               callback-success success
               async-results results
               callback-error error-msg)))
      
      ;; Wait for callback
      (while (not callback-called)
        (sleep-for 0.01))
      
      (should callback-called)
      ;; Note: current implementation returns success=true with empty results when all sources fail
      ;; This might be changed in the future to return failure instead
      (should callback-success)
      (should (= (length async-results) 0)))))

;;; Result Sorting Tests

(ert-deftest bfepm-search-test-sort-results ()
  "Test result sorting by relevance."
  (let ((results (list
                  (make-bfepm-search-result
                   :name "test-package" :description "A test package" 
                   :version "1.0" :source "melpa" :dependencies nil :kind 'tar :installed-p nil)
                  (make-bfepm-search-result
                   :name "package-test" :description "Another package"
                   :version "1.0" :source "melpa" :dependencies nil :kind 'tar :installed-p nil)
                  (make-bfepm-search-result
                   :name "test" :description "Exact match"
                   :version "1.0" :source "melpa" :dependencies nil :kind 'tar :installed-p nil)
                  (make-bfepm-search-result
                   :name "other-pkg" :description "Contains test keyword"
                   :version "1.0" :source "melpa" :dependencies nil :kind 'tar :installed-p nil))))
    
    (let ((sorted (bfepm-search--sort-results results "test")))
      ;; Exact name match should be first
      (should (string= (bfepm-search-result-name (car sorted)) "test"))
      
      ;; Name starting with query should be next
      (should (string= (bfepm-search-result-name (cadr sorted)) "test-package")))))

;;; Archive Entry Processing Tests

(ert-deftest bfepm-search-test-create-result-from-archive-entry ()
  "Test creation of search results from archive entries."
  (let ((result (bfepm-search--create-result-from-archive-entry
                 "test-package"
                 [(1 2 3) ((dep1 "1.0") (dep2 "2.0")) "Test package description" tar]
                 "melpa")))
    
    (should (bfepm-search-result-p result))
    (should (string= (bfepm-search-result-name result) "test-package"))
    (should (string= (bfepm-search-result-version result) "1.2.3"))
    (should (string= (bfepm-search-result-description result) "Test package description"))
    (should (string= (bfepm-search-result-source result) "melpa"))
    (should (equal (bfepm-search-result-dependencies result) '((dep1 "1.0") (dep2 "2.0"))))
    (should (eq (bfepm-search-result-kind result) 'tar))))

(ert-deftest bfepm-search-test-handle-vector-and-list-formats ()
  "Test handling of both vector and list formats from archives."
  ;; Vector format (typical MELPA)
  (let ((result1 (bfepm-search--create-result-from-archive-entry
                  "pkg1"
                  [(1 0 0) nil "Vector format package" single]
                  "melpa")))
    (should (string= (bfepm-search-result-version result1) "1.0.0")))
  
  ;; List format
  (let ((result2 (bfepm-search--create-result-from-archive-entry
                  "pkg2"
                  '((2 0 0) nil "List format package" tar)
                  "gnu")))
    (should (string= (bfepm-search-result-version result2) "2.0.0"))))

;;; Installed Package Search Tests

(ert-deftest bfepm-search-test-installed-packages-search ()
  "Test searching within installed packages."
  ;; Mock installed packages
  (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
             (lambda () '("emacs-helper" "test-package" "utility-pkg")))
            ((symbol-function 'bfepm-search--get-package-description)
             (lambda (name)
               (cond
                ((string= name "emacs-helper") "Helper functions for Emacs")
                ((string= name "test-package") "A test package")
                ((string= name "utility-pkg") "Utility functions")
                (t nil))))
            ((symbol-function 'bfepm-core-get-package-version)
             (lambda (_name) "1.0.0")))
    
    (let ((results (bfepm-search-installed-packages "emacs")))
      (should (= (length results) 1))
      (should (string= (bfepm-search-result-name (car results)) "emacs-helper"))
      (should (bfepm-search-result-installed-p (car results))))
    
    (let ((results (bfepm-search-installed-packages "package")))
      (should (= (length results) 1))
      (should (string= (bfepm-search-result-name (car results)) "test-package")))
    
    (let ((results (bfepm-search-installed-packages "utility")))
      (should (= (length results) 1))
      (should (string= (bfepm-search-result-name (car results)) "utility-pkg")))))

;;; Integration Tests

(ert-deftest bfepm-search-test-public-api ()
  "Test public API functions."
  (bfepm-search-test--with-mock-network
   ;; Test bfepm-search function (non-interactive)
   (let ((results (bfepm-search "helper")))
     (should (> (length results) 0))
     (let ((result (car results)))
       (should (bfepm-search-result-p result))
       (should (string-match-p "helper" (bfepm-search-result-name result)))))
   
   ;; Test bfepm-search-async function
   (let ((callback-called nil)
         (async-results nil))
     (bfepm-search-async 
      "test"
      (lambda (success results _error-msg)
        (setq callback-called t
              async-results (when success results))))
     
     ;; Wait for callback
     (while (not callback-called)
       (sleep-for 0.01))
     
     (should async-results)
     (should (> (length async-results) 0)))))

;;; Error Handling Tests

(ert-deftest bfepm-search-test-network-error-handling ()
  "Test handling of network errors during search."
  (cl-letf (((symbol-function 'bfepm-search--fetch-archive-contents-sync)
             (lambda (_url) (error "Network timeout"))))
    
    (let ((results (bfepm-search--search-source-sync 
                    "test"
                    "melpa"
                    "https://melpa.org/packages/")))
      (should (null results)))))

(ert-deftest bfepm-search-test-malformed-archive-handling ()
  "Test handling of malformed archive data."
  (cl-letf (((symbol-function 'bfepm-search--fetch-archive-contents-sync)
             (lambda (_url) "invalid-data")))
    
    ;; The current implementation handles malformed data gracefully by returning empty results
    (let ((results (bfepm-search--search-source-sync 
                    "test"
                    "melpa"
                    "https://melpa.org/packages/")))
      (should (null results)))))

;;; Performance Tests

(ert-deftest bfepm-search-test-large-archive-performance ()
  "Test search performance with large archive data."
  ;; Create a large mock archive
  (let ((large-archive (cl-loop for i from 1 to 1000
                               collect (list (intern (format "package-%d" i))
                                           (vector (list 1 0 0) nil
                                                  (format "Description for package %d" i)
                                                  'single)))))
    
    (cl-letf (((symbol-function 'bfepm-search--fetch-archive-contents-sync)
               (lambda (_url) large-archive)))
      
      (let ((start-time (current-time)))
        (let ((results (bfepm-search--search-sync "package" nil)))
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            (should (< elapsed 1.0)) ; Should complete in under 1 second
            (should (> (length results) 0))
            ;; Should find packages containing "package" in the name
            (should (cl-some (lambda (r) (string-match-p "package" 
                                                        (bfepm-search-result-name r))) results))))))))

;;; Cache Tests

(ert-deftest bfepm-search-test-result-caching ()
  "Test that search results are properly cached."
  (bfepm-search-test--with-mock-network
   ;; Clear cache
   (setq bfepm-search--last-query ""
         bfepm-search--last-results nil)
   
   ;; First search
   (let ((results1 (bfepm-search--search-sync "helper" nil)))
     (should (> (length results1) 0))
     (should (string= bfepm-search--last-query "helper"))
     (should (equal bfepm-search--last-results results1))
     
     ;; Verify cache is populated
     (should bfepm-search--last-results))))

(provide 'bfepm-search-test)

;;; bfepm-search-test.el ends here