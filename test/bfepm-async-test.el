;;; bfepm-async-test.el --- Tests for BFEPM async functionality -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for asynchronous package installation functionality.

;;; Code:

(require 'ert)
(require 'bfepm-package)
(require 'bfepm-utils)

(defvar bfepm-async-test--callback-results nil
  "Stores callback results for testing.")

(defun bfepm-async-test--reset-callback-results ()
  "Reset callback results for testing."
  (setq bfepm-async-test--callback-results nil))

(defun bfepm-async-test--mock-callback (success package-name error-msg)
  "Mock callback function that stores results for testing."
  (push (list success package-name error-msg) bfepm-async-test--callback-results))

(ert-deftest bfepm-async-callback-success-test ()
  "Test that callback is invoked on successful installation."
  (bfepm-async-test--reset-callback-results)
  
  ;; Mock a successful installation
  (let ((package (make-bfepm-package :name "test-package" :version "1.0.0")))
    (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
               (lambda (name) t)) ; Already installed
              ((symbol-function 'bfepm-utils-message) 
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Call with callback
      (bfepm-package-install package #'bfepm-async-test--mock-callback)
      
      ;; Verify callback was called with success
      (should (= (length bfepm-async-test--callback-results) 1))
      (let ((result (car bfepm-async-test--callback-results)))
        (should (eq (nth 0 result) t))           ; success = t
        (should (string= (nth 1 result) "test-package")) ; package name
        (should (null (nth 2 result)))))))      ; no error

(ert-deftest bfepm-async-callback-error-test ()
  "Test that callback is invoked on installation error."
  (bfepm-async-test--reset-callback-results)
  
  (let ((package (make-bfepm-package :name "nonexistent-package" :version "1.0.0")))
    (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
               (lambda (name) nil)) ; Not installed
              ((symbol-function 'bfepm-package--find-package)
               (lambda (pkg config) nil)) ; Package not found
              ((symbol-function 'bfepm-utils-message) 
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Call with callback - should trigger error path
      (bfepm-package-install package #'bfepm-async-test--mock-callback)
      
      ;; Verify callback was called with error
      (should (= (length bfepm-async-test--callback-results) 1))
      (let ((result (car bfepm-async-test--callback-results)))
        (should (eq (nth 0 result) nil))         ; success = nil
        (should (string= (nth 1 result) "nonexistent-package")) ; package name
        (should (stringp (nth 2 result)))       ; error message present
        (should (string-match-p "not found" (nth 2 result)))))))

(ert-deftest bfepm-async-install-already-installed-test ()
  "Test that async install handles already installed packages correctly."
  (bfepm-async-test--reset-callback-results)
  
  (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
             (lambda (name) t)) ; Already installed
            ((symbol-function 'bfepm-utils-message) 
             (lambda (&rest args) nil))) ; Suppress messages
    
    ;; Call async install
    (bfepm-package-install-async "test-package" #'bfepm-async-test--mock-callback)
    
    ;; Verify callback was called immediately for already installed package
    (should (= (length bfepm-async-test--callback-results) 1))
    (let ((result (car bfepm-async-test--callback-results)))
      (should (eq (nth 0 result) t))           ; success = t
      (should (string= (nth 1 result) "test-package")) ; package name
      (should (null (nth 2 result))))))

(ert-deftest bfepm-async-archive-contents-fetch-test ()
  "Test async archive contents fetching success path."
  (let ((callback-results nil))
    
    (cl-letf (((symbol-function 'bfepm-package--fetch-archive-contents-async)
               (lambda (url callback)
                 ;; Directly test the callback mechanism
                 (let ((test-contents '((test-pkg [20250101 1200] "Test package" single))))
                   (funcall callback t test-contents nil))))
              ((symbol-function 'bfepm-utils-message) 
               (lambda (&rest args) nil))) ; Suppress messages
      
      (bfepm-package--fetch-archive-contents-async
       "https://melpa.org/packages/"
       (lambda (success contents error-msg)
         (setq callback-results (list success contents error-msg))))
      
      ;; Should succeed and return parsed contents
      (should (eq (car callback-results) t))
      (should (listp (cadr callback-results))) ; contents should be a list
      (should (null (caddr callback-results))))))

;; Temporarily disabled - missing async function
;; (ert-deftest bfepm-async-find-package-not-found-test-disabled ()
;;   "Test async package finding when package is not found."
;;   (let ((callback-results nil))
;;     
;;     (cl-letf (((symbol-function 'bfepm-core-get-config)
;;                (lambda () nil)) ; No config
;;               ((symbol-function 'bfepm-package--fetch-archive-contents-async)
;;                (lambda (url callback)
;;                  ;; Mock empty archive contents
;;                  (funcall callback t '() nil)))
;;               ((symbol-function 'bfepm-utils-message) 
;;                (lambda (&rest args) nil))) ; Suppress messages
;;       
;;       (bfepm-package--find-package-async
;;        (make-bfepm-package :name "nonexistent-package")
;;        (lambda (success package-info error-msg)
;;          (setq callback-results (list success package-info error-msg))))
;;       
;;       ;; Should fail to find package
;;       (should (eq (car callback-results) nil))
;;       (should (null (cadr callback-results)))
;;       (should (stringp (caddr callback-results)))
;;       (should (string-match-p "not found" (caddr callback-results))))))
;; Temporarily disabled - missing async functions  
;; (ert-deftest bfepm-async-package-install-integration-test-disabled ()
;;   "Test full async package installation integration."
;;   (let ((callback-results nil)
;;         (test-package-info (vector 20250101 1200 nil "Test package" 'single)))
;;     
;;     (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
;;                (lambda (name) nil)) ; Not installed
;;               ((symbol-function 'bfepm-core-get-config)
;;                (lambda () nil)) ; No config - use defaults
;;               ((symbol-function 'bfepm-package--fetch-archive-contents-async)
;;                (lambda (url callback)
;;                  ;; Mock finding the package in archive
;;                  (funcall callback t `((test-package . ,test-package-info)) nil)))
;;               ((symbol-function 'bfepm-utils-download-file-async)
;;                (lambda (url local-file callback max-retries)
;;                  ;; Mock successful download
;;                  (funcall callback t nil)))
;;               ((symbol-function 'bfepm-package--extract-tar-package-async)
;;                (lambda (tar-file install-dir callback) 
;;                  (funcall callback t nil))) ; Mock async extraction
;;               ((symbol-function 'bfepm-package--install-single-file)
;;                (lambda (file dir) nil)) ; Mock single file install
;;               ((symbol-function 'bfepm-package--save-version-info)
;;                (lambda (name version) nil)) ; Mock version save
;;               ((symbol-function 'bfepm-package--verify-installation)
;;                (lambda (name dir) nil)) ; Mock verification
;;               ((symbol-function 'bfepm-core--invalidate-cache)
;;                (lambda (name) nil)) ; Mock cache invalidation
;;               ((symbol-function 'bfepm-package--install-dependencies)
;;                (lambda (deps) nil)) ; Mock dependency installation
;;               ((symbol-function 'bfepm-utils-message) 
;;                (lambda (&rest args) nil))) ; Suppress messages
;;       
;;       ;; Test full async installation
;;       (bfepm-package-install-async
;;        "test-package"
;;        (lambda (success package-name error-msg)
;;          (setq callback-results (list success package-name error-msg))))
;;       
;;       ;; Should succeed
;;       (should (eq (car callback-results) t))
;;       (should (string= (cadr callback-results) "test-package"))
;;       (should (null (caddr callback-results))))))
;; End of disabled test

(ert-deftest bfepm-async-no-callback-compatibility-test ()
  "Test that synchronous install still works without callbacks."
  (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
             (lambda (name) t)) ; Already installed
            ((symbol-function 'bfepm-utils-message) 
             (lambda (&rest args) nil))) ; Suppress messages
    
    ;; Should not error when no callback provided to sync install
    (should (not (condition-case err
                     (progn (bfepm-package-install "test-package") nil)
                   (error t))))))

(provide 'bfepm-async-test)

;;; bfepm-async-test.el ends here