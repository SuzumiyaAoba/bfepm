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

(ert-deftest bfepm-async-install-timer-test ()
  "Test that bfepm-package-install-async uses timer for scheduling."
  (bfepm-async-test--reset-callback-results)
  
  ;; Mock timer functions to capture timer usage
  (let ((timer-calls nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (delay repeat function)
                 (push (list delay repeat function) timer-calls)
                 ;; Execute immediately for testing
                 (funcall function)))
              ((symbol-function 'bfepm-core-package-installed-p) 
               (lambda (name) t)) ; Already installed
              ((symbol-function 'bfepm-utils-message) 
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Call async install
      (bfepm-package-install-async "test-package" #'bfepm-async-test--mock-callback)
      
      ;; Verify timer was scheduled
      (should (= (length timer-calls) 1))
      (let ((timer-call (car timer-calls)))
        (should (= (nth 0 timer-call) 0.01))    ; 0.01 second delay
        (should (eq (nth 1 timer-call) nil))    ; no repeat
        (should (functionp (nth 2 timer-call)))) ; function scheduled
      
      ;; Verify callback was eventually called
      (should (= (length bfepm-async-test--callback-results) 1)))))

(ert-deftest bfepm-async-download-file-validation-test ()
  "Test async download file size validation."
  (let ((temp-dir (make-temp-file "bfepm-test-" t))
        (callback-results nil))
    
    (unwind-protect
        (let ((test-file (expand-file-name "test-download" temp-dir)))
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (url local-file overwrite)
                       ;; Mock successful download but create empty file
                       (with-temp-file local-file
                         (insert "")))) ; Empty file
                    ((symbol-function 'bfepm-utils-message) 
                     (lambda (&rest args) nil)) ; Suppress messages
                    ((symbol-function 'run-with-timer)
                     (lambda (delay repeat function)
                       ;; Execute immediately for testing
                       (funcall function))))
            
            (bfepm-utils-download-file-async 
             "http://example.com/test.tar" test-file
             (lambda (success error-msg)
               (setq callback-results (list success error-msg))))
            
            ;; Should fail due to zero-byte file
            (should (eq (car callback-results) nil))
            (should (stringp (cadr callback-results)))
            (should (string-match-p "Failed to download" (cadr callback-results)))))
      
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest bfepm-async-download-directory-creation-test ()
  "Test that async download creates parent directories."
  (let ((temp-dir (make-temp-file "bfepm-test-" t))
        (directory-created nil))
    
    (unwind-protect
        (let ((test-file (expand-file-name "subdir/test-download" temp-dir)))
          (cl-letf (((symbol-function 'bfepm-utils-ensure-directory)
                     (lambda (dir)
                       (setq directory-created dir)
                       (make-directory dir t))) ; Actually create for testing
                    ((symbol-function 'url-copy-file)
                     (lambda (url local-file overwrite)
                       (with-temp-file local-file
                         (insert "test content"))))
                    ((symbol-function 'bfepm-utils-message) 
                     (lambda (&rest args) nil))) ; Suppress messages
            
            (bfepm-utils-download-file-async 
             "http://example.com/test.tar" test-file
             (lambda (success error-msg) nil))
            
            ;; Verify directory creation was called
            (should (string= directory-created (file-name-directory test-file)))))
      
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest bfepm-async-no-callback-compatibility-test ()
  "Test that async functions work without callbacks (backward compatibility)."
  (cl-letf (((symbol-function 'bfepm-core-package-installed-p) 
             (lambda (name) t)) ; Already installed
            ((symbol-function 'bfepm-utils-message) 
             (lambda (&rest args) nil))) ; Suppress messages
    
    ;; Should not error when no callback provided
    (should (not (condition-case err
                     (progn (bfepm-package-install "test-package") nil)
                   (error t))))))

(provide 'bfepm-async-test)

;;; bfepm-async-test.el ends here