;;; bfepm-network-test.el --- Tests for BFEPM network operations -*- lexical-binding: t -*-

;;; Commentary:

;; Comprehensive tests for the network operations module.
;; Tests HTTP requests, async downloads, rate limiting,
;; URL validation, and error handling.

;;; Code:

(require 'ert)
(require 'bfepm-network)

;;; URL Validation Tests

(ert-deftest bfepm-network-validate-url-https-test ()
  "Test URL validation for HTTPS URLs."
  ;; Valid HTTPS URLs
  (should (string= (bfepm-network-validate-url "https://example.com") "https://example.com"))
  (should (string= (bfepm-network-validate-url "https://melpa.org/packages/") "https://melpa.org/packages/"))
  
  ;; HTTP URLs should be upgraded to HTTPS
  (should (string= (bfepm-network-validate-url "http://example.com") "https://example.com")))

(ert-deftest bfepm-network-validate-url-error-test ()
  "Test URL validation error cases."
  ;; Invalid URL types
  (should-error (bfepm-network-validate-url nil))
  (should-error (bfepm-network-validate-url 123))
  (should-error (bfepm-network-validate-url '("not" "a" "url")))
  
  ;; Empty URL
  (should-error (bfepm-network-validate-url ""))
  
  ;; Non-HTTP/HTTPS URLs
  (should-error (bfepm-network-validate-url "ftp://example.com"))
  (should-error (bfepm-network-validate-url "file:///path/to/file"))
  (should-error (bfepm-network-validate-url "invalid-url")))

;;; URL Building Tests

(ert-deftest bfepm-network-build-url-test ()
  "Test URL building with path components."
  ;; Basic URL building
  (should (string= (bfepm-network-build-url "https://example.com" "path" "to" "resource")
                   "https://example.com/path/to/resource"))
  
  ;; Handle base URL with trailing slash
  (should (string= (bfepm-network-build-url "https://example.com/" "path")
                   "https://example.com/path"))
  
  ;; URL encoding of components
  (should (string= (bfepm-network-build-url "https://example.com" "path with spaces")
                   "https://example.com/path%20with%20spaces"))
  
  ;; Handle nil and empty components
  (should (string= (bfepm-network-build-url "https://example.com" nil "path" "" "resource")
                   "https://example.com/path/resource"))
  
  ;; Single component
  (should (string= (bfepm-network-build-url "https://example.com" "single")
                   "https://example.com/single"))
  
  ;; No components
  (should (string= (bfepm-network-build-url "https://example.com")
                   "https://example.com")))

;;; Rate Limiting Tests

(ert-deftest bfepm-network-rate-limit-configuration-test ()
  "Test rate limiting configuration."
  (let ((original-delay (bfepm-network-get-rate-limit)))
    ;; Test setting rate limit
    (bfepm-network-set-rate-limit 2.0)
    (should (= (bfepm-network-get-rate-limit) 2.0))
    
    ;; Test reset
    (bfepm-network-reset-rate-limit)
    
    ;; Restore original value
    (bfepm-network-set-rate-limit original-delay)))

;;; Mock Network Operations Tests

(ert-deftest bfepm-network-download-file-sync-mock-test ()
  "Test synchronous file download with mocking."
  (let ((test-file (make-temp-file "bfepm-network-test"))
        (test-url "https://httpbin.org/status/200"))
    
    ;; Mock url-copy-file to simulate successful download
    (cl-letf (((symbol-function 'url-copy-file)
               (lambda (url local-file overwrite)
                 (with-temp-file local-file
                   (insert "test content"))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test successful download
      (should (bfepm-network-download-file test-url test-file))
      (should (file-exists-p test-file))
      (should (> (file-attribute-size (file-attributes test-file)) 0)))
    
    ;; Cleanup
    (when (file-exists-p test-file)
      (delete-file test-file))))

(ert-deftest bfepm-network-download-file-async-mock-test ()
  "Test asynchronous file download with mocking."
  (let ((test-file (make-temp-file "bfepm-network-async-test"))
        (callback-results nil)
        (test-url "https://httpbin.org/status/200"))
    
    ;; Mock url-retrieve for successful download
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 ;; Simulate successful download by calling callback directly
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n\ntest content")
                   (goto-char (point-min))
                   (funcall callback nil))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test async download
      (bfepm-network-download-file-async 
       test-url test-file
       (lambda (success error-msg)
         (setq callback-results (list success error-msg))))
      
      ;; Verify callback was called with success
      (should (eq (car callback-results) t))
      (should (null (cadr callback-results))))
    
    ;; Cleanup
    (when (file-exists-p test-file)
      (delete-file test-file))))

(ert-deftest bfepm-network-fetch-archive-contents-mock-test ()
  "Test archive contents fetching with mocking."
  (let ((callback-results nil)
        (test-archive-url "https://melpa.org/packages/")
        (mock-contents '((test-package [20250101 1200] "Test package" single))))
    
    ;; Mock url-retrieve for archive contents
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n\n")
                   (prin1 mock-contents (current-buffer))
                   (goto-char (point-min))
                   (funcall callback nil))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test archive contents fetch
      (bfepm-network-fetch-archive-contents-async
       test-archive-url
       (lambda (success contents error-msg)
         (setq callback-results (list success contents error-msg))))
      
      ;; Verify callback was called with success and correct contents
      (should (eq (car callback-results) t))
      (should (equal (cadr callback-results) mock-contents))
      (should (null (caddr callback-results))))))

(ert-deftest bfepm-network-http-get-async-mock-test ()
  "Test asynchronous HTTP GET with mocking."
  (let ((callback-results nil)
        (test-url "https://httpbin.org/json")
        (mock-response "{\"test\": \"data\"}"))
    
    ;; Mock url-retrieve for HTTP GET
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n\n" mock-response)
                   (goto-char (point-min))
                   (funcall callback nil))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test HTTP GET
      (bfepm-network-http-get-async
       test-url
       (lambda (success response error-msg)
         (setq callback-results (list success response error-msg))))
      
      ;; Verify callback was called with success and correct response
      (should (eq (car callback-results) t))
      (should (string= (cadr callback-results) mock-response))
      (should (null (caddr callback-results))))))

;;; Error Handling Tests

(ert-deftest bfepm-network-download-error-handling-test ()
  "Test error handling in download operations."
  (let ((test-file (make-temp-file "bfepm-network-error-test"))
        (test-url "https://httpbin.org/status/404")
        (callback-results nil))
    
    ;; Mock url-retrieve to simulate network error
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (funcall callback '(:error (error . "404 Not Found")))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test async download error handling
      (bfepm-network-download-file-async
       test-url test-file
       (lambda (success error-msg)
         (setq callback-results (list success error-msg))))
      
      ;; Verify callback was called with error
      (should (eq (car callback-results) nil))
      (should (stringp (cadr callback-results)))
      (should (string-match-p "404 Not Found" (cadr callback-results))))
    
    ;; Cleanup
    (when (file-exists-p test-file)
      (delete-file test-file))))

(ert-deftest bfepm-network-retry-logic-test ()
  "Test retry logic in async downloads."
  (let ((test-file (make-temp-file "bfepm-network-retry-test"))
        (test-url "https://httpbin.org/status/500")
        (callback-results nil)
        (attempt-count 0))
    
    ;; Mock url-retrieve to fail multiple times then succeed
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (setq attempt-count (1+ attempt-count))
                 (if (< attempt-count 3)
                     ;; Fail first 2 attempts
                     (funcall callback '(:error (error . "Server Error")))
                   ;; Succeed on 3rd attempt
                   (with-temp-buffer
                     (insert "HTTP/1.1 200 OK\n\nretry success")
                     (goto-char (point-min))
                     (funcall callback nil)))))
              ((symbol-function 'message)
               (lambda (&rest args) nil))) ; Suppress messages
      
      ;; Test download with retries (max 3)
      (bfepm-network-download-file-async
       test-url test-file
       (lambda (success error-msg)
         (setq callback-results (list success error-msg)))
       3)
      
      ;; Should succeed after retries
      (should (eq (car callback-results) t))
      (should (null (cadr callback-results)))
      (should (= attempt-count 3))) ; Should have tried 3 times
    
    ;; Cleanup
    (when (file-exists-p test-file)
      (delete-file test-file))))

;;; Integration Tests (Optional - require network access)

(ert-deftest bfepm-network-connectivity-test ()
  "Test network connectivity check."
  ;; This test requires actual network access
  ;; Skip if we're in a restricted environment
  (when (and (not (getenv "CI")) ; Skip in CI
             (not (getenv "BFEPM_OFFLINE"))) ; Skip if offline flag set
    
    ;; Test with a very short timeout to avoid hanging
    (let ((connected (bfepm-network-check-connectivity 2)))
      ;; We can't guarantee network access, so just verify the function returns a boolean
      (should (or (eq connected t) (eq connected nil))))))

;;; Performance Tests

(ert-deftest bfepm-network-url-building-performance-test ()
  "Test URL building performance with many components."
  (let ((base-url "https://example.com")
        (components (mapcar (lambda (i) (format "path%d" i)) (number-sequence 1 100))))
    
    ;; Should complete quickly even with many components
    (should (stringp (apply #'bfepm-network-build-url base-url components)))))

;;; Utility Function Tests

(ert-deftest bfepm-network-ensure-directory-test ()
  "Test directory creation utility."
  (let ((test-dir (make-temp-file "bfepm-network-dir-test" t)))
    ;; Directory should exist after creation
    (should (file-directory-p test-dir))
    
    ;; Should work with nested directories
    (let ((nested-dir (expand-file-name "nested/deep/path" test-dir)))
      (bfepm-network--ensure-directory nested-dir)
      (should (file-directory-p nested-dir)))
    
    ;; Cleanup
    (delete-directory test-dir t)))

(provide 'bfepm-network-test)

;;; bfepm-network-test.el ends here