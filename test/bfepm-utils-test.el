;;; bfepm-utils-test.el --- Tests for BFEPM utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM utility functions.

;;; Code:

(require 'buttercup)
(require 'bfepm-utils)

(describe "BFEPM Utils Version Handling"
  (describe "bfepm-utils-version-compare"
    (it "should return 0 for equal versions"
      (expect (bfepm-utils-version-compare "1.0.0" "1.0.0") :to-equal 0)
      (expect (bfepm-utils-version-compare "1.2.3" "1.2.3") :to-equal 0))

    (it "should return 1 when first version is greater"
      (expect (bfepm-utils-version-compare "1.0.1" "1.0.0") :to-equal 1)
      (expect (bfepm-utils-version-compare "1.1.0" "1.0.9") :to-equal 1)
      (expect (bfepm-utils-version-compare "2.0.0" "1.9.9") :to-equal 1))

    (it "should return -1 when first version is lesser"
      (expect (bfepm-utils-version-compare "1.0.0" "1.0.1") :to-equal -1)
      (expect (bfepm-utils-version-compare "1.0.9" "1.1.0") :to-equal -1)
      (expect (bfepm-utils-version-compare "1.9.9" "2.0.0") :to-equal -1))

    (it "should handle different length version strings"
      (expect (bfepm-utils-version-compare "1.0" "1.0.0") :to-equal -1)
      (expect (bfepm-utils-version-compare "1.0.0" "1.0") :to-equal 1)))

  (describe "bfepm-utils-version-satisfies-p"
    (it "should always satisfy 'latest'"
      (expect (bfepm-utils-version-satisfies-p "1.0.0" "latest") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "99.99.99" "latest") :to-be t))

    (it "should handle exact version matches"
      (expect (bfepm-utils-version-satisfies-p "1.0.0" "1.0.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.0.1" "1.0.0") :to-be nil)
      (expect (bfepm-utils-version-satisfies-p "1.0.0" "1.0.1") :to-be nil))

    (it "should handle caret version requirements"
      ;; ^1.2.3 means >=1.2.3 <2.0.0
      (expect (bfepm-utils-version-satisfies-p "1.2.3" "^1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.2.4" "^1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.3.0" "^1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.9.9" "^1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "2.0.0" "^1.2.3") :to-be nil)
      (expect (bfepm-utils-version-satisfies-p "1.2.2" "^1.2.3") :to-be nil))

    (it "should handle tilde version requirements"
      ;; ~1.2.3 means >=1.2.3 <1.3.0
      (expect (bfepm-utils-version-satisfies-p "1.2.3" "~1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.2.4" "~1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.2.9" "~1.2.3") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.3.0" "~1.2.3") :to-be nil)
      (expect (bfepm-utils-version-satisfies-p "1.2.2" "~1.2.3") :to-be nil))))

(describe "BFEPM Utils File Operations"
  (describe "bfepm-utils-file-sha256"
    (it "should calculate correct SHA256 for file"
      (let ((test-file (make-temp-file "pm-test")))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "Hello, World!"))
              (let ((checksum (bfepm-utils-file-sha256 test-file)))
                (expect checksum :to-be-truthy)
                (expect (length checksum) :to-equal 64)
                (expect checksum :to-match "^[a-fA-F0-9]+$")))
          (delete-file test-file))))

    (it "should return nil for non-existent file"
      (expect (bfepm-utils-file-sha256 "/path/to/non/existent/file") :to-be nil))

    (it "should return different checksums for different content"
      (let ((file1 (make-temp-file "pm-test1"))
            (file2 (make-temp-file "pm-test2")))
        (unwind-protect
            (progn
              (with-temp-file file1 (insert "content1"))
              (with-temp-file file2 (insert "content2"))
              (let ((checksum1 (bfepm-utils-file-sha256 file1))
                    (checksum2 (bfepm-utils-file-sha256 file2)))
                (expect checksum1 :not :to-equal checksum2)))
          (delete-file file1)
          (delete-file file2)))))

  (describe "bfepm-utils-ensure-directory"
    (it "should create directory if it doesn't exist"
      (let ((test-dir (expand-file-name "pm-test-dir" temporary-file-directory)))
        (unwind-protect
            (progn
              (when (file-exists-p test-dir)
                (delete-directory test-dir t))
              (bfepm-utils-ensure-directory test-dir)
              (expect (file-directory-p test-dir) :to-be t))
          (when (file-exists-p test-dir)
            (delete-directory test-dir t)))))

    (it "should not error if directory already exists"
      (let ((test-dir (expand-file-name "pm-test-dir" temporary-file-directory)))
        (unwind-protect
            (progn
              (make-directory test-dir t)
              (expect (lambda () (bfepm-utils-ensure-directory test-dir))
                      :not :to-throw))
          (when (file-exists-p test-dir)
            (delete-directory test-dir t)))))))

(describe "BFEPM Utils Message Functions"
  (describe "bfepm-utils-message"
    (it "should format message with BFEPM prefix"
      (let ((messages '()))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (bfepm-utils-message "Test message: %s" "value")
          (expect (car messages) :to-equal "[BFEPM] Test message: value")))))

  (describe "bfepm-utils-error"
    (it "should signal error with BFEPM prefix"
      (expect (lambda () (bfepm-utils-error "Test error: %s" "value"))
              :to-throw 'error (lambda (err)
                                 (string-match-p "\\[BFEPM\\] Test error: value"
                                                 (error-message-string err)))))))

;;; bfepm-utils-test.el ends here