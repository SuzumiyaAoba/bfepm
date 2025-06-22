;;; bfepm-utils-test.el --- Tests for BFEPM utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM utility functions using ERT.

;;; Code:

(require 'ert)
(require 'bfepm-utils)

;;; Error Handling Tests

(ert-deftest bfepm-utils-error-test ()
  "Test error function raises proper error."
  (should-error (bfepm-utils-error "Test error message")
                :type 'error))

;;; Message Tests

(ert-deftest bfepm-utils-message-test ()
  "Test message formatting."
  (let ((message-log-max 100))
    (bfepm-utils-message "Test message")
    (should (string-match-p "\\[BFEPM\\] Test message"
                           (with-current-buffer "*Messages*"
                             (buffer-string))))))

;;; File System Tests

(ert-deftest bfepm-utils-ensure-directory-test ()
  "Test directory creation utility."
  (let ((test-dir (expand-file-name "test-ensure-dir" temporary-file-directory)))
    (unwind-protect
        (progn
          (when (file-exists-p test-dir)
            (delete-directory test-dir t))
          (should-not (file-exists-p test-dir))
          (bfepm-utils-ensure-directory test-dir)
          (should (file-directory-p test-dir)))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest bfepm-utils-ensure-directory-existing ()
  "Test directory creation when directory already exists."
  (let ((test-dir (expand-file-name "test-existing-dir" temporary-file-directory)))
    (unwind-protect
        (progn
          (bfepm-utils-ensure-directory test-dir)
          (should (file-directory-p test-dir))
          ;; Should not fail when directory already exists
          (bfepm-utils-ensure-directory test-dir)
          (should (file-directory-p test-dir)))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;;; File Copying Tests

(ert-deftest bfepm-utils-copy-file-test ()
  "Test file copying utility."
  (let ((source-file (make-temp-file "bfepm-copy-source"))
        (target-file (make-temp-file "bfepm-copy-target")))
    (unwind-protect
        (progn
          (with-temp-file source-file
            (insert "test content"))
          (bfepm-utils-copy-file source-file target-file)
          (should (file-exists-p target-file))
          (with-temp-buffer
            (insert-file-contents target-file)
            (should (string= (buffer-string) "test content"))))
      (when (file-exists-p source-file)
        (delete-file source-file))
      (when (file-exists-p target-file)
        (delete-file target-file)))))

;;; Checksum Tests

(ert-deftest bfepm-utils-file-checksum-test ()
  "Test file checksum calculation."
  (let ((test-file (make-temp-file "bfepm-checksum-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "test content"))
          (let ((checksum1 (bfepm-utils-file-checksum test-file))
                (checksum2 (bfepm-utils-file-checksum test-file)))
            (should (stringp checksum1))
            (should (string= checksum1 checksum2))
            (should (> (length checksum1) 10))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest bfepm-utils-file-checksum-different-content ()
  "Test file checksum for different content."
  (let ((test-file1 (make-temp-file "bfepm-checksum-test1"))
        (test-file2 (make-temp-file "bfepm-checksum-test2")))
    (unwind-protect
        (progn
          (with-temp-file test-file1
            (insert "test content 1"))
          (with-temp-file test-file2
            (insert "test content 2"))
          (let ((checksum1 (bfepm-utils-file-checksum test-file1))
                (checksum2 (bfepm-utils-file-checksum test-file2)))
            (should-not (string= checksum1 checksum2))))
      (when (file-exists-p test-file1)
        (delete-file test-file1))
      (when (file-exists-p test-file2)
        (delete-file test-file2)))))

(ert-deftest bfepm-utils-verify-checksum-test ()
  "Test checksum verification."
  (let ((test-file (make-temp-file "bfepm-verify-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "test content"))
          (let ((correct-checksum (bfepm-utils-file-checksum test-file))
                (wrong-checksum "deadbeefcafebabe"))
            (should (bfepm-utils-verify-checksum test-file correct-checksum))
            (should-not (bfepm-utils-verify-checksum test-file wrong-checksum))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest bfepm-utils-verify-checksum-case-insensitive ()
  "Test checksum verification is case insensitive."
  (let ((test-file (make-temp-file "bfepm-verify-case-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "test content"))
          (let* ((checksum (bfepm-utils-file-checksum test-file))
                 (uppercase-checksum (upcase checksum))
                 (lowercase-checksum (downcase checksum)))
            (should (bfepm-utils-verify-checksum test-file uppercase-checksum))
            (should (bfepm-utils-verify-checksum test-file lowercase-checksum))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'bfepm-utils-test)

;;; bfepm-utils-test.el ends here