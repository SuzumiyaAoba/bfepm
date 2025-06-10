;;; bfepm-utils-test.el --- Tests for BFEPM utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM utility functions using ERT.

;;; Code:

(require 'ert)
(require 'bfepm-utils)

;;; Version Comparison Tests

(ert-deftest bfepm-utils-version-compare-equal ()
  "Test version comparison for equal versions."
  (should (= (bfepm-utils-version-compare "1.0.0" "1.0.0") 0))
  (should (= (bfepm-utils-version-compare "2.3.4" "2.3.4") 0)))

(ert-deftest bfepm-utils-version-compare-greater ()
  "Test version comparison for greater version."
  (should (> (bfepm-utils-version-compare "1.1.0" "1.0.0") 0))
  (should (> (bfepm-utils-version-compare "2.0.0" "1.9.9") 0))
  (should (> (bfepm-utils-version-compare "1.0.1" "1.0.0") 0)))

(ert-deftest bfepm-utils-version-compare-lesser ()
  "Test version comparison for lesser version."
  (should (< (bfepm-utils-version-compare "1.0.0" "1.1.0") 0))
  (should (< (bfepm-utils-version-compare "1.9.9" "2.0.0") 0))
  (should (< (bfepm-utils-version-compare "1.0.0" "1.0.1") 0)))

(ert-deftest bfepm-utils-version-compare-date-versions ()
  "Test version comparison for MELPA date-based versions."
  (should (> (bfepm-utils-version-compare "20250602.1300" "20250601.1200") 0))
  (should (< (bfepm-utils-version-compare "20250601.1200" "20250602.1300") 0))
  (should (= (bfepm-utils-version-compare "20250601.1200" "20250601.1200") 0)))

;;; Version Satisfaction Tests

(ert-deftest bfepm-utils-version-satisfies-p-exact ()
  "Test exact version satisfaction."
  (should (bfepm-utils-version-satisfies-p "1.0.0" "1.0.0"))
  (should-not (bfepm-utils-version-satisfies-p "1.0.1" "1.0.0")))

(ert-deftest bfepm-utils-version-satisfies-p-latest ()
  "Test latest version satisfaction."
  (should (bfepm-utils-version-satisfies-p "1.0.0" "latest"))
  (should (bfepm-utils-version-satisfies-p "999.999.999" "latest")))

(ert-deftest bfepm-utils-version-satisfies-p-caret ()
  "Test caret version constraint satisfaction."
  (should (bfepm-utils-version-satisfies-p "1.2.3" "^1.0.0"))
  (should (bfepm-utils-version-satisfies-p "1.9.9" "^1.0.0"))
  (should-not (bfepm-utils-version-satisfies-p "2.0.0" "^1.0.0"))
  (should-not (bfepm-utils-version-satisfies-p "0.9.9" "^1.0.0")))

(ert-deftest bfepm-utils-version-satisfies-p-tilde ()
  "Test tilde version constraint satisfaction."
  (should (bfepm-utils-version-satisfies-p "1.0.5" "~1.0.0"))
  (should (bfepm-utils-version-satisfies-p "1.0.9" "~1.0.0"))
  (should-not (bfepm-utils-version-satisfies-p "1.1.0" "~1.0.0"))
  (should-not (bfepm-utils-version-satisfies-p "0.9.9" "~1.0.0")))

(ert-deftest bfepm-utils-version-satisfies-p-melpa-caret ()
  "Test caret version constraint for MELPA date versions."
  (should (bfepm-utils-version-satisfies-p "20250602.1300" "^20250601"))
  (should (bfepm-utils-version-satisfies-p "20251231.2359" "^20250601"))
  (should-not (bfepm-utils-version-satisfies-p "20260101.0000" "^20250601")))

(ert-deftest bfepm-utils-version-satisfies-p-melpa-tilde ()
  "Test tilde version constraint for MELPA date versions."
  (should (bfepm-utils-version-satisfies-p "20250601.1300" "~20250601.1200"))
  (should (bfepm-utils-version-satisfies-p "20250601.2359" "~20250601.1200"))
  (should-not (bfepm-utils-version-satisfies-p "20250602.0000" "~20250601.1200")))

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

;;; Git Utility Tests

(ert-deftest bfepm-utils-git-clone-test ()
  "Test git clone functionality with a mock."
  (let ((test-url "https://github.com/test/repo.git")
        (test-dir (expand-file-name "test-git-clone" temporary-file-directory)))
    (unwind-protect
        (progn
          (when (file-exists-p test-dir)
            (delete-directory test-dir t))
          ;; Mock call-process to simulate successful git clone
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (when (string= program "git")
                         (make-directory test-dir t)
                         (with-temp-file (expand-file-name "test.el" test-dir)
                           (insert ";;; Test file\n"))
                         0))))
            (should-not (file-exists-p test-dir))
            (bfepm-utils-git-clone test-url test-dir)
            (should (file-directory-p test-dir))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest bfepm-utils-git-clone-with-ref-test ()
  "Test git clone with specific reference."
  (let ((test-url "https://github.com/test/repo.git")
        (test-dir (expand-file-name "test-git-clone-ref" temporary-file-directory))
        (test-ref "v1.0.0")
        (call-process-args nil))
    (unwind-protect
        (progn
          (when (file-exists-p test-dir)
            (delete-directory test-dir t))
          ;; Mock call-process to capture arguments
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (setq call-process-args (cons program args))
                       (when (string= program "git")
                         (make-directory test-dir t)
                         0))))
            (bfepm-utils-git-clone test-url test-dir test-ref)
            (should (member "--branch" call-process-args))
            (should (member test-ref call-process-args))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest bfepm-utils-git-clone-shallow-test ()
  "Test git clone with shallow option."
  (let ((test-url "https://github.com/test/repo.git")
        (test-dir (expand-file-name "test-git-clone-shallow" temporary-file-directory))
        (call-process-args nil))
    (unwind-protect
        (progn
          (when (file-exists-p test-dir)
            (delete-directory test-dir t))
          ;; Mock call-process to capture arguments
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (setq call-process-args (cons program args))
                       (when (string= program "git")
                         (make-directory test-dir t)
                         0))))
            (bfepm-utils-git-clone test-url test-dir nil t)
            (should (member "--depth" call-process-args))
            (should (member "1" call-process-args))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest bfepm-utils-git-get-commit-hash-test ()
  "Test getting git commit hash."
  (let ((test-dir (expand-file-name "test-git-commit" temporary-file-directory))
        (test-hash "abc123def456"))
    (unwind-protect
        (progn
          (make-directory test-dir t)
          ;; Mock call-process to return test hash
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (when (and (string= program "git")
                                  (member "rev-parse" args))
                         (insert test-hash "\n")
                         0))))
            (let ((default-directory test-dir))
              (should (string= test-hash (bfepm-utils-git-get-commit-hash test-dir))))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest bfepm-utils-git-get-latest-tag-test ()
  "Test getting latest git tag."
  (let ((test-dir (expand-file-name "test-git-tag" temporary-file-directory))
        (test-tag "v1.2.3"))
    (unwind-protect
        (progn
          (make-directory test-dir t)
          ;; Mock call-process to return test tag
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (when (and (string= program "git")
                                  (member "describe" args))
                         (insert test-tag "\n")
                         0))))
            (let ((default-directory test-dir))
              (should (string= test-tag (bfepm-utils-git-get-latest-tag test-dir))))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(provide 'bfepm-utils-test)

;;; bfepm-utils-test.el ends here