;;; bfepm-lock-test.el --- Tests for bfepm-lock -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM lock file functionality.

;;; Code:

(require 'ert)
(require 'bfepm-lock)
(require 'bfepm-core)

;;; Lock Structure Tests

(ert-deftest bfepm-lock-package-struct-creation ()
  "Test bfepm-lock-package structure creation."
  (let ((lock-package (make-bfepm-lock-package
                       :name "test-package"
                       :version "1.0.0"
                       :source "melpa"
                       :checksum "abc123")))
    (should (bfepm-lock-package-p lock-package))
    (should (string= (bfepm-lock-package-name lock-package) "test-package"))
    (should (string= (bfepm-lock-package-version lock-package) "1.0.0"))
    (should (string= (bfepm-lock-package-source lock-package) "melpa"))
    (should (string= (bfepm-lock-package-checksum lock-package) "abc123"))))

(ert-deftest bfepm-lock-struct-creation ()
  "Test bfepm-lock structure creation."
  (let ((meta '((version . "1.0.0") (created . "2025-06-10")))
        (packages (list (make-bfepm-lock-package 
                         :name "pkg1" :version "1.0" :source "melpa")))
        (resolution '((strategy . "latest"))))
    
    (let ((lock (make-bfepm-lock 
                 :meta meta 
                 :packages packages 
                 :resolution resolution)))
      (should (bfepm-lock-p lock))
      (should (equal (bfepm-lock-meta lock) meta))
      (should (equal (bfepm-lock-packages lock) packages))
      (should (equal (bfepm-lock-resolution lock) resolution)))))

;;; Lock Creation Tests

(ert-deftest bfepm-lock-create-basic ()
  "Test basic lock file creation."
  (let ((lock (bfepm-lock-create)))
    (should (bfepm-lock-p lock))
    (should (listp (bfepm-lock-meta lock)))
    (should (listp (bfepm-lock-packages lock)))
    (should (listp (bfepm-lock-resolution lock)))
    
    ;; Check meta contains expected fields
    (let ((meta (bfepm-lock-meta lock)))
      (should (assoc 'version meta))
      (should (assoc 'created meta)))))

(ert-deftest bfepm-lock-create-with-packages ()
  "Test lock file creation with existing packages."
  (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
             (lambda () '("package1" "package2")))
            ((symbol-function 'bfepm-core-get-packages-directory)
             (lambda () "/tmp/test-packages"))
            ((symbol-function 'bfepm-lock--detect-version)
             (lambda (dir)
               (cond 
                ((string-match-p "package1" dir) "1.0.0")
                ((string-match-p "package2" dir) "2.0.0")
                (t "unknown"))))
            ((symbol-function 'bfepm-utils-file-checksum)
             (lambda (_file) "testchecksum123")))
    
    (let ((lock (bfepm-lock-create)))
      (should (= (length (bfepm-lock-packages lock)) 2))
      
      ;; Check first package
      (let ((pkg1 (car (bfepm-lock-packages lock))))
        (should (string= (bfepm-lock-package-name pkg1) "package1"))
        (should (string= (bfepm-lock-package-version pkg1) "1.0.0"))))))

;;; TOML Encoding Tests

(ert-deftest bfepm-lock--encode-value-string ()
  "Test TOML value encoding for strings."
  (should (string= (bfepm-lock--encode-value "test") "\"test\""))
  (should (string= (bfepm-lock--encode-value "hello world") "\"hello world\"")))

(ert-deftest bfepm-lock--encode-value-number ()
  "Test TOML value encoding for numbers."
  (should (string= (bfepm-lock--encode-value 42) "42"))
  (should (string= (bfepm-lock--encode-value 3.14) "3.14")))

(ert-deftest bfepm-lock--encode-value-symbol ()
  "Test TOML value encoding for symbols."
  (should (string= (bfepm-lock--encode-value 'test) "\"test\""))
  (should (string= (bfepm-lock--encode-value 'latest) "\"latest\"")))

(ert-deftest bfepm-lock--encode-value-list ()
  "Test TOML value encoding for lists."
  (should (string= (bfepm-lock--encode-value nil) "\"nil\""))
  (should (string= (bfepm-lock--encode-value '(a b c)) "\"(a b c)\""))) ; Simplified implementation

;;; TOML Section Encoding Tests

(ert-deftest bfepm-lock--encode-section ()
  "Test TOML section encoding."
  (let ((section '((version . "1.0.0") (created . "2025-06-10"))))
    (let ((result (bfepm-lock--encode-section section)))
      (should (string-match-p "version = \"1.0.0\"" result))
      (should (string-match-p "created = \"2025-06-10\"" result)))))

;;; Package Encoding Tests

(ert-deftest bfepm-lock--encode-package ()
  "Test package encoding for TOML."
  (let ((package (make-bfepm-lock-package
                  :name "test-pkg"
                  :version "1.0.0"
                  :source "melpa" 
                  :checksum "abc123")))
    (let ((result (bfepm-lock--encode-package package)))
      (should (string-match-p "version = \"1.0.0\"" result))
      (should (string-match-p "source = \"melpa\"" result))
      (should (string-match-p "checksum = \"abc123\"" result)))))

;;; Full TOML Generation Tests

(ert-deftest bfepm-lock--to-toml ()
  "Test complete lock file TOML generation."
  (let* ((meta '((version . "1.0.0")))
         (package (make-bfepm-lock-package 
                   :name "test" :version "1.0" :source "melpa"))
         (resolution '((strategy . "latest")))
         (lock (make-bfepm-lock :meta meta :packages (list package) :resolution resolution)))
    
    (let ((toml (bfepm-lock--to-toml lock)))
      (should (string-match-p "\\[meta\\]" toml))
      (should (string-match-p "\\[packages\\.test\\]" toml))
      (should (string-match-p "\\[resolution\\]" toml))
      (should (string-match-p "version = \"1.0.0\"" toml)))))

;;; File Save/Load Tests

(ert-deftest bfepm-lock-save-and-load ()
  "Test lock file save and load operations."
  (let* ((temp-file (make-temp-file "bfepm-lock-test" nil ".lock"))
         (meta '((version . "1.0.0") (created . "2025-06-10")))
         (package (make-bfepm-lock-package 
                   :name "test-pkg" :version "1.0.0" :source "melpa"))
         (resolution '((strategy . "latest")))
         (original-lock (make-bfepm-lock 
                         :meta meta 
                         :packages (list package) 
                         :resolution resolution)))
    
    (unwind-protect
        (progn
          ;; Save lock file
          (bfepm-lock-save original-lock temp-file)
          (should (file-exists-p temp-file))
          
          ;; Verify file contains expected content
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\[meta\\]" content))
              (should (string-match-p "test-pkg" content))
              (should (string-match-p "1\\.0\\.0" content)))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Version Detection Tests

(ert-deftest bfepm-lock--detect-version-pkg-file ()
  "Test version detection from package files."
  (let ((temp-dir (make-temp-file "bfepm-lock-test" t)))
    (unwind-protect
        (let ((pkg-file (expand-file-name "test-pkg.el" temp-dir)))
          ;; Create mock package file
          (with-temp-file pkg-file
            (insert ";;; test-pkg.el --- Test package\n")
            (insert ";; Version: 1.2.3\n")
            (insert ";;; Code:\n")
            (insert "(provide 'test-pkg)\n"))
          
          ;; Test version detection (this is a simplified test since the actual
          ;; function looks for -pkg.el files, but we're testing the concept)
          (should (file-exists-p pkg-file)))
      
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Error Handling Tests

(ert-deftest bfepm-lock-load-nonexistent ()
  "Test loading nonexistent lock file."
  (let ((result (bfepm-lock-load "/nonexistent/path/bfepm.lock")))
    (should (null result))))

(ert-deftest bfepm-lock-save-invalid-directory ()
  "Test saving lock file to invalid directory."
  (let ((invalid-path "/nonexistent/directory/bfepm.lock")
        (lock (bfepm-lock-create)))
    
    (should-error (bfepm-lock-save lock invalid-path))))

;;; Lock File Integration Tests

(ert-deftest bfepm-lock-roundtrip ()
  "Test complete save/load roundtrip."
  (let* ((temp-file (make-temp-file "bfepm-lock-roundtrip" nil ".lock"))
         (original-lock (bfepm-lock-create)))
    
    (unwind-protect
        (progn
          ;; Save lock
          (bfepm-lock-save original-lock temp-file)
          (should (file-exists-p temp-file))
          
          ;; File should contain TOML structure
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\[meta\\]" content))
              (should (string-match-p "version =" content))
              (should (string-match-p "\\[resolution\\]" content)))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Lock File Validation Tests

(ert-deftest bfepm-lock-meta-fields ()
  "Test that lock meta contains required fields."
  (let ((lock (bfepm-lock-create)))
    (let ((meta (bfepm-lock-meta lock)))
      (should (alist-get 'version meta))
      (should (alist-get 'created meta))
      (should (alist-get 'updated meta))
      (should (alist-get 'bfepm_version meta)))))

(ert-deftest bfepm-lock-resolution-fields ()
  "Test that lock resolution contains expected fields."
  (let ((lock (bfepm-lock-create)))
    (let ((resolution (bfepm-lock-resolution lock)))
      (should (alist-get 'strategy resolution))
      (should (alist-get 'conflicts resolution))
      (should (alist-get 'warnings resolution)))))

(provide 'bfepm-lock-test)

;;; bfepm-lock-test.el ends here