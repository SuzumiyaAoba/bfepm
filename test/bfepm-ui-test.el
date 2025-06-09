;;; bfepm-ui-test.el --- Tests for bfepm-ui -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for BFEPM UI functionality.

;;; Code:

(require 'ert)
(require 'bfepm-ui)
(require 'bfepm-core)

(ert-deftest bfepm-ui-mode-initialization ()
  "Test that bfepm-ui-mode initializes correctly."
  (with-temp-buffer
    (bfepm-ui-mode)
    (should (eq major-mode 'bfepm-ui-mode))
    (should (vectorp tabulated-list-format))
    (should (= (length tabulated-list-format) 4))))

(ert-deftest bfepm-ui-package-description-extraction ()
  "Test package description extraction from .el files."
  (let ((temp-dir (make-temp-file "bfepm-test" t))
        (package-name "test-package"))
    (unwind-protect
        (let ((package-dir (expand-file-name package-name temp-dir))
              (main-file nil))
          ;; Create package directory and main file
          (make-directory package-dir)
          (setq main-file (expand-file-name (format "%s.el" package-name) package-dir))
          
          ;; Write test content with Commentary section
          (with-temp-file main-file
            (insert ";;; test-package.el --- A test package -*- lexical-binding: t -*-\n")
            (insert "\n")
            (insert ";;; Commentary:\n")
            (insert "\n")
            (insert ";; This is a test package for testing purposes.\n")
            (insert "\n")
            (insert ";;; Code:\n")
            (insert "\n")
            (insert "(provide 'test-package)\n")
            (insert "\n")
            (insert ";;; test-package.el ends here\n"))
          
          ;; Debug: Show the file content
          (message "File exists: %s" (file-exists-p main-file))
          (when (file-exists-p main-file)
            (with-temp-buffer
              (insert-file-contents main-file)
              (message "File contents:\n%s" (buffer-string))))
          
          ;; Mock bfepm-core-get-packages-directory to return our temp directory
          (cl-letf (((symbol-function 'bfepm-core-get-packages-directory)
                     (lambda () temp-dir)))
            ;; Test description extraction
            (let ((description (bfepm-ui--get-package-description package-name)))
              (message "Extracted description: %s" description)
              (should (string= description "This is a test package for testing purposes.")))))
      
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest bfepm-ui-package-description-fallback ()
  "Test package description fallback to first comment line."
  (let ((temp-dir (make-temp-file "bfepm-test" t))
        (package-name "test-package-fallback"))
    (unwind-protect
        (let ((package-dir (expand-file-name package-name temp-dir))
              (main-file nil))
          ;; Create package directory and main file
          (make-directory package-dir)
          (setq main-file (expand-file-name (format "%s.el" package-name) package-dir))
          
          ;; Write test content without Commentary section
          (with-temp-file main-file
            (insert ";;; test-package-fallback.el --- Fallback test package\n")
            (insert "\n")
            (insert ";; This should be picked up as fallback description.\n")
            (insert "\n")
            (insert ";;; Code:\n")
            (insert "\n")
            (insert "(provide 'test-package-fallback)\n"))
          
          ;; Mock bfepm-core-get-packages-directory to return our temp directory
          (cl-letf (((symbol-function 'bfepm-core-get-packages-directory)
                     (lambda () temp-dir)))
            ;; Test description extraction fallback
            (let ((description (bfepm-ui--get-package-description package-name)))
              (should (string= description "This should be picked up as fallback description.")))))
      
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest bfepm-ui-package-description-no-file ()
  "Test package description when main file doesn't exist."
  (let ((package-name "nonexistent-package"))
    ;; Test with non-existent package
    (let ((description (bfepm-ui--get-package-description package-name)))
      (should (null description)))))

(ert-deftest bfepm-ui-update-package-list ()
  "Test that package list updates correctly."
  ;; Mock the core functions
  (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
             (lambda () '("package1" "package2")))
            ((symbol-function 'bfepm-core-get-package-version)
             (lambda (pkg) (if (string= pkg "package1") "1.0.0" "2.0.0")))
            ((symbol-function 'bfepm-core-package-installed-p)
             (lambda (_) t))
            ((symbol-function 'bfepm-ui--get-package-description)
             (lambda (pkg) (format "Description for %s" pkg))))
    
    (with-temp-buffer
      (bfepm-ui-mode)
      (bfepm-ui-update-package-list)
      
      ;; Check that entries were created
      (should (= (length tabulated-list-entries) 2))
      
      ;; Check first entry
      (let ((entry1 (car tabulated-list-entries)))
        (should (string= (car entry1) "package1"))
        (let ((vector1 (cadr entry1)))
          (should (string= (aref vector1 0) "package1"))
          (should (string= (aref vector1 1) "1.0.0"))
          (should (string= (aref vector1 2) "Installed"))
          (should (string= (aref vector1 3) "Description for package1")))))))

(provide 'bfepm-ui-test)

;;; bfepm-ui-test.el ends here