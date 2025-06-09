;;; bfepm-ui-test-simple.el --- Simple tests for bfepm-ui -*- lexical-binding: t -*-

;;; Commentary:

;; Simple tests for BFEPM UI functionality.

;;; Code:

(require 'ert)
(require 'bfepm-ui)
(require 'bfepm-core)

(ert-deftest bfepm-ui-mode-initialization ()
  "Test that bfepm-ui-mode initializes correctly."
  (with-temp-buffer
    ;; Ensure the function exists before calling
    (should (fboundp 'bfepm-ui--update-mode-line))
    (bfepm-ui-mode)
    (should (eq major-mode 'bfepm-ui-mode))
    (should (vectorp tabulated-list-format))
    (should (= (length tabulated-list-format) 4))))

(ert-deftest bfepm-ui-update-package-list-basic ()
  "Test that package list updates with mocked data."
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

(ert-deftest bfepm-ui-mode-line-update ()
  "Test that mode line updates correctly with view changes."
  (with-temp-buffer
    (bfepm-ui-mode)
    ;; Test installed view
    (setq bfepm-ui-current-view 'installed)
    (bfepm-ui--update-mode-line)
    (should (string-match "Installed" mode-name))
    
    ;; Test available view
    (setq bfepm-ui-current-view 'available)
    (bfepm-ui--update-mode-line)
    (should (string-match "Available" mode-name))))

;; Note: Testing helper functions is limited due to file loading issues
;; Focus on testing the main UI functionality that is available

(ert-deftest bfepm-ui-external-commands-exist ()
  "Test that external UI commands are properly defined."
  (should (fboundp 'bfepm-ui-show-available-external))
  (should (fboundp 'bfepm-ui-show-installed-external))
  (should (commandp 'bfepm-ui-show-available-external))
  (should (commandp 'bfepm-ui-show-installed-external)))

(provide 'bfepm-ui-test-simple)

;;; bfepm-ui-test-simple.el ends here
