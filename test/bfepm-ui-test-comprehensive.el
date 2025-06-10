;;; bfepm-ui-test-comprehensive.el --- Comprehensive tests for bfepm-ui -*- lexical-binding: t -*-

;;; Commentary:

;; Comprehensive test suite for BFEPM UI functionality.

;;; Code:

(require 'ert)
(require 'bfepm-ui)
(require 'bfepm-core)

;;; Configuration Package Tests

(ert-deftest bfepm-ui--get-config-packages-demo ()
  "Test config package discovery in demo environment."
  (let ((bfepm-demo-packages '(("test-pkg" "Test package description")
                               ("another-pkg" "Another package"))))
    (cl-letf (((symbol-function 'bfepm-demo-get-package-version)
               (lambda (name) 
                 (if (string= name "test-pkg") "1.0.0" "2.0.0"))))
      
      (let ((packages (bfepm-ui--get-config-packages)))
        (should (= (length packages) 2))
        (should (assoc "test-pkg" packages))
        (should (string= (cdr (assoc "test-pkg" packages)) "1.0.0"))))))

(ert-deftest bfepm-ui--get-config-packages-fallback ()
  "Test config package discovery fallback to file parsing."
  (let ((bfepm-demo-packages nil))
    (cl-letf (((symbol-function 'bfepm-core-get-config)
               (lambda () nil))
              ((symbol-function 'bfepm-ui--parse-config-file-packages)
               (lambda () '(("file-pkg" . "3.0.0")))))
      
      (let ((packages (bfepm-ui--get-config-packages)))
        (should (= (length packages) 1))
        (should (assoc "file-pkg" packages))
        (should (string= (cdr (assoc "file-pkg" packages)) "3.0.0"))))))

;;; TOML Parsing Tests

(ert-deftest bfepm-ui--simple-toml-parse ()
  "Test simple TOML parsing functionality."
  (let ((temp-file (make-temp-file "bfepm-ui-toml-test" nil ".toml")))
    (unwind-protect
        (progn
          ;; Create test TOML file
          (with-temp-file temp-file
            (insert "[packages]\n")
            (insert "company = \"latest\"\n")
            (insert "magit = \"^3.0\"\n")
            (insert "\n")
            (insert "[sources]\n")
            (insert "melpa = \"https://melpa.org/packages/\"\n"))
          
          ;; Parse and verify
          (let ((packages (bfepm-ui--simple-toml-parse temp-file)))
            (should (= (length packages) 2))
            (should (assoc "company" packages))
            (should (assoc "magit" packages))
            (should (string= (cdr (assoc "company" packages)) "latest"))
            (should (string= (cdr (assoc "magit" packages)) "^3.0"))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bfepm-ui--simple-toml-parse-empty ()
  "Test TOML parsing with empty or missing packages section."
  (let ((temp-file (make-temp-file "bfepm-ui-toml-empty" nil ".toml")))
    (unwind-protect
        (progn
          ;; Create TOML file without packages section
          (with-temp-file temp-file
            (insert "[sources]\n")
            (insert "melpa = \"https://melpa.org/packages/\"\n"))
          
          ;; Parse and verify empty result
          (let ((packages (bfepm-ui--simple-toml-parse temp-file)))
            (should (= (length packages) 0))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; String Utility Tests

(ert-deftest bfepm-ui--string-trim ()
  "Test string trimming utility function."
  (should (string= (bfepm-ui--string-trim "  hello  ") "hello"))
  (should (string= (bfepm-ui--string-trim "\n\ttest\r\n") "test"))
  (should (string= (bfepm-ui--string-trim "no-trim") "no-trim"))
  (should (string= (bfepm-ui--string-trim "") "")))

;;; Available Package List Tests

(ert-deftest bfepm-ui-update-available-package-list ()
  "Test updating available package list."
  (cl-letf (((symbol-function 'bfepm-ui--get-config-packages)
             (lambda () '(("pkg1" . "1.0.0") ("pkg2" . "2.0.0"))))
            ((symbol-function 'bfepm-core-package-installed-p)
             (lambda (name) (string= name "pkg1")))
            ((symbol-function 'bfepm-ui--get-package-description)
             (lambda (_) nil))
            ((symbol-function 'bfepm-ui--get-config-description)
             (lambda (name) (format "Description for %s" name))))
    
    (with-temp-buffer
      (bfepm-ui-mode)
      (bfepm-ui-update-available-package-list)
      
      ;; Check entries were created
      (should (= (length tabulated-list-entries) 2))
      
      ;; Check first entry (installed)
      (let ((entry1 (car tabulated-list-entries)))
        (should (string= (car entry1) "pkg1"))
        (let ((vector1 (cadr entry1)))
          (should (string= (aref vector1 2) "Installed"))))
      
      ;; Check second entry (available)
      (let ((entry2 (cadr tabulated-list-entries)))
        (should (string= (car entry2) "pkg2"))
        (let ((vector2 (cadr entry2)))
          (should (string= (aref vector2 2) "Available")))))))

;;; Package Description Tests

(ert-deftest bfepm-ui--get-config-description ()
  "Test getting package descriptions from demo data."
  (let ((bfepm-demo-package-descriptions 
         '(("test-pkg" "Test package description")
           ("another-pkg" "Another description"))))
    
    (should (string= (bfepm-ui--get-config-description "test-pkg") 
                     "Test package description"))
    (should (string= (bfepm-ui--get-config-description "another-pkg")
                     "Another description"))
    (should (null (bfepm-ui--get-config-description "nonexistent")))))

;;; View Toggle Tests

(ert-deftest bfepm-ui-toggle-view ()
  "Test view toggling between installed and available."
  (with-temp-buffer
    (bfepm-ui-mode)
    (setq bfepm-ui-current-view 'installed)
    
    (cl-letf (((symbol-function 'bfepm-ui-show-available-external)
               (lambda () (setq bfepm-ui-current-view 'available)))
              ((symbol-function 'bfepm-ui-show-installed-external)
               (lambda () (setq bfepm-ui-current-view 'installed))))
      
      ;; Test toggle from installed to available
      (bfepm-ui-toggle-view)
      (should (eq bfepm-ui-current-view 'available))
      
      ;; Test toggle from available to installed
      (bfepm-ui-toggle-view)
      (should (eq bfepm-ui-current-view 'installed)))))

;;; Package Information Display Tests

(ert-deftest bfepm-ui--show-package-info-buffer ()
  "Test package information buffer display."
  (cl-letf (((symbol-function 'bfepm-core-get-packages-directory)
             (lambda () "/tmp/test-packages"))
            ((symbol-function 'bfepm-core-get-package-version)
             (lambda (_) "1.0.0"))
            ((symbol-function 'file-directory-p)
             (lambda (_) t))
            ((symbol-function 'directory-files)
             (lambda (_dir _full _match) '("test.el" "test-autoloads.el")))
            ((symbol-function 'bfepm-ui--get-package-description)
             (lambda (_) "Test package description"))
            ((symbol-function 'pop-to-buffer)
             (lambda (buffer) buffer)))
    
    (let ((buffer (bfepm-ui--show-package-info-buffer "test-package")))
      (should (bufferp buffer))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (should (string-match-p "Package: test-package" content))
          (should (string-match-p "Version: 1.0.0" content))
          (should (string-match-p "Files:" content))
          (should (string-match-p "test.el" content)))))))

;;; Package Action Tests

(ert-deftest bfepm-ui-install-package-at-point ()
  "Test installing package at point."
  (let ((bfepm-test-installed nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () "test-package"))
              ((symbol-function 'bfepm-package-install)
               (lambda (_pkg) (setq bfepm-test-installed t)))
              ((symbol-function 'bfepm-ui-refresh)
               (lambda ())))
      
      (bfepm-ui-install-package)
      (should bfepm-test-installed))))

(ert-deftest bfepm-ui-remove-package-confirmed ()
  "Test removing package with confirmation."
  (let ((bfepm-test-removed nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () "test-package"))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'bfepm-package-remove)
               (lambda (_pkg) (setq bfepm-test-removed t)))
              ((symbol-function 'bfepm-ui-refresh)
               (lambda ())))
      
      (bfepm-ui-remove-package)
      (should bfepm-test-removed))))

(ert-deftest bfepm-ui-remove-package-cancelled ()
  "Test removing package when user cancels."
  (let ((bfepm-test-removed nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () "test-package"))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil))
              ((symbol-function 'bfepm-package-remove)
               (lambda (_pkg) (setq bfepm-test-removed t))))
      
      (bfepm-ui-remove-package)
      (should-not bfepm-test-removed))))

;;; External Command Tests

(ert-deftest bfepm-ui-show-available-external ()
  "Test showing available packages via external command."
  (with-temp-buffer
    (cl-letf (((symbol-function 'bfepm-ui)
               (lambda () (bfepm-ui-mode)))
              ((symbol-function 'bfepm-ui-update-available-package-list)
               (lambda () (setq tabulated-list-entries '())))
              ((symbol-function 'tabulated-list-print)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (_format-str &rest _args))))
      
      (bfepm-ui-show-available-external)
      (should (eq major-mode 'bfepm-ui-mode))
      (should (eq bfepm-ui-current-view 'available)))))

(ert-deftest bfepm-ui-show-installed-external ()
  "Test showing installed packages via external command."
  (with-temp-buffer
    (cl-letf (((symbol-function 'bfepm-ui)
               (lambda () (bfepm-ui-mode))))
      
      (bfepm-ui-show-installed-external)
      (should (eq major-mode 'bfepm-ui-mode)))))

;;; Error Handling Tests

(ert-deftest bfepm-ui-install-package-error ()
  "Test package installation error handling."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () "test-package"))
            ((symbol-function 'bfepm-package-install)
             (lambda (_pkg) (error "Installation failed")))
            ((symbol-function 'message)
             (lambda (format-str &rest args)
               (should (string-match-p "Failed to install" format-str)))))
    
    (bfepm-ui-install-package)))

(ert-deftest bfepm-ui-update-package-error ()
  "Test package update error handling."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () "test-package"))
            ((symbol-function 'bfepm-package-update)
             (lambda (_pkg) (error "Update failed")))
            ((symbol-function 'message)
             (lambda (format-str &rest args)
               (should (string-match-p "Failed to update" format-str)))))
    
    (bfepm-ui-update-package)))

;;; Buffer Refresh Tests

(ert-deftest bfepm-ui-refresh-buffer-installed ()
  "Test buffer refresh in installed view."
  (with-temp-buffer
    (bfepm-ui-mode)
    (setq bfepm-ui-current-view 'installed)
    
    (cl-letf (((symbol-function 'bfepm-ui-update-package-list)
               (lambda () (setq tabulated-list-entries '())))
              ((symbol-function 'tabulated-list-print)
               (lambda (_remember-pos))))
      
      (bfepm-ui-refresh-buffer)
      ;; Should call update-package-list for installed view
      (should (eq bfepm-ui-current-view 'installed)))))

(ert-deftest bfepm-ui-refresh-buffer-available ()
  "Test buffer refresh in available view."
  (with-temp-buffer
    (bfepm-ui-mode)
    (setq bfepm-ui-current-view 'available)
    
    (cl-letf (((symbol-function 'bfepm-ui-update-available-package-list)
               (lambda () (setq tabulated-list-entries '())))
              ((symbol-function 'tabulated-list-print)
               (lambda (_remember-pos))))
      
      (bfepm-ui-refresh-buffer)
      ;; Should call update-available-package-list for available view
      (should (eq bfepm-ui-current-view 'available)))))

;;; Help System Tests

(ert-deftest bfepm-ui-help ()
  "Test help system display."
  (cl-letf (((symbol-function 'help--window-setup)
             (lambda (_buffer-name _function)
               ;; Mock help window setup
               (with-temp-buffer
                 (funcall _function)
                 (let ((content (buffer-string)))
                   (should (string-match-p "BFEPM Package Management UI" content))
                   (should (string-match-p "Key bindings:" content))
                   (should (string-match-p "RET.*Show package details" content))
                   (should (string-match-p "i.*Install package" content)))))))
    
    (bfepm-ui-help)))

(provide 'bfepm-ui-test-comprehensive)

;;; bfepm-ui-test-comprehensive.el ends here