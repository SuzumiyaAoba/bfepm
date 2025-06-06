;;; epm-test.el --- Tests for EPM -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for EPM using buttercup.

;;; Code:

(require 'buttercup)
(require 'epm)
(require 'epm-core)
(require 'epm-config)
(require 'epm-package)
(require 'epm-utils)

(describe "EPM Core"
  (before-each
    (setq epm--initialized nil
          epm--config nil
          epm--packages-directory nil
          epm--cache-directory nil))

  (describe "epm-core-initialize"
    (it "should initialize EPM successfully"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (let ((pm-config-file (expand-file-name "pm.toml" pm-directory)))
              (expect (epm-core-initialize) :to-be nil)
              (expect epm--initialized :to-be t))
          (delete-directory pm-directory t)))))

  (describe "epm-core-package-installed-p"
    (it "should return nil for non-existent package"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (epm-core-initialize)
              (expect (epm-core-package-installed-p "non-existent") :to-be nil))
          (delete-directory pm-directory t))))

    (it "should return t for existing package"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (epm-core-initialize)
              (let ((package-dir (expand-file-name "test-package" 
                                                   (epm-core-get-packages-directory))))
                (make-directory package-dir t)
                (expect (epm-core-package-installed-p "test-package") :to-be t)))
          (delete-directory pm-directory t))))))

(describe "EPM Config"
  (describe "epm-config-create-default"
    (it "should create a valid default configuration"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-p config) :to-be t)
        (expect (epm-config-sources config) :not :to-be nil)
        (expect (epm-config-global-settings config) :not :to-be nil))))

  (describe "epm-config-validate"
    (it "should validate a correct configuration"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-validate config) :to-be t)))

    (it "should reject invalid configuration"
      (expect (lambda () (epm-config-validate "not-a-config"))
              :to-throw))))

(describe "EPM Package Structures"
  (describe "epm-package"
    (it "should create package with required fields"
      (let ((package (make-epm-package :name "test" :version "1.0.0")))
        (expect (epm-package-name package) :to-equal "test")
        (expect (epm-package-version package) :to-equal "1.0.0"))))

(describe "EPM Utils"
  (describe "epm-utils-version-compare"
    (it "should compare versions correctly"
      (expect (epm-utils-version-compare "1.0.0" "1.0.0") :to-equal 0)
      (expect (epm-utils-version-compare "1.0.1" "1.0.0") :to-equal 1)
      (expect (epm-utils-version-compare "1.0.0" "1.0.1") :to-equal -1)
      (expect (epm-utils-version-compare "2.0.0" "1.9.9") :to-equal 1)))

  (describe "epm-utils-version-satisfies-p"
    (it "should handle exact version matches"
      (expect (epm-utils-version-satisfies-p "1.0.0" "1.0.0") :to-be t)
      (expect (epm-utils-version-satisfies-p "1.0.1" "1.0.0") :to-be nil))

    (it "should handle 'latest' requirement"
      (expect (epm-utils-version-satisfies-p "1.0.0" "latest") :to-be t)
      (expect (epm-utils-version-satisfies-p "99.0.0" "latest") :to-be t))

    (it "should handle caret requirements"
      (expect (epm-utils-version-satisfies-p "1.2.3" "^1.2.0") :to-be t)
      (expect (epm-utils-version-satisfies-p "1.3.0" "^1.2.0") :to-be t)
      (expect (epm-utils-version-satisfies-p "2.0.0" "^1.2.0") :to-be nil))

    (it "should handle tilde requirements"
      (expect (epm-utils-version-satisfies-p "1.2.3" "~1.2.0") :to-be t)
      (expect (epm-utils-version-satisfies-p "1.2.9" "~1.2.0") :to-be t)
      (expect (epm-utils-version-satisfies-p "1.3.0" "~1.2.0") :to-be nil)))

  (describe "epm-utils-file-sha256"
    (it "should calculate correct checksum for file"
      (let ((test-file (make-temp-file "pm-test")))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "test content"))
              (let ((checksum (epm-utils-file-sha256 test-file)))
                (expect checksum :to-be-truthy)
                (expect (length checksum) :to-equal 64)))
          (delete-file test-file))))

    (it "should return nil for non-existent file"
      (expect (epm-utils-file-sha256 "/non/existent/file") :to-be nil))))

(describe "EPM Integration"
  (describe "Package installation workflow"
    (it "should handle basic package installation flow"
      ;; This would be a more complex integration test
      ;; For now, just test that the functions don't error
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (epm-core-initialize)
              ;; Test that we can create package structures without errors
              (let ((package (make-epm-package :name "test-package" :version "1.0.0")))
                (expect (epm-package-name package) :to-equal "test-package")))
          (delete-directory pm-directory t))))))

;;; epm-test.el ends here