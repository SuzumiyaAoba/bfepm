;;; bfepm-test.el --- Tests for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM using buttercup.

;;; Code:

(require 'buttercup)
(require 'bfepm)
(require 'bfepm-core)
(require 'bfepm-config)
(require 'bfepm-package)
(require 'bfepm-utils)

(describe "BFEPM Core"
  (before-each
    (setq bfepm--initialized nil
          bfepm--config nil
          bfepm--packages-directory nil
          bfepm--cache-directory nil))

  (describe "bfepm-core-initialize"
    (it "should initialize BFEPM successfully"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (let ((pm-config-file (expand-file-name "pm.toml" pm-directory)))
              (expect (bfepm-core-initialize) :to-be nil)
              (expect bfepm--initialized :to-be t))
          (delete-directory pm-directory t)))))

  (describe "bfepm-core-package-installed-p"
    (it "should return nil for non-existent package"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (bfepm-core-initialize)
              (expect (bfepm-core-package-installed-p "non-existent") :to-be nil))
          (delete-directory pm-directory t))))

    (it "should return t for existing package"
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (bfepm-core-initialize)
              (let ((package-dir (expand-file-name "test-package" 
                                                   (bfepm-core-get-packages-directory))))
                (make-directory package-dir t)
                (expect (bfepm-core-package-installed-p "test-package") :to-be t)))
          (delete-directory pm-directory t))))))

(describe "BFEPM Config"
  (describe "bfepm-config-create-default"
    (it "should create a valid default configuration"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-p config) :to-be t)
        (expect (bfepm-config-sources config) :not :to-be nil)
        (expect (bfepm-config-global-settings config) :not :to-be nil))))

  (describe "bfepm-config-validate"
    (it "should validate a correct configuration"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-validate config) :to-be t)))

    (it "should reject invalid configuration"
      (expect (lambda () (bfepm-config-validate "not-a-config"))
              :to-throw))))

(describe "BFEPM Package Structures"
  (describe "bfepm-package"
    (it "should create package with required fields"
      (let ((package (make-bfepm-package :name "test" :version "1.0.0")))
        (expect (bfepm-package-name package) :to-equal "test")
        (expect (bfepm-package-version package) :to-equal "1.0.0"))))

(describe "BFEPM Utils"
  (describe "bfepm-utils-version-compare"
    (it "should compare versions correctly"
      (expect (bfepm-utils-version-compare "1.0.0" "1.0.0") :to-equal 0)
      (expect (bfepm-utils-version-compare "1.0.1" "1.0.0") :to-equal 1)
      (expect (bfepm-utils-version-compare "1.0.0" "1.0.1") :to-equal -1)
      (expect (bfepm-utils-version-compare "2.0.0" "1.9.9") :to-equal 1)))

  (describe "bfepm-utils-version-satisfies-p"
    (it "should handle exact version matches"
      (expect (bfepm-utils-version-satisfies-p "1.0.0" "1.0.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.0.1" "1.0.0") :to-be nil))

    (it "should handle 'latest' requirement"
      (expect (bfepm-utils-version-satisfies-p "1.0.0" "latest") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "99.0.0" "latest") :to-be t))

    (it "should handle caret requirements"
      (expect (bfepm-utils-version-satisfies-p "1.2.3" "^1.2.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.3.0" "^1.2.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "2.0.0" "^1.2.0") :to-be nil))

    (it "should handle tilde requirements"
      (expect (bfepm-utils-version-satisfies-p "1.2.3" "~1.2.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.2.9" "~1.2.0") :to-be t)
      (expect (bfepm-utils-version-satisfies-p "1.3.0" "~1.2.0") :to-be nil)))

  (describe "bfepm-utils-file-sha256"
    (it "should calculate correct checksum for file"
      (let ((test-file (make-temp-file "pm-test")))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "test content"))
              (let ((checksum (bfepm-utils-file-sha256 test-file)))
                (expect checksum :to-be-truthy)
                (expect (length checksum) :to-equal 64)))
          (delete-file test-file))))

    (it "should return nil for non-existent file"
      (expect (bfepm-utils-file-sha256 "/non/existent/file") :to-be nil))))

(describe "BFEPM Integration"
  (describe "Package installation workflow"
    (it "should handle basic package installation flow"
      ;; This would be a more complex integration test
      ;; For now, just test that the functions don't error
      (let ((pm-directory (make-temp-file "pm-test" t)))
        (unwind-protect
            (progn
              (bfepm-core-initialize)
              ;; Test that we can create package structures without errors
              (let ((package (make-bfepm-package :name "test-package" :version "1.0.0")))
                (expect (bfepm-package-name package) :to-equal "test-package")))
          (delete-directory pm-directory t))))))

;;; bfepm-test.el ends here