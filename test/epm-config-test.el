;;; epm-config-test.el --- Tests for EPM configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for EPM configuration functionality.

;;; Code:

(require 'buttercup)
(require 'epm-config)

(describe "EPM Config"
  (describe "epm-config--parse-package-spec"
    (it "should parse simple string version"
      (let ((package (epm-config--parse-package-spec "company" "latest")))
        (expect (epm-package-name package) :to-equal "company")
        (expect (epm-package-version package) :to-equal "latest")))

    (it "should parse complex package specification"
      (let ((spec '((version . "1.0.0")
                    (optional . t)
                    (config . ((idle-delay . 0.3)))))
            (package (epm-config--parse-package-spec "company" spec)))
        (expect (epm-package-name package) :to-equal "company")
        (expect (epm-package-version package) :to-equal "1.0.0")
        (expect (epm-package-status package) :to-equal 'optional)
        (expect (epm-package-config package) :to-equal '((idle-delay . 0.3)))))

    (it "should handle missing version in complex spec"
      (let ((spec '((optional . t)))
            (package (epm-config--parse-package-spec "company" spec)))
        (expect (epm-package-version package) :to-equal "latest"))))

  (describe "epm-config--parse-source-spec"
    (it "should parse source specification"
      (let ((spec '((url . "https://melpa.org/packages/")
                    (type . "elpa")
                    (priority . 10)))
            (source (epm-config--parse-source-spec spec)))
        (expect (epm-source-url source) :to-equal "https://melpa.org/packages/")
        (expect (epm-source-type source) :to-equal "elpa")
        (expect (epm-source-priority source) :to-equal 10)))

    (it "should use defaults for missing fields"
      (let ((spec '((url . "https://example.com/")))
            (source (epm-config--parse-source-spec spec)))
        (expect (epm-source-type source) :to-equal "elpa")
        (expect (epm-source-priority source) :to-equal 10))))

  (describe "epm-config-create-default"
    (it "should create valid default configuration"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-p config) :to-be t)
        (expect (epm-config-packages config) :to-be nil)
        (expect (epm-config-sources config) :not :to-be nil)
        (expect (length (epm-config-sources config)) :to-equal 3))))

  (describe "epm-config-validate"
    (it "should validate correct configuration"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-validate config) :to-be t)))

    (it "should reject non-config structure"
      (expect (lambda () (epm-config-validate "not-a-config"))
              :to-throw 'error))

    (it "should reject config without sources"
      (let ((config (make-epm-config :sources nil)))
        (expect (lambda () (epm-config-validate config))
                :to-throw 'error))))

  (describe "epm-config-get-package"
    (it "should find existing package"
      (let* ((package (make-epm-package :name "company" :version "1.0.0"))
             (config (make-epm-config :packages (list package))))
        (expect (epm-config-get-package config "company") :to-equal package)))

    (it "should return nil for non-existent package"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-get-package config "non-existent") :to-be nil))))

  (describe "epm-config-get-source"
    (it "should find existing source"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-get-source config "melpa") :not :to-be nil)))

    (it "should return nil for non-existent source"
      (let ((config (epm-config-create-default)))
        (expect (epm-config-get-source config "non-existent") :to-be nil)))))

;;; epm-config-test.el ends here