;;; bfepm-config-test.el --- Tests for BFEPM configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM configuration functionality.

;;; Code:

(require 'buttercup)
(require 'bfepm-config)

(describe "BFEPM Config"
  (describe "bfepm-config--parse-package-spec"
    (it "should parse simple string version"
      (let ((package (bfepm-config--parse-package-spec "company" "latest")))
        (expect (bfepm-package-name package) :to-equal "company")
        (expect (bfepm-package-version package) :to-equal "latest")))

    (it "should parse complex package specification"
      (let ((spec '((version . "1.0.0")
                    (optional . t)
                    (config . ((idle-delay . 0.3)))))
            (package (bfepm-config--parse-package-spec "company" spec)))
        (expect (bfepm-package-name package) :to-equal "company")
        (expect (bfepm-package-version package) :to-equal "1.0.0")
        (expect (bfepm-package-status package) :to-equal 'optional)
        (expect (bfepm-package-config package) :to-equal '((idle-delay . 0.3)))))

    (it "should handle missing version in complex spec"
      (let ((spec '((optional . t)))
            (package (bfepm-config--parse-package-spec "company" spec)))
        (expect (bfepm-package-version package) :to-equal "latest"))))

  (describe "bfepm-config--parse-source-spec"
    (it "should parse source specification"
      (let ((spec '((url . "https://melpa.org/packages/")
                    (type . "elpa")
                    (priority . 10)))
            (source (bfepm-config--parse-source-spec spec)))
        (expect (bfepm-source-url source) :to-equal "https://melpa.org/packages/")
        (expect (bfepm-source-type source) :to-equal "elpa")
        (expect (bfepm-source-priority source) :to-equal 10)))

    (it "should use defaults for missing fields"
      (let ((spec '((url . "https://example.com/")))
            (source (bfepm-config--parse-source-spec spec)))
        (expect (bfepm-source-type source) :to-equal "elpa")
        (expect (bfepm-source-priority source) :to-equal 10))))

  (describe "bfepm-config-create-default"
    (it "should create valid default configuration"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-p config) :to-be t)
        (expect (bfepm-config-packages config) :to-be nil)
        (expect (bfepm-config-sources config) :not :to-be nil)
        (expect (length (bfepm-config-sources config)) :to-equal 3))))

  (describe "bfepm-config-validate"
    (it "should validate correct configuration"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-validate config) :to-be t)))

    (it "should reject non-config structure"
      (expect (lambda () (bfepm-config-validate "not-a-config"))
              :to-throw 'error))

    (it "should reject config without sources"
      (let ((config (make-bfepm-config :sources nil)))
        (expect (lambda () (bfepm-config-validate config))
                :to-throw 'error))))

  (describe "bfepm-config-get-package"
    (it "should find existing package"
      (let* ((package (make-bfepm-package :name "company" :version "1.0.0"))
             (config (make-bfepm-config :packages (list package))))
        (expect (bfepm-config-get-package config "company") :to-equal package)))

    (it "should return nil for non-existent package"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-get-package config "non-existent") :to-be nil))))

  (describe "bfepm-config-get-source"
    (it "should find existing source"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-get-source config "melpa") :not :to-be nil)))

    (it "should return nil for non-existent source"
      (let ((config (bfepm-config-create-default)))
        (expect (bfepm-config-get-source config "non-existent") :to-be nil)))))

;;; bfepm-config-test.el ends here