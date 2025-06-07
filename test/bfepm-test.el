;;; bfepm-test.el --- Tests for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM using ERT.

;;; Code:

(require 'ert)
(require 'bfepm)
(require 'bfepm-core)
(require 'bfepm-config)
(require 'bfepm-package)
(require 'bfepm-utils)

;;; Core Tests

(ert-deftest bfepm-core-initialize-test ()
  "Test that BFEPM core initializes successfully."
  (let ((bfepm--initialized nil)
        (bfepm--config nil)
        (bfepm--packages-directory nil)
        (bfepm--cache-directory nil)
        (bfepm-directory (make-temp-file "bfepm-test" t)))
    (unwind-protect
        (progn
          (bfepm-core-initialize)
          (should bfepm--initialized))
      (when (file-directory-p bfepm-directory)
        (delete-directory bfepm-directory t)))))

(ert-deftest bfepm-core-package-installed-p-non-existent ()
  "Test that non-existent packages return nil."
  (let ((bfepm-directory (make-temp-file "bfepm-test" t)))
    (unwind-protect
        (progn
          (bfepm-core-initialize)
          (should-not (bfepm-core-package-installed-p "non-existent-package")))
      (when (file-directory-p bfepm-directory)
        (delete-directory bfepm-directory t)))))

(ert-deftest bfepm-core-package-installed-p-existing ()
  "Test that existing packages return t."
  (let ((bfepm-directory (make-temp-file "bfepm-test" t)))
    (unwind-protect
        (progn
          (bfepm-core-initialize)
          (let ((package-dir (expand-file-name "test-package" 
                                               (bfepm-core-get-packages-directory))))
            (make-directory package-dir t)
            (should (bfepm-core-package-installed-p "test-package"))))
      (when (file-directory-p bfepm-directory)
        (delete-directory bfepm-directory t)))))

;;; Package Structure Tests

(ert-deftest bfepm-package-struct-creation ()
  "Test BFEPM package structure creation."
  (let ((package (make-bfepm-package
                  :name "test-package"
                  :version "1.0.0"
                  :source "melpa"
                  :dependencies '()
                  :status 'installed)))
    (should (bfepm-package-p package))
    (should (string= (bfepm-package-name package) "test-package"))
    (should (string= (bfepm-package-version package) "1.0.0"))
    (should (string= (bfepm-package-source package) "melpa"))
    (should (eq (bfepm-package-status package) 'installed))))

;;; Configuration Tests

(ert-deftest bfepm-config-struct-creation ()
  "Test BFEPM configuration structure creation."
  (let ((config (make-bfepm-config
                 :packages '()
                 :sources '(("melpa" . "https://melpa.org/packages/")))))
    (should (bfepm-config-p config))
    (should (listp (bfepm-config-packages config)))
    (should (listp (bfepm-config-sources config)))))

(provide 'bfepm-test)

;;; bfepm-test.el ends here