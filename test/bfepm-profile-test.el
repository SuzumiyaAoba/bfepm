;;; bfepm-profile-test.el --- Tests for bfepm-profile -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for BFEPM profile management functionality.

;;; Code:

(require 'ert)
(require 'bfepm-profile)
(require 'bfepm-core)

(defvar bfepm-profile-test-temp-dir nil
  "Temporary directory for profile tests.")

(defun bfepm-profile-test--setup ()
  "Set up test environment for profile tests."
  (setq bfepm-profile-test-temp-dir (make-temp-file "bfepm-profile-test" t))
  (setq bfepm-directory bfepm-profile-test-temp-dir)
  (setq bfepm--profiles-directory (expand-file-name "profiles" bfepm-profile-test-temp-dir))
  (bfepm-profile--ensure-profiles-directory))

(defun bfepm-profile-test--teardown ()
  "Clean up test environment for profile tests."
  (when (and bfepm-profile-test-temp-dir (file-exists-p bfepm-profile-test-temp-dir))
    (delete-directory bfepm-profile-test-temp-dir t))
  (setq bfepm-profile-test-temp-dir nil))

(ert-deftest bfepm-profile-test-create-profile ()
  "Test creating a new profile."
  (bfepm-profile-test--setup)
  (unwind-protect
      (let ((profile-name "test-profile"))
        (bfepm-profile-create profile-name)
        (should (bfepm-profile-exists-p profile-name))
        (let ((profile (bfepm-profile-load profile-name)))
          (should (string= (bfepm-profile-name profile) profile-name))
          (should (null (bfepm-profile-includes profile)))
          (should (null (bfepm-profile-packages profile)))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-create-profile-with-base ()
  "Test creating a profile with a base profile."
  (bfepm-profile-test--setup)
  (unwind-protect
      (let ((base-profile "base")
            (new-profile "derived"))
        ;; Create base profile with some packages
        (bfepm-profile-create base-profile)
        (let ((base (bfepm-profile-load base-profile)))
          (setf (bfepm-profile-packages base) '(("package1" . "1.0") ("package2" . "2.0")))
          (bfepm-profile--save-profile base))
        
        ;; Create derived profile
        (bfepm-profile-create new-profile base-profile)
        (should (bfepm-profile-exists-p new-profile))
        
        (let ((derived (bfepm-profile-load new-profile)))
          (should (string= (bfepm-profile-name derived) new-profile))
          (should (member base-profile (bfepm-profile-includes derived)))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-save-and-load-profile ()
  "Test saving and loading profile configurations."
  (bfepm-profile-test--setup)
  (unwind-protect
      (let ((profile-name "test-save-load")
            (packages '(("emacs-lisp-mode" . "latest") ("org-mode" . "9.5")))
            (includes '("base")))
        
        ;; Create and configure profile
        (let ((profile (make-bfepm-profile
                        :name profile-name
                        :includes includes
                        :packages packages
                        :config '()
                        :active nil)))
          (bfepm-profile--save-profile profile))
        
        ;; Load and verify
        (let ((loaded-profile (bfepm-profile-load profile-name)))
          (should (string= (bfepm-profile-name loaded-profile) profile-name))
          (should (equal (bfepm-profile-includes loaded-profile) includes))
          (should (equal (bfepm-profile-packages loaded-profile) packages))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-list-profiles ()
  "Test listing available profiles."
  (bfepm-profile-test--setup)
  (unwind-protect
      (progn
        ;; Initially no profiles
        (should (null (bfepm-profile-list-names)))
        
        ;; Create some profiles
        (bfepm-profile-create "profile1")
        (bfepm-profile-create "profile2")
        (bfepm-profile-create "profile3")
        
        ;; Check they are listed
        (let ((profiles (bfepm-profile-list-names)))
          (should (= (length profiles) 3))
          (should (member "profile1" profiles))
          (should (member "profile2" profiles))
          (should (member "profile3" profiles))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-profile-exists-p ()
  "Test checking if profile exists."
  (bfepm-profile-test--setup)
  (unwind-protect
      (progn
        (should-not (bfepm-profile-exists-p "nonexistent"))
        
        (bfepm-profile-create "existing")
        (should (bfepm-profile-exists-p "existing"))
        (should-not (bfepm-profile-exists-p "still-nonexistent")))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-remove-profile ()
  "Test removing profiles."
  (bfepm-profile-test--setup)
  (unwind-protect
      (let ((profile-name "to-be-removed"))
        ;; Create profile
        (bfepm-profile-create profile-name)
        (should (bfepm-profile-exists-p profile-name))
        
        ;; Remove profile  
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (bfepm-profile-remove profile-name))
        (should-not (bfepm-profile-exists-p profile-name)))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-copy-profile ()
  "Test copying profiles."
  (bfepm-profile-test--setup)
  (unwind-protect
      (let ((source-profile "source")
            (target-profile "target")
            (packages '(("package1" . "1.0") ("package2" . "2.0"))))
        
        ;; Create source profile with packages
        (bfepm-profile-create source-profile)
        (let ((source (bfepm-profile-load source-profile)))
          (setf (bfepm-profile-packages source) packages)
          (bfepm-profile--save-profile source))
        
        ;; Copy profile
        (bfepm-profile-copy source-profile target-profile)
        (should (bfepm-profile-exists-p target-profile))
        
        ;; Verify copied profile has same packages
        (let ((target (bfepm-profile-load target-profile)))
          (should (string= (bfepm-profile-name target) target-profile))
          (should (equal (bfepm-profile-packages target) packages))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-resolve-packages ()
  "Test package resolution with profile inheritance."
  (bfepm-profile-test--setup)
  (unwind-protect
      (progn
        ;; Create base profile
        (bfepm-profile-create "base")
        (let ((base (bfepm-profile-load "base")))
          (setf (bfepm-profile-packages base) '(("base-pkg1" . "1.0") ("base-pkg2" . "2.0")))
          (bfepm-profile--save-profile base))
        
        ;; Create derived profile
        (bfepm-profile-create "derived")
        (let ((derived (bfepm-profile-load "derived")))
          (setf (bfepm-profile-includes derived) '("base")
                (bfepm-profile-packages derived) '(("derived-pkg" . "3.0") ("base-pkg1" . "1.5"))) ; Override base-pkg1
          (bfepm-profile--save-profile derived))
        
        ;; Test package resolution
        (let* ((derived-profile (bfepm-profile-load "derived"))
               (resolved-packages (bfepm-profile--resolve-packages derived-profile)))
          (should (= (length resolved-packages) 3))
          
          ;; Check that base-pkg1 is overridden to version 1.5
          (let ((base-pkg1 (cl-find-if (lambda (pkg) 
                                         (string= (bfepm-package-name pkg) "base-pkg1"))
                                       resolved-packages)))
            (should base-pkg1)
            (should (string= (bfepm-package-version base-pkg1) "1.5")))
          
          ;; Check that base-pkg2 is inherited
          (let ((base-pkg2 (cl-find-if (lambda (pkg) 
                                         (string= (bfepm-package-name pkg) "base-pkg2"))
                                       resolved-packages)))
            (should base-pkg2)
            (should (string= (bfepm-package-version base-pkg2) "2.0")))
          
          ;; Check that derived-pkg is included
          (let ((derived-pkg (cl-find-if (lambda (pkg) 
                                           (string= (bfepm-package-name pkg) "derived-pkg"))
                                         resolved-packages)))
            (should derived-pkg)
            (should (string= (bfepm-package-version derived-pkg) "3.0")))))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-current-profile ()
  "Test current profile tracking."
  (bfepm-profile-test--setup)
  (unwind-protect
      (progn
        ;; Default profile should be "default"
        (should (string= (bfepm-profile-current) "default"))
        
        ;; Create and switch to a new profile
        (bfepm-profile-create "test-current")
        (bfepm-profile-switch "test-current")
        (should (string= (bfepm-profile-current) "test-current")))
    (bfepm-profile-test--teardown)))

(ert-deftest bfepm-profile-test-format-profile-content ()
  "Test profile content formatting for TOML output."
  (let ((profile (make-bfepm-profile
                  :name "test"
                  :includes '("base" "dev")
                  :packages '(("package1" . "1.0") ("package2" . "2.0"))
                  :config '()
                  :active nil)))
    
    (let ((content (bfepm-profile--format-profile-content profile)))
      (should (string-match-p "includes = \\[\"base\", \"dev\"\\]" content))
      (should (string-match-p "\\[packages\\]" content))
      (should (string-match-p "package1 = \"1.0\"" content))
      (should (string-match-p "package2 = \"2.0\"" content)))))

(ert-deftest bfepm-profile-test-parse-profile-content ()
  "Test parsing profile content from TOML string."
  (let ((content "includes = [\"base\", \"dev\"]

[packages]
package1 = \"1.0\"
package2 = \"2.0\""))
    
    (let ((profile (bfepm-profile--parse-profile-content content "test-parse")))
      (should (string= (bfepm-profile-name profile) "test-parse"))
      (should (equal (bfepm-profile-includes profile) '("base" "dev")))
      (should (equal (bfepm-profile-packages profile) '(("package1" . "1.0") ("package2" . "2.0")))))))

(ert-deftest bfepm-profile-test-validate-name ()
  "Test profile name validation."
  ;; Valid names should pass
  (should-not (condition-case nil (bfepm-profile--validate-name "valid-name") (error t)))
  (should-not (condition-case nil (bfepm-profile--validate-name "valid_name") (error t)))
  (should-not (condition-case nil (bfepm-profile--validate-name "ValidName123") (error t)))
  
  ;; Invalid names should fail
  (should (condition-case nil (bfepm-profile--validate-name "") (error t)))
  (should (condition-case nil (bfepm-profile--validate-name "invalid name") (error t)))
  (should (condition-case nil (bfepm-profile--validate-name "invalid/name") (error t)))
  (should (condition-case nil (bfepm-profile--validate-name ".") (error t)))
  (should (condition-case nil (bfepm-profile--validate-name "CON") (error t))))

(ert-deftest bfepm-profile-test-escape-toml-string ()
  "Test TOML string escaping functionality."
  (should (string= (bfepm-profile--escape-toml-string "simple") "simple"))
  (should (string= (bfepm-profile--escape-toml-string "has\"quote") "has\\\"quote"))
  (should (string= (bfepm-profile--escape-toml-string "has\\backslash") "has\\\\backslash"))
  (should (string= (bfepm-profile--escape-toml-string "has\nnewline") "has\\\\nnewline")))

(provide 'bfepm-profile-test)

;;; bfepm-profile-test.el ends here