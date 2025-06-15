;;; bfepm-git-test.el --- Tests for bfepm-git.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for bfepm-git functionality.

;;; Code:

(require 'ert)
(require 'bfepm-git)

(ert-deftest bfepm-git-validate-url-test ()
  "Test git URL validation."
  (should (bfepm-git-validate-url "https://github.com/user/repo.git"))
  (should (bfepm-git-validate-url "git@github.com:user/repo.git"))
  (should (bfepm-git-validate-url "ssh://git@github.com/user/repo.git"))
  (should (bfepm-git-validate-url "file:///path/to/repo"))
  (should-not (bfepm-git-validate-url "not-a-git-url"))
  (should-not (bfepm-git-validate-url "https://example.com"))
  (should-not (bfepm-git-validate-url nil)))

(ert-deftest bfepm-git-looks-like-commit-hash-p-test ()
  "Test commit hash detection."
  (should (bfepm-git--looks-like-commit-hash-p "a1b2c3d"))
  (should (bfepm-git--looks-like-commit-hash-p "abc123def456"))
  (should (bfepm-git--looks-like-commit-hash-p "1234567890abcdef1234567890abcdef12345678"))
  (should-not (bfepm-git--looks-like-commit-hash-p "main"))
  (should-not (bfepm-git--looks-like-commit-hash-p "v1.0.0"))
  (should-not (bfepm-git--looks-like-commit-hash-p "release/v1.0"))
  (should-not (bfepm-git--looks-like-commit-hash-p nil))
  (should-not (bfepm-git--looks-like-commit-hash-p "123"))) ; too short

(ert-deftest bfepm-git-clone-test ()
  "Test git clone functionality with mocking."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               0)) ; Return success
            ((symbol-function 'bfepm-git--ensure-directory)
             (lambda (_dir) t))
            ((symbol-function 'file-name-directory)
             (lambda (_path) "/tmp/")))
    
    ;; Test basic clone
    (should (bfepm-git-clone "https://github.com/test/repo.git" "/tmp/test-repo"))
    
    ;; Test shallow clone
    (should (bfepm-git-clone "https://github.com/test/repo.git" "/tmp/test-repo" nil t))))

(ert-deftest bfepm-git-clone-with-ref-test ()
  "Test git clone with branch/tag reference."
  (let ((checkout-called nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (should (string= program bfepm-git-executable))
                 0))
              ((symbol-function 'bfepm-git--ensure-directory)
               (lambda (_dir) t))
              ((symbol-function 'file-name-directory)
               (lambda (_path) "/tmp/"))
              ((symbol-function 'bfepm-git-checkout)
               (lambda (_repo-dir _ref)
                 (setq checkout-called t)
                 t)))
      
      ;; Test clone with branch (should not call checkout)
      (setq checkout-called nil)
      (should (bfepm-git-clone "https://github.com/test/repo.git" "/tmp/test-repo" "main"))
      (should-not checkout-called)
      
      ;; Test clone with commit hash (should call checkout)
      (setq checkout-called nil)
      (should (bfepm-git-clone "https://github.com/test/repo.git" "/tmp/test-repo" "abc123def"))
      (should checkout-called))))

(ert-deftest bfepm-git-checkout-test ()
  "Test git checkout functionality."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               (should (member "checkout" args))
               0)))
    
    (should (bfepm-git-checkout "/tmp/repo" "main"))
    (should (bfepm-git-checkout "/tmp/repo" "v1.0.0"))
    (should (bfepm-git-checkout "/tmp/repo" "abc123def"))))

(ert-deftest bfepm-git-get-commit-hash-test ()
  "Test git commit hash retrieval."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               (should (member "rev-parse" args))
               0))
            ((symbol-function 'string-trim)
             (lambda (_str) "abc123def456789"))
            ((symbol-function 'buffer-string)
             (lambda () "abc123def456789\n")))
    
    (should (string= (bfepm-git-get-commit-hash "/tmp/repo") "abc123def456789"))
    (should (string= (bfepm-git-get-commit-hash "/tmp/repo" "HEAD") "abc123def456789"))))

(ert-deftest bfepm-git-get-latest-tag-test ()
  "Test git latest tag retrieval."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               (if (member "describe" args)
                   0  ; Success for describe command
                 1))) ; Failure for other commands
            ((symbol-function 'string-trim)
             (lambda (_str) "v1.2.3"))
            ((symbol-function 'buffer-string)
             (lambda () "v1.2.3\n")))
    
    (should (string= (bfepm-git-get-latest-tag "/tmp/repo") "v1.2.3"))))

(ert-deftest bfepm-git-list-tags-test ()
  "Test git tags listing."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               0))
            ((symbol-function 'string-trim)
             (lambda (_str) "v1.0.0\nv1.1.0\nv1.2.0"))
            ((symbol-function 'buffer-string)
             (lambda () "v1.0.0\nv1.1.0\nv1.2.0\n"))
            ((symbol-function 'string-empty-p)
             (lambda (_str) nil)))
    
    (let ((tags (bfepm-git-list-tags "/tmp/repo")))
      (should (listp tags))
      (should (= (length tags) 3))
      (should (member "v1.0.0" tags))
      (should (member "v1.1.0" tags))
      (should (member "v1.2.0" tags)))))

(ert-deftest bfepm-git-get-latest-version-test ()
  "Test git version resolution logic."
  (cl-letf (((symbol-function 'bfepm-git-get-latest-tag)
             (lambda (_repo-dir) "v1.2.3"))
            ((symbol-function 'bfepm-git-get-commit-hash)
             (lambda (_repo-dir &optional _ref) "abc123def")))
    
    ;; Test with no ref (should return latest tag)
    (should (string= (bfepm-git-get-latest-version "/tmp/repo") "v1.2.3"))
    
    ;; Test with "latest" ref (should return latest tag)
    (should (string= (bfepm-git-get-latest-version "/tmp/repo" "latest") "v1.2.3"))
    
    ;; Test with commit hash (should return commit hash)
    (should (string= (bfepm-git-get-latest-version "/tmp/repo" "abc123def") "abc123def"))
    
    ;; Test with branch name (should resolve to commit hash)
    (should (string= (bfepm-git-get-latest-version "/tmp/repo" "main") "abc123def"))))

(ert-deftest bfepm-git-is-repository-p-test ()
  "Test git repository detection."
  (cl-letf (((symbol-function 'file-directory-p)
             (lambda (path)
               (string= path "/tmp/repo/.git")))
            ((symbol-function 'file-exists-p)
             (lambda (path)
               (string= path "/tmp/repo/.git"))))
    
    (should (bfepm-git-is-repository-p "/tmp/repo"))
    (should-not (bfepm-git-is-repository-p "/tmp/not-repo"))))

(ert-deftest bfepm-git-get-remote-url-test ()
  "Test git remote URL retrieval."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               (should (member "remote" args))
               (should (member "get-url" args))
               0))
            ((symbol-function 'string-trim)
             (lambda (_str) "https://github.com/user/repo.git"))
            ((symbol-function 'buffer-string)
             (lambda () "https://github.com/user/repo.git\n")))
    
    (should (string= (bfepm-git-get-remote-url "/tmp/repo") "https://github.com/user/repo.git"))
    (should (string= (bfepm-git-get-remote-url "/tmp/repo" "origin") "https://github.com/user/repo.git"))))

(ert-deftest bfepm-git-fetch-tags-test ()
  "Test git fetch tags functionality."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (should (string= program bfepm-git-executable))
               (should (member "fetch" args))
               (should (member "--tags" args))
               0)))
    
    (should (bfepm-git-fetch-tags "/tmp/repo"))))

(provide 'bfepm-git-test)

;;; bfepm-git-test.el ends here