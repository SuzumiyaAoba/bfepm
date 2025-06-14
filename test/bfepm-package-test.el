;;; bfepm-package-test.el --- Tests for bfepm-package -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for BFEPM package management functionality.

;;; Code:

(require 'ert)
(require 'bfepm-package)
(require 'bfepm-core)
(require 'bfepm-utils)

;;; Version Format Tests

(ert-deftest bfepm-package--format-version-string ()
  "Test version formatting for string inputs."
  (should (string= (bfepm-package--format-version "1.2.3") "1.2.3"))
  (should (string= (bfepm-package--format-version "latest") "latest")))

(ert-deftest bfepm-package--format-version-number ()
  "Test version formatting for numeric inputs."
  (should (string= (bfepm-package--format-version 123) "123"))
  (should (string= (bfepm-package--format-version 20250426) "20250426")))

(ert-deftest bfepm-package--format-version-list ()
  "Test version formatting for list inputs."
  (should (string= (bfepm-package--format-version '(20250426)) "20250426"))
  (should (string= (bfepm-package--format-version '(20250426 1319)) "20250426.1319"))
  (should (string= (bfepm-package--format-version '(1 2 3)) "1.2.3")))

;;; Version Matching Tests

(ert-deftest bfepm-package--version-matches-p-exact ()
  "Test exact version matching."
  (should (bfepm-package--version-matches-p "1.2.3" "1.2.3"))
  (should-not (bfepm-package--version-matches-p "1.2.3" "1.2.4")))

(ert-deftest bfepm-package--version-matches-p-latest ()
  "Test latest version matching."
  (should (bfepm-package--version-matches-p "1.2.3" "latest"))
  (should (bfepm-package--version-matches-p "20250426.1319" "latest")))

(ert-deftest bfepm-package--version-matches-p-caret ()
  "Test caret version constraint matching."
  (should (bfepm-package--version-matches-p "20250426.1319" "^20250426"))
  (should (bfepm-package--version-matches-p "20250426.1500" "^20250426"))
  (should-not (bfepm-package--version-matches-p "20250425.1319" "^20250426")))

(ert-deftest bfepm-package--version-matches-p-tilde ()
  "Test tilde version constraint matching."
  (should (bfepm-package--version-matches-p "20250426.1319" "~20250426.1300"))
  (should (bfepm-package--version-matches-p "20250426.1350" "~20250426.1300"))
  (should-not (bfepm-package--version-matches-p "20250426.1500" "~20250426.1300")))

;;; URL Building Tests

(ert-deftest bfepm-package--build-archive-url-tar ()
  "Test archive URL building for tar packages."
  (should (string= (bfepm-package--build-archive-url "test-pkg" "1.0.0" 'tar)
                   "https://melpa.org/packages/test-pkg-1.0.0.tar")))

(ert-deftest bfepm-package--build-archive-url-single ()
  "Test archive URL building for single file packages."
  (should (string= (bfepm-package--build-archive-url "test-pkg" "1.0.0" 'single)
                   "https://melpa.org/packages/test-pkg-1.0.0.el")))

;;; Source Priority Tests

(ert-deftest bfepm-package--get-source-priority-struct ()
  "Test source priority extraction from bfepm-source struct."
  (let ((source (make-bfepm-source :name "test" :url "http://test.com" :priority 15)))
    (should (= (bfepm-package--get-source-priority source) 15))))

(ert-deftest bfepm-package--get-source-priority-alist ()
  "Test source priority extraction from alist."
  (let ((source '((priority . 20) (url . "http://test.com"))))
    (should (= (bfepm-package--get-source-priority source) 20))))

(ert-deftest bfepm-package--get-source-priority-default ()
  "Test default source priority when not specified."
  (let ((source '((url . "http://test.com"))))
    (should (= (bfepm-package--get-source-priority source) 10))))

;;; Package Installation Tests (with mocking)

(ert-deftest bfepm-package-install-string-spec ()
  "Test package installation with string specification."
  (let ((bfepm-test-installed nil))
    (cl-letf (((symbol-function 'bfepm-core-package-installed-p)
               (lambda (_) bfepm-test-installed))
              ((symbol-function 'bfepm-package--find-package)
               (lambda (pkg _config) 
                 (when (string= (bfepm-package-name pkg) "test-package")
                   '(20250426 1319) ; Mock package info
                   )))
              ((symbol-function 'bfepm-package--download-and-install)
               (lambda (_pkg _info) 
                 (setq bfepm-test-installed t))))
      
      ;; Test installation
      (bfepm-package-install "test-package")
      (should bfepm-test-installed))))

(ert-deftest bfepm-package-install-already-installed ()
  "Test package installation when package is already installed."
  (cl-letf (((symbol-function 'bfepm-core-package-installed-p)
             (lambda (_) t))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args) 
               (should (string-match-p "already installed" format-str)))))
    
    (bfepm-package-install "test-package")))

;;; Package Removal Tests

(ert-deftest bfepm-package-remove-success ()
  "Test successful package removal."
  (let ((bfepm-test-removed nil))
    (cl-letf (((symbol-function 'bfepm-core-package-installed-p)
               (lambda (_) t))
              ((symbol-function 'bfepm-core-get-packages-directory)
               (lambda () "/tmp/bfepm-test"))
              ((symbol-function 'file-directory-p)
               (lambda (_) t))
              ((symbol-function 'delete-directory)
               (lambda (_dir _recursive) 
                 (setq bfepm-test-removed t)))
              ((symbol-function 'bfepm-utils-message)
               (lambda (_format-str &rest _args))))
      
      (bfepm-package-remove "test-package")
      (should bfepm-test-removed))))

(ert-deftest bfepm-package-remove-not-installed ()
  "Test package removal when package is not installed."
  (cl-letf (((symbol-function 'bfepm-core-package-installed-p)
             (lambda (_) nil))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args) 
               (should (string-match-p "not installed" format-str)))))
    
    (bfepm-package-remove "test-package")))

;;; Package Update Tests

(ert-deftest bfepm-package-update-single ()
  "Test updating a single package."
  (let ((bfepm-test-reinstalled nil))
    (cl-letf (((symbol-function 'bfepm-core-package-installed-p)
               (lambda (_) t))
              ((symbol-function 'bfepm-package-remove)
               (lambda (_pkg)))
              ((symbol-function 'bfepm-package-install)
               (lambda (_pkg) 
                 (setq bfepm-test-reinstalled t)))
              ((symbol-function 'bfepm-utils-message)
               (lambda (_format-str &rest _args))))
      
      (bfepm-package-update "test-package")
      (should bfepm-test-reinstalled))))

;;; Package Listing Tests

(ert-deftest bfepm-package-list-with-packages ()
  "Test package listing when packages are installed."
  (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
             (lambda () '("package1" "package2")))
            ((symbol-function 'bfepm-core-get-package-version)
             (lambda (pkg) 
               (if (string= pkg "package1") "1.0.0" "2.0.0")))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args)
               (when (string-match-p "Installed packages" format-str)
                 (should t))))
            ((symbol-function 'message)
             (lambda (format-str &rest args)
               (should (or (string-match-p "package1" format-str)
                          (string-match-p "package2" format-str))))))
    
    (bfepm-package-list)))

(ert-deftest bfepm-package-list-no-packages ()
  "Test package listing when no packages are installed."
  (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
             (lambda () '()))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args)
               (should (string-match-p "No packages installed" format-str)))))
    
    (bfepm-package-list)))

;;; Default Sources Tests

(ert-deftest bfepm-package--get-default-sources ()
  "Test that default sources are properly defined."
  (let ((sources (bfepm-package--get-default-sources)))
    (should (listp sources))
    (should (> (length sources) 0))
    ;; Should have MELPA source
    (should (assoc "melpa" sources))))

;;; Package Info Tests

(ert-deftest bfepm-package-info-found ()
  "Test package info display for found package."
  (cl-letf (((symbol-function 'bfepm-package--find-package)
             (lambda (_pkg _config)
               '(20250426 1319 ((dep1 "1.0")) "Test package description")))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args)
               (should (or (string-match-p "Package:" format-str)
                          (string-match-p "test-package" format-str)))))
            ((symbol-function 'message)
             (lambda (format-str &rest args)
               (should (or (string-match-p "Version:" format-str)
                          (string-match-p "Description:" format-str)
                          (string-match-p "Dependencies:" format-str))))))
    
    (bfepm-package-info "test-package")))

(ert-deftest bfepm-package-info-not-found ()
  "Test package info display for package not found."
  (cl-letf (((symbol-function 'bfepm-package--find-package)
             (lambda (_pkg _config) nil))
            ((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args)
               (should (string-match-p "not found" format-str)))))
    
    (bfepm-package-info "nonexistent-package")))

;;; Search Tests

(ert-deftest bfepm-package-search ()
  "Test package search functionality (currently placeholder)."
  (cl-letf (((symbol-function 'bfepm-utils-message)
             (lambda (format-str &rest args)
               (should (or (string-match-p "Searching" format-str)
                          (string-match-p "not yet implemented" format-str))))))
    
    (bfepm-package-search "test-query")))

;;; Git Package Installation Tests

(ert-deftest bfepm-package--find-in-git ()
  "Test finding packages in git sources."
  (let ((git-source '(:url "https://github.com/test/repo.git" 
                      :type "git" 
                      :ref "v1.0.0")))
    (let ((result (bfepm-package--find-in-git "test-package" git-source)))
      (should (listp result))
      (should (string= (car result) "v1.0.0"))
      (should (eq (cadddr result) 'git))
      (should (equal (nth 4 result) git-source)))))

(ert-deftest bfepm-package--find-in-git-no-ref ()
  "Test finding packages in git sources without specific ref."
  (let ((git-source '(:url "https://github.com/test/repo.git" 
                      :type "git")))
    (let ((result (bfepm-package--find-in-git "test-package" git-source)))
      (should (listp result))
      (should (string= (car result) "latest"))
      (should (eq (cadddr result) 'git))
      (should (equal (nth 4 result) git-source)))))

(ert-deftest bfepm-package--get-git-version-latest ()
  "Test getting git version for latest ref."
  (let ((test-dir "/tmp/test-git-repo"))
    (cl-letf (((symbol-function 'bfepm-utils-git-get-latest-tag)
               (lambda (_dir) "v2.1.0"))
              ((symbol-function 'bfepm-utils-git-get-commit-hash)
               (lambda (_dir &optional _ref) "abc123def456")))
      (should (string= (bfepm-package--get-git-version test-dir "latest") "v2.1.0"))
      (should (string= (bfepm-package--get-git-version test-dir nil) "v2.1.0")))))

(ert-deftest bfepm-package--get-git-version-commit-hash ()
  "Test getting git version for commit hash ref."
  (let ((test-dir "/tmp/test-git-repo")
        (commit-hash "abc123def456789"))
    (cl-letf (((symbol-function 'bfepm-utils-git-get-commit-hash)
               (lambda (_dir ref) ref)))
      (should (string= (bfepm-package--get-git-version test-dir commit-hash) commit-hash)))))

(ert-deftest bfepm-package--get-git-version-tag ()
  "Test getting git version for tag ref."
  (let ((test-dir "/tmp/test-git-repo")
        (tag "v1.5.0"))
    (should (string= (bfepm-package--get-git-version test-dir tag) tag))))

(ert-deftest bfepm-package--download-and-install-git ()
  "Test git package installation with mocking."
  (let ((test-package (make-bfepm-package :name "git-test-package" :version "latest"))
        (test-info '("latest" nil "Git package from https://github.com/test/repo.git" git (:url "https://github.com/test/repo.git" :type "git" :ref "main")))
        (bfepm-test-cloned nil)
        (bfepm-test-version-saved nil))
    (cl-letf (((symbol-function 'bfepm-core-get-config)
               (lambda () nil))
              ((symbol-function 'bfepm-package--get-default-sources)
               (lambda () '(("git-source" . (:url "https://github.com/test/repo.git" 
                                            :type "git" 
                                            :ref "main")))))
              ((symbol-function 'bfepm-core-get-packages-directory)
               (lambda () "/tmp/bfepm-packages"))
              ((symbol-function 'file-directory-p)
               (lambda (_) nil))
              ((symbol-function 'bfepm-utils-git-clone)
               (lambda (_url _dir &optional _ref _shallow) 
                 (setq bfepm-test-cloned t)))
              ((symbol-function 'bfepm-package--get-git-version)
               (lambda (_dir _ref) "main-abc123"))
              ((symbol-function 'bfepm-package--save-version-info)
               (lambda (_name version) 
                 (setq bfepm-test-version-saved version)))
              ((symbol-function 'bfepm-package--verify-installation)
               (lambda (_name _dir)))
              ((symbol-function 'add-to-list)
               (lambda (_list _item)))
              ((symbol-function 'bfepm-core--invalidate-cache)
               (lambda (_name)))
              ((symbol-function 'bfepm-utils-message)
               (lambda (_format &rest _args)))
              ((symbol-function 'bfepm-package--find-in-git)
               (lambda (_name _source) test-info)))
      
      (bfepm-package--download-and-install-git test-package test-info)
      (should bfepm-test-cloned)
      (should (string= bfepm-test-version-saved "main-abc123")))))

(provide 'bfepm-package-test)

;;; bfepm-package-test.el ends here