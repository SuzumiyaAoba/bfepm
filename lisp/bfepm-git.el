;;; bfepm-git.el --- Git operations for package management -*- lexical-binding: t -*-

;;; Commentary:

;; Standalone git operations module that provides git functionality
;; without dependencies on BFEPM-specific code. This module can be
;; used independently or as part of the BFEPM package manager.

;;; Code:

(require 'cl-lib)


(defcustom bfepm-git-executable "git"
  "Path to git executable."
  :type 'string
  :group 'bfepm-git)

(defun bfepm-git--ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-git--message (format-string &rest args)
  "Display a git operation message.
FORMAT-STRING is the format string, ARGS are the arguments."
  (message "[Git] %s" (apply #'format format-string args)))

(defun bfepm-git--error (format-string &rest args)
  "Signal a git operation error.
FORMAT-STRING is the format string, ARGS are the arguments."
  (error "[Git] %s" (apply #'format format-string args)))

(defun bfepm-git-clone (url target-dir &optional ref shallow)
  "Clone git repository from URL to TARGET-DIR.
Optional REF can be a branch, tag, or commit hash.
If SHALLOW is non-nil, perform a shallow clone.
Returns t on success, signals error on failure."
  (bfepm-git--ensure-directory (file-name-directory target-dir))
  
  (let* ((git-cmd (list bfepm-git-executable "clone"))
         (default-directory (file-name-directory target-dir)))
    
    ;; Add shallow clone option if requested
    (when shallow
      (setq git-cmd (append git-cmd '("--depth" "1"))))
    
    ;; Add specific branch/tag if specified, but not if it's a commit hash
    (when (and ref 
               (not (string= ref "latest")) 
               (not (bfepm-git--looks-like-commit-hash-p ref)))
      (setq git-cmd (append git-cmd (list "--branch" ref))))
    
    ;; Add URL and target directory
    (setq git-cmd (append git-cmd (list url target-dir)))
    
    (bfepm-git--message "Cloning repository: %s" url)
    (let ((result (apply #'call-process (car git-cmd) nil nil nil (cdr git-cmd))))
      (unless (= result 0)
        (bfepm-git--error "Clone failed with exit code %d" result))
      
      ;; If ref is a commit hash, checkout after clone
      (when (and ref
                 (not (string= ref "latest"))
                 (bfepm-git--looks-like-commit-hash-p ref))
        (bfepm-git-checkout target-dir ref))
      
      (bfepm-git--message "Successfully cloned to %s" target-dir)
      t)))

(defun bfepm-git-checkout (repo-dir ref)
  "Checkout REF in git repository at REPO-DIR.
REF can be a branch, tag, or commit hash.
Returns t on success, signals error on failure."
  (let ((default-directory repo-dir))
    (bfepm-git--message "Checking out %s in %s" ref repo-dir)
    (let ((result (call-process bfepm-git-executable nil nil nil "checkout" ref)))
      (unless (= result 0)
        (bfepm-git--error "Checkout of %s failed with exit code %d" ref result))
      t)))

(defun bfepm-git-get-latest-tag (repo-dir)
  "Get the latest tag from git repository at REPO-DIR.
If no tags are found locally (e.g., due to shallow clone), fetch tags first.
Returns tag string or nil if no tags found."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process bfepm-git-executable nil t nil "describe" "--tags" "--abbrev=0")))
        (if (= result 0)
            (string-trim (buffer-string))
          ;; If tag discovery fails, try fetching tags and retry
          (progn
            (bfepm-git--message "No tags found locally, fetching tags from remote...")
            (let ((fetch-result (call-process bfepm-git-executable nil nil nil "fetch" "--tags")))
              (if (= fetch-result 0)
                  (progn
                    (erase-buffer)
                    (let ((retry-result (call-process bfepm-git-executable nil t nil "describe" "--tags" "--abbrev=0")))
                      (if (= retry-result 0)
                          (string-trim (buffer-string))
                        nil)))
                (progn
                  (bfepm-git--message "Failed to fetch tags from remote")
                  nil)))))))))

(defun bfepm-git-get-commit-hash (repo-dir &optional ref)
  "Get commit hash for REF in git repository at REPO-DIR.
If REF is nil, gets current HEAD commit hash.
Returns commit hash string or nil on failure."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process bfepm-git-executable nil t nil "rev-parse" (or ref "HEAD"))))
        (if (= result 0)
            (string-trim (buffer-string))
          nil)))))

(defun bfepm-git-list-tags (repo-dir)
  "List all tags in git repository at REPO-DIR.
If no tags are found locally (e.g., due to shallow clone), fetch tags first.
Returns list of tag strings or nil if no tags found."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process bfepm-git-executable nil t nil "tag" "-l")))
        (if (and (= result 0) (not (string-empty-p (string-trim (buffer-string)))))
            (split-string (string-trim (buffer-string)) "\n" t)
          ;; If no tags found, try fetching tags and retry
          (progn
            (bfepm-git--message "No tags found locally, fetching tags from remote...")
            (let ((fetch-result (call-process bfepm-git-executable nil nil nil "fetch" "--tags")))
              (if (= fetch-result 0)
                  (progn
                    (erase-buffer)
                    (let ((retry-result (call-process bfepm-git-executable nil t nil "tag" "-l")))
                      (if (= retry-result 0)
                          (let ((tag-output (string-trim (buffer-string))))
                            (if (not (string-empty-p tag-output))
                                (split-string tag-output "\n" t)
                              nil))
                        nil)))
                (progn
                  (bfepm-git--message "Failed to fetch tags from remote")
                  nil)))))))))

(defun bfepm-git-get-latest-version (repo-dir &optional ref)
  "Get version for git repository at REPO-DIR with optional REF.
Returns version string based on the following priority:
1. If REF is specified and looks like commit hash, return it
2. If REF is specified, resolve to commit hash for reproducibility  
3. If no REF or \\='latest\\=', try latest tag then commit hash
Returns version string or \\='unknown\\=' on failure."
  (cond
   ;; If ref is "latest" or nil, try to get latest tag, then commit hash
   ((or (not ref) (string= ref "latest"))
    (or (bfepm-git-get-latest-tag repo-dir)
        (bfepm-git-get-commit-hash repo-dir)
        "unknown"))
   ;; If ref looks like a commit hash, return it
   ((bfepm-git--looks-like-commit-hash-p ref)
    (or (bfepm-git-get-commit-hash repo-dir ref) ref))
   ;; For tags or branches, resolve to commit hash for reproducibility
   (t (or (bfepm-git-get-commit-hash repo-dir ref) ref))))

(defun bfepm-git-fetch-tags (repo-dir)
  "Fetch tags from remote in git repository at REPO-DIR.
Returns t on success, nil on failure."
  (let ((default-directory repo-dir))
    (bfepm-git--message "Fetching tags from remote...")
    (let ((result (call-process bfepm-git-executable nil nil nil "fetch" "--tags")))
      (if (= result 0)
          (progn
            (bfepm-git--message "Successfully fetched tags")
            t)
        (progn
          (bfepm-git--message "Failed to fetch tags (exit code: %d)" result)
          nil)))))

(defun bfepm-git-is-repository-p (dir)
  "Check if DIR is a git repository.
Returns t if DIR contains a .git directory or file, nil otherwise."
  (or (file-directory-p (expand-file-name ".git" dir))
      (file-exists-p (expand-file-name ".git" dir))))

(defun bfepm-git-get-remote-url (repo-dir &optional remote)
  "Get remote URL for REMOTE (defaults to \\='origin\\=') in REPO-DIR.
Returns URL string or nil if remote not found."
  (let ((default-directory repo-dir)
        (remote-name (or remote "origin")))
    (with-temp-buffer
      (let ((result (call-process bfepm-git-executable nil t nil "remote" "get-url" remote-name)))
        (if (= result 0)
            (string-trim (buffer-string))
          nil)))))

(defun bfepm-git--looks-like-commit-hash-p (ref)
  "Check if REF looks like a commit hash.
Returns t if REF matches commit hash pattern (7-40 hex characters)."
  (and (stringp ref)
       (string-match-p "^[A-Fa-f0-9]\\{7,40\\}$" ref)))

(defun bfepm-git-validate-url (url)
  "Validate that URL is a valid git repository URL.
Returns t if URL looks valid, nil otherwise."
  (and (stringp url)
       (or (string-match-p "^https://.*\\(?:\\.git\\)?$" url)
           (string-match-p "^git@.*:" url)
           (string-match-p "^ssh://.*\\.git$" url)
           (string-match-p "^file://.*" url))))

(defun bfepm-git-get-default-branch (repo-dir)
  "Get the default branch name for git repository at REPO-DIR.
Returns branch name string or nil on failure."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process bfepm-git-executable nil t nil "symbolic-ref" "refs/remotes/origin/HEAD")))
        (if (= result 0)
            (let ((symbolic-ref (string-trim (buffer-string))))
              (when (string-match "refs/remotes/origin/\\(.+\\)" symbolic-ref)
                (match-string 1 symbolic-ref)))
          ;; Fallback: try common default branch names
          (erase-buffer)
          (dolist (branch '("main" "master" "develop"))
            (let ((check-result (call-process bfepm-git-executable nil t nil "show-ref" "--verify" "--quiet" (format "refs/remotes/origin/%s" branch))))
              (when (= check-result 0)
                (cl-return branch)))))))))

(provide 'bfepm-git)

;;; bfepm-git.el ends here