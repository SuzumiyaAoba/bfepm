;;; bfepm-utils.el --- BFEPM Utility functions -*- lexical-binding: t -*-

;;; Commentary:

;; Utility functions for BFEPM.

;;; Code:

(require 'url)
(require 'json)
(require 'bfepm-version)
(require 'bfepm-network)

(defun bfepm-utils-message (format-string &rest args)
  "Display a formatted message with BFEPM prefix.
FORMAT-STRING is the format string, ARGS are the arguments."
  (message "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-error (format-string &rest args)
  "Signal an error with BFEPM prefix.
FORMAT-STRING is the format string, ARGS are the arguments."
  (error "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-version-compare (v1 v2)
  "Compare version strings V1 and V2.
Return 1 if V1 > V2, -1 if V1 < V2, 0 if equal.

This function is deprecated. Use `bfepm-version-compare' instead."
  (bfepm-version-compare v1 v2))


(defun bfepm-utils-version-satisfies-p (version requirement)
  "Check if VERSION satisfies REQUIREMENT.

This function is deprecated. Use `bfepm-version-satisfies-p' instead."
  (bfepm-version-satisfies-p version requirement))




(defun bfepm-utils-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-utils-download-file (url local-file &optional max-retries)
  "Download file from URL to LOCAL-FILE with retry logic.
MAX-RETRIES defaults to 3.

This function is deprecated. Use `bfepm-network-download-file' instead."
  (bfepm-network-download-file url local-file max-retries))

(defun bfepm-utils-download-file-async (url local-file callback &optional max-retries)
  "Download file from URL to LOCAL-FILE asynchronously.
CALLBACK is called with (success error-message) when complete.
MAX-RETRIES defaults to 3.

This function is deprecated. Use `bfepm-network-download-file-async' instead."
  (bfepm-network-download-file-async url local-file callback max-retries))


(defun bfepm-utils-extract-tar (tar-file target-dir)
  "Extract TAR-FILE to TARGET-DIR."
  (bfepm-utils-message "Extracting %s..." tar-file)
  (let ((default-directory target-dir))
    (with-temp-buffer
      (call-process "tar" nil t nil "-xf" tar-file)
      (when (> (buffer-size) 0)
        (bfepm-utils-message "Extraction output: %s" (buffer-string))))))

(defun bfepm-utils-copy-file (source target)
  "Copy SOURCE file to TARGET."
  (copy-file source target t))

(defun bfepm-utils-file-checksum (file)
  "Calculate SHA256 checksum of FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun bfepm-utils-verify-checksum (file expected-checksum)
  "Verify FILE against EXPECTED-CHECKSUM.
Returns t if checksum matches, nil otherwise."
  (when (and file expected-checksum (file-exists-p file))
    (let ((actual-checksum (bfepm-utils-file-checksum file)))
      (string= (downcase actual-checksum) (downcase expected-checksum)))))

(defun bfepm-utils-http-get (url)
  "Make HTTP GET request to URL and return response body.

This function is deprecated. Use `bfepm-network-http-get' instead."
  (bfepm-network-http-get url))

(defun bfepm-utils-git-clone (url target-dir &optional ref shallow)
  "Clone git repository from URL to TARGET-DIR.
Optional REF can be a branch, tag, or commit hash.
If SHALLOW is non-nil, perform a shallow clone."
  (bfepm-utils-ensure-directory (file-name-directory target-dir))
  
  (let* ((git-cmd (list "git" "clone"))
         (default-directory (file-name-directory target-dir)))
    
    ;; Add shallow clone option if requested
    (when shallow
      (setq git-cmd (append git-cmd '("--depth" "1"))))
    
    ;; Add specific branch/tag if specified, but not if it's a commit hash
    (when (and ref (not (string= ref "latest")) (not (string-match-p "^[a-f0-9]\\{7,40\\}$" ref)))
      (setq git-cmd (append git-cmd (list "--branch" ref))))
    
    ;; Add URL and target directory
    (setq git-cmd (append git-cmd (list url target-dir)))
    
    (bfepm-utils-message "Cloning git repository: %s" url)
    (let ((result (apply #'call-process (car git-cmd) nil nil nil (cdr git-cmd))))
      (unless (= result 0)
        (bfepm-utils-error "Git clone failed with exit code %d" result))
      
      ;; If ref is a commit hash, checkout after clone
      (when (and ref
                 (not (string= ref "latest"))
                 (string-match-p "^[a-f0-9]\\{7,40\\}$" ref)) ; Looks like a commit hash
        (bfepm-utils-git-checkout target-dir ref))
      
      (bfepm-utils-message "Successfully cloned to %s" target-dir))))

(defun bfepm-utils-git-checkout (repo-dir ref)
  "Checkout REF in git repository at REPO-DIR."
  (let ((default-directory repo-dir))
    (bfepm-utils-message "Checking out %s in %s" ref repo-dir)
    (let ((result (call-process "git" nil nil nil "checkout" ref)))
      (unless (= result 0)
        (bfepm-utils-error "Git checkout of %s failed with exit code %d" ref result)))))

(defun bfepm-utils-git-get-latest-tag (repo-dir)
  "Get the latest tag from git repository at REPO-DIR.
If no tags are found locally (e.g., due to shallow clone), fetch tags first."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process "git" nil t nil "describe" "--tags" "--abbrev=0")))
        (if (= result 0)
            (string-trim (buffer-string))
          ;; If tag discovery fails, try fetching tags and retry
          (progn
            (bfepm-utils-message "No tags found locally, fetching tags from remote...")
            (let ((fetch-result (call-process "git" nil nil nil "fetch" "--tags")))
              (if (= fetch-result 0)
                  (progn
                    (erase-buffer)
                    (let ((retry-result (call-process "git" nil t nil "describe" "--tags" "--abbrev=0")))
                      (if (= retry-result 0)
                          (string-trim (buffer-string))
                        nil)))
                (progn
                  (bfepm-utils-message "Failed to fetch tags from remote")
                  nil)))))))))

(defun bfepm-utils-git-get-commit-hash (repo-dir &optional ref)
  "Get commit hash for REF in git repository at REPO-DIR.
If REF is nil, gets current HEAD commit hash."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process "git" nil t nil "rev-parse" (or ref "HEAD"))))
        (if (= result 0)
            (string-trim (buffer-string))
          nil)))))

(defun bfepm-utils-git-list-tags (repo-dir)
  "List all tags in git repository at REPO-DIR.
If no tags are found locally (e.g., due to shallow clone), fetch tags first."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process "git" nil t nil "tag" "-l")))
        (if (and (= result 0) (not (string-empty-p (string-trim (buffer-string)))))
            (split-string (string-trim (buffer-string)) "\n" t)
          ;; If no tags found, try fetching tags and retry
          (progn
            (bfepm-utils-message "No tags found locally, fetching tags from remote...")
            (let ((fetch-result (call-process "git" nil nil nil "fetch" "--tags")))
              (if (= fetch-result 0)
                  (progn
                    (erase-buffer)
                    (let ((retry-result (call-process "git" nil t nil "tag" "-l")))
                      (if (= retry-result 0)
                          (let ((tag-output (string-trim (buffer-string))))
                            (if (not (string-empty-p tag-output))
                                (split-string tag-output "\n" t)
                              nil))
                        nil)))
                (progn
                  (bfepm-utils-message "Failed to fetch tags from remote")
                  nil)))))))))

(provide 'bfepm-utils)

;;; bfepm-utils.el ends here