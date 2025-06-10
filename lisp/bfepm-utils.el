;;; bfepm-utils.el --- BFEPM Utility functions -*- lexical-binding: t -*-

;;; Commentary:

;; Utility functions for BFEPM.

;;; Code:

(require 'url)
(require 'json)

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
Return 1 if V1 > V2, -1 if V1 < V2, 0 if equal."
  (if (and (bfepm-utils--is-melpa-date-version-p v1)
           (bfepm-utils--is-melpa-date-version-p v2))
      ;; Both are MELPA date versions
      (let ((num1 (string-to-number (replace-regexp-in-string "\\." "" v1)))
            (num2 (string-to-number (replace-regexp-in-string "\\." "" v2))))
        (cond ((> num1 num2) 1)
              ((< num1 num2) -1)
              (t 0)))
    ;; Standard semantic version handling
    (let ((parts1 (mapcar #'string-to-number (split-string v1 "\\.")))
          (parts2 (mapcar #'string-to-number (split-string v2 "\\."))))
      (bfepm-utils--version-compare-parts parts1 parts2))))

(defun bfepm-utils--version-compare-parts (parts1 parts2)
  "Compare version parts lists PARTS1 and PARTS2."
  (cond
   ((and (null parts1) (null parts2)) 0)
   ((null parts1) -1)
   ((null parts2) 1)
   ((> (car parts1) (car parts2)) 1)
   ((< (car parts1) (car parts2)) -1)
   (t (bfepm-utils--version-compare-parts (cdr parts1) (cdr parts2)))))

(defun bfepm-utils-version-satisfies-p (version requirement)
  "Check if VERSION satisfies REQUIREMENT."
  (cond
   ((string= requirement "latest") t)
   ((string-prefix-p "^" requirement)
    (bfepm-utils--version-satisfies-caret-p version (substring requirement 1)))
   ((string-prefix-p "~" requirement)
    (bfepm-utils--version-satisfies-tilde-p version (substring requirement 1)))
   (t (string= version requirement))))

(defun bfepm-utils--is-melpa-date-version-p (version)
  "Check if VERSION is a MELPA date version (YYYYMMDD.HHMM format)."
  (string-match-p "^[0-9]\\{8\\}\\.[0-9]\\{4\\}$" version))

(defun bfepm-utils--version-satisfies-caret-p (version requirement)
  "Check if VERSION satisfies caret REQUIREMENT."
  (if (bfepm-utils--is-melpa-date-version-p version)
      ;; MELPA date version handling (YYYYMMDD.HHMM)
      (if (bfepm-utils--is-melpa-date-version-p requirement)
          ;; Both are MELPA date versions - same year
          (let ((ver-date (car (split-string version "\\.")))
                (req-date (car (split-string requirement "\\."))))
            (and (>= (bfepm-utils-version-compare version requirement) 0)
                 (string= (substring ver-date 0 4) (substring req-date 0 4))))
        ;; Version is MELPA date but requirement is not - assume requirement is date prefix
        (let ((ver-date (car (split-string version "\\."))))
          (unless (string-match-p "^[0-9]\\{4,8\\}$" requirement)
            (error "Invalid date prefix requirement: %s" requirement))
          (and (>= (string-to-number ver-date) (string-to-number requirement))
               (string= (substring ver-date 0 4) (substring requirement 0 4)))))
    ;; Standard semantic version handling
    (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
          (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
      (and (>= (bfepm-utils-version-compare version requirement) 0)
           (< (car ver-parts) (1+ (car req-parts)))))))

(defun bfepm-utils--version-satisfies-tilde-p (version requirement)
  "Check if VERSION satisfies tilde REQUIREMENT."
  (if (bfepm-utils--is-melpa-date-version-p version)
      ;; MELPA date version handling - same day
      (if (bfepm-utils--is-melpa-date-version-p requirement)
          ;; Both are MELPA date versions - same day
          (let ((ver-date (car (split-string version "\\.")))
                (req-date (car (split-string requirement "\\."))))
            (and (>= (bfepm-utils-version-compare version requirement) 0)
                 (string= ver-date req-date)))
        ;; Version is MELPA date but requirement is not - check day prefix match
        (let ((ver-date (car (split-string version "\\."))))
          (unless (string-match-p "^[0-9]\\{4,8\\}$" requirement)
            (error "Invalid date prefix requirement: %s" requirement))
          (and (>= (string-to-number ver-date) (string-to-number requirement))
               (string-prefix-p requirement ver-date))))
    ;; Standard semantic version handling
    (let ((req-parts (mapcar #'string-to-number (split-string requirement "\\.")))
          (ver-parts (mapcar #'string-to-number (split-string version "\\."))))
      (and (>= (bfepm-utils-version-compare version requirement) 0)
           (= (car ver-parts) (car req-parts))
           (= (cadr ver-parts) (cadr req-parts))))))

(defun bfepm-utils-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-utils-download-file (url local-file &optional max-retries)
  "Download file from URL to LOCAL-FILE with retry logic.
MAX-RETRIES defaults to 3."
  (let ((retries (or max-retries 3))
        (attempt 0)
        (success nil))
    (while (and (< attempt retries) (not success))
      (setq attempt (1+ attempt))
      (bfepm-utils-message "Downloading from %s... (attempt %d/%d)" url attempt retries)
      (condition-case err
          (progn
            (url-copy-file url local-file t)
            (when (file-exists-p local-file)
              (bfepm-utils-message "Downloaded to %s" local-file)
              (setq success t)))
        (error
         (bfepm-utils-message "Download attempt %d failed: %s" attempt (error-message-string err))
         (when (= attempt retries)
           (bfepm-utils-error "Failed to download %s after %d attempts: %s"
                             url retries (error-message-string err))))))
    success))

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
  "Make HTTP GET request to URL and return response body."
  (with-temp-buffer
    (condition-case err
        (progn
          (url-insert-file-contents url)
          (buffer-string))
      (error
       (bfepm-utils-error "HTTP GET failed for %s: %s" url (error-message-string err))))))

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
  "Get the latest tag from git repository at REPO-DIR."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process "git" nil t nil "describe" "--tags" "--abbrev=0")))
        (if (= result 0)
            (string-trim (buffer-string))
          nil)))))

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
  "List all tags in git repository at REPO-DIR."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (let ((result (call-process "git" nil t nil "tag" "-l")))
        (if (= result 0)
            (split-string (string-trim (buffer-string)) "\n" t)
          nil)))))

(provide 'bfepm-utils)

;;; bfepm-utils.el ends here