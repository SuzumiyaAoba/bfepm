;;; bfepm-package.el --- BFEPM Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Package installation, removal, and management functionality for BFEPM.

;;; Code:

(require 'url)
(require 'tar-mode)
(require 'bfepm-core)
(require 'bfepm-utils)
(require 'bfepm-git)
(require 'bfepm-version)
(require 'bfepm-network)

;; Forward declaration for config functions
(declare-function bfepm-config-get-package "bfepm-config")

;; Try to load bfepm-config, fall back to minimal if not available
(condition-case nil
    (require 'bfepm-config)
  (error
   (condition-case nil
       (require 'bfepm-config-minimal)
     (error
      (message "Warning: Neither bfepm-config nor bfepm-config-minimal available")))))

(defvar bfepm-package--melpa-archive-url "https://melpa.org/packages/archive-contents"
  "URL for MELPA archive contents.")

(defvar bfepm-package--gnu-archive-url "https://elpa.gnu.org/packages/archive-contents"
  "URL for GNU ELPA archive contents.")

(defvar bfepm-package--archive-cache nil
  "Cache for package archive contents.")

(defvar bfepm-package--last-archive-fetch-time nil
  "Time of last archive fetch to implement rate limiting.")

(defvar bfepm-package--archive-fetch-delay 1.0
  "Minimum delay in seconds between archive fetches.")

(defun bfepm-package--get-default-sources ()
  "Get default package sources when config is not available."
  '(("melpa" . (:url "https://melpa.org/packages/" :type "elpa" :priority 10))
    ("gnu" . (:url "https://elpa.gnu.org/packages/" :type "elpa" :priority 5))))

(defun bfepm-package--get-source-priority (source)
  "Get priority from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-priority source))
   ((and (listp source) (keywordp (car source))) (plist-get source :priority))
   ((listp source) (or (cdr (assoc 'priority source)) 10))
   (t 10)))

(defun bfepm-package--get-source-type (source)
  "Get type from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-type source))
   ((plistp source) (plist-get source :type))
   (t "elpa")))

(defun bfepm-package--get-source-url (source)
  "Get URL from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-url source))
   ((plistp source) (plist-get source :url))
   (t nil)))

(defun bfepm-package--format-version (version)
  "Format VERSION from various formats to string.

This function is deprecated. Use `bfepm-version-normalize' instead."
  (bfepm-version-normalize version))

(defun bfepm-package--version-matches-p (available-version requested-version)
  "Check if AVAILABLE-VERSION satisfies REQUESTED-VERSION.

This function is deprecated. Use `bfepm-version-satisfies-p' instead."
  (bfepm-version-satisfies-p available-version requested-version))

(defun bfepm-package-install (package-spec &optional callback)
  "Install package specified by PACKAGE-SPEC.
PACKAGE-SPEC can be a string (package name), a list (name version),
or a bfepm-package structure.
CALLBACK is an optional function called with (success name error-message)."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (config (bfepm-core-get-config)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p (bfepm-package-name package))
        (progn
          (bfepm-utils-message "Package %s already installed" (bfepm-package-name package))
          (when callback (funcall callback t (bfepm-package-name package) nil)))
      (condition-case err
          (progn
            (bfepm-utils-message "Installing package: %s" (bfepm-package-name package))

            ;; Find package in sources
            (let ((package-info (bfepm-package--find-package package config)))
              (unless package-info
                (bfepm-utils-error "Package not found: %s" (bfepm-package-name package)))

              ;; Validate version if specified
              (when (and (not (string= (bfepm-package-version package) "latest"))
                         package-info)
                (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                       (available-version (bfepm-package--format-version (car info-list)))
                       (requested-version (bfepm-package-version package)))
                  (unless (bfepm-package--version-matches-p available-version requested-version)
                    (bfepm-utils-message "Warning: Requested version %s not available. Latest is %s"
                                      requested-version available-version))))

              ;; Download and install
              (bfepm-package--download-and-install package package-info))
            (when callback (funcall callback t (bfepm-package-name package) nil)))
        (error
         (when callback (funcall callback nil (bfepm-package-name package)
                                 (error-message-string err)))
         (unless callback (signal (car err) (cdr err))))))))

(defun bfepm-package-install-async (package-spec callback)
  "Install package specified by PACKAGE-SPEC asynchronously.
CALLBACK is called with (success package-name error-message) when complete."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (package-name (bfepm-package-name package)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p package-name)
        (progn
          (bfepm-utils-message "Package %s already installed" package-name)
          (when callback (funcall callback t package-name nil)))
      ;; Use async archive fetching for truly non-blocking operation
      (bfepm-utils-message "🔄 Starting truly async installation of %s" package-name)
      ;; First check if this is a git package from config
      (let* ((config (bfepm-core-get-config))
             (config-package (when config (bfepm-config-get-package config package-name))))
        (if (and config-package (bfepm-package-source config-package))
            ;; This is a git package from config - install it directly async
            (progn
              (bfepm-utils-message "Installing git package %s from configuration" package-name)
              (run-with-timer 0.01 nil
                              (lambda ()
                                (condition-case err
                                    (progn
                                      (bfepm-package-install package-spec)
                                      (when callback (funcall callback t package-name nil)))
                                  (error 
                                   (when callback (funcall callback nil package-name (error-message-string err))))))))
          ;; Not a git package - use ELPA sources
          (let* ((sources (if config
                             (bfepm-config-sources config)
                           (bfepm-package--get-default-sources)))
                 ;; Use the first ELPA source (highest priority)
                 (elpa-source (cl-find-if (lambda (source)
                                           (string= (bfepm-package--get-source-type (cdr source)) "elpa"))
                                         sources)))
            (if elpa-source
            (let ((archive-url (bfepm-package--get-source-url (cdr elpa-source))))
              (bfepm-package--fetch-archive-contents-async
               archive-url
               (lambda (success contents error-msg)
                 (if success
                     (let ((package-info (alist-get (intern package-name) contents)))
                       (if package-info
                           ;; Get dependency info first
                           (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                                  (deps (cadr info-list)))
                             (if deps
                                 ;; Install dependencies asynchronously first
                                 (bfepm-package--install-dependencies-async
                                  deps
                                  (lambda (dep-success dep-error-msg)
                                    (if dep-success
                                        ;; Download and install main package (NON-BLOCKING)
                                        (bfepm-package--download-and-install-async package package-info
                                          (lambda (success error-msg)
                                            (if success
                                                (funcall callback t package-name nil)
                                              (funcall callback nil package-name error-msg))))
                                      (funcall callback nil package-name
                                             (format "Failed to install dependencies: %s" dep-error-msg)))))
                               ;; No dependencies, install directly (NON-BLOCKING)
                               (bfepm-package--download-and-install-async package package-info
                                 (lambda (success error-msg)
                                   (if success
                                       (funcall callback t package-name nil)
                                     (funcall callback nil package-name error-msg))))))
                         (funcall callback nil package-name (format "Package not found: %s" package-name))))
                   (funcall callback nil package-name error-msg)))))
              ;; Fallback to sync method
              (run-with-timer 0.01 nil
                              (lambda ()
                                (condition-case err
                                    (progn
                                      (bfepm-package-install package-spec)
                                      (when callback (funcall callback t package-name nil)))
                                  (error 
                                   (when callback (funcall callback nil package-name (error-message-string err))))))))))))))

(defun bfepm-package--find-package (package config)
  "Find PACKAGE in available sources from CONFIG."
  (let* ((package-name (bfepm-package-name package))
         (sources (if config
                     (bfepm-config-sources config)
                   (bfepm-package--get-default-sources))))

    ;; First check if package has its own source definition in config
    (or (when config
          (let ((configured-package (cl-find-if (lambda (pkg)
                                                  (string= (bfepm-package-name pkg) package-name))
                                                (bfepm-config-packages config))))
            (when (and configured-package (bfepm-package-source configured-package))
              ;; Package has its own source definition
              (let ((package-source (bfepm-package-source configured-package)))
                (when package-source
                  (bfepm-package--find-in-source package-name package-source))))))
        
        ;; Try each source in priority order
        (cl-loop for source in (sort sources (lambda (a b)
                                               (> (bfepm-package--get-source-priority (cdr a))
                                                  (bfepm-package--get-source-priority (cdr b)))))
                 for package-info = (bfepm-package--find-in-source package-name (cdr source))
                 when package-info return package-info))))

(defun bfepm-package--find-in-source (package-name source)
  "Find PACKAGE-NAME in SOURCE."
  (let ((source-type (bfepm-package--get-source-type source)))
    (cond
     ((string= source-type "elpa")
      (bfepm-package--find-in-elpa package-name source))
     ((string= source-type "git")
      (bfepm-package--find-in-git package-name source))
     (t nil))))

(defun bfepm-package--find-in-elpa (package-name source)
  "Find PACKAGE-NAME in ELPA SOURCE."
  (let* ((archive-url (bfepm-package--get-source-url source))
         (archive-contents (bfepm-package--get-archive-contents archive-url)))
    (alist-get (intern package-name) archive-contents)))

(defun bfepm-package--get-archive-contents (archive-url)
  "Get archive contents from ARCHIVE-URL, using cache if available."
  (let ((cache-key archive-url))
    (or (alist-get cache-key bfepm-package--archive-cache nil nil #'string=)
        (let ((contents (bfepm-package--fetch-archive-contents archive-url)))
          (push (cons cache-key contents) bfepm-package--archive-cache)
          contents))))

(defun bfepm-package--find-in-git (_package-name source)
  "Find PACKAGE-NAME in git SOURCE."
  (let ((url (bfepm-package--get-source-url source))
        (ref (plist-get source :ref)))
    (when url
      ;; For git sources, we return a synthetic package info
      ;; The version will be determined when actually cloning
      ;; Include source information in the package info
      (list (or ref "latest") nil (format "Git package from %s" url) 'git source))))

(defun bfepm-package--fetch-archive-contents (archive-url)
  "Fetch archive contents from ARCHIVE-URL with error handling."
  (let ((archive-file (concat archive-url "archive-contents")))
    (condition-case err
        (with-temp-buffer
          (bfepm-utils-message "🔍 Fetching archive contents from %s (BLOCKING - will be fixed)" archive-file)
          (url-insert-file-contents archive-file)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (bfepm-utils-error "Failed to fetch archive contents from %s: %s"
                         archive-file (error-message-string err))))))

(defun bfepm-package--fetch-archive-contents-async (archive-url callback)
  "Fetch archive contents from ARCHIVE-URL asynchronously.
CALLBACK is called with (success contents error-message) when complete.

This function is deprecated. Use `bfepm-network-fetch-archive-contents-async'."
  (bfepm-network-fetch-archive-contents-async 
   archive-url callback bfepm-package--archive-fetch-delay))

(defun bfepm-package--download-and-install (package package-info)
  "Download and install PACKAGE using PACKAGE-INFO."
  (let* (;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (kind (cadddr info-list)))
    ;; Dispatch to appropriate installation method based on package type
    (cond
     ((eq kind 'git)
      (bfepm-package--download-and-install-git package package-info))
     (t
      (bfepm-package--download-and-install-elpa package package-info)))))

(defun bfepm-package--download-and-install-async (package package-info callback)
  "Download and install PACKAGE using PACKAGE-INFO asynchronously.
CALLBACK is called with (success error-message) when complete."
  (let* (;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (kind (cadddr info-list)))
    ;; Dispatch to appropriate installation method based on package type
    (cond
     ((eq kind 'git)
      ;; Git packages still use sync method for now
      (condition-case err
          (progn
            (bfepm-package--download-and-install-git package package-info)
            (funcall callback t nil))
        (error
         (funcall callback nil (error-message-string err)))))
     (t
      (bfepm-package--download-and-install-elpa-async package package-info callback)))))

(defun bfepm-package--download-and-install-elpa (package package-info)
  "Download and install ELPA PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (deps (cadr info-list))
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file with checksum verification
    (bfepm-utils-message "📥 Downloading %s (%s)..." package-name version-string)
    (bfepm-package--download-with-checksum archive-file local-file package-name version-string kind)

    ;; Install dependencies first
    (bfepm-package--install-dependencies deps)

    ;; Extract/install package with rollback on failure
    (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
      (condition-case err
          (progn
            (bfepm-package--extract-and-install package-name local-file kind)
            ;; Save version information
            (bfepm-package--save-version-info package-name version-string)
            ;; Verify installation
            (bfepm-package--verify-installation package-name install-dir)
            ;; Invalidate caches after successful installation
            (bfepm-core--invalidate-cache package-name))
        (error
         ;; Rollback on failure
         (when (file-directory-p install-dir)
           (bfepm-utils-message "Rolling back failed installation of %s" package-name)
           (ignore-errors (delete-directory install-dir t)))
         (bfepm-utils-error "Failed to install %s: %s" package-name (error-message-string err)))))

    (bfepm-utils-message "✅ Successfully installed %s" package-name)))

(defun bfepm-package--download-and-install-elpa-async (package package-info callback)
  "Download and install ELPA PACKAGE using PACKAGE-INFO asynchronously.
CALLBACK is called with (success error-message) when complete."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (_deps (cadr info-list)) ; Dependencies already handled by caller
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file asynchronously with checksum verification
    (bfepm-utils-message "📥 Downloading %s (%s) (NON-BLOCKING)..." package-name version-string)
    (bfepm-network-download-file-async
     archive-file local-file
     (lambda (success error-msg)
       (if success
           ;; Verify file and continue with installation
           (condition-case err
               (progn
                 ;; Verify download
                 (unless (and (file-exists-p local-file)
                             (> (file-attribute-size (file-attributes local-file)) 0))
                   (error "Downloaded file %s is empty or missing" local-file))
                 
                 ;; Extract/install package with rollback on failure
                 (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
                   (condition-case extract-err
                       (progn
                         (bfepm-package--extract-and-install package-name local-file kind)
                         ;; Save version information
                         (bfepm-package--save-version-info package-name version-string)
                         ;; Verify installation
                         (bfepm-package--verify-installation package-name install-dir)
                         ;; Invalidate caches after successful installation
                         (bfepm-core--invalidate-cache package-name)
                         (bfepm-utils-message "✅ Successfully installed %s" package-name)
                         (funcall callback t nil))
                     (error
                      ;; Rollback on failure
                      (when (file-directory-p install-dir)
                        (bfepm-utils-message "Rolling back failed installation of %s" package-name)
                        (ignore-errors (delete-directory install-dir t)))
                      (funcall callback nil (format "Failed to install %s: %s" 
                                                   package-name (error-message-string extract-err)))))))
             (error
              (funcall callback nil (error-message-string err))))
         ;; Download failed
         (funcall callback nil (format "Failed to download %s: %s" package-name error-msg))))
     3))) ; 3 retries

(defun bfepm-package--download-and-install-elpa-async-simple (package package-info callback)
  "Download and install ELPA PACKAGE using PACKAGE-INFO asynchronously.
This dependency version does not install dependencies to avoid circular deps.
CALLBACK is called with (success package-name error-message) when complete."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (_deps (cadr info-list)) ; Skip dependencies for dependency installation
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file asynchronously with checksum verification
    (bfepm-utils-message "📥 Downloading dependency %s (%s) (NON-BLOCKING)..." package-name version-string)
    (bfepm-network-download-file-async
     archive-file local-file
     (lambda (success error-msg)
       (if success
           ;; Verify file and continue with installation
           (condition-case err
               (progn
                 ;; Verify download
                 (unless (and (file-exists-p local-file)
                             (> (file-attribute-size (file-attributes local-file)) 0))
                   (error "Downloaded file %s is empty or missing" local-file))
                 
                 ;; Extract/install package with rollback on failure
                 (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
                   (condition-case extract-err
                       (progn
                         (bfepm-package--extract-and-install package-name local-file kind)
                         ;; Save version information
                         (bfepm-package--save-version-info package-name version-string)
                         ;; Verify installation
                         (bfepm-package--verify-installation package-name install-dir)
                         ;; Invalidate caches after successful installation
                         (bfepm-core--invalidate-cache package-name)
                         (bfepm-utils-message "✅ Successfully installed dependency %s" package-name)
                         (funcall callback t package-name nil))
                     (error
                      ;; Rollback on failure
                      (when (file-directory-p install-dir)
                        (bfepm-utils-message "Rolling back failed dependency installation of %s" package-name)
                        (ignore-errors (delete-directory install-dir t)))
                      (funcall callback nil package-name (format "Failed to install dependency %s: %s" 
                                                   package-name (error-message-string extract-err)))))))
             (error
              (funcall callback nil package-name (error-message-string err))))
         ;; Download failed
         (funcall callback nil package-name (format "Failed to download dependency %s: %s" package-name error-msg))))
     3))) ; 3 retries

(defun bfepm-package--download-and-install-git (package package-info)
  "Download and install git PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (source-config (nth 4 info-list)) ; Source config is included in package-info
         (url (bfepm-package--get-source-url source-config))
         (ref (or (plist-get source-config :ref) 
                  (bfepm-package-version package)
                  "latest"))
         (shallow (plist-get source-config :shallow))
         (install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))

    (unless url
      (bfepm-utils-error "No git URL found for package %s" package-name))

    (bfepm-utils-message "Installing git package %s from %s" package-name url)

    ;; Remove existing installation if it exists
    (when (file-directory-p install-dir)
      (delete-directory install-dir t))

    ;; Clone repository with rollback on failure
    (condition-case err
        (progn
          ;; Clone the repository
          (bfepm-git-clone url install-dir ref shallow)
          
          ;; Get actual version from git repository
          (let ((version (bfepm-package--get-git-version install-dir ref)))
            ;; Save version information
            (bfepm-package--save-version-info package-name version)
            
            ;; Install dependencies by scanning Package-Requires
            (bfepm-package--install-git-dependencies package-name install-dir)
            
            ;; Verify installation
            (bfepm-package--verify-installation package-name install-dir)
            
            ;; Add to load-path
            (add-to-list 'load-path install-dir)
            
            ;; Invalidate caches after successful installation
            (bfepm-core--invalidate-cache package-name)
            
            (bfepm-utils-message "Successfully installed git package %s (version: %s)" package-name version)))
      (error
       ;; Rollback on failure
       (when (file-directory-p install-dir)
         (bfepm-utils-message "Rolling back failed git installation of %s" package-name)
         (ignore-errors (delete-directory install-dir t)))
       (bfepm-utils-error "Failed to install git package %s: %s" package-name (error-message-string err))))))

(defun bfepm-package--get-git-version (repo-dir ref)
  "Get version for git package at REPO-DIR with REF."
  (bfepm-git-get-latest-version repo-dir ref))

(defun bfepm-package--install-git-dependencies (package-name install-dir)
  "Scan PACKAGE-NAME in INSTALL-DIR for Package-Requires and install deps."
  (let ((main-file (expand-file-name (format "%s.el" package-name) install-dir)))
    (when (file-exists-p main-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB to find headers
            (goto-char (point-min))
            ;; Look for Package-Requires header
            (when (re-search-forward "^[[:space:];]*Package-Requires: *\\(.*\\)$" nil t)
              (let ((requires-string (match-string 1)))
                (condition-case parse-err
                    (let ((deps (read requires-string)))
                      (when (and deps (listp deps))
                        (bfepm-utils-message "Installing dependencies for git package %s: %s" 
                                           package-name 
                                           (mapconcat (lambda (dep) (symbol-name (car dep))) deps ", "))
                        (bfepm-package--install-dependencies deps)))
                  (error
                   (bfepm-utils-message "Warning: Failed to parse Package-Requires header for %s: %s"
                                      package-name (error-message-string parse-err)))))))
        (error
         (bfepm-utils-message "Warning: Failed to read Package-Requires for git package %s: %s"
                            package-name (error-message-string err)))))))

(defun bfepm-package--build-archive-url (package-name version kind)
  "Build archive URL for PACKAGE-NAME with improved format detection.
VERSION is the package version, KIND is the package type."
  (let ((extension (cond
                    ((eq kind 'tar) "tar")
                    ((eq kind 'single) "el")
                    (t "el"))))
    (format "https://melpa.org/packages/%s-%s.%s"
            package-name version extension)))

(defun bfepm-package--install-dependencies (deps)
  "Install package dependencies DEPS."
  (when deps
    (dolist (dep deps)
      (let ((dep-name (symbol-name (car dep))))
        ;; Skip built-in packages like 'emacs'
        (unless (or (string= dep-name "emacs")
                    (bfepm-core-package-installed-p dep-name))
          (bfepm-utils-message "📥 Installing dependency: %s (BLOCKING - will be fixed)" dep-name)
          (condition-case err
              (bfepm-package-install dep-name)
            (error
             (bfepm-utils-message "Warning: Failed to install dependency %s: %s"
                               dep-name (error-message-string err)))))))))

(defun bfepm-package--install-dependencies-async (deps callback)
  "Install package dependencies DEPS asynchronously.
CALLBACK is called with (success error-message) when all are done."
  (if (null deps)
      (funcall callback t nil)
    (let ((remaining-deps (cl-remove-if (lambda (dep)
                                         (let ((dep-name (symbol-name (car dep))))
                                           (or (string= dep-name "emacs")
                                               (bfepm-core-package-installed-p dep-name))))
                                       deps)))
      (if (null remaining-deps)
          (funcall callback t nil)
        (bfepm-package--install-dependencies-async-recursive remaining-deps 0 callback)))))

(defun bfepm-package--install-dependencies-async-recursive (deps index callback)
  "Recursively install dependencies DEPS starting from INDEX.
CALLBACK is called with (success error-message) when all are done."
  (if (>= index (length deps))
      (funcall callback t nil)
    (let* ((dep (nth index deps))
           (dep-name (symbol-name (car dep))))
      (bfepm-utils-message "📥 Installing dependency: %s (NON-BLOCKING)" dep-name)
      (bfepm-package--install-single-dependency-async
       dep-name
       (lambda (success package-name error-msg)
         (if success
             ;; Add delay between dependency installations to avoid rate limiting
             (run-with-timer 0.5 nil
                           (lambda ()
                             (bfepm-package--install-dependencies-async-recursive deps (1+ index) callback)))
           (bfepm-utils-message "Warning: Failed to install dependency %s: %s" package-name error-msg)
           ;; Continue with next dependency even if one fails (with delay)
           (run-with-timer 0.5 nil
                          (lambda ()
                            (bfepm-package--install-dependencies-async-recursive deps (1+ index) callback)))))))))

(defun bfepm-package--install-single-dependency-async (package-name callback)
  "Install a single dependency PACKAGE-NAME asynchronously.
CALLBACK is called with (success package-name error-message) when complete.
This function ensures no fallback to synchronous operations."
  (let* ((package (make-bfepm-package :name package-name :version "latest"))
         (config (bfepm-core-get-config)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p package-name)
        (progn
          (bfepm-utils-message "Dependency %s already installed" package-name)
          (funcall callback t package-name nil))
      ;; Only use async method, no fallback to sync
      (let* ((sources (if config
                         (bfepm-config-sources config)
                       (bfepm-package--get-default-sources)))
             ;; Use the first ELPA source (highest priority)
             (elpa-source (cl-find-if (lambda (source)
                                       (string= (bfepm-package--get-source-type (cdr source)) "elpa"))
                                     sources)))
        (if elpa-source
            (let ((archive-url (bfepm-package--get-source-url (cdr elpa-source))))
              (bfepm-package--fetch-archive-contents-async
               archive-url
               (lambda (success contents error-msg)
                 (if success
                     (let ((package-info (alist-get (intern package-name) contents)))
                       (if package-info
                           ;; Found dependency, install without its own dependencies (to avoid circular deps)
                           (bfepm-package--download-and-install-elpa-async-simple package package-info callback)
                         (funcall callback nil package-name (format "Dependency not found: %s" package-name))))
                   (funcall callback nil package-name error-msg)))))
          ;; No ELPA source available
          (funcall callback nil package-name "No ELPA source available for dependency"))))))

(defun bfepm-package--extract-and-install (package-name archive-file kind)
  "Extract and install PACKAGE-NAME from ARCHIVE-FILE.
KIND specifies the package type (tar or single file)."
  (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
    (bfepm-utils-ensure-directory install-dir)

    (cond
     ((eq kind 'tar)
      (bfepm-package--extract-tar-package archive-file install-dir))
     (t
      (bfepm-package--install-single-file archive-file install-dir)))

    ;; Add to load-path
    (add-to-list 'load-path install-dir)))

(defun bfepm-package--extract-tar-package (tar-file install-dir)
  "Extract TAR-FILE to INSTALL-DIR with improved compression detection."
  (let ((default-directory install-dir))
    (bfepm-utils-message "📦 Extracting package to %s..." install-dir)
    
    ;; Detect compression format and use appropriate tar flags
    (let* ((tar-flags (cond
                       ((or (string-suffix-p ".tar.gz" tar-file)
                            (string-suffix-p ".tgz" tar-file)) "-xzf")
                       ((string-suffix-p ".tar.bz2" tar-file) "-xjf")
                       ((string-suffix-p ".tar.xz" tar-file) "-xJf")
                       ((string-suffix-p ".tar.lz" tar-file) "--lzip -xf")
                       (t "-xf")))
           (result (call-process "tar" nil nil nil tar-flags tar-file "--strip-components=1")))
      
      (unless (= result 0)
        (bfepm-utils-error "Failed to extract tar file %s (exit code: %d)" tar-file result))
      
      ;; Verify extraction succeeded
      (let ((extracted-files (directory-files install-dir nil "^[^.]")))
        (unless (> (length extracted-files) 0)
          (bfepm-utils-error "Tar extraction failed: no files found in %s" install-dir))
        (bfepm-utils-message "✅ Extracted %d files to %s" (length extracted-files) install-dir)))))

(defun bfepm-package--install-single-file (el-file install-dir)
  "Install single EL-FILE to INSTALL-DIR with verification."
  (let ((target-file (expand-file-name (file-name-nondirectory el-file) install-dir)))
    (bfepm-utils-message "Installing single file to %s" target-file)
    (copy-file el-file target-file t)
    ;; Verify file was copied
    (unless (file-exists-p target-file)
      (bfepm-utils-error "Failed to copy file %s to %s" el-file target-file))))

(defun bfepm-package--save-version-info (package-name version)
  "Save VERSION information for PACKAGE-NAME."
  (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
         (version-file (expand-file-name ".bfepm-version" package-dir)))
    (when (file-directory-p package-dir)
      (with-temp-file version-file
        (insert (format "%s\n" version))))))

(defun bfepm-package--parse-metadata (package-name install-dir)
  "Parse basic metadata for PACKAGE-NAME in INSTALL-DIR."
  (let ((main-file (expand-file-name (format "%s.el" package-name) install-dir))
        (metadata-file (expand-file-name ".bfepm-metadata" install-dir))
        (metadata (list)))
    
    ;; Parse main package file if it exists
    (when (file-exists-p main-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB for headers
            (goto-char (point-min))
            
            ;; Extract Package-Requires (parse as Lisp data structure)
            (when (re-search-forward "^[[:space:];]*Package-Requires:[[:space:]]*\\(.+\\)$" nil t)
              (condition-case parse-err
                  (let* ((requires-str (match-string 1))
                         (requires (ignore-errors (read requires-str))))
                    (setq metadata (cons (cons 'requires requires) metadata)))
                (error
                 (bfepm-utils-message "Warning: Failed to parse Package-Requires for %s: %s"
                                    package-name (error-message-string parse-err)))))
            
            ;; Extract Version
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Version:[[:space:]]*\\(.+\\)$" nil t)
              (setq metadata (cons (cons 'version (string-trim (match-string 1))) metadata)))
            
            ;; Extract Keywords
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Keywords:[[:space:]]*\\(.+\\)$" nil t)
              (setq metadata (cons (cons 'keywords (string-trim (match-string 1))) metadata))))
        (error
         (bfepm-utils-message "Warning: Failed to parse metadata for %s: %s"
                            package-name (error-message-string err)))))
    
    ;; Cache metadata to file for quick access (only if non-empty)
    (when metadata
      (condition-case err
          (with-temp-file metadata-file
            (prin1 metadata (current-buffer)))
        (error
         (bfepm-utils-message "Warning: Failed to cache metadata for %s: %s"
                            package-name (error-message-string err)))))
    
    metadata))

(defun bfepm-package-remove (package-name)
  "Remove installed package PACKAGE-NAME."
  (let ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
    (if (file-directory-p package-dir)
        (progn
          (delete-directory package-dir t)
          ;; Invalidate caches after removal
          (bfepm-core--invalidate-cache package-name)
          (bfepm-utils-message "Removed package: %s" package-name))
      (bfepm-utils-message "Package not installed: %s" package-name))))

(defun bfepm-package-update (package-name)
  "Update installed package PACKAGE-NAME."
  (when (bfepm-core-package-installed-p package-name)
    (bfepm-package-remove package-name)
    (bfepm-package-install package-name)))

;; Parallel dependency installation for improved performance

(defun bfepm-package--install-dependency-async-simple (package callback)
  "Install single dependency PACKAGE asynchronously.
CALLBACK is called with (success package-name error-message)."
  (let ((package-name (bfepm-package-name package)))
    (bfepm-package-install-async 
     package-name
     (lambda (success pkg-name error-msg)
       (funcall callback success pkg-name error-msg)))))

(defun bfepm-package--install-dependencies-parallel-async (dependencies callback)
  "Install DEPENDENCIES in parallel for better performance.
CALLBACK is called with (success failed-packages) when all are complete."
  (if (null dependencies)
      (funcall callback t nil)
    (let* ((total-deps (length dependencies))
           (completed-deps 0)
           (failed-deps '())
           (success-deps '())
           (completion-callback
            (lambda (success dep-name error-msg)
              (setq completed-deps (1+ completed-deps))
              (if success
                  (push dep-name success-deps)
                (push (cons dep-name error-msg) failed-deps))
              
              ;; Check if all dependencies are complete
              (when (= completed-deps total-deps)
                (if (null failed-deps)
                    (progn
                      (bfepm-utils-message "✅ Successfully installed %d dependencies in parallel" 
                                         (length success-deps))
                      (funcall callback t nil))
                  (progn
                    (bfepm-utils-message "⚠️ Parallel installation completed with %d failures: %s"
                                       (length failed-deps)
                                       (mapconcat (lambda (pair) (format "%s (%s)" (car pair) (cdr pair)))
                                                failed-deps ", "))
                    (funcall callback nil failed-deps)))))))
      
      (bfepm-utils-message "🚀 Starting parallel installation of %d dependencies..." total-deps)
      
      ;; Start all dependency installations in parallel
      (dolist (dep dependencies)
        (let ((dep-name (bfepm-package-name dep)))
          (if (bfepm-core-package-installed-p dep-name)
              ;; Already installed - mark as success immediately
              (funcall completion-callback t dep-name nil)
            ;; Install dependency asynchronously
            (bfepm-package--install-dependency-async-simple dep completion-callback)))))))

(defun bfepm-package--install-dependencies-async-enhanced (package-name dependencies callback)
  "Install DEPENDENCIES for PACKAGE-NAME with enhanced parallel processing.
CALLBACK is called with (success error-message) when complete."
  (if (null dependencies)
      (funcall callback t nil)
    (let ((filtered-deps (cl-remove-if 
                         (lambda (dep) 
                           (let ((name (bfepm-package-name dep)))
                             (or (bfepm-core-package-installed-p name)
                                 (string= name package-name)))) ; Skip self
                         dependencies)))
      
      (if (null filtered-deps)
          (progn
            (bfepm-utils-message "All dependencies for %s already installed" package-name)
            (funcall callback t nil))
        
        (bfepm-utils-message "Installing %d dependencies for %s..." 
                           (length filtered-deps) package-name)
        
        ;; Use parallel installation for better performance
        (bfepm-package--install-dependencies-parallel-async
         filtered-deps
         (lambda (success failed-deps)
           (if success
               (progn
                 (bfepm-utils-message "✅ All dependencies for %s installed successfully" package-name)
                 (funcall callback t nil))
             (let ((error-msg (format "Failed to install dependencies: %s"
                                    (mapconcat (lambda (pair) (car pair)) failed-deps ", "))))
               (bfepm-utils-message "❌ Dependency installation failed for %s: %s" package-name error-msg)
               (funcall callback nil error-msg)))))))))

(defun bfepm-package-update-all ()
  "Update all installed packages with optimized batch processing."
  (let ((installed-packages (bfepm-core-get-installed-packages)))
    (when installed-packages
      (bfepm-utils-message "Updating %d packages..." (length installed-packages))
      ;; Clear all caches once at the beginning for efficiency
      (bfepm-core--clear-all-caches)
      (dolist (package-name installed-packages)
        (bfepm-utils-message "Updating %s..." package-name)
        (condition-case err
            (bfepm-package-update package-name)
          (error
           (bfepm-utils-message "Warning: Failed to update %s: %s"
                               package-name (error-message-string err)))))
      (bfepm-utils-message "Package update complete."))))

(defun bfepm-package-list ()
  "List installed packages with version information."
  (let ((packages (bfepm-core-get-installed-packages)))
    (if packages
        (progn
          (bfepm-utils-message "Installed packages:")
          (dolist (package packages)
            (let ((version (bfepm-core-get-package-version package)))
              (if (string= version "unknown")
                  (message "  - %s (version unknown)" package)
                (message "  - %s (%s)" package version)))))
      (bfepm-utils-message "No packages installed"))))

(defun bfepm-package-info (package-name)
  "Show information about PACKAGE-NAME."
  (let* ((config (bfepm-core-get-config))
         (package-info (bfepm-package--find-package
                        (make-bfepm-package :name package-name) config)))
    (if package-info
        (progn
          (bfepm-utils-message "Package: %s" package-name)
          (message "  Version: %s" (cadr package-info))
          (message "  Description: %s" (cadddr package-info))
          (when (caddr package-info)
            (message "  Dependencies: %s"
                     (mapconcat (lambda (dep) (symbol-name (car dep)))
                                (caddr package-info) ", "))))
      (bfepm-utils-message "Package not found: %s" package-name))))

(defun bfepm-package-search (query)
  "Search for packages matching QUERY."
  (bfepm-utils-message "Searching for packages matching: %s" query)
  ;; This would implement actual search functionality
  ;; For now, just a placeholder
  (bfepm-utils-message "Search functionality not yet implemented"))

(defun bfepm-package--verify-installation (package-name install-dir)
  "Verify that PACKAGE-NAME was installed correctly in INSTALL-DIR."
  (unless (file-directory-p install-dir)
    (bfepm-utils-error "Installation directory %s does not exist" install-dir))

  ;; Check for at least one .el file
  (let ((el-files (directory-files install-dir nil "\\.el$")))
    (unless el-files
      (bfepm-utils-error "No .el files found in %s" install-dir))

    ;; Check that main package file exists (package-name.el)
    (let ((main-file (format "%s.el" package-name)))
      (unless (member main-file el-files)
        ;; If main file doesn't exist, check if any .el file contains the package name
        (unless (cl-some (lambda (file) (string-match-p package-name file)) el-files)
          (bfepm-utils-message "Warning: Main package file %s not found, but other .el files exist" main-file))))

    ;; Parse and cache package metadata
    (bfepm-package--parse-metadata package-name install-dir)
    
    (bfepm-utils-message "Installation verified: %d .el files found in %s"
                        (length el-files) install-dir)))

(defun bfepm-package--get-package-checksum (_package-name _version _kind)
  "Get expected checksum for PACKAGE-NAME VERSION of KIND from MELPA."
  ;; This is a placeholder - MELPA doesn't currently provide checksums
  ;; In the future, this could fetch from a checksum database or MELPA API
  (bfepm-utils-message "Checksum verification not available for MELPA packages")
  nil)

(defun bfepm-package--download-with-checksum (url local-file package-name version kind)
  "Download file from URL with optional checksum verification.
LOCAL-FILE is the destination path for download.
PACKAGE-NAME, VERSION, and KIND are used for checksum verification."
  ;; Download the file
  (unless (bfepm-network-download-file url local-file 3)
    (bfepm-utils-error "Failed to download %s after retries" package-name))

  ;; Verify file was downloaded successfully
  (unless (and (file-exists-p local-file)
               (> (file-attribute-size (file-attributes local-file)) 0))
    (bfepm-utils-error "Downloaded file %s is empty or missing" local-file))

  ;; Optional checksum verification (currently not available for MELPA)
  (let ((expected-checksum (bfepm-package--get-package-checksum package-name version kind)))
    (when expected-checksum
      (bfepm-utils-message "Verifying checksum for %s" package-name)
      (unless (bfepm-utils-verify-checksum local-file expected-checksum)
        (bfepm-utils-error "Checksum verification failed for %s" package-name))))

  t)

;; Enhanced MELPA/ELPA source management and discovery

(defun bfepm-package--find-package-in-sources-parallel (package-name sources callback)
  "Search for PACKAGE-NAME across SOURCES in parallel for better performance.
CALLBACK is called with (source package-info) when found, or (nil nil) if not
found."
  (if (null sources)
      (funcall callback nil nil)
    (let* ((total-sources (length sources))
           (completed-sources 0)
           (found-result nil)
           (completion-callback
            (lambda (source package-info)
              (setq completed-sources (1+ completed-sources))
              (when (and package-info (not found-result))
                (setq found-result (cons source package-info)))
              
              ;; Check if all sources are complete or we found the package
              (when (or found-result (= completed-sources total-sources))
                (if found-result
                    (funcall callback (car found-result) (cdr found-result))
                  (funcall callback nil nil))))))
      
      (bfepm-utils-message "🔍 Searching for %s across %d sources in parallel..." 
                         package-name total-sources)
      
      ;; Search all sources in parallel
      (dolist (source sources)
        (let* ((source-name (car source))
               (source-config (cdr source))
               (archive-url (bfepm-package--get-source-url source-config)))
          (bfepm-package--fetch-archive-contents-async
           archive-url
           (lambda (success contents error-msg)
             (if success
                 (let ((package-info (alist-get (intern package-name) contents)))
                   (if package-info
                       (progn
                         (bfepm-utils-message "✅ Found %s in source %s" package-name source-name)
                         (funcall completion-callback source package-info))
                     (funcall completion-callback nil nil)))
               (progn
                 (bfepm-utils-message "⚠️ Failed to fetch from source %s: %s" source-name error-msg)
                 (funcall completion-callback nil nil))))))))))

(defun bfepm-package--batch-install-async (package-names callback)
  "Install multiple PACKAGE-NAMES efficiently with parallel processing.
CALLBACK is called with (success-list failed-list) when complete."
  (if (null package-names)
      (funcall callback nil nil)
    (let* ((total-packages (length package-names))
           (completed-packages 0)
           (success-packages '())
           (failed-packages '())
           (completion-callback
            (lambda (success package-name error-msg)
              (setq completed-packages (1+ completed-packages))
              (if success
                  (push package-name success-packages)
                (push (cons package-name error-msg) failed-packages))
              
              ;; Check if all packages are complete
              (when (= completed-packages total-packages)
                (bfepm-utils-message "📦 Batch installation complete: %d success, %d failed"
                                   (length success-packages) (length failed-packages))
                (funcall callback success-packages failed-packages)))))
      
      (bfepm-utils-message "🚀 Starting batch installation of %d packages..." total-packages)
      
      ;; Install all packages in parallel
      (dolist (package-name package-names)
        (bfepm-package-install-async package-name completion-callback)))))

(defun bfepm-package--check-source-health-async (sources callback)
  "Check health of SOURCES by testing archive-contents availability.
CALLBACK is called with (healthy-sources unhealthy-sources)."
  (if (null sources)
      (funcall callback nil nil)
    (let* ((total-sources (length sources))
           (completed-sources 0)
           (healthy-sources '())
           (unhealthy-sources '())
           (completion-callback
            (lambda (source-name healthy)
              (setq completed-sources (1+ completed-sources))
              (if healthy
                  (push source-name healthy-sources)
                (push source-name unhealthy-sources))
              
              ;; Check if all sources are complete
              (when (= completed-sources total-sources)
                (bfepm-utils-message "🏥 Source health check complete: %d healthy, %d unhealthy"
                                   (length healthy-sources) (length unhealthy-sources))
                (funcall callback healthy-sources unhealthy-sources)))))
      
      (bfepm-utils-message "🔍 Checking health of %d package sources..." total-sources)
      
      ;; Check all sources in parallel
      (dolist (source sources)
        (let* ((source-name (car source))
               (source-config (cdr source))
               (archive-url (bfepm-package--get-source-url source-config)))
          (bfepm-network-http-get-async
           archive-url
           (lambda (success _data error-msg)
             (if success
                 (progn
                   (bfepm-utils-message "✅ Source %s is healthy" source-name)
                   (funcall completion-callback source-name t))
               (progn
                 (bfepm-utils-message "❌ Source %s is unhealthy: %s" source-name error-msg)
                 (funcall completion-callback source-name nil))))))))))

;;;###autoload
(defun bfepm-package-health-check ()
  "Check health of all configured package sources."
  (interactive)
  (let* ((config (bfepm-core-get-config))
         (sources (if config 
                     (bfepm-config-sources config)
                   (bfepm-package--get-default-sources))))
    (bfepm-package--check-source-health-async
     sources
     (lambda (healthy unhealthy)
       (let ((buffer (get-buffer-create "*BFEPM Source Health*")))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert "=== BFEPM Source Health Report ===\n\n")
             (insert (format "Checked at: %s\n\n" (current-time-string)))
             
             (insert (format "✅ Healthy Sources (%d):\n" (length healthy)))
             (dolist (source healthy)
               (insert (format "  • %s\n" source)))
             
             (insert (format "\n❌ Unhealthy Sources (%d):\n" (length unhealthy)))
             (dolist (source unhealthy)
               (insert (format "  • %s\n" source)))
             
             (insert "\nPress 'q' to close this buffer.\n")
             (goto-char (point-min))
             (read-only-mode 1)
             (local-set-key (kbd "q") 'quit-window)))
         (pop-to-buffer buffer))))))

(provide 'bfepm-package)

;;; bfepm-package.el ends here

