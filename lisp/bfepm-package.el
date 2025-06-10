;;; bfepm-package.el --- BFEPM Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Package installation, removal, and management functionality for BFEPM.

;;; Code:

(require 'url)
(require 'tar-mode)
(require 'bfepm-core)
(require 'bfepm-utils)
;; bfepm-config is optional - loaded conditionally

(defvar bfepm-package--melpa-archive-url "https://melpa.org/packages/archive-contents"
  "URL for MELPA archive contents.")

(defvar bfepm-package--gnu-archive-url "https://elpa.gnu.org/packages/archive-contents"
  "URL for GNU ELPA archive contents.")

(defvar bfepm-package--archive-cache nil
  "Cache for package archive contents.")

(defun bfepm-package--get-default-sources ()
  "Get default package sources when config is not available."
  '(("melpa" . (:url "https://melpa.org/packages/" :type "elpa" :priority 10))
    ("gnu" . (:url "https://elpa.gnu.org/packages/" :type "elpa" :priority 5))))

(defun bfepm-package--get-source-priority (source)
  "Get priority from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-priority source))
   ((plistp source) (plist-get source :priority))
   (t 5)))

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
  "Format VERSION from various formats to string."
  (cond
   ((stringp version) version)
   ((numberp version) (number-to-string version))
   ((listp version)
    ;; Handle list format like (20250426 1319)
    (cond
     ((= (length version) 1) (number-to-string (car version)))
     ((= (length version) 2) (format "%s.%s" (car version) (cadr version)))
     (t (mapconcat #'number-to-string version "."))))
   (t "unknown")))

(defun bfepm-package--version-matches-p (available-version requested-version)
  "Check if AVAILABLE-VERSION satisfies REQUESTED-VERSION."
  (cond
   ((string= requested-version "latest") t)
   ((string= available-version requested-version) t)
   ((string-prefix-p "^" requested-version)
    ;; Compatible version (e.g., ^1.2.3)
    (bfepm-utils-version-satisfies-p available-version requested-version))
   ((string-prefix-p "~" requested-version)
    ;; Patch level (e.g., ~1.2.3)
    (bfepm-utils-version-satisfies-p available-version requested-version))
   (t nil)))

(defun bfepm-package-install (package-spec)
  "Install package specified by PACKAGE-SPEC.
PACKAGE-SPEC can be a string (package name), a list (name version),
or a bfepm-package structure."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (config (bfepm-core-get-config)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p (bfepm-package-name package))
        (bfepm-utils-message "Package %s already installed" (bfepm-package-name package))
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
          (bfepm-package--download-and-install package package-info))))))

(defun bfepm-package--find-package (package config)
  "Find PACKAGE in available sources from CONFIG."
  (let* ((package-name (bfepm-package-name package))
         (sources (if config
                     (bfepm-config-sources config)
                   (bfepm-package--get-default-sources))))

    ;; Try each source in priority order
    (cl-loop for source in (sort sources (lambda (a b)
                                           (> (bfepm-package--get-source-priority (cdr a))
                                              (bfepm-package--get-source-priority (cdr b)))))
             for package-info = (bfepm-package--find-in-source package-name (cdr source))
             when package-info return package-info)))

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

(defun bfepm-package--find-in-git (package-name source)
  "Find PACKAGE-NAME in git SOURCE."
  ;; Stub implementation - would implement git-based package discovery
  (bfepm-utils-error "Git source support not yet implemented for %s from %s"
                     package-name source))

(defun bfepm-package--fetch-archive-contents (archive-url)
  "Fetch archive contents from ARCHIVE-URL with error handling."
  (let ((archive-file (concat archive-url "archive-contents")))
    (condition-case err
        (with-temp-buffer
          (bfepm-utils-message "Fetching archive contents from %s" archive-file)
          (url-insert-file-contents archive-file)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (bfepm-utils-error "Failed to fetch archive contents from %s: %s"
                         archive-file (error-message-string err))))))

(defun bfepm-package--download-and-install (package package-info)
  "Download and install PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         ;; Handle both list and vector formats from ELPA
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
    (bfepm-utils-message "Downloading %s..." package-name)
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

    (bfepm-utils-message "Successfully installed %s" package-name)))

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
          (bfepm-utils-message "Installing dependency: %s" dep-name)
          (condition-case err
              (bfepm-package-install dep-name)
            (error
             (bfepm-utils-message "Warning: Failed to install dependency %s: %s"
                               dep-name (error-message-string err)))))))))

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
  "Extract TAR-FILE to INSTALL-DIR with error checking."
  (let ((default-directory install-dir))
    (bfepm-utils-message "Extracting tar package to %s" install-dir)
    (let ((result (call-process "tar" nil nil nil "-xf" tar-file "--strip-components=1")))
      (unless (= result 0)
        (bfepm-utils-error "Failed to extract tar file %s (exit code: %d)" tar-file result))
      ;; Verify extraction succeeded
      (unless (> (length (directory-files install-dir nil "^[^.]")) 0)
        (bfepm-utils-error "Tar extraction failed: no files found in %s" install-dir)))))

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
  (unless (bfepm-utils-download-file url local-file 3)
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

(provide 'bfepm-package)

;;; bfepm-package.el ends here
