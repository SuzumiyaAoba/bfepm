;;; epm-package.el --- EPM Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Package installation, removal, and management functionality for EPM.

;;; Code:

(require 'url)
(require 'tar-mode)
(require 'epm-core)
(require 'epm-utils)
;; epm-config is optional - loaded conditionally

(defvar epm-package--melpa-archive-url "https://melpa.org/packages/archive-contents"
  "URL for MELPA archive contents.")

(defvar epm-package--gnu-archive-url "https://elpa.gnu.org/packages/archive-contents"
  "URL for GNU ELPA archive contents.")

(defvar epm-package--archive-cache nil
  "Cache for package archive contents.")

(defun epm-package--get-default-sources ()
  "Get default package sources when config is not available."
  '(("melpa" . (:url "https://melpa.org/packages/" :type "elpa" :priority 10))
    ("gnu" . (:url "https://elpa.gnu.org/packages/" :type "elpa" :priority 5))))

(defun epm-package--get-source-priority (source)
  "Get priority from SOURCE (supports both struct and plist)."
  (cond
   ((epm-source-p source) (epm-source-priority source))
   ((plistp source) (plist-get source :priority))
   (t 5)))

(defun epm-package--get-source-type (source)
  "Get type from SOURCE (supports both struct and plist)."
  (cond
   ((epm-source-p source) (epm-source-type source))
   ((plistp source) (plist-get source :type))
   (t "elpa")))

(defun epm-package--get-source-url (source)
  "Get URL from SOURCE (supports both struct and plist)."
  (cond
   ((epm-source-p source) (epm-source-url source))
   ((plistp source) (plist-get source :url))
   (t nil)))

(defun epm-package--format-version (version)
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

(defun epm-package--version-matches-p (available-version requested-version)
  "Check if AVAILABLE-VERSION satisfies REQUESTED-VERSION."
  (cond
   ((string= requested-version "latest") t)
   ((string= available-version requested-version) t)
   ((string-prefix-p "^" requested-version)
    ;; Compatible version (e.g., ^1.2.3)
    (epm-utils-version-satisfies-p available-version requested-version))
   ((string-prefix-p "~" requested-version)
    ;; Patch level (e.g., ~1.2.3)
    (epm-utils-version-satisfies-p available-version requested-version))
   (t nil)))

(defun epm-package-install (package-spec)
  "Install package specified by PACKAGE-SPEC.
PACKAGE-SPEC can be a string (package name), a list (name version), or a epm-package structure."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-epm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-epm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (config (epm-core-get-config)))
    
    (epm-utils-message "Installing package: %s" (epm-package-name package))
    
    ;; Check if already installed
    (when (epm-core-package-installed-p (epm-package-name package))
      (epm-utils-message "Package %s already installed" (epm-package-name package))
      (return))
    
    ;; Find package in sources
    (let ((package-info (epm-package--find-package package config)))
      (unless package-info
        (epm-utils-error "Package not found: %s" (epm-package-name package)))
      
      ;; Validate version if specified
      (when (and (not (string= (epm-package-version package) "latest"))
                 package-info)
        (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
               (available-version (epm-package--format-version (car info-list)))
               (requested-version (epm-package-version package)))
          (unless (epm-package--version-matches-p available-version requested-version)
            (epm-utils-message "Warning: Requested version %s not available. Latest is %s" 
                              requested-version available-version))))
      
      ;; Download and install
      (epm-package--download-and-install package package-info))))

(defun epm-package--find-package (package config)
  "Find PACKAGE in available sources from CONFIG."
  (let* ((package-name (epm-package-name package))
         (sources (if config 
                     (epm-config-sources config)
                   (epm-package--get-default-sources))))
    
    ;; Try each source in priority order
    (cl-loop for source in (sort sources (lambda (a b) 
                                           (> (epm-package--get-source-priority (cdr a))
                                              (epm-package--get-source-priority (cdr b)))))
             for package-info = (epm-package--find-in-source package-name (cdr source))
             when package-info return package-info)))

(defun epm-package--find-in-source (package-name source)
  "Find PACKAGE-NAME in SOURCE."
  (let ((source-type (epm-package--get-source-type source)))
    (cond
     ((string= source-type "elpa")
      (epm-package--find-in-elpa package-name source))
     ((string= source-type "git")
      (epm-package--find-in-git package-name source))
     (t nil))))

(defun epm-package--find-in-elpa (package-name source)
  "Find PACKAGE-NAME in ELPA SOURCE."
  (let* ((archive-url (epm-package--get-source-url source))
         (archive-contents (epm-package--get-archive-contents archive-url)))
    (alist-get (intern package-name) archive-contents)))

(defun epm-package--get-archive-contents (archive-url)
  "Get archive contents from ARCHIVE-URL, using cache if available."
  (let ((cache-key archive-url))
    (or (alist-get cache-key epm-package--archive-cache nil nil #'string=)
        (let ((contents (epm-package--fetch-archive-contents archive-url)))
          (push (cons cache-key contents) epm-package--archive-cache)
          contents))))

(defun epm-package--fetch-archive-contents (archive-url)
  "Fetch archive contents from ARCHIVE-URL."
  (let ((archive-file (concat archive-url "archive-contents")))
    (with-temp-buffer
      (url-insert-file-contents archive-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun epm-package--download-and-install (package package-info)
  "Download and install PACKAGE using PACKAGE-INFO."
  (let* ((package-name (epm-package-name package))
         ;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (deps (cadr info-list))
         (desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (epm-package--format-version version))
         (archive-file (epm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (epm-core-get-cache-directory)))
         (local-file (expand-file-name 
                      (format "%s-%s.%s" package-name version-string 
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))
    
    (epm-utils-ensure-directory download-dir)
    
    ;; Download package file
    (epm-utils-message "Downloading %s..." package-name)
    (condition-case err
        (epm-utils-download-file archive-file local-file)
      (error 
       (epm-utils-error "Failed to download %s: %s" package-name (error-message-string err))))
    
    ;; Install dependencies first
    (epm-package--install-dependencies deps)
    
    ;; Extract/install package
    (condition-case err
        (progn
          (epm-package--extract-and-install package-name local-file kind)
          ;; Save version information
          (epm-package--save-version-info package-name version-string))
      (error 
       (epm-utils-error "Failed to install %s: %s" package-name (error-message-string err))))
    
    (epm-utils-message "Successfully installed %s" package-name)))

(defun epm-package--build-archive-url (package-name version kind)
  "Build archive URL for package."
  ;; MELPA uses specific URL format
  (let ((extension (if (eq kind 'tar) "tar" "el")))
    (format "https://melpa.org/packages/%s-%s.%s"
            package-name version extension)))

(defun epm-package--install-dependencies (deps)
  "Install package dependencies DEPS."
  (when deps
    (dolist (dep deps)
      (let ((dep-name (symbol-name (car dep))))
        ;; Skip built-in packages like 'emacs'
        (unless (or (string= dep-name "emacs")
                    (epm-core-package-installed-p dep-name))
          (epm-utils-message "Installing dependency: %s" dep-name)
          (condition-case err
              (epm-package-install dep-name)
            (error 
             (epm-utils-message "Warning: Failed to install dependency %s: %s" 
                               dep-name (error-message-string err)))))))))

(defun epm-package--extract-and-install (package-name archive-file kind)
  "Extract and install package from ARCHIVE-FILE."
  (let ((install-dir (expand-file-name package-name (epm-core-get-packages-directory))))
    (epm-utils-ensure-directory install-dir)
    
    (cond
     ((eq kind 'tar)
      (epm-package--extract-tar-package archive-file install-dir))
     (t
      (epm-package--install-single-file archive-file install-dir)))
    
    ;; Add to load-path
    (add-to-list 'load-path install-dir)))

(defun epm-package--extract-tar-package (tar-file install-dir)
  "Extract TAR-FILE to INSTALL-DIR."
  (let ((default-directory install-dir))
    (call-process "tar" nil nil nil "-xf" tar-file "--strip-components=1")))

(defun epm-package--install-single-file (el-file install-dir)
  "Install single EL-FILE to INSTALL-DIR."
  (copy-file el-file (expand-file-name (file-name-nondirectory el-file) install-dir)))

(defun epm-package--save-version-info (package-name version)
  "Save VERSION information for PACKAGE-NAME."
  (let* ((package-dir (expand-file-name package-name (epm-core-get-packages-directory)))
         (version-file (expand-file-name ".epm-version" package-dir)))
    (when (file-directory-p package-dir)
      (with-temp-file version-file
        (insert (format "%s\n" version))))))

(defun epm-package-remove (package-name)
  "Remove installed package PACKAGE-NAME."
  (let ((package-dir (expand-file-name package-name (epm-core-get-packages-directory))))
    (if (file-directory-p package-dir)
        (progn
          (delete-directory package-dir t)
          (epm-utils-message "Removed package: %s" package-name))
      (epm-utils-message "Package not installed: %s" package-name))))

(defun epm-package-update (package-name)
  "Update installed package PACKAGE-NAME."
  (when (epm-core-package-installed-p package-name)
    (epm-package-remove package-name)
    (epm-package-install package-name)))

(defun epm-package-update-all ()
  "Update all installed packages."
  (let ((installed-packages (epm-core-get-installed-packages)))
    (dolist (package-name installed-packages)
      (epm-utils-message "Updating %s..." package-name)
      (epm-package-update package-name))))

(defun epm-package-list ()
  "List installed packages with version information."
  (let ((packages (epm-core-get-installed-packages)))
    (if packages
        (progn
          (epm-utils-message "Installed packages:")
          (dolist (package packages)
            (let ((version (epm-core-get-package-version package)))
              (if (string= version "unknown")
                  (message "  - %s (version unknown)" package)
                (message "  - %s (%s)" package version)))))
      (epm-utils-message "No packages installed"))))

(defun epm-package-info (package-name)
  "Show information about PACKAGE-NAME."
  (let* ((config (epm-core-get-config))
         (package-info (epm-package--find-package 
                        (make-epm-package :name package-name) config)))
    (if package-info
        (progn
          (epm-utils-message "Package: %s" package-name)
          (message "  Version: %s" (cadr package-info))
          (message "  Description: %s" (cadddr package-info))
          (when (caddr package-info)
            (message "  Dependencies: %s" 
                     (mapconcat (lambda (dep) (symbol-name (car dep)))
                                (caddr package-info) ", "))))
      (epm-utils-message "Package not found: %s" package-name))))

(defun epm-package-search (query)
  "Search for packages matching QUERY."
  (epm-utils-message "Searching for packages matching: %s" query)
  ;; This would implement actual search functionality
  ;; For now, just a placeholder
  (epm-utils-message "Search functionality not yet implemented"))

(provide 'epm-package)

;;; epm-package.el ends here
