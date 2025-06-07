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
PACKAGE-SPEC can be a string (package name), a list (name version), or a bfepm-package structure."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (config (bfepm-core-get-config)))
    
    (bfepm-utils-message "Installing package: %s" (bfepm-package-name package))
    
    ;; Check if already installed
    (when (bfepm-core-package-installed-p (bfepm-package-name package))
      (bfepm-utils-message "Package %s already installed" (bfepm-package-name package))
      (return))
    
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
      (bfepm-package--download-and-install package package-info))))

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

(defun bfepm-package--fetch-archive-contents (archive-url)
  "Fetch archive contents from ARCHIVE-URL."
  (let ((archive-file (concat archive-url "archive-contents")))
    (with-temp-buffer
      (url-insert-file-contents archive-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun bfepm-package--download-and-install (package package-info)
  "Download and install PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         ;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (deps (cadr info-list))
         (desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name 
                      (format "%s-%s.%s" package-name version-string 
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))
    
    (bfepm-utils-ensure-directory download-dir)
    
    ;; Download package file
    (bfepm-utils-message "Downloading %s..." package-name)
    (condition-case err
        (bfepm-utils-download-file archive-file local-file)
      (error 
       (bfepm-utils-error "Failed to download %s: %s" package-name (error-message-string err))))
    
    ;; Install dependencies first
    (bfepm-package--install-dependencies deps)
    
    ;; Extract/install package
    (condition-case err
        (progn
          (bfepm-package--extract-and-install package-name local-file kind)
          ;; Save version information
          (bfepm-package--save-version-info package-name version-string))
      (error 
       (bfepm-utils-error "Failed to install %s: %s" package-name (error-message-string err))))
    
    (bfepm-utils-message "Successfully installed %s" package-name)))

(defun bfepm-package--build-archive-url (package-name version kind)
  "Build archive URL for package."
  ;; MELPA uses specific URL format
  (let ((extension (if (eq kind 'tar) "tar" "el")))
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
  "Extract and install package from ARCHIVE-FILE."
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
  "Extract TAR-FILE to INSTALL-DIR."
  (let ((default-directory install-dir))
    (call-process "tar" nil nil nil "-xf" tar-file "--strip-components=1")))

(defun bfepm-package--install-single-file (el-file install-dir)
  "Install single EL-FILE to INSTALL-DIR."
  (copy-file el-file (expand-file-name (file-name-nondirectory el-file) install-dir)))

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
          (bfepm-utils-message "Removed package: %s" package-name))
      (bfepm-utils-message "Package not installed: %s" package-name))))

(defun bfepm-package-update (package-name)
  "Update installed package PACKAGE-NAME."
  (when (bfepm-core-package-installed-p package-name)
    (bfepm-package-remove package-name)
    (bfepm-package-install package-name)))

(defun bfepm-package-update-all ()
  "Update all installed packages."
  (let ((installed-packages (bfepm-core-get-installed-packages)))
    (dolist (package-name installed-packages)
      (bfepm-utils-message "Updating %s..." package-name)
      (bfepm-package-update package-name))))

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

(provide 'bfepm-package)

;;; bfepm-package.el ends here
