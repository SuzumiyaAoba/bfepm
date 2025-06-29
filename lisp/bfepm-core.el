;;; bfepm-core.el --- BFEPM Core functionality -*- lexical-binding: t -*-

;;; Commentary:

;; Core functionality and data structures for BFEPM.

;;; Code:

(require 'cl-lib)

;; Add lib directory to load path for framework libraries
(let ((lib-dir (expand-file-name "lib" (file-name-directory
                                       (or load-file-name buffer-file-name)))))
  (when (and (file-directory-p lib-dir)
             (not (member lib-dir load-path)))
    (add-to-list 'load-path lib-dir)))

;; Declare external functions to avoid compilation warnings
(declare-function bfepm-config-save "bfepm-config")
(declare-function bfepm-config-load "bfepm-config")
(declare-function bfepm-config-create-default "bfepm-config")

(defgroup bfepm nil
  "Better Fast Emacs Package Manager."
  :group 'applications
  :prefix "bfepm-")

(defcustom bfepm-directory (expand-file-name "bfepm" user-emacs-directory)
  "Directory where BFEPM stores its data."
  :type 'directory
  :group 'bfepm)

(defvar bfepm--initialized nil
  "Whether BFEPM has been initialized.")

(defvar bfepm--config nil
  "Current BFEPM configuration.")

(defvar bfepm--packages-directory nil
  "Directory where packages are installed.")

(defvar bfepm--cache-directory nil
  "Directory for BFEPM cache.")

(defvar bfepm--profiles-directory nil
  "Directory where profile configurations are stored.")

;; Performance caching variables
(defvar bfepm--directory-cache nil
  "Cache for directory listings to improve performance.")

(defvar bfepm--version-cache nil
  "Cache for package version information.")

(cl-defstruct bfepm-package
  "Structure representing a package."
  name
  version
  source
  dependencies
  status
  checksum
  installed-path)

;; Configuration structures - defined here to avoid circular dependencies
;; These will be available even when bfepm-config module cannot be loaded
(cl-defstruct bfepm-config
  "Structure representing BFEPM configuration."
  packages
  sources
  profiles)

(cl-defstruct bfepm-source
  "Structure representing a package source."
  name
  url
  type
  priority)

(defun bfepm-core-initialize ()
  "Initialize BFEPM core system."
  (unless bfepm--initialized
    (bfepm-core--setup-directories)
    (bfepm-core--load-config)
    (setq bfepm--initialized t)
    (message "BFEPM initialized successfully")))

(defun bfepm-core--setup-directories ()
  "Create necessary directories for BFEPM."
  (let ((bfepm-dir (expand-file-name bfepm-directory)))
    (unless (file-exists-p bfepm-dir)
      (make-directory bfepm-dir t))
    
    (setq bfepm--packages-directory (expand-file-name "packages" bfepm-dir))
    (unless (file-exists-p bfepm--packages-directory)
      (make-directory bfepm--packages-directory t))
    
    (setq bfepm--cache-directory (expand-file-name "cache" bfepm-dir))
    (unless (file-exists-p bfepm--cache-directory)
      (make-directory bfepm--cache-directory t))
    
    (setq bfepm--profiles-directory (expand-file-name "profiles" bfepm-dir))
    (unless (file-exists-p bfepm--profiles-directory)
      (make-directory bfepm--profiles-directory t))))

(defun bfepm-core--load-config ()
  "Load BFEPM configuration from file."
  (condition-case err
      (if (and (boundp 'bfepm-config-file) (file-exists-p bfepm-config-file))
          (cond
           ;; Prefer minimal config when TOML is not available
           ((and (featurep 'bfepm-config-minimal) 
                 (not (and (featurep 'bfepm-config) 
                          (boundp 'bfepm-config--toml-available) 
                          bfepm-config--toml-available)))
            (let ((config (bfepm-config-create-default))
                  (packages '()))
              ;; Use minimal config parsing logic directly
              (with-temp-buffer
                (insert-file-contents bfepm-config-file)
                (goto-char (point-min))
                (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*{\\s-*git\\s-*=\\s-*\"\\([^\"]+\\)\"\\(.*\\)}" nil t)
                  (let* ((package-name (match-string 1))
                         (git-url (match-string 2))
                         (rest (match-string 3))
                         (branch nil)
                         (tag nil)
                         (ref nil))
                    ;; Parse branch, tag, ref
                    (when (string-match "branch\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                      (setq branch (match-string 1 rest)))
                    (when (string-match "tag\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                      (setq tag (match-string 1 rest)))
                    (when (string-match "ref\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                      (setq ref (match-string 1 rest)))
                    
                    ;; Create git source
                    (let ((git-source (list :url git-url :type "git")))
                      (when branch (setq git-source (plist-put git-source :ref branch)))
                      (when tag (setq git-source (plist-put git-source :ref tag)))
                      (when ref (setq git-source (plist-put git-source :ref ref)))
                      
                      ;; Create package entry
                      (push (make-bfepm-package
                             :name package-name
                             :version (or branch tag ref "latest")
                             :source git-source
                             :status 'required)
                            packages)))))
              
              ;; Update config with parsed packages
              (setf (bfepm-config-packages config) packages)
              (setq bfepm--config config)))
           ;; Use full config when TOML is available
           ((and (featurep 'bfepm-config) 
                 (boundp 'bfepm-config--toml-available) 
                 bfepm-config--toml-available)
            (setq bfepm--config (bfepm-config-load bfepm-config-file)))
           ;; Fall back to minimal config
           ((featurep 'bfepm-config-minimal)
            (setq bfepm--config (bfepm-config-load bfepm-config-file)))
           ;; Try any available config module
           ((or (featurep 'bfepm-config) (featurep 'bfepm-config-minimal))
            (setq bfepm--config (bfepm-config-load bfepm-config-file)))
           (t
            (message "Warning: Cannot load config file %s (bfepm-config module not available)" bfepm-config-file)
            (setq bfepm--config nil)))
        (if (or (featurep 'bfepm-config) (featurep 'bfepm-config-minimal))
            (progn
              (setq bfepm--config (bfepm-config-create-default))
              (when (boundp 'bfepm-config-file)
                (bfepm-config-save bfepm--config bfepm-config-file)))
          (progn
            (message "Warning: bfepm-config module not available, using minimal configuration")
            (setq bfepm--config nil))))
    (error
     (message "Warning: Failed to load configuration: %s" (error-message-string err))
     (setq bfepm--config nil))))

(defun bfepm-core-get-config ()
  "Get current BFEPM configuration."
  (unless bfepm--initialized
    (bfepm-core-initialize))
  bfepm--config)

(defun bfepm-core-get-packages-directory ()
  "Get packages installation directory."
  (unless bfepm--initialized
    (bfepm-core-initialize))
  bfepm--packages-directory)

(defun bfepm-core-get-cache-directory ()
  "Get cache directory."
  (unless bfepm--initialized
    (bfepm-core-initialize))
  bfepm--cache-directory)

(defun bfepm-core-package-installed-p (package-name)
  "Check if PACKAGE-NAME is installed."
  (let ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
    (file-directory-p package-dir)))

(defun bfepm-core-get-installed-packages (&optional force-refresh)
  "Get list of installed packages.
When FORCE-REFRESH is non-nil, bypass cache and refresh from filesystem.
Uses an internal cache with 60-second expiration to improve performance.
Cache entries are stored as (directory-path cache-time file-list)."
  (let ((packages-dir (bfepm-core-get-packages-directory)))
    (when (file-directory-p packages-dir)
      (let* ((cache-key packages-dir)
             (cached-entry (assoc cache-key bfepm--directory-cache))
             (cache-time (cadr cached-entry))
             (cached-files (caddr cached-entry))
             (current-time (current-time))
             (cache-expired-p (or force-refresh
                                  (not cached-entry)
                                  (> (float-time (time-subtract current-time cache-time)) 60)))) ; 60 second cache
        (if cache-expired-p
            (let ((files (directory-files packages-dir nil "^[^.]")))
              ;; Update or add cache entry
              (if cached-entry
                  (setcdr cached-entry (list current-time files))
                (push (list cache-key current-time files) bfepm--directory-cache))
              files)
          cached-files)))))

(defun bfepm-core-get-package-version (package-name &optional force-refresh)
  "Get version of installed PACKAGE-NAME.
When FORCE-REFRESH is non-nil, bypass cache and refresh from filesystem."
  (let* ((cache-key package-name)
         (cached-entry (assoc cache-key bfepm--version-cache))
         (cache-time (cadr cached-entry))
         (cached-version (caddr cached-entry))
         (current-time (current-time))
         (cache-expired-p (or force-refresh
                              (not cached-entry)
                              (> (float-time (time-subtract current-time cache-time)) 300)))) ; 5 minute cache
    (if cache-expired-p
        (let ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
          (let ((version (when (file-directory-p package-dir)
                           (or (bfepm-core--get-version-from-bfepm-file package-dir)
                               (bfepm-core--get-version-from-pkg-file package-dir package-name)
                               (bfepm-core--get-version-from-main-file package-dir package-name)
                               (bfepm-core--get-version-from-directory-name package-name)
                               "unknown"))))
            ;; Update or add cache entry
            (if cached-entry
                (setcdr cached-entry (list current-time version))
              (push (list cache-key current-time version) bfepm--version-cache))
            version))
      cached-version)))

;; Cache management functions
(defun bfepm-core--invalidate-cache (package-name)
  "Invalidate caches for PACKAGE-NAME.
Clears entire directory cache since package operations affect listings.
Only clears specific package from version cache for targeted invalidation."
  (setq bfepm--directory-cache nil ; Clear entire directory cache
        bfepm--version-cache (delq (assoc package-name bfepm--version-cache) bfepm--version-cache)))

(defun bfepm-core--clear-all-caches ()
  "Clear all performance caches."
  (setq bfepm--directory-cache nil
        bfepm--version-cache nil))

(defun bfepm-core--get-version-from-bfepm-file (package-dir)
  "Get version from .bfepm-version file in PACKAGE-DIR."
  (let ((version-file (expand-file-name ".bfepm-version" package-dir)))
    (when (and (file-exists-p version-file)
               (> (file-attribute-size (file-attributes version-file)) 0)) ; Check if file is not empty
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents version-file nil 0 100) ; Read only first 100 chars for version
            (goto-char (point-min))
            (let ((content (buffer-substring-no-properties (point) (line-end-position))))
              ;; Manual trim for compatibility - optimized with single regex
              (if (string-match "\\`[ \t\n\r]*\\(.*?\\)[ \t\n\r]*\\'" content)
                  (match-string 1 content)
                content)))
        (error nil)))))

(defun bfepm-core--get-version-from-pkg-file (package-dir package-name)
  "Get version from PACKAGE-NAME-pkg.el file in PACKAGE-DIR.
PACKAGE-NAME is the name of the package to get version for."
  (let ((pkg-file (expand-file-name (format "%s-pkg.el" package-name) package-dir)))
    (when (file-exists-p pkg-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents pkg-file)
            (goto-char (point-min))
            (when (re-search-forward "(define-package\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" nil t)
              (match-string 1)))
        (error nil)))))

(defun bfepm-core--get-version-from-main-file (package-dir package-name)
  "Get version from main .el file header in PACKAGE-DIR for PACKAGE-NAME."
  (let ((main-file (expand-file-name (format "%s.el" package-name) package-dir)))
    (when (file-exists-p main-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents main-file nil 0 2000) ; Read first 2000 chars for headers
            (goto-char (point-min))
            (when (re-search-forward "^;; Version:\\s-+\\([0-9][^\\s-]*\\)" nil t)
              (match-string 1)))
        (error nil)))))

(defun bfepm-core--get-version-from-directory-name (package-name)
  "Extract version from directory name if it contain version info.
PACKAGE-NAME is the name of the package to extract version for."
  ;; Try to extract version from directory names like "package-20250426.1319"
  (let ((packages-dir (bfepm-core-get-packages-directory)))
    (when (file-directory-p packages-dir)
      (let ((dirs (directory-files packages-dir nil (format "^%s-[0-9]" (regexp-quote package-name)))))
        (when dirs
          (let ((versioned-dir (car dirs)))
            (when (string-match (format "^%s-\\(.+\\)$" (regexp-quote package-name)) versioned-dir)
              (match-string 1 versioned-dir))))))))

(provide 'bfepm-core)

;;; bfepm-core.el ends here
