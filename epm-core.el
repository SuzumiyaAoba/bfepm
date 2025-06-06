;;; epm-core.el --- EPM Core functionality -*- lexical-binding: t -*-

;;; Commentary:

;; Core functionality and data structures for EPM.

;;; Code:

(require 'cl-lib)

(defvar epm--initialized nil
  "Whether EPM has been initialized.")

(defvar epm--config nil
  "Current EPM configuration.")

(defvar epm--packages-directory nil
  "Directory where packages are installed.")

(defvar epm--cache-directory nil
  "Directory for EPM cache.")

(cl-defstruct epm-package
  "Structure representing a package."
  name
  version
  source
  dependencies
  status
  checksum
  installed-path)

;; Configuration structures - defined here to avoid circular dependencies
;; These will be available even when epm-config module cannot be loaded
(cl-defstruct epm-config
  "Structure representing EPM configuration."
  packages
  sources)

(cl-defstruct epm-source
  "Structure representing a package source."
  name
  url
  type
  priority)

(defun epm-core-initialize ()
  "Initialize EPM core system."
  (unless epm--initialized
    (epm-core--setup-directories)
    (epm-core--load-config)
    (setq epm--initialized t)
    (message "EPM initialized successfully")))

(defun epm-core--setup-directories ()
  "Create necessary directories for EPM."
  (let ((epm-dir (expand-file-name epm-directory)))
    (unless (file-exists-p epm-dir)
      (make-directory epm-dir t))
    
    (setq epm--packages-directory (expand-file-name "packages" epm-dir))
    (unless (file-exists-p epm--packages-directory)
      (make-directory epm--packages-directory t))
    
    (setq epm--cache-directory (expand-file-name "cache" epm-dir))
    (unless (file-exists-p epm--cache-directory)
      (make-directory epm--cache-directory t))))

(defun epm-core--load-config ()
  "Load EPM configuration from file."
  (condition-case err
      (if (and (boundp 'epm-config-file) (file-exists-p epm-config-file))
          (if (or (featurep 'epm-config) (featurep 'epm-config-minimal))
              (setq epm--config (epm-config-load epm-config-file))
            (progn
              (message "Warning: Cannot load config file %s (epm-config module not available)" epm-config-file)
              (setq epm--config nil)))
        (if (or (featurep 'epm-config) (featurep 'epm-config-minimal))
            (progn
              (setq epm--config (epm-config-create-default))
              (when (boundp 'epm-config-file)
                (epm-config-save epm--config epm-config-file)))
          (progn
            (message "Warning: epm-config module not available, using minimal configuration")
            (setq epm--config nil))))
    (error 
     (message "Warning: Failed to load configuration: %s" (error-message-string err))
     (setq epm--config nil))))

(defun epm-core-get-config ()
  "Get current EPM configuration."
  (unless epm--initialized
    (epm-core-initialize))
  epm--config)

(defun epm-core-get-packages-directory ()
  "Get packages installation directory."
  (unless epm--initialized
    (epm-core-initialize))
  epm--packages-directory)

(defun epm-core-get-cache-directory ()
  "Get cache directory."
  (unless epm--initialized
    (epm-core-initialize))
  epm--cache-directory)

(defun epm-core-package-installed-p (package-name)
  "Check if PACKAGE-NAME is installed."
  (let ((package-dir (expand-file-name package-name (epm-core-get-packages-directory))))
    (file-directory-p package-dir)))

(defun epm-core-get-installed-packages ()
  "Get list of installed packages."
  (when (file-directory-p (epm-core-get-packages-directory))
    (directory-files (epm-core-get-packages-directory) nil "^[^.]")))

(defun epm-core-get-package-version (package-name)
  "Get version of installed PACKAGE-NAME."
  (let ((package-dir (expand-file-name package-name (epm-core-get-packages-directory))))
    (when (file-directory-p package-dir)
      (or (epm-core--get-version-from-epm-file package-dir)
          (epm-core--get-version-from-pkg-file package-dir package-name)
          (epm-core--get-version-from-main-file package-dir package-name)
          (epm-core--get-version-from-directory-name package-name)
          "unknown"))))

(defun epm-core--get-version-from-epm-file (package-dir)
  "Get version from .epm-version file in PACKAGE-DIR."
  (let ((version-file (expand-file-name ".epm-version" package-dir)))
    (when (file-exists-p version-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents version-file)
            (let ((content (buffer-string)))
              ;; Manual trim for compatibility
              (replace-regexp-in-string "\\`[ \t\n\r]+" "" 
                                        (replace-regexp-in-string "[ \t\n\r]+\\'" "" content))))
        (error nil)))))

(defun epm-core--get-version-from-pkg-file (package-dir package-name)
  "Get version from PACKAGE-NAME-pkg.el file in PACKAGE-DIR."
  (let ((pkg-file (expand-file-name (format "%s-pkg.el" package-name) package-dir)))
    (when (file-exists-p pkg-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents pkg-file)
            (goto-char (point-min))
            (when (re-search-forward "(define-package\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" nil t)
              (match-string 1)))
        (error nil)))))

(defun epm-core--get-version-from-main-file (package-dir package-name)
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

(defun epm-core--get-version-from-directory-name (package-name)
  "Extract version from directory name if it contains version info."
  ;; Try to extract version from directory names like "package-20250426.1319"
  (let ((packages-dir (epm-core-get-packages-directory)))
    (when (file-directory-p packages-dir)
      (let ((dirs (directory-files packages-dir nil (format "^%s-[0-9]" (regexp-quote package-name)))))
        (when dirs
          (let ((versioned-dir (car dirs)))
            (when (string-match (format "^%s-\\(.+\\)$" (regexp-quote package-name)) versioned-dir)
              (match-string 1 versioned-dir))))))))

(provide 'epm-core)

;;; epm-core.el ends here
