;;; epm.el --- Emacs Package Manager -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (toml "0.1.0") (async "1.9.4"))
;; Keywords: package management
;; URL: https://github.com/SuzumiyaAoba/epm

;;; Commentary:

;; EPM is a package manager for Emacs.
;; It provides declarative package management with version constraints
;; and dependency resolution.

;;; Code:

(require 'epm-core)

;; Load optional modules
(defvar epm--config-available nil
  "Whether epm-config module is available.")

(defvar epm--package-available nil
  "Whether epm-package module is available.")

;; Try to load full epm-config, fallback to minimal version
(condition-case nil
    (if (locate-library "toml")
        (progn
          (require 'epm-config)
          (setq epm--config-available t)
          (message "EPM: Full epm-config loaded successfully"))
      (progn
        (require 'epm-config-minimal)
        (setq epm--config-available t)
        (message "EPM: Minimal epm-config loaded (TOML support not available)")))
  (error 
   (message "Warning: Could not load any epm-config version")
   (setq epm--config-available nil)))

;; Load epm-package module
(condition-case nil
    (progn
      (require 'epm-package)
      (setq epm--package-available t)
      (message "EPM: epm-package loaded successfully"))
  (error 
   (message "Warning: epm-package could not be loaded")
   (setq epm--package-available nil)))

(defgroup epm nil
  "Emacs Package Manager."
  :group 'applications
  :prefix "epm-")

(defcustom epm-directory (expand-file-name "epm" user-emacs-directory)
  "Directory where EPM stores its data."
  :type 'directory
  :group 'epm)

(defcustom epm-config-file (expand-file-name "epm.toml" user-emacs-directory)
  "Path to the main EPM configuration file."
  :type 'file
  :group 'epm)

;;;###autoload
(defun epm-install (package-spec)
  "Install a package specified by PACKAGE-SPEC."
  (interactive "sPackage: ")
  (if epm--package-available
      (epm-package-install package-spec)
    (message "Package installation not available (epm-package module not loaded)")))

;;;###autoload
(defun epm-remove (package-name)
  "Remove PACKAGE-NAME."
  (interactive "sPackage to remove: ")
  (if epm--package-available
      (epm-package-remove package-name)
    (message "Package removal not available (epm-package module not loaded)")))

;;;###autoload
(defun epm-update (&optional package-name)
  "Update PACKAGE-NAME or all packages if nil."
  (interactive "P")
  (if epm--package-available
      (if package-name
          (epm-package-update (read-string "Package: "))
        (epm-package-update-all))
    (message "Package update not available (epm-package module not loaded)")))

;;;###autoload
(defun epm-list ()
  "List installed packages."
  (interactive)
  (if epm--package-available
      (epm-package-list)
    (message "Package listing not available (epm-package module not loaded)")))

;;;###autoload
(defun epm-init ()
  "Initialize EPM in the current Emacs session."
  (interactive)
  (epm-core-initialize))

(provide 'epm)

;;; epm.el ends here
