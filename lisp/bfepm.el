;;; bfepm.el --- Better Fast Package Manager -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (toml "0.1.0") (async "1.9.4"))
;; Keywords: tools package-management
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; bfepm is a package manager for Emacs.
;; It provides declarative package management with version constraints
;; and dependency resolution.

;;; Code:

(require 'bfepm-core)

;; Load optional modules
(defvar bfepm--config-available nil
  "Whether bfepm-config module is available.")

(defvar bfepm--package-available nil
  "Whether bfepm-package module is available.")

;; Try to load full bfepm-config, fallback to minimal version
(condition-case nil
    (if (locate-library "toml")
        (progn
          (require 'bfepm-config)
          (setq bfepm--config-available t)
          (message "BFEPM: Full bfepm-config loaded successfully"))
      (progn
        (require 'bfepm-config-minimal)
        (setq bfepm--config-available t)
        (message "BFEPM: Minimal bfepm-config loaded (TOML support not available)")))
  (error 
   (message "Warning: Could not load any bfepm-config version")
   (setq bfepm--config-available nil)))

;; Load epm-package module
(condition-case nil
    (progn
      (require 'bfepm-package)
      (setq bfepm--package-available t)
      (message "BFEPM: bfepm-package loaded successfully"))
  (error 
   (message "Warning: bfepm-package could not be loaded")
   (setq bfepm--package-available nil)))

;; Declare external functions to avoid compilation warnings
(declare-function bfepm-package-list "bfepm-package")
(declare-function bfepm-package-install "bfepm-package")
(declare-function bfepm-package-update "bfepm-package")
(declare-function bfepm-package-update-all "bfepm-package")
(declare-function bfepm-package-remove "bfepm-package")

(defcustom bfepm-config-file (expand-file-name "bfepm.toml" user-emacs-directory)
  "Path to the main BFEPM configuration file."
  :type 'file
  :group 'bfepm)

;;;###autoload
(defun bfepm-install (package-spec)
  "Install a package specified by PACKAGE-SPEC."
  (interactive "sPackage: ")
  (if bfepm--package-available
      (bfepm-package-install package-spec)
    (message "Package installation not available (bfepm-package module not loaded)")))

;;;###autoload
(defun bfepm-remove (package-name)
  "Remove PACKAGE-NAME."
  (interactive "sPackage to remove: ")
  (if bfepm--package-available
      (bfepm-package-remove package-name)
    (message "Package removal not available (bfepm-package module not loaded)")))

;;;###autoload
(defun bfepm-update (&optional package-name)
  "Update PACKAGE-NAME or all packages if nil."
  (interactive "P")
  (if bfepm--package-available
      (if package-name
          (bfepm-package-update (read-string "Package: "))
        (bfepm-package-update-all))
    (message "Package update not available (bfepm-package module not loaded)")))

;;;###autoload
(defun bfepm-list ()
  "List installed packages."
  (interactive)
  (if bfepm--package-available
      (bfepm-package-list)
    (message "Package listing not available (bfepm-package module not loaded)")))

;;;###autoload
(defun bfepm-init ()
  "Initialize BFEPM in the current Emacs session."
  (interactive)
  (bfepm-core-initialize))

(provide 'bfepm)

;;; bfepm.el ends here
