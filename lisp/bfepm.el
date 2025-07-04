;;; bfepm.el --- Better Fast Package Manager -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: SuzumiyaAoba
;; Version: 0.2.0
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

;; Load bfepm-package module
(condition-case nil
    (progn
      (require 'bfepm-package)
      (setq bfepm--package-available t)
      (message "BFEPM: bfepm-package loaded successfully"))
  (error
   (message "Warning: bfepm-package could not be loaded")
   (setq bfepm--package-available nil)))

;; Load bfepm-ui module
(condition-case nil
    (progn
      (require 'bfepm-ui)
      (message "BFEPM: bfepm-ui loaded successfully"))
  (error
   (message "Warning: bfepm-ui could not be loaded")))

;; Load bfepm-profile module
(condition-case nil
    (progn
      (require 'bfepm-profile)
      (message "BFEPM: bfepm-profile loaded successfully"))
  (error
   (message "Warning: bfepm-profile could not be loaded")))

;; Load bfepm-search module
(condition-case nil
    (progn
      (require 'bfepm-search)
      (message "BFEPM: bfepm-search loaded successfully"))
  (error
   (message "Warning: bfepm-search could not be loaded")))

;; Declare external functions to avoid compilation warnings
(declare-function bfepm-package-list "bfepm-package")
(declare-function bfepm-package-install "bfepm-package")
(declare-function bfepm-package-install-async "bfepm-package")
(declare-function bfepm-package-update "bfepm-package")
(declare-function bfepm-package-update-all "bfepm-package")
(declare-function bfepm-package-remove "bfepm-package")
(declare-function bfepm-ui "bfepm-ui")
(declare-function bfepm-ui-show-available-external "bfepm-ui")
(declare-function bfepm-profile-create "bfepm-profile")
(declare-function bfepm-profile-switch "bfepm-profile")
(declare-function bfepm-profile-list "bfepm-profile")
(declare-function bfepm-profile-remove "bfepm-profile")
(declare-function bfepm-profile-copy "bfepm-profile")
(declare-function bfepm-profile-current "bfepm-profile")
(declare-function bfepm-profile-list-names "bfepm-profile")
(declare-function bfepm-ui-show-installed-external "bfepm-ui")
(declare-function bfepm-search "bfepm-search")
(declare-function bfepm-search-async "bfepm-search")
(declare-function bfepm-search-installed-packages "bfepm-search")

(defcustom bfepm-config-file (expand-file-name "bfepm.toml" user-emacs-directory)
  "Path to the main BFEPM configuration file."
  :type 'file
  :group 'bfepm)

;;;###autoload
(defun bfepm-install (package-spec &optional async)
  "Install a package specified by PACKAGE-SPEC.
If ASYNC is non-nil, install asynchronously (non-blocking)."
  (interactive "sPackage: \nP")
  (if bfepm--package-available
      (if async
          (progn
            (message "Starting installation of %s in background..." package-spec)
            (bfepm-package-install-async
             package-spec
             (lambda (success package-name error-msg)
               (if success
                   (message "✓ Successfully installed %s" package-name)
                 (message "✗ Failed to install %s: %s" package-name error-msg)))))
        (bfepm-package-install package-spec))
    (message "Package installation not available (bfepm-package module not loaded)")))

;;;###autoload
(defun bfepm-install-async (package-spec)
  "Install a package specified by PACKAGE-SPEC asynchronously (non-blocking)."
  (interactive "sPackage: ")
  (bfepm-install package-spec t))

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
(defun bfepm-ui-show ()
  "Show the BFEPM package management interface."
  (interactive)
  (bfepm-ui))

;;;###autoload
(defun bfepm-search-packages (query)
  "Search for packages matching QUERY across available archives."
  (interactive "sSearch packages: ")
  (bfepm-search query))

;;;###autoload
(defun bfepm-search-installed (query)
  "Search within installed packages matching QUERY."
  (interactive "sSearch installed packages: ")
  (bfepm-search-installed-packages query))

;;;###autoload
(defun bfepm-init ()
  "Initialize BFEPM in the current Emacs session."
  (interactive)
  (bfepm-core-initialize))

;; Profile management commands

;;;###autoload
(defun bfepm-profile-create-interactive (name &optional base-profile)
  "Create a new profile with NAME, optionally based on BASE-PROFILE."
  (interactive "sProfile name: ")
  (bfepm-profile-create name base-profile))

;;;###autoload
(defun bfepm-profile-switch-interactive (profile-name)
  "Switch to profile PROFILE-NAME."
  (interactive 
   (list (completing-read "Switch to profile: " 
                          (bfepm-profile-list-names)
                          nil t)))
  (bfepm-profile-switch profile-name))

;;;###autoload
(defun bfepm-profile-list-interactive ()
  "List all available profiles."
  (interactive)
  (bfepm-profile-list))

;;;###autoload
(defun bfepm-profile-remove-interactive (profile-name)
  "Remove profile PROFILE-NAME."
  (interactive 
   (list (completing-read "Remove profile: " 
                          (bfepm-profile-list-names)
                          nil t)))
  (bfepm-profile-remove profile-name))

;;;###autoload
(defun bfepm-profile-copy-interactive (source-profile target-profile)
  "Copy SOURCE-PROFILE to TARGET-PROFILE."
  (interactive
   (list (completing-read "Copy from profile: " 
                          (bfepm-profile-list-names) nil t)
         (read-string "Copy to profile: ")))
  (bfepm-profile-copy source-profile target-profile))

;;;###autoload
(defun bfepm-profile-current-interactive ()
  "Show current active profile."
  (interactive)
  (message "Current profile: %s" (bfepm-profile-current)))

(provide 'bfepm)

;;; bfepm.el ends here
