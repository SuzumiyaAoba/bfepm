;;; demo-init.el --- EPM Demo Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file demonstrates how to set up EPM in your Emacs configuration.
;; Copy the relevant parts to your init.el file.

;;; Code:

;; Add EPM to load-path (adjust path as needed)
(add-to-list 'load-path ".")

;; Load EPM with error handling
(condition-case err
    (progn
      (message "[EPM Demo] Loading EPM modules...")
      (require 'epm-utils)
      (message "[EPM Demo] ✅ epm-utils loaded")
      
      (require 'epm-core)
      (message "[EPM Demo] ✅ epm-core loaded")
      
      ;; Set up EPM variables for demo (using temporary directory)
      (setq epm-demo-temp-dir (make-temp-file "epm-demo-" t))
      (setq epm-config-file (expand-file-name "sample/epm.toml"))
      (setq epm-directory epm-demo-temp-dir)
      (message "[EPM Demo] Configuration file set to: %s" epm-config-file)
      (message "[EPM Demo] Demo EPM directory set to: %s" epm-directory)
      (message "[EPM Demo] Note: Using temporary directory for demo (will be cleaned up on exit)")
      
      ;; Load main EPM module (which handles optional dependencies)
      (require 'epm)
      (message "[EPM Demo] ✅ EPM main module loaded")
      
      ;; Initialize EPM
      (condition-case init-err
          (progn
            (epm-init)
            (message "[EPM Demo] ✅ EPM initialized successfully"))
        (error 
         (message "[EPM Demo] ⚠️  EPM initialization had issues: %s" (error-message-string init-err))
         (message "[EPM Demo] Demo will continue with basic functionality"))))
  (error 
   (message "[EPM Demo] ❌ Failed to load EPM: %s" (error-message-string err))
   (message "[EPM Demo] This demo will have limited functionality")))

;; Package descriptions for demo
(defvar epm-demo-package-descriptions
  '(("company" "Modular text completion framework")
    ("vertico" "Vertical interactive completion")
    ("consult" "Consulting completing-read")
    ("marginalia" "Enrich existing commands with completion annotations")
    ("embark" "Conveniently act on minibuffer completions")
    ("orderless" "Completion style for matching regexps in any order")
    ("magit" "A Git porcelain inside Emacs")
    ("which-key" "Display available keybindings in popup")
    ("projectile" "Manage and navigate projects in Emacs easily")
    ("doom-themes" "An opinionated pack of modern color-themes")
    ("helm" "Incremental completion and selection narrowing framework")
    ("ivy" "Incremental completion and selection narrowing framework")
    ("lsp-mode" "Language Server Protocol client")
    ("flycheck" "On-the-fly syntax checking")
    ("yasnippet" "Yet another snippet extension")
    ("org" "Outline-based notes management and organizer")
    ("use-package" "Declaration macro for simplifying your .emacs")
    ("evil" "Extensible Vi layer for Emacs")
    ("treemacs" "Tree style file explorer")
    ("dashboard" "Startup screen extracted from Spacemacs")
    ("rainbow-delimiters" "Highlight delimiters according to their depth")
    ("smartparens" "Automatic insertion, wrapping and paredit-like navigation")
    ("expand-region" "Increase selected region by semantic units")
    ("multiple-cursors" "Multiple cursors for Emacs")
    ("ace-window" "Quickly switch windows"))
  "Package descriptions for demo purposes.")

(defvar epm-demo-packages nil
  "List of packages loaded from sample/epm.toml for demo purposes.")

(defun epm-demo-load-packages-from-toml ()
  "Load package list from sample/epm.toml file."
  (condition-case err
      (let ((config-file (expand-file-name "sample/epm.toml")))
        (if (file-exists-p config-file)
            (let ((packages-with-versions (epm-demo-parse-toml-packages config-file)))
              (setq epm-demo-packages 
                    (mapcar (lambda (pkg-info)
                              (let ((pkg-name (car pkg-info))
                                    (version (cdr pkg-info)))
                                (list pkg-name 
                                      (format "%s (version: %s)" 
                                              (or (cadr (assoc pkg-name epm-demo-package-descriptions))
                                                  "Package from sample/epm.toml")
                                              version))))
                            packages-with-versions))
              (message "[EPM Demo] Loaded %d packages from sample/epm.toml" (length packages-with-versions)))
          (progn
            (message "[EPM Demo] sample/epm.toml not found, using fallback list")
            (epm-demo-use-fallback-packages))))
    (error 
     (message "[EPM Demo] Error loading sample/epm.toml: %s" (error-message-string err))
     (epm-demo-use-fallback-packages))))

(defun epm-demo-use-fallback-packages ()
  "Use fallback package list when sample/epm.toml is not available."
  (setq epm-demo-packages
        '(("company" "Modular text completion framework")
          ("vertico" "Vertical interactive completion")
          ("consult" "Consulting completing-read")
          ("marginalia" "Enrich existing commands with completion annotations")
          ("which-key" "Display available keybindings in popup"))))

(defun epm-demo-parse-toml-packages (file)
  "Simple TOML parser to extract package names and versions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((packages '())
          (in-packages-section nil))
      (while (not (eobp))
        (let ((line (epm-demo-string-trim (buffer-substring-no-properties 
                                            (line-beginning-position) 
                                            (line-end-position)))))
          (cond
           ;; Check for [packages] section
           ((string= line "[packages]")
            (setq in-packages-section t))
           ;; Check for any other sections (including [packages.*.config])
           ((and (string-prefix-p "[" line)
                 (not (string= line "[packages]")))
            (setq in-packages-section nil))
           ;; Parse package lines in [packages] section
           ((and in-packages-section
                 (not (string-prefix-p "#" line))
                 (not (string= line ""))
                 (not (string-prefix-p "[packages." line))  ; Skip config subsections
                 (string-match "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*\"\\([^\"]+\\)\"" line))
            (let ((pkg-name (match-string 1 line))
                  (version (match-string 2 line)))
              (push (cons pkg-name version) packages)))))
        (forward-line 1))
      (reverse packages))))

(defun epm-demo-string-trim (string)
  "Trim whitespace from STRING (compatibility function)."
  (replace-regexp-in-string "\\`[ \t\n\r]+" "" 
                            (replace-regexp-in-string "[ \t\n\r]+\\'" "" string)))

(defun epm-demo-get-popular-package-set ()
  "Get a curated subset of packages for the popular package demo."
  (let ((all-packages (mapcar #'car epm-demo-packages)))
    ;; Select first 5 packages, or core completion packages if available
    (let ((core-packages '("company" "vertico" "consult" "marginalia" "which-key")))
      (or (cl-intersection core-packages all-packages :test #'string=)
          (cl-subseq all-packages 0 (min 5 (length all-packages)))))))

(defun epm-demo-get-package-version (package-name)
  "Get version specification for PACKAGE-NAME from sample/epm.toml."
  (condition-case err
      (let ((config-file (expand-file-name "sample/epm.toml")))
        (if (file-exists-p config-file)
            (let ((packages-with-versions (epm-demo-parse-toml-packages config-file)))
              (or (cdr (assoc package-name packages-with-versions))
                  "latest"))
          "latest"))
    (error "latest")))

;; Load packages from sample/epm.toml at startup
(epm-demo-load-packages-from-toml)

;; Demo functions for interactive testing
(defun epm-demo-install-package ()
  "Demo function to install a selected package."
  (interactive)
  (let* ((package-choices (mapcar (lambda (pkg) 
                                    (format "%s - %s" (car pkg) (cadr pkg)))
                                  epm-demo-packages))
         (selected (completing-read "[EPM Demo] Select package to install: " package-choices))
         (package-name (car (split-string selected " - "))))
    (epm-demo-install-single-package package-name)))

(defun epm-demo-install-multiple-packages ()
  "Demo function to install multiple selected packages."
  (interactive)
  (let* ((package-choices (mapcar (lambda (pkg) 
                                    (format "%s - %s" (car pkg) (cadr pkg)))
                                  epm-demo-packages))
         (selected-packages '())
         (continue t))
    (while continue
      (let ((selected (completing-read 
                       (format "[EPM Demo] Select package (%d selected): " (length selected-packages))
                       package-choices)))
        (if (string= selected "")
            (setq continue nil)
          (let ((package-name (car (split-string selected " - "))))
            (unless (member package-name selected-packages)
              (push package-name selected-packages))
            (setq package-choices (remove selected package-choices))
            (when (null package-choices)
              (setq continue nil))))))
    (if selected-packages
        (progn
          (message "[EPM Demo] Installing %d packages: %s" 
                   (length selected-packages) 
                   (string-join (reverse selected-packages) ", "))
          (dolist (package selected-packages)
            (epm-demo-install-single-package package)))
      (message "[EPM Demo] No packages selected for installation."))))

(defun epm-demo-install-popular-packages ()
  "Demo function to install a curated set of popular packages from sample/epm.toml."
  (interactive)
  (let ((popular-set (epm-demo-get-popular-package-set)))
    (if (y-or-n-p (format "[EPM Demo] Install package set from sample/epm.toml (%s)? " 
                          (string-join popular-set ", ")))
        (progn
          (message "[EPM Demo] Installing package set from sample/epm.toml...")
          (dolist (package popular-set)
            (epm-demo-install-single-package package)))
      (message "[EPM Demo] Package installation cancelled."))))

(defun epm-demo-install-single-package (package-name)
  "Install a single PACKAGE-NAME with demo options."
  (let ((version (epm-demo-get-package-version package-name)))
    (if (y-or-n-p (format "[EPM Demo] Attempt real installation of %s (%s)? This requires internet access. " package-name version))
        (progn
          (message "[EPM Demo] Attempting real installation of %s with version %s..." package-name version)
          (condition-case err
              (progn
                (if (string= version "latest")
                    (epm-install package-name)
                  (epm-install (list package-name version)))
                (message "[EPM Demo] ✅ %s package installation completed" (capitalize package-name)))
            (error 
             (message "[EPM Demo] ❌ Installation of %s failed: %s" package-name (error-message-string err)))))
      (epm-demo-simulate-installation package-name version))))

(defun epm-demo-simulate-installation (package-name &optional version)
  "Simulate installation of PACKAGE-NAME with optional VERSION for demo purposes."
  (let ((mock-version (or version (epm-demo-get-package-version package-name))))
    (message "[EPM Demo] Simulating installation of %s (%s)..." package-name mock-version)
    (message "[EPM Demo] 📦 Finding %s package in MELPA..." package-name)
    (sleep-for 0.5)
    (let ((download-version (if (string= mock-version "latest") "20250426.1319" 
                             (replace-regexp-in-string "^[~^]" "" mock-version))))
      (message "[EPM Demo] 📥 Downloading %s-%s.tar..." package-name download-version)
      (sleep-for 0.5)
      (message "[EPM Demo] 📦 Extracting package files...")
      (sleep-for 0.5)
      (message "[EPM Demo] 🔧 Installing to ~/.emacs.d/epm/packages/%s/..." package-name)
      (sleep-for 0.5)
      
      ;; Create mock package directory and version file for demo
      (let* ((package-dir (expand-file-name package-name (epm-core-get-packages-directory)))
             (version-file (expand-file-name ".epm-version" package-dir)))
        (epm-utils-ensure-directory package-dir)
        (with-temp-file version-file
          (insert (format "%s\n" download-version)))
        ;; Create a mock main file
        (with-temp-file (expand-file-name (format "%s.el" package-name) package-dir)
          (insert (format ";;; %s.el --- Mock package for EPM demo\n" package-name)
                  (format ";; Version: %s\n" download-version)
                  (format ";;; %s.el ends here\n" package-name))))
      
      (message "[EPM Demo] ✅ Mock installation of %s (%s) completed successfully!" 
               (capitalize package-name) mock-version))))

(defun epm-demo-install-company ()
  "Demo function to install company package (legacy function)."
  (interactive)
  (epm-demo-install-single-package "company"))

(defun epm-demo-cleanup ()
  "Clean up temporary demo directory."
  (interactive)
  (when (and (boundp 'epm-demo-temp-dir) 
             (file-directory-p epm-demo-temp-dir))
    (condition-case err
        (progn
          (delete-directory epm-demo-temp-dir t)
          (message "[EPM Demo] ✅ Temporary directory cleaned up: %s" epm-demo-temp-dir))
      (error 
       (message "[EPM Demo] ⚠️  Failed to cleanup temp directory: %s" (error-message-string err))))))

;; Set up automatic cleanup on exit
(when (boundp 'epm-demo-temp-dir)
  (add-hook 'kill-emacs-hook #'epm-demo-cleanup))

(defun epm-demo-install-company-mock ()
  "Demo function to simulate company package installation (for batch mode)."
  (interactive)
  (epm-demo-simulate-installation "company")
  (message "[EPM Demo] Note: This was a simulation for demonstration purposes."))

(defun epm-demo-install-popular-mock ()
  "Demo function to simulate packages installation from sample/epm.toml (for batch mode)."
  (interactive)
  (let ((popular-set (epm-demo-get-popular-package-set)))
    (message "[EPM Demo] Simulating installation of packages from sample/epm.toml...")
    (dolist (package popular-set)
      (epm-demo-simulate-installation package))
    (message "[EPM Demo] ✅ Mock installation of %d packages completed!" (length popular-set))
    (message "[EPM Demo] Note: This was a simulation for demonstration purposes.")))

(defun epm-demo-show-package-list ()
  "Show available packages for demo (for batch mode)."
  (interactive)
  (message "[EPM Demo] Available packages for demo:")
  (let ((counter 1))
    (dolist (pkg epm-demo-packages)
      (message "  %d. %s - %s" counter (car pkg) (cadr pkg))
      (setq counter (1+ counter))))
  (message "[EPM Demo] Use C-c e 1 to select a package interactively")
  (message "[EPM Demo] Use C-c e t to install popular package set")
  (message "[EPM Demo] Use C-c e M to mock install popular packages"))

(defun epm-demo-show-directories ()
  "Show EPM directory locations."
  (interactive)
  (message "[EPM Demo] EPM Directory Locations (TEMPORARY):")
  (message "[EPM Demo] Demo temp directory: %s" (if (boundp 'epm-demo-temp-dir) epm-demo-temp-dir "Not set"))
  (message "[EPM Demo] EPM directory: %s" (if (boundp 'epm-directory) epm-directory "Not set"))
  (when (boundp 'epm-directory)
    (message "[EPM Demo] - Packages: %s/packages/" epm-directory)
    (message "[EPM Demo] - Downloads cache: %s/cache/downloads/" epm-directory)
    (message "[EPM Demo] - Archive cache: %s/cache/" epm-directory))
  (condition-case nil
      (progn
        (message "[EPM Demo] Packages directory: %s" (epm-core-get-packages-directory))
        (message "[EPM Demo] Cache directory: %s" (epm-core-get-cache-directory)))
    (error 
     (message "[EPM Demo] Could not get directory information (EPM not initialized)")))
  (message "[EPM Demo] Note: All demo files will be automatically deleted on exit"))

(defun epm-demo-show-package-info ()
  "Show detailed package information for company."
  (interactive)
  (message "[EPM Demo] Fetching package information for 'company'...")
  (condition-case err
      (if (and (boundp 'epm--package-available) epm--package-available)
          (let* ((sources (epm-package--get-default-sources))
                 (melpa-source (cdr (assoc "melpa" sources)))
                 (package-info (epm-package--find-in-elpa "company" melpa-source)))
            (if package-info
                (progn
                  (message "[EPM Demo] Raw package info: %S" package-info)
                  (let ((info-list (if (vectorp package-info) (append package-info nil) package-info)))
                    (message "[EPM Demo] Version (raw): %S" (car info-list))
                    (message "[EPM Demo] Version (formatted): %S" (epm-package--format-version (car info-list)))
                    (message "[EPM Demo] Dependencies: %S" (cadr info-list))
                    (message "[EPM Demo] Description: %S" (caddr info-list))
                    (message "[EPM Demo] Package type: %S" (cadddr info-list))
                    (let ((archive-url (epm-package--build-archive-url "company" 
                                                                       (epm-package--format-version (car info-list))
                                                                       (cadddr info-list))))
                      (message "[EPM Demo] Download URL would be: %s" archive-url))))
              (message "[EPM Demo] ❌ Package 'company' not found in MELPA")))
        (message "[EPM Demo] ❌ Package functionality not available"))
    (error 
     (message "[EPM Demo] ❌ Error fetching package info: %s" (error-message-string err)))))

(defun epm-demo-install-with-version ()
  "Demo function to install package with version specification."
  (interactive)
  (if (y-or-n-p "[EPM Demo] Test version specification? This will show version handling. ")
      (progn
        (message "[EPM Demo] Testing version specification...")
        (message "[EPM Demo] Available version examples from sample/epm.toml:")
        (message "[EPM Demo] - 'latest' (company)")
        (message "[EPM Demo] - '^20250601' (vertico - compatible version)")
        (message "[EPM Demo] - '~20250520.1200' (consult - patch level)")
        (message "[EPM Demo] - '20250515.800' (marginalia - exact version)")
        (message "[EPM Demo] - '^20240101' (orderless - compatible from 2024)")
        (message "")
        (let ((toml-versions (mapcar (lambda (pkg-info) 
                                      (list (car pkg-info) (cdr pkg-info)))
                                    (epm-demo-parse-toml-packages (expand-file-name "sample/epm.toml"))))
              (test-versions '("latest" "20250426.1319" "20240101.1200" "^20250426" "~20250426.1319")))
          ;; Test actual versions from sample/epm.toml
          (message "[EPM Demo] Testing real versions from sample/epm.toml:")
          (dolist (pkg-version (cl-subseq toml-versions 0 (min 5 (length toml-versions))))
            (let ((package-name (car pkg-version))
                  (version (cadr pkg-version)))
              (message "[EPM Demo] Package %s uses version: %s" package-name version)))
          (message "")
          (message "[EPM Demo] Testing generic version patterns:")
          (dolist (version test-versions)
            (message "[EPM Demo] Testing version specification: %s" version)
            (condition-case err
                (progn
                  (let ((package-spec (list "company" version)))
                    (message "[EPM Demo] Would install: %S" package-spec)
                    (message "[EPM Demo] Checking version compatibility...")
                    ;; Simulate version check without actual installation
                    (let* ((sources (epm-package--get-default-sources))
                           (melpa-source (cdr (assoc "melpa" sources)))
                           (package-info (epm-package--find-in-elpa "company" melpa-source)))
                      (when package-info
                        (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                               (available-version (epm-package--format-version (car info-list))))
                          (message "[EPM Demo] Available version: %s" available-version)
                          (if (epm-package--version-matches-p available-version version)
                              (message "[EPM Demo] ✅ Version %s is compatible" version)
                            (message "[EPM Demo] ❌ Version %s not compatible with %s" version available-version)))))))
              (error 
               (message "[EPM Demo] ❌ Error testing version %s: %s" version (error-message-string err))))
            (message ""))))
    (message "[EPM Demo] Version specification demo skipped.")))

(defun epm-demo-test-version-spec ()
  "Test version specification without user interaction (for batch mode)."
  (interactive)
  (message "[EPM Demo] Testing version specification handling...")
  (let ((test-versions '("latest" "20250426.1319" "20240101.1200" "^20250426")))
    (dolist (version test-versions)
      (message "[EPM Demo] Testing version: %s" version)
      (condition-case err
          (let* ((sources (epm-package--get-default-sources))
                 (melpa-source (cdr (assoc "melpa" sources)))
                 (package-info (epm-package--find-in-elpa "company" melpa-source)))
            (when package-info
              (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                     (available-version (epm-package--format-version (car info-list))))
                (if (epm-package--version-matches-p available-version version)
                    (message "[EPM Demo] ✅ %s: Compatible" version)
                  (message "[EPM Demo] ❌ %s: Not compatible with %s" version available-version)))))
        (error 
         (message "[EPM Demo] ❌ Error testing %s: %s" version (error-message-string err)))))
  (message "[EPM Demo] Version specification test completed.")))

(defun epm-demo-show-config ()
  "Demo function to show current EPM configuration."
  (interactive)
  (message "[EPM Demo] Showing EPM configuration...")
  (condition-case err
      (progn
        (if (boundp 'epm-config-file)
            (progn
              (message "[EPM Demo] Configuration file: %s" epm-config-file)
              (if (file-exists-p epm-config-file)
                  (message "[EPM Demo] ✅ Configuration file found")
                (message "[EPM Demo] ❌ Configuration file not found")))
          (message "[EPM Demo] ❌ epm-config-file variable not set"))
        (if (boundp 'epm-directory)
            (message "[EPM Demo] EPM directory: %s" epm-directory)
          (message "[EPM Demo] ❌ epm-directory variable not set"))
        (if (featurep 'epm)
            (message "[EPM Demo] ✅ EPM module is loaded")
          (message "[EPM Demo] ❌ EPM module not loaded")))
    (error 
     (message "[EPM Demo] ❌ Error checking configuration: %s" (error-message-string err)))))

(defun epm-demo-list-packages ()
  "Demo function to list installed packages."
  (interactive)
  (condition-case err
      (if (and (boundp 'epm--package-available) epm--package-available)
          (progn
            (message "[EPM Demo] Listing installed packages...")
            (epm-list))
        (progn
          (message "[EPM Demo] Listing packages directory manually...")
          (if (boundp 'epm-directory)
              (let ((packages-dir (expand-file-name "packages" epm-directory)))
                (if (file-directory-p packages-dir)
                    (let ((packages (directory-files packages-dir nil "^[^.]")))
                      (if packages
                          (progn
                            (message "[EPM Demo] Installed packages:")
                            (dolist (package packages)
                              (let ((version (epm-core-get-package-version package)))
                                (if (string= version "unknown")
                                    (message "[EPM Demo]   - %s (version unknown)" package)
                                  (message "[EPM Demo]   - %s (%s)" package version)))))
                        (message "[EPM Demo] No packages installed yet")))
                  (message "[EPM Demo] Packages directory not yet created")))
            (message "[EPM Demo] ❌ epm-directory variable not set"))))
    (error 
     (message "[EPM Demo] ❌ Error listing packages: %s" (error-message-string err)))))

(defun epm-demo-test-version ()
  "Demo function to test version comparison."
  (interactive)
  (message "[EPM Demo] Testing version comparison...")
  (condition-case err
      (progn
        (if (featurep 'epm-utils)
            (progn
              (message "[EPM Demo] 1.0.0 vs 1.0.0: %d" (epm-utils-version-compare "1.0.0" "1.0.0"))
              (message "[EPM Demo] 1.0.1 vs 1.0.0: %d" (epm-utils-version-compare "1.0.1" "1.0.0"))
              (message "[EPM Demo] 1.0.0 vs 1.0.1: %d" (epm-utils-version-compare "1.0.0" "1.0.1")))
          (message "[EPM Demo] ❌ epm-utils module not loaded")))
    (error 
     (message "[EPM Demo] ❌ Error testing version comparison: %s" (error-message-string err)))))

(defun epm-demo-help ()
  "Show demo help message."
  (interactive)
  (message "[EPM Demo] Available commands:")
  (message "  === Single Package ===")
  (message "  C-c e c  - Show configuration")
  (message "  C-c e i  - Install company (real download)")
  (message "  C-c e m  - Mock install company (simulation)")
  (message "  === Multiple Packages ===")
  (message "  C-c e 1  - Install selected package")
  (message "  C-c e n  - Install multiple packages")
  (message "  C-c e t  - Install packages from sample/epm.toml")
  (message "  C-c e M  - Mock install packages from sample/epm.toml")
  (message "  C-c e P  - Show available package list")
  (message "  === Package Management ===")
  (message "  C-c e l  - List packages")
  (message "  C-c e d  - Show directory locations (temporary)")
  (message "  C-c e p  - Show package info (version details)")
  (message "  C-c e s  - Test version specification")
  (message "  === System ===")
  (message "  C-c e x  - Cleanup temporary directory")
  (message "  C-c e v  - Test version comparison")
  (message "  C-c e h  - Show this help")
  (message "  === Direct EPM Commands ===")
  (message "  C-c e I  - Interactive install")
  (message "  C-c e R  - Remove package")
  (message "  C-c e U  - Update packages"))

;; Bind demo functions to keys for easy testing
;; Single package
(global-set-key (kbd "C-c e c") #'epm-demo-show-config)
(global-set-key (kbd "C-c e i") #'epm-demo-install-company)
(global-set-key (kbd "C-c e m") #'epm-demo-install-company-mock)
;; Multiple packages
(global-set-key (kbd "C-c e 1") #'epm-demo-install-package)
(global-set-key (kbd "C-c e n") #'epm-demo-install-multiple-packages)
(global-set-key (kbd "C-c e t") #'epm-demo-install-popular-packages)
(global-set-key (kbd "C-c e M") #'epm-demo-install-popular-mock)
(global-set-key (kbd "C-c e P") #'epm-demo-show-package-list)
;; Package management
(global-set-key (kbd "C-c e l") #'epm-demo-list-packages)
(global-set-key (kbd "C-c e d") #'epm-demo-show-directories)
(global-set-key (kbd "C-c e p") #'epm-demo-show-package-info)
(global-set-key (kbd "C-c e s") #'epm-demo-install-with-version)
;; System
(global-set-key (kbd "C-c e x") #'epm-demo-cleanup)
(global-set-key (kbd "C-c e v") #'epm-demo-test-version)
(global-set-key (kbd "C-c e h") #'epm-demo-help)

;; Interactive EPM commands
(global-set-key (kbd "C-c e I") #'epm-install)
(global-set-key (kbd "C-c e R") #'epm-remove)
(global-set-key (kbd "C-c e U") #'epm-update)
(global-set-key (kbd "C-c e L") #'epm-list)

;; Welcome message
(defun epm-demo-welcome ()
  "Display welcome message."
  (with-current-buffer (get-buffer-create "*EPM Demo*")
    (erase-buffer)
    (insert "🎉 Welcome to EPM (Emacs Package Manager) Demo!\n")
    (insert "=============================================\n\n")
    
    ;; Show EPM loading status
    (insert "📊 EPM Status:\n")
    (condition-case nil
        (progn
          (if (featurep 'epm)
              (insert "  ✅ EPM loaded successfully\n")
            (insert "  ❌ EPM not loaded\n"))
          (if (boundp 'epm-config-file)
              (progn
                (insert (format "  📁 Config file: %s\n" epm-config-file))
                (if (file-exists-p epm-config-file)
                    (insert "  ✅ Config file exists\n")
                  (insert "  ⚠️  Config file not found\n")))
            (insert "  ❌ Config file not set\n"))
          (if (boundp 'epm-directory)
              (insert (format "  📂 EPM directory: %s\n" epm-directory))
            (insert "  ❌ EPM directory not set\n"))
          (if (featurep 'epm-utils)
              (insert "  ✅ epm-utils module loaded\n")
            (insert "  ❌ epm-utils module not loaded\n"))
          (if (featurep 'epm-core)
              (insert "  ✅ epm-core module loaded\n")
            (insert "  ❌ epm-core module not loaded\n"))
          (cond 
           ((featurep 'epm-config)
            (insert "  ✅ epm-config module loaded (full TOML support)\n"))
           ((featurep 'epm-config-minimal)
            (insert "  ✅ epm-config-minimal module loaded (basic support)\n"))
           ((and (boundp 'epm--config-available) epm--config-available)
            (insert "  ✅ epm-config available\n"))
           (t
            (insert "  ⚠️  epm-config module not loaded (missing toml.el)\n")))
          (if (featurep 'epm-package)
              (insert "  ✅ epm-package module loaded\n")
            (insert "  ❌ epm-package module not loaded\n"))
          (if (and (boundp 'epm--package-available) epm--package-available)
              (insert "  ✅ Package installation available\n")
            (insert "  ❌ Package installation not available\n")))
      (error 
       (insert "  ⚠️  EPM status check failed\n")))
    (insert "\n")
    
    (insert "EPM is a package manager for Emacs that provides:\n")
    (insert "• Declarative package management with TOML\n")
    (insert "• Version constraints and dependency resolution\n")
    (insert "• Multiple package sources support\n\n")
    (insert "📋 Demo Commands:\n")
    (insert "  === Single Package ===\n")
    (insert "  C-c e i  - Install company package (real)\n")
    (insert "  C-c e m  - Mock install company (safe demo)\n")
    (insert "  === Multiple Packages ===\n")
    (insert "  C-c e 1  - Install selected package\n")
    (insert "  C-c e t  - Install packages from sample/epm.toml\n")
    (insert "  C-c e M  - Mock install packages from sample/epm.toml\n")
    (insert "  C-c e P  - Show available package list\n")
    (insert "  === Management ===\n")
    (insert "  C-c e c  - Show current configuration\n")
    (insert "  C-c e l  - List installed packages\n")
    (insert "  C-c e d  - Show directory locations (temp)\n")
    (insert "  C-c e s  - Test version specification\n")
    (insert "  C-c e h  - Show all commands\n\n")
    (insert "🚀 Try starting with: C-c e 1, C-c e t, or C-c e M\n\n")
    (insert "⚠️  Note: Demo uses temporary directories (auto-cleanup on exit)\n\n")
    (insert "💡 Press C-x C-c to exit when done\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Show welcome message
(run-with-timer 0.5 nil #'epm-demo-welcome)

(message "[EPM Demo] Demo configuration loaded! Press C-c e h for help or C-c e c to check EPM status")

(provide 'demo-init)

;;; demo-init.el ends here