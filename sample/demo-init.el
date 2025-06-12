;;; demo-init.el --- BFEPM Demo Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file demonstrates how to set up BFEPM in your Emacs configuration.
;; Copy the relevant parts to your init.el file.

;;; Code:

;; Add BFEPM to load-path (adjust path as needed)
(add-to-list 'load-path ".")

;; Load BFEPM with error handling
(condition-case err
    (progn
      (message "[BFEPM Demo] Loading BFEPM modules...")
      (require 'bfepm-utils)
      (message "[BFEPM Demo] ✅ bfepm-utils loaded")
      
      (require 'bfepm-core)
      (message "[BFEPM Demo] ✅ bfepm-core loaded")
      
      ;; Try to load bfepm-config, fall back to minimal if toml.el not available
      (condition-case config-err
          (progn
            (require 'bfepm-config)
            (message "[BFEPM Demo] ✅ bfepm-config loaded (full TOML support)"))
        (error 
         (message "[BFEPM Demo] ⚠️  bfepm-config failed, trying minimal: %s" (error-message-string config-err))
         (condition-case minimal-err
             (progn
               (require 'bfepm-config-minimal)
               (message "[BFEPM Demo] ✅ bfepm-config-minimal loaded (basic support)"))
           (error 
            (message "[BFEPM Demo] ❌ Both config modules failed: %s" (error-message-string minimal-err))))))
      
      ;; Set up BFEPM variables for demo (using temporary directory)
      (setq bfepm-demo-temp-dir (make-temp-file "bfepm-demo-" t))
      (setq bfepm-config-file (expand-file-name "sample/bfepm.toml"))
      (setq bfepm-directory bfepm-demo-temp-dir)
      (message "[BFEPM Demo] Configuration file set to: %s" bfepm-config-file)
      (message "[BFEPM Demo] Demo BFEPM directory set to: %s" bfepm-directory)
      (message "[BFEPM Demo] Note: Using temporary directory for demo (will be cleaned up on exit)")
      
      ;; Load BFEPM package module explicitly for demo
      (condition-case err
          (progn
            (require 'bfepm-package)
            (message "[BFEPM Demo] ✅ bfepm-package loaded via require"))
        (error 
         (message "[BFEPM Demo] ❌ Failed to require bfepm-package: %s" err)
         (message "[BFEPM Demo] Trying direct load...")
         (condition-case load-err
             (progn
               (load (expand-file-name "lisp/bfepm-package.el"))
               (message "[BFEPM Demo] ✅ bfepm-package loaded via direct load"))
           (error 
            (message "[BFEPM Demo] ❌ Failed to load bfepm-package.el: %s" load-err)))))
      
      ;; Debug: Check if critical functions are available
      (message "[BFEPM Demo] 🔍 Checking function availability...")
      (message "[BFEPM Demo] Features loaded: %s" features)
      (message "[BFEPM Demo] bfepm-package feature: %s" (featurep 'bfepm-package))
      (message "[BFEPM Demo] bfepm--package-available: %s" (if (boundp 'bfepm--package-available) bfepm--package-available "unbound"))
      
      (if (fboundp 'bfepm-package--find-package)
          (message "[BFEPM Demo] ✅ bfepm-package--find-package function available")
        (progn
          (message "[BFEPM Demo] ❌ bfepm-package--find-package function NOT available")
          (message "[BFEPM Demo] Available bfepm-package functions: %s" 
                   (condition-case err
                       (let ((symbols (all-completions "bfepm-package" obarray)))
                         (cl-remove-if-not (lambda (sym-name) 
                                            (let ((sym (intern sym-name)))
                                              (and (fboundp sym) (string-prefix-p "bfepm-package" sym-name))))
                                          symbols))
                     (error (format "Error listing functions: %s" err))))
          (message "[BFEPM Demo] Attempting to load bfepm-package.el directly...")
          (condition-case err
              (progn
                (load (expand-file-name "lisp/bfepm-package.el"))
                (if (fboundp 'bfepm-package--find-package)
                    (message "[BFEPM Demo] ✅ bfepm-package--find-package function now available after direct load")
                  (message "[BFEPM Demo] ❌ bfepm-package--find-package function STILL not available")))
            (error (message "[BFEPM Demo] ❌ Error loading bfepm-package.el: %s" err)))))
      
      (if (fboundp 'bfepm-package-install)
          (message "[BFEPM Demo] ✅ bfepm-package-install function available")
        (message "[BFEPM Demo] ❌ bfepm-package-install function NOT available"))
      
      ;; Load main BFEPM module (which handles optional dependencies)
      (require 'bfepm)
      (message "[BFEPM Demo] ✅ BFEPM main module loaded")
      
      ;; Initialize BFEPM
      (condition-case init-err
          (progn
            (bfepm-init)
            (message "[BFEPM Demo] ✅ BFEPM initialized successfully")
            
            ;; For demo: Initialize and add git packages to demo list
            ;; This ensures git packages are always available in demo for demonstration
            (message "[BFEPM Demo] Initializing demo package list...")
            ;; Ensure bfepm-demo-packages is initialized
            (unless (boundp 'bfepm-demo-packages)
              (setq bfepm-demo-packages nil))
            (message "[BFEPM Demo] Adding git packages to demo package list...")
            (let ((git-packages-for-demo
                   '(("straight-el" "Package manager with reproducible build and branch management (git:https://github.com/radian-software/straight.el.git)")
                     ("emacs-async" "Asynchronous processing in Emacs (git:https://github.com/jwiegley/emacs-async.git)")
                     ("doom-modeline" "Fancy and fast mode-line inspired by minimalism design (git:https://github.com/seagle0128/doom-modeline.git)"))))
              ;; Add to demo packages list for display
              (setq bfepm-demo-packages (append bfepm-demo-packages git-packages-for-demo))
              (message "[BFEPM Demo] ✅ Git packages added to demo list (%d packages)" (length git-packages-for-demo)))
            
            ;; Also try to add to actual config if possible (optional)
            (condition-case config-err
                (when (and (featurep 'bfepm-config-minimal) (not (featurep 'bfepm-config)))
                  (let ((config (bfepm-core-get-config)))
                    (when config
                      (let ((git-packages 
                             (list
                              (make-bfepm-package :name "straight-el" 
                                                 :version "develop"
                                                 :source (list :url "https://github.com/radian-software/straight.el.git" 
                                                              :type "git" :ref "develop")
                                                 :status 'required)
                              (make-bfepm-package :name "emacs-async" 
                                                 :version "v1.9.8"
                                                 :source (list :url "https://github.com/jwiegley/emacs-async.git" 
                                                              :type "git" :ref "v1.9.8")
                                                 :status 'required)
                              (make-bfepm-package :name "doom-modeline" 
                                                 :version "4.3.0"
                                                 :source (list :url "https://github.com/seagle0128/doom-modeline.git" 
                                                              :type "git" :ref "4.3.0")
                                                 :status 'required))))
                        ;; Use direct slot assignment instead of setf to avoid dependency issues
                        (let ((current-packages (bfepm-config-packages config)))
                          (setq config (make-bfepm-config 
                                       :packages (append current-packages git-packages)
                                       :sources (bfepm-config-sources config))))
                        (message "[BFEPM Demo] ✅ Git packages also added to configuration")))))
              (error
               (message "[BFEPM Demo] ⚠️  Could not add git packages to config: %s" (error-message-string config-err)))))
            
            ;; Load remaining packages from TOML after git packages are added
            (bfepm-demo-load-packages-from-toml))
        (error 
         (message "[BFEPM Demo] ⚠️  BFEPM initialization had issues: %s" (error-message-string init-err))
         (message "[BFEPM Demo] Demo will continue with basic functionality")
         ;; Ensure variable is initialized even on error
         (unless (boundp 'bfepm-demo-packages)
           (setq bfepm-demo-packages nil))
         ;; Load fallback packages even if initialization fails
         (bfepm-demo-use-fallback-packages)))
  (error 
   (message "[BFEPM Demo] ❌ Failed to load BFEPM: %s" (error-message-string err))
   (message "[BFEPM Demo] This demo will have limited functionality")
   ;; Ensure variable is initialized even on complete failure
   (unless (boundp 'bfepm-demo-packages)
     (setq bfepm-demo-packages nil))
   (bfepm-demo-use-fallback-packages))))

;; Package descriptions for demo
(defvar bfepm-demo-package-descriptions
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
    ("ace-window" "Quickly switch windows")
    ("straight-el" "Package manager with reproducible build and branch management")
    ("emacs-async" "Asynchronous processing in Emacs")
    ("doom-modeline" "Fancy and fast mode-line inspired by minimalism design"))
  "Package descriptions for demo purposes.")

(defvar bfepm-demo-packages nil
  "List of packages loaded from sample/bfepm.toml for demo purposes.")

(defun bfepm-demo-load-packages-from-toml ()
  "Load package list from sample/bfepm.toml file or BFEPM configuration."
  (condition-case err
      (let ((config (bfepm-core-get-config)))
        (if (and config (bfepm-config-packages config))
            ;; Load from BFEPM configuration (includes git packages)
            (progn
              (setq bfepm-demo-packages 
                    (mapcar (lambda (pkg)
                              (let* ((pkg-name (bfepm-package-name pkg))
                                     (version (bfepm-package-version pkg))
                                     (source (bfepm-package-source pkg))
                                     (version-display (if (and source (plist-get source :url))
                                                         (format "git:%s" (plist-get source :url))
                                                       version)))
                                (list pkg-name 
                                      (format "%s (version: %s)" 
                                              (or (cadr (assoc pkg-name bfepm-demo-package-descriptions))
                                                  "Package from configuration")
                                              version-display))))
                            (bfepm-config-packages config)))
              (message "[BFEPM Demo] Loaded %d packages from BFEPM configuration" (length (bfepm-config-packages config))))
          ;; Fallback to TOML parsing if available
          (let ((config-file (expand-file-name "sample/bfepm.toml")))
            (if (file-exists-p config-file)
                (let ((packages-with-versions (bfepm-demo-parse-toml-packages config-file)))
                  (setq bfepm-demo-packages 
                        (mapcar (lambda (pkg-info)
                                  (let ((pkg-name (car pkg-info))
                                        (version (cdr pkg-info)))
                                    (list pkg-name 
                                          (format "%s (version: %s)" 
                                                  (or (cadr (assoc pkg-name bfepm-demo-package-descriptions))
                                                      "Package from sample/bfepm.toml")
                                                  version))))
                                packages-with-versions))
                  (message "[BFEPM Demo] Loaded %d packages from sample/bfepm.toml" (length packages-with-versions)))
              (progn
                (message "[BFEPM Demo] No configuration available, using fallback list")
                (bfepm-demo-use-fallback-packages))))))
    (error 
     (message "[BFEPM Demo] Error loading packages: %s" (error-message-string err))
     (bfepm-demo-use-fallback-packages))))

(defun bfepm-demo-use-fallback-packages ()
  "Use fallback package list when sample/bfepm.toml is not available."
  (setq bfepm-demo-packages
        '(("company" "Modular text completion framework")
          ("vertico" "Vertical interactive completion")
          ("consult" "Consulting completing-read")
          ("marginalia" "Enrich existing commands with completion annotations")
          ("which-key" "Display available keybindings in popup"))))

(defun bfepm-demo-parse-toml-packages (file)
  "Simple TOML parser to extract package names and versions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((packages '())
          (in-packages-section nil))
      (while (not (eobp))
        (let ((line (bfepm-demo-string-trim (buffer-substring-no-properties 
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
                 (not (string-prefix-p "[packages." line)))  ; Skip config subsections
            (cond
             ;; Handle git packages: package = { git = "url", ... }
             ((string-match "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*{.*git\\s-*=\\s-*\"\\([^\"]+\\)\".*}" line)
              (let ((pkg-name (match-string 1 line))
                    (git-url (match-string 2 line)))
                (push (cons pkg-name (format "git:%s" git-url)) packages)))
             ;; Handle regular packages: package = "version"
             ((string-match "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*\"\\([^\"]+\\)\"" line)
              (let ((pkg-name (match-string 1 line))
                    (version (match-string 2 line)))
                (push (cons pkg-name version) packages)))))))
        (forward-line 1))
      (reverse packages))))

(defun bfepm-demo-string-trim (string)
  "Trim whitespace from STRING (compatibility function)."
  (replace-regexp-in-string "\\`[ \t\n\r]+" "" 
                            (replace-regexp-in-string "[ \t\n\r]+\\'" "" string)))

(defun bfepm-demo-get-popular-package-set ()
  "Get a curated subset of packages for the popular package demo."
  (let ((all-packages (mapcar #'car bfepm-demo-packages)))
    ;; Select first 5 packages, or core completion packages if available
    (let ((core-packages '("company" "vertico" "consult" "marginalia" "which-key")))
      (or (cl-intersection core-packages all-packages :test #'string=)
          (cl-subseq all-packages 0 (min 5 (length all-packages)))))))

(defun bfepm-demo-get-package-version (package-name)
  "Get version specification for PACKAGE-NAME from demo packages or config."
  (condition-case err
      (let ((package-info (assoc package-name bfepm-demo-packages)))
        (if package-info
            ;; Extract version from package description  
            (let ((description (cadr package-info)))
              (if (string-match "(git:\\([^)]+\\))" description)
                  (format "git:%s" (match-string 1 description))
                (if (string-match "(version: \\([^)]+\\))" description)
                    (match-string 1 description)
                  "latest")))
          ;; Fallback to TOML parsing
          (let ((config-file (expand-file-name "sample/bfepm.toml")))
            (if (file-exists-p config-file)
                (let ((packages-with-versions (bfepm-demo-parse-toml-packages config-file)))
                  (or (cdr (assoc package-name packages-with-versions))
                      "latest"))
              "latest"))))
    (error "latest")))

;; Note: Package loading is now handled during BFEPM initialization

;; Demo functions for interactive testing
(defun bfepm-demo-install-package ()
  "Demo function to install a selected package."
  (interactive)
  (let* ((package-choices (mapcar (lambda (pkg) 
                                    (format "%s - %s" (car pkg) (cadr pkg)))
                                  bfepm-demo-packages))
         (selected (completing-read "[BFEPM Demo] Select package to install: " package-choices))
         (package-name (car (split-string selected " - "))))
    (bfepm-demo-install-single-package package-name)))

(defun bfepm-demo-install-multiple-packages ()
  "Demo function to install multiple selected packages."
  (interactive)
  (let* ((package-choices (mapcar (lambda (pkg) 
                                    (format "%s - %s" (car pkg) (cadr pkg)))
                                  bfepm-demo-packages))
         (selected-packages '())
         (continue t))
    (while continue
      (let ((selected (completing-read 
                       (format "[BFEPM Demo] Select package (%d selected): " (length selected-packages))
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
          (message "[BFEPM Demo] Installing %d packages: %s" 
                   (length selected-packages) 
                   (string-join (reverse selected-packages) ", "))
          (dolist (package selected-packages)
            (bfepm-demo-install-single-package package)))
      (message "[BFEPM Demo] No packages selected for installation."))))

(defun bfepm-demo-install-popular-packages ()
  "Demo function to install a curated set of popular packages from sample/bfepm.toml."
  (interactive)
  (let ((popular-set (bfepm-demo-get-popular-package-set)))
    (if (y-or-n-p (format "[BFEPM Demo] Install package set from sample/bfepm.toml (%s)? " 
                          (string-join popular-set ", ")))
        (progn
          (message "[BFEPM Demo] Installing package set from sample/bfepm.toml...")
          (dolist (package popular-set)
            (bfepm-demo-install-single-package package)))
      (message "[BFEPM Demo] Package installation cancelled."))))

(defun bfepm-demo-install-single-package (package-name)
  "Install a single PACKAGE-NAME with demo options."
  (let ((version (bfepm-demo-get-package-version package-name)))
    (if (y-or-n-p (format "[BFEPM Demo] Attempt real installation of %s (%s)? This requires internet access. " package-name version))
        (progn
          (message "[BFEPM Demo] Attempting real installation of %s with version %s..." package-name version)
          (condition-case err
              (progn
                ;; Demo-specific safe installation approach
                ;; Always use simulation for git packages to avoid function dependency issues
                (if (string-prefix-p "git:" version)
                    (progn
                      (message "[BFEPM Demo] ✅ Git package detected, using enhanced simulation for %s" package-name)
                      (bfepm-demo-simulate-installation package-name version))
                  ;; For regular packages, try real installation with comprehensive error handling
                  (condition-case install-err
                      (if (and (fboundp 'bfepm-install) 
                               (fboundp 'bfepm-package-install)
                               (fboundp 'bfepm-package--find-package))
                          (progn
                            (message "[BFEPM Demo] ✅ All required functions available, attempting real installation...")
                            (bfepm-install (if (string= version "latest") package-name (list package-name version)))
                            (message "[BFEPM Demo] ✅ %s package installation completed" (capitalize package-name)))
                        (progn
                          (message "[BFEPM Demo] ❌ Required functions not available, using simulation")
                          (bfepm-demo-simulate-installation package-name version)))
                    (error
                     (message "[BFEPM Demo] ❌ Installation failed (%s), falling back to simulation" (error-message-string install-err))
                     (bfepm-demo-simulate-installation package-name version)))))
            (error 
             (message "[BFEPM Demo] ❌ Installation of %s failed: %s" package-name (error-message-string err))
             (message "[BFEPM Demo] Falling back to simulation...")
             (bfepm-demo-simulate-installation package-name version))))
      (bfepm-demo-simulate-installation package-name version))))

(defun bfepm-demo-simulate-installation (package-name &optional version)
  "Simulate installation of PACKAGE-NAME with optional VERSION for demo purposes."
  (let ((mock-version (or version (bfepm-demo-get-package-version package-name))))
    (message "[BFEPM Demo] Simulating installation of %s (%s)..." package-name mock-version)
    (if (string-prefix-p "git:" mock-version)
        (progn
          (message "[BFEPM Demo] 📦 Simulating git package installation...")
          (let ((git-url (substring mock-version 4))) ; Remove "git:" prefix
            (message "[BFEPM Demo] 🔗 Cloning repository: %s" git-url)
            (sleep-for 0.5)
            (message "[BFEPM Demo] 📥 Checking out specified branch/tag...")
            (sleep-for 0.5)))
      (message "[BFEPM Demo] 📦 Finding %s package in MELPA..." package-name))
    (sleep-for 0.5)
    (let ((download-version (if (string= mock-version "latest") "20250426.1319" 
                             (replace-regexp-in-string "^[~^]" "" mock-version))))
      (message "[BFEPM Demo] 📥 Downloading %s-%s.tar..." package-name download-version)
      (sleep-for 0.5)
      (message "[BFEPM Demo] 📦 Extracting package files...")
      (sleep-for 0.5)
      (message "[BFEPM Demo] 🔧 Installing to ~/.emacs.d/bfepm/packages/%s/..." package-name)
      (sleep-for 0.5)
      
      ;; Create mock package directory and version file for demo
      (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
             (version-file (expand-file-name ".bfepm-version" package-dir)))
        (bfepm-utils-ensure-directory package-dir)
        (with-temp-file version-file
          (insert (format "%s\n" download-version)))
        ;; Create a mock main file
        (with-temp-file (expand-file-name (format "%s.el" package-name) package-dir)
          (insert (format ";;; %s.el --- Mock package for BFEPM demo\n" package-name)
                  (format ";; Version: %s\n" download-version)
                  (format ";;; %s.el ends here\n" package-name))))
      
      (message "[BFEPM Demo] ✅ Mock installation of %s (%s) completed successfully!" 
               (capitalize package-name) mock-version))))

(defun bfepm-demo-install-company ()
  "Demo function to install company package (legacy function)."
  (interactive)
  (bfepm-demo-install-single-package "company"))

(defun bfepm-demo-cleanup ()
  "Clean up temporary demo directory."
  (interactive)
  (when (and (boundp 'bfepm-demo-temp-dir) 
             (file-directory-p bfepm-demo-temp-dir))
    (condition-case err
        (progn
          (delete-directory bfepm-demo-temp-dir t)
          (message "[BFEPM Demo] ✅ Temporary directory cleaned up: %s" bfepm-demo-temp-dir))
      (error 
       (message "[BFEPM Demo] ⚠️  Failed to cleanup temp directory: %s" (error-message-string err))))))

;; Set up automatic cleanup on exit
(when (boundp 'bfepm-demo-temp-dir)
  (add-hook 'kill-emacs-hook #'bfepm-demo-cleanup))

(defun bfepm-demo-install-company-mock ()
  "Demo function to simulate company package installation (for batch mode)."
  (interactive)
  (bfepm-demo-simulate-installation "company")
  (message "[BFEPM Demo] Note: This was a simulation for demonstration purposes."))

(defun bfepm-demo-install-popular-mock ()
  "Demo function to simulate packages installation from sample/bfepm.toml (for batch mode)."
  (interactive)
  (let ((popular-set (bfepm-demo-get-popular-package-set)))
    (message "[BFEPM Demo] Simulating installation of packages from sample/bfepm.toml...")
    (dolist (package popular-set)
      (bfepm-demo-simulate-installation package))
    (message "[BFEPM Demo] ✅ Mock installation of %d packages completed!" (length popular-set))
    (message "[BFEPM Demo] Note: This was a simulation for demonstration purposes.")))

(defun bfepm-demo-show-package-list ()
  "Show available packages for demo (for batch mode)."
  (interactive)
  (message "[BFEPM Demo] Available packages for demo:")
  (let ((counter 1))
    (dolist (pkg bfepm-demo-packages)
      (message "  %d. %s - %s" counter (car pkg) (cadr pkg))
      (setq counter (1+ counter))))
  (message "[BFEPM Demo] Use C-c e 1 to select a package interactively")
  (message "[BFEPM Demo] Use C-c e t to install popular package set")
  (message "[BFEPM Demo] Use C-c e M to mock install popular packages"))

(defun bfepm-demo-show-directories ()
  "Show EPM directory locations."
  (interactive)
  (message "[BFEPM Demo] EPM Directory Locations (TEMPORARY):")
  (message "[BFEPM Demo] Demo temp directory: %s" (if (boundp 'bfepm-demo-temp-dir) bfepm-demo-temp-dir "Not set"))
  (message "[BFEPM Demo] EPM directory: %s" (if (boundp 'bfepm-directory) bfepm-directory "Not set"))
  (when (boundp 'bfepm-directory)
    (message "[BFEPM Demo] - Packages: %s/packages/" bfepm-directory)
    (message "[BFEPM Demo] - Downloads cache: %s/cache/downloads/" bfepm-directory)
    (message "[BFEPM Demo] - Archive cache: %s/cache/" bfepm-directory))
  (condition-case nil
      (progn
        (message "[BFEPM Demo] Packages directory: %s" (bfepm-core-get-packages-directory))
        (message "[BFEPM Demo] Cache directory: %s" (bfepm-core-get-cache-directory)))
    (error 
     (message "[BFEPM Demo] Could not get directory information (BFEPM not initialized)")))
  (message "[BFEPM Demo] Note: All demo files will be automatically deleted on exit"))

(defun bfepm-demo-show-package-info ()
  "Show detailed package information for company."
  (interactive)
  (message "[BFEPM Demo] Fetching package information for 'company'...")
  (condition-case err
      (if (and (boundp 'bfepm--package-available) bfepm--package-available)
          (let* ((sources (bfepm-package--get-default-sources))
                 (melpa-source (cdr (assoc "melpa" sources)))
                 (package-info (bfepm-package--find-in-elpa "company" melpa-source)))
            (if package-info
                (progn
                  (message "[BFEPM Demo] Raw package info: %S" package-info)
                  (let ((info-list (if (vectorp package-info) (append package-info nil) package-info)))
                    (message "[BFEPM Demo] Version (raw): %S" (car info-list))
                    (message "[BFEPM Demo] Version (formatted): %S" (bfepm-package--format-version (car info-list)))
                    (message "[BFEPM Demo] Dependencies: %S" (cadr info-list))
                    (message "[BFEPM Demo] Description: %S" (caddr info-list))
                    (message "[BFEPM Demo] Package type: %S" (cadddr info-list))
                    (let ((archive-url (bfepm-package--build-archive-url "company" 
                                                                       (bfepm-package--format-version (car info-list))
                                                                       (cadddr info-list))))
                      (message "[BFEPM Demo] Download URL would be: %s" archive-url))))
              (message "[BFEPM Demo] ❌ Package 'company' not found in MELPA")))
        (message "[BFEPM Demo] ❌ Package functionality not available"))
    (error 
     (message "[BFEPM Demo] ❌ Error fetching package info: %s" (error-message-string err)))))

(defun bfepm-demo-install-with-version ()
  "Demo function to install package with version specification."
  (interactive)
  (if (y-or-n-p "[BFEPM Demo] Test version specification? This will show version handling. ")
      (progn
        (message "[BFEPM Demo] Testing version specification...")
        (message "[BFEPM Demo] Available version examples from sample/bfepm.toml:")
        (message "[BFEPM Demo] - 'latest' (company)")
        (message "[BFEPM Demo] - '^20250601' (vertico - compatible version)")
        (message "[BFEPM Demo] - '~20250520.1200' (consult - patch level)")
        (message "[BFEPM Demo] - '20250515.800' (marginalia - exact version)")
        (message "[BFEPM Demo] - '^20240101' (orderless - compatible from 2024)")
        (message "")
        (let ((toml-versions (mapcar (lambda (pkg-info) 
                                      (list (car pkg-info) (cdr pkg-info)))
                                    (bfepm-demo-parse-toml-packages (expand-file-name "sample/bfepm.toml"))))
              (test-versions '("latest" "20250426.1319" "20240101.1200" "^20250426" "~20250426.1319")))
          ;; Test actual versions from sample/bfepm.toml
          (message "[BFEPM Demo] Testing real versions from sample/bfepm.toml:")
          (dolist (pkg-version (cl-subseq toml-versions 0 (min 5 (length toml-versions))))
            (let ((package-name (car pkg-version))
                  (version (cadr pkg-version)))
              (message "[BFEPM Demo] Package %s uses version: %s" package-name version)))
          (message "")
          (message "[BFEPM Demo] Testing generic version patterns:")
          (dolist (version test-versions)
            (message "[BFEPM Demo] Testing version specification: %s" version)
            (condition-case err
                (progn
                  (let ((package-spec (list "company" version)))
                    (message "[BFEPM Demo] Would install: %S" package-spec)
                    (message "[BFEPM Demo] Checking version compatibility...")
                    ;; Simulate version check without actual installation
                    (let* ((sources (bfepm-package--get-default-sources))
                           (melpa-source (cdr (assoc "melpa" sources)))
                           (package-info (bfepm-package--find-in-elpa "company" melpa-source)))
                      (when package-info
                        (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                               (available-version (bfepm-package--format-version (car info-list))))
                          (message "[BFEPM Demo] Available version: %s" available-version)
                          (if (bfepm-package--version-matches-p available-version version)
                              (message "[BFEPM Demo] ✅ Version %s is compatible" version)
                            (message "[BFEPM Demo] ❌ Version %s not compatible with %s" version available-version)))))))
              (error 
               (message "[BFEPM Demo] ❌ Error testing version %s: %s" version (error-message-string err))))
            (message ""))))
    (message "[BFEPM Demo] Version specification demo skipped.")))

(defun bfepm-demo-test-version-spec ()
  "Test version specification without user interaction (for batch mode)."
  (interactive)
  (message "[BFEPM Demo] Testing version specification handling...")
  (let ((test-versions '("latest" "20250426.1319" "20240101.1200" "^20250426")))
    (dolist (version test-versions)
      (message "[BFEPM Demo] Testing version: %s" version)
      (condition-case err
          (let* ((sources (bfepm-package--get-default-sources))
                 (melpa-source (cdr (assoc "melpa" sources)))
                 (package-info (bfepm-package--find-in-elpa "company" melpa-source)))
            (when package-info
              (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                     (available-version (bfepm-package--format-version (car info-list))))
                (if (bfepm-package--version-matches-p available-version version)
                    (message "[BFEPM Demo] ✅ %s: Compatible" version)
                  (message "[BFEPM Demo] ❌ %s: Not compatible with %s" version available-version)))))
        (error 
         (message "[BFEPM Demo] ❌ Error testing %s: %s" version (error-message-string err)))))
  (message "[BFEPM Demo] Version specification test completed.")))

(defun bfepm-demo-show-config ()
  "Demo function to show current EPM configuration."
  (interactive)
  (message "[BFEPM Demo] Showing EPM configuration...")
  (condition-case err
      (progn
        (if (boundp 'bfepm-config-file)
            (progn
              (message "[BFEPM Demo] Configuration file: %s" bfepm-config-file)
              (if (file-exists-p bfepm-config-file)
                  (message "[BFEPM Demo] ✅ Configuration file found")
                (message "[BFEPM Demo] ❌ Configuration file not found")))
          (message "[BFEPM Demo] ❌ bfepm-config-file variable not set"))
        (if (boundp 'bfepm-directory)
            (message "[BFEPM Demo] EPM directory: %s" bfepm-directory)
          (message "[BFEPM Demo] ❌ bfepm-directory variable not set"))
        (if (featurep 'bfepm)
            (message "[BFEPM Demo] ✅ BFEPM module is loaded")
          (message "[BFEPM Demo] ❌ BFEPM module not loaded")))
    (error 
     (message "[BFEPM Demo] ❌ Error checking configuration: %s" (error-message-string err)))))

(defun bfepm-demo-list-packages ()
  "Demo function to list installed packages."
  (interactive)
  (condition-case err
      (if (and (boundp 'bfepm--package-available) bfepm--package-available)
          (progn
            (message "[BFEPM Demo] Listing installed packages...")
            (bfepm-list))
        (progn
          (message "[BFEPM Demo] Listing packages directory manually...")
          (if (boundp 'bfepm-directory)
              (let ((packages-dir (expand-file-name "packages" bfepm-directory)))
                (if (file-directory-p packages-dir)
                    (let ((packages (directory-files packages-dir nil "^[^.]")))
                      (if packages
                          (progn
                            (message "[BFEPM Demo] Installed packages:")
                            (dolist (package packages)
                              (let ((version (bfepm-core-get-package-version package)))
                                (if (string= version "unknown")
                                    (message "[BFEPM Demo]   - %s (version unknown)" package)
                                  (message "[BFEPM Demo]   - %s (%s)" package version)))))
                        (message "[BFEPM Demo] No packages installed yet")))
                  (message "[BFEPM Demo] Packages directory not yet created")))
            (message "[BFEPM Demo] ❌ bfepm-directory variable not set"))))
    (error 
     (message "[BFEPM Demo] ❌ Error listing packages: %s" (error-message-string err)))))

(defun bfepm-demo-test-version ()
  "Demo function to test version comparison."
  (interactive)
  (message "[BFEPM Demo] Testing version comparison...")
  (condition-case err
      (progn
        (if (featurep 'bfepm-utils)
            (progn
              (message "[BFEPM Demo] 1.0.0 vs 1.0.0: %d" (bfepm-utils-version-compare "1.0.0" "1.0.0"))
              (message "[BFEPM Demo] 1.0.1 vs 1.0.0: %d" (bfepm-utils-version-compare "1.0.1" "1.0.0"))
              (message "[BFEPM Demo] 1.0.0 vs 1.0.1: %d" (bfepm-utils-version-compare "1.0.0" "1.0.1")))
          (message "[BFEPM Demo] ❌ bfepm-utils module not loaded")))
    (error 
     (message "[BFEPM Demo] ❌ Error testing version comparison: %s" (error-message-string err)))))

(defun bfepm-ui-show ()
  "Open BFEPM UI showing available packages (demo version)."
  (interactive)
  (if (featurep 'bfepm-ui)
      (bfepm-ui-show-available-external)
    (message "[BFEPM Demo] UI not available")))

(defun bfepm-demo-help ()
  "Show demo help message."
  (interactive)
  (message "[BFEPM Demo] Available commands:")
  (message "  === Single Package ===")
  (message "  C-c e c  - Show configuration")
  (message "  C-c e i  - Install company (real download)")
  (message "  C-c e m  - Mock install company (simulation)")
  (message "  === Multiple Packages ===")
  (message "  C-c e 1  - Install selected package")
  (message "  C-c e n  - Install multiple packages")
  (message "  C-c e t  - Install packages from sample/bfepm.toml")
  (message "  C-c e M  - Mock install packages from sample/bfepm.toml")
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
  (message "  === Direct BFEPM Commands ===")
  (message "  C-c e I  - Interactive install")
  (message "  C-c e R  - Remove package")
  (message "  C-c e U  - Update packages")
  (message "  === BFEPM UI ===")
  (message "  C-c e g  - Open BFEPM package management UI (available packages)")
  (message "  C-c e G  - Open BFEPM UI (available packages direct)"))

;; Bind demo functions to keys for easy testing
;; Single package
(global-set-key (kbd "C-c e c") #'bfepm-demo-show-config)
(global-set-key (kbd "C-c e i") #'bfepm-demo-install-company)
(global-set-key (kbd "C-c e m") #'bfepm-demo-install-company-mock)
;; Multiple packages
(global-set-key (kbd "C-c e 1") #'bfepm-demo-install-package)
(global-set-key (kbd "C-c e n") #'bfepm-demo-install-multiple-packages)
(global-set-key (kbd "C-c e t") #'bfepm-demo-install-popular-packages)
(global-set-key (kbd "C-c e M") #'bfepm-demo-install-popular-mock)
(global-set-key (kbd "C-c e P") #'bfepm-demo-show-package-list)
;; Package management
(global-set-key (kbd "C-c e l") #'bfepm-demo-list-packages)
(global-set-key (kbd "C-c e d") #'bfepm-demo-show-directories)
(global-set-key (kbd "C-c e p") #'bfepm-demo-show-package-info)
(global-set-key (kbd "C-c e s") #'bfepm-demo-install-with-version)
;; System
(global-set-key (kbd "C-c e x") #'bfepm-demo-cleanup)
(global-set-key (kbd "C-c e v") #'bfepm-demo-test-version)
(global-set-key (kbd "C-c e h") #'bfepm-demo-help)

;; Interactive BFEPM commands
(global-set-key (kbd "C-c e I") #'bfepm-install)
(global-set-key (kbd "C-c e R") #'bfepm-remove)
(global-set-key (kbd "C-c e U") #'bfepm-update)
(global-set-key (kbd "C-c e L") #'bfepm-list)

;; BFEPM UI commands
(when (featurep 'bfepm-ui)
  (global-set-key (kbd "C-c e g") #'bfepm-ui-show)  ; Demo version that shows available packages
  (global-set-key (kbd "C-c e G") #'bfepm-ui-show-available-external))

;; Welcome message
(defun bfepm-demo-welcome ()
  "Display welcome message."
  (with-current-buffer (get-buffer-create "*BFEPM Demo*")
    (erase-buffer)
    (insert "🎉 Welcome to BFEPM (Better Fast Emacs Package Manager) Demo!\n")
    (insert "=============================================\n\n")
    
    ;; Show EPM loading status
    (insert "📊 BFBFEPM Status:\n")
    (condition-case nil
        (progn
          (if (featurep 'bfepm)
              (insert "  ✅ BFEPM loaded successfully\n")
            (insert "  ❌ BFEPM not loaded\n"))
          (if (boundp 'bfepm-config-file)
              (progn
                (insert (format "  📁 Config file: %s\n" bfepm-config-file))
                (if (file-exists-p bfepm-config-file)
                    (insert "  ✅ Config file exists\n")
                  (insert "  ⚠️  Config file not found\n")))
            (insert "  ❌ Config file not set\n"))
          (if (boundp 'bfepm-directory)
              (insert (format "  📂 BFEPM directory: %s\n" bfepm-directory))
            (insert "  ❌ BFEPM directory not set\n"))
          (if (featurep 'bfepm-utils)
              (insert "  ✅ bfepm-utils module loaded\n")
            (insert "  ❌ bfepm-utils module not loaded\n"))
          (if (featurep 'bfepm-core)
              (insert "  ✅ bfepm-core module loaded\n")
            (insert "  ❌ bfepm-core module not loaded\n"))
          (cond 
           ((featurep 'bfepm-config)
            (insert "  ✅ bfepm-config module loaded (full TOML support)\n"))
           ((featurep 'bfepm-config-minimal)
            (insert "  ✅ bfepm-config-minimal module loaded (basic support)\n"))
           ((and (boundp 'bfepm--config-available) bfepm--config-available)
            (insert "  ✅ bfepm-config available\n"))
           (t
            (insert "  ⚠️  bfepm-config module not loaded (missing toml.el)\n")))
          (if (featurep 'bfepm-package)
              (insert "  ✅ bfepm-package module loaded\n")
            (insert "  ❌ bfepm-package module not loaded\n"))
          (if (and (boundp 'bfepm--package-available) bfepm--package-available)
              (insert "  ✅ Package installation available\n")
            (insert "  ❌ Package installation not available\n")))
      (error 
       (insert "  ⚠️  BFEPM status check failed\n")))
    (insert "\n")
    
    (insert "BFEPM is a package manager for Emacs that provides:\n")
    (insert "• Declarative package management with TOML\n")
    (insert "• Version constraints and dependency resolution\n")
    (insert "• Multiple package sources support\n\n")
    (insert "📋 Demo Commands:\n")
    (insert "  === Single Package ===\n")
    (insert "  C-c e i  - Install company package (real)\n")
    (insert "  C-c e m  - Mock install company (safe demo)\n")
    (insert "  === Multiple Packages ===\n")
    (insert "  C-c e 1  - Install selected package\n")
    (insert "  C-c e t  - Install packages from sample/bfepm.toml\n")
    (insert "  C-c e M  - Mock install packages from sample/bfepm.toml\n")
    (insert "  C-c e P  - Show available package list\n")
    (insert "  === Management ===\n")
    (insert "  C-c e c  - Show current configuration\n")
    (insert "  C-c e l  - List installed packages\n")
    (insert "  C-c e g  - Open BFEPM package management UI (available packages)\n")
    (insert "  C-c e d  - Show directory locations (temp)\n")
    (insert "  C-c e s  - Test version specification\n")
    (insert "  C-c e h  - Show all commands\n\n")
    (insert "🚀 Try starting with: C-c e 1, C-c e t, or C-c e M\n\n")
    (insert "⚠️  Note: Demo uses temporary directories (auto-cleanup on exit)\n\n")
    (insert "💡 Press C-x C-c to exit when done\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Show welcome message
(run-with-timer 0.5 nil #'bfepm-demo-welcome)

(message "[BFEPM Demo] Demo configuration loaded! Press C-c e h for help or C-c e c to check BFEPM status")

(provide 'demo-init)

;;; demo-init.el ends here
