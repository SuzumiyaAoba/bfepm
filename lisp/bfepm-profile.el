;;; bfepm-profile.el --- BFEPM Profile Management -*- lexical-binding: t -*-

;;; Commentary:

;; Profile management functionality for BFEPM.
;; Provides support for multiple package configurations and profile switching.

;;; Code:

(require 'cl-lib)
(require 'bfepm-core)

(declare-function bfepm-config-save "bfepm-config")
(declare-function bfepm-config-load "bfepm-config") 
(declare-function bfepm-utils-error "bfepm-utils")

;; Declare external variables
(defvar bfepm--config)

(defvar bfepm--current-profile "default"
  "Currently active profile name.")

(defvar bfepm--profiles-directory nil
  "Directory where profile configurations are stored.")

(cl-defstruct bfepm-profile
  "Structure representing a BFEPM profile."
  name
  includes
  packages
  config
  active)

(defun bfepm-profile--validate-name (name)
  "Validate that NAME is safe for use as a filename."
  (unless (string-match-p "^[a-zA-Z0-9_-]+$" name)
    (user-error "Profile name must contain only letters, numbers, hyphens, and underscores"))
  (when (member name '("." ".." "CON" "PRN" "AUX" "NUL"))
    (user-error "Profile name '%s' is reserved" name))
  (when (> (length name) 255)
    (user-error "Profile name is too long"))
  (when (string-empty-p name)
    (user-error "Profile name cannot be empty")))

(defun bfepm-profile--ensure-profiles-directory ()
  "Ensure profiles directory exists."
  (unless bfepm--profiles-directory
    (setq bfepm--profiles-directory 
          (expand-file-name "profiles" bfepm-directory)))
  (unless (file-exists-p bfepm--profiles-directory)
    (make-directory bfepm--profiles-directory t)))

(defun bfepm-profile-create (name &optional base-profile)
  "Create a new profile with NAME, optionally based on BASE-PROFILE."
  (interactive "sProfile name: ")
  (bfepm-profile--validate-name name)
  (when (and base-profile (not (string-empty-p base-profile)))
    (bfepm-profile--validate-name base-profile))
  (bfepm-profile--ensure-profiles-directory)
  
  (let* ((profile-file (expand-file-name (format "%s.toml" name) bfepm--profiles-directory))
         (valid-base-profile (when (and base-profile (not (string-empty-p base-profile))) base-profile))
         (base-packages (when valid-base-profile
                         (bfepm-profile--get-profile-packages valid-base-profile)))
         (profile (make-bfepm-profile
                   :name name
                   :includes (when valid-base-profile (list valid-base-profile))
                   :packages (or base-packages '())
                   :config '()
                   :active nil)))
    
    (when (file-exists-p profile-file)
      (unless (y-or-n-p (format "Profile '%s' already exists. Overwrite? " name))
        (user-error "Profile creation cancelled")))
    
    (bfepm-profile--save-profile profile)
    (message "Profile '%s' created successfully" name)
    profile))

(defun bfepm-profile-switch (profile-name)
  "Switch to profile PROFILE-NAME."
  (interactive 
   (list (completing-read "Switch to profile: " 
                          (bfepm-profile-list-names)
                          nil t)))
  
  (bfepm-profile--validate-name profile-name)
  (unless (bfepm-profile-exists-p profile-name)
    (user-error "Profile '%s' does not exist" profile-name))
  
  (let ((profile (bfepm-profile-load profile-name)))
    (setq bfepm--current-profile profile-name)
    (bfepm-profile--apply-profile profile)
    (message "Switched to profile '%s'" profile-name)))

(defun bfepm-profile-list ()
  "List all available profiles."
  (interactive)
  (bfepm-profile--ensure-profiles-directory)
  (let ((profiles (bfepm-profile-list-names)))
    (if profiles
        (progn
          (message "Available profiles:")
          (dolist (profile profiles)
            (message "  %s%s" profile 
                     (if (string= profile bfepm--current-profile) " (current)" ""))))
      (message "No profiles found"))))

(defun bfepm-profile-list-names ()
  "Return list of available profile names."
  (bfepm-profile--ensure-profiles-directory)
  (when (file-directory-p bfepm--profiles-directory)
    (mapcar (lambda (file)
              (file-name-sans-extension file))
            (directory-files bfepm--profiles-directory nil "\\.toml$"))))

(defun bfepm-profile-exists-p (profile-name)
  "Check if profile PROFILE-NAME exists."
  (let ((profile-file (expand-file-name (format "%s.toml" profile-name) 
                                        bfepm--profiles-directory)))
    (file-exists-p profile-file)))

(defun bfepm-profile-load (profile-name)
  "Load profile PROFILE-NAME from file."
  (bfepm-profile--validate-name profile-name)
  (bfepm-profile--ensure-profiles-directory)
  (let ((profile-file (expand-file-name (format "%s.toml" profile-name) 
                                        bfepm--profiles-directory)))
    (unless (file-exists-p profile-file)
      (user-error "Profile '%s' does not exist" profile-name))
    
    (condition-case err
        (with-temp-buffer
          (insert-file-contents profile-file)
          (bfepm-profile--parse-profile-content (buffer-string) profile-name))
      (error
       (user-error "Failed to load profile '%s': %s" 
                   profile-name (error-message-string err))))))

(defun bfepm-profile--parse-profile-content (content profile-name)
  "Parse profile CONTENT for PROFILE-NAME and return bfepm-profile structure."
  (let ((profile (make-bfepm-profile :name profile-name))
        (packages '())
        (includes '())
        (config '()))
    
    ;; Simple TOML-like parsing for profile content
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      
      ;; Parse includes - more robust parsing
      (when (re-search-forward "^includes\\s-*=\\s-*\\[\\([^]]+\\)\\]" nil t)
        (let ((includes-str (string-trim (match-string 1))))
          (setq includes (delq nil
                               (mapcar (lambda (s)
                                         (let ((trimmed (string-trim s)))
                                           (when (string-match "^\"\\([^\"]*\\)\"$" trimmed)
                                             (match-string 1 trimmed))))
                                       (split-string includes-str ","))))))
      
      ;; Parse packages section - more robust parsing
      (goto-char (point-min))
      (when (re-search-forward "^\\[packages\\]" nil t)
        (forward-line)
        (while (and (< (point) (point-max))
                    (not (looking-at "^\\[")))
          (let ((line (string-trim (buffer-substring-no-properties 
                                   (line-beginning-position) 
                                   (line-end-position)))))
            (when (and (not (string-empty-p line))
                       (not (string-match-p "^#" line))  ; Skip comments
                       (string-match "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*\"\\([^\"]*\\)\"" line))
              (let ((package-name (match-string 1 line))
                    (version (match-string 2 line)))
                (push (cons package-name version) packages))))
          (forward-line))))
    
    (setf (bfepm-profile-includes profile) includes
          (bfepm-profile-packages profile) (nreverse packages)
          (bfepm-profile-config profile) config)
    
    profile))

(defun bfepm-profile--save-profile (profile)
  "Save PROFILE to file."
  (bfepm-profile--ensure-profiles-directory)
  (let ((profile-file (expand-file-name 
                       (format "%s.toml" (bfepm-profile-name profile))
                       bfepm--profiles-directory))
        (content (bfepm-profile--format-profile-content profile)))
    
    (with-temp-file profile-file
      (insert content))
    
    (message "Profile '%s' saved to %s" 
             (bfepm-profile-name profile) profile-file)))

(defun bfepm-profile--escape-toml-string (str)
  "Escape STR for use in TOML format."
  (replace-regexp-in-string
   "\\([\"\\\\]\\)" "\\\\\\1"
   (replace-regexp-in-string "\n" "\\\\n" str)))

(defun bfepm-profile--format-profile-content (profile)
  "Format PROFILE content as TOML string."
  (let ((content ""))
    
    ;; Add includes if any
    (when (bfepm-profile-includes profile)
      (setq content (concat content
                            "includes = ["
                            (mapconcat (lambda (inc) (format "\"%s\"" (bfepm-profile--escape-toml-string inc)))
                                       (bfepm-profile-includes profile) ", ")
                            "]\n\n")))
    
    ;; Add packages section
    (when (bfepm-profile-packages profile)
      (setq content (concat content "[packages]\n"))
      (dolist (package (bfepm-profile-packages profile))
        (setq content (concat content
                              (format "%s = \"%s\"\n" 
                                      (bfepm-profile--escape-toml-string (car package))
                                      (bfepm-profile--escape-toml-string (cdr package)))))))
    
    content))

(defun bfepm-profile--apply-profile (profile)
  "Apply PROFILE configuration to current session."
  ;; Resolve profile inheritance
  (let ((resolved-packages (bfepm-profile--resolve-packages profile)))
    ;; Update current configuration with profile packages
    (when (and (boundp 'bfepm--config) bfepm--config resolved-packages)
      (setf (bfepm-config-packages bfepm--config) resolved-packages))
    
    ;; Mark profile as active
    (setf (bfepm-profile-active profile) t)))

(defun bfepm-profile--resolve-packages (profile)
  "Resolve packages for PROFILE including inherited packages."
  (let ((all-packages '())
        (visited-profiles '()))
    
    (setq all-packages (bfepm-profile--collect-packages profile all-packages visited-profiles))
    
    ;; Convert to bfepm-package structures
    (mapcar (lambda (pkg-spec)
              (make-bfepm-package
               :name (car pkg-spec)
               :version (cdr pkg-spec)
               :status 'required))
            all-packages)))

(defun bfepm-profile--collect-packages (profile all-packages visited-profiles)
  "Recursively collect packages from PROFILE and its includes.
ALL-PACKAGES is modified in place. VISITED-PROFILES tracks circular dependencies."
  (let ((profile-name (bfepm-profile-name profile)))
    
    ;; Check for circular dependencies
    (when (member profile-name visited-profiles)
      (user-error "Circular dependency detected in profile '%s'" profile-name))
    
    (push profile-name visited-profiles)
    
    ;; Process included profiles first
    (dolist (include (bfepm-profile-includes profile))
      (when (bfepm-profile-exists-p include)
        (let ((included-profile (bfepm-profile-load include)))
          (setq all-packages (bfepm-profile--collect-packages included-profile all-packages visited-profiles)))))
    
    ;; Add this profile's packages (override included packages)
    (dolist (package (bfepm-profile-packages profile))
      (let ((existing (assoc (car package) all-packages)))
        (if existing
            (setcdr existing (cdr package))
          (setq all-packages (cons package all-packages)))))
    
    (pop visited-profiles)
    all-packages))

(defun bfepm-profile--get-profile-packages (profile-name)
  "Get packages list for PROFILE-NAME."
  (when (bfepm-profile-exists-p profile-name)
    (bfepm-profile-packages (bfepm-profile-load profile-name))))

(defun bfepm-profile-current ()
  "Get current active profile name."
  bfepm--current-profile)

(defun bfepm-profile-remove (profile-name)
  "Remove profile PROFILE-NAME."
  (interactive 
   (list (completing-read "Remove profile: " 
                          (bfepm-profile-list-names)
                          nil t)))
  
  (bfepm-profile--validate-name profile-name)
  (when (string= profile-name "default")
    (user-error "Cannot remove default profile"))
  
  (unless (bfepm-profile-exists-p profile-name)
    (user-error "Profile '%s' does not exist" profile-name))
  
  (when (y-or-n-p (format "Really remove profile '%s'? " profile-name))
    (let ((profile-file (expand-file-name (format "%s.toml" profile-name) 
                                          bfepm--profiles-directory)))
      (delete-file profile-file)
      (when (string= profile-name bfepm--current-profile)
        (setq bfepm--current-profile "default"))
      (message "Profile '%s' removed" profile-name))))

(defun bfepm-profile-copy (source-profile target-profile)
  "Copy SOURCE-PROFILE to TARGET-PROFILE."
  (interactive
   (list (completing-read "Copy from profile: " 
                          (bfepm-profile-list-names) nil t)
         (read-string "Copy to profile: ")))
  
  (bfepm-profile--validate-name source-profile)
  (bfepm-profile--validate-name target-profile)
  (unless (bfepm-profile-exists-p source-profile)
    (user-error "Source profile '%s' does not exist" source-profile))
  
  (when (bfepm-profile-exists-p target-profile)
    (unless (y-or-n-p (format "Profile '%s' already exists. Overwrite? " target-profile))
      (user-error "Profile copy cancelled")))
  
  (let ((source (bfepm-profile-load source-profile)))
    (setf (bfepm-profile-name source) target-profile)
    (bfepm-profile--save-profile source)
    (message "Profile '%s' copied to '%s'" source-profile target-profile)))

(provide 'bfepm-profile)

;;; bfepm-profile.el ends here