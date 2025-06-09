;;; bfepm-ui.el --- BFEPM Package management UI -*- lexical-binding: t -*-

;;; Commentary:

;; Interactive UI for managing BFEPM packages.
;; Provides tabulated list interface for viewing and managing installed packages.

;;; Code:

(require 'tabulated-list)
(require 'bfepm-core)
(require 'bfepm-package)
(require 'bfepm-utils)


(defvar bfepm-ui-buffer-name "*BFEPM Packages*"
  "Name of the BFEPM package management buffer.")

(defvar bfepm-ui-available-buffer-name "*BFEPM Available Packages*"
  "Name of the BFEPM available packages buffer.")

(defvar bfepm-ui-current-view 'installed
  "Current view mode: \\='installed or \\='available.")

(defvar bfepm-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'bfepm-ui-show-package-details)
    (define-key map (kbd "i") 'bfepm-ui-install-package)
    (define-key map (kbd "d") 'bfepm-ui-remove-package)
    (define-key map (kbd "u") 'bfepm-ui-update-package)
    (define-key map (kbd "U") 'bfepm-ui-update-all-packages)
    (define-key map (kbd "g") 'bfepm-ui-refresh)
    (define-key map (kbd "a") 'bfepm-ui-show-available-external)
    (define-key map (kbd "I") 'bfepm-ui-show-installed-external)
    (define-key map (kbd "t") 'bfepm-ui-toggle-view)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'bfepm-ui-help)
    map)
  "Keymap for BFEPM UI mode.")

(defun bfepm-ui--update-mode-line ()
  "Update mode line to show current view."
  (setq mode-name (format "BFEPM Packages [%s]" 
                         (if (eq bfepm-ui-current-view 'installed)
                             "Installed"
                           "Available"))))

(define-derived-mode bfepm-ui-mode tabulated-list-mode "BFEPM Packages"
  "Major mode for managing BFEPM packages."
  (setq tabulated-list-format
        [("Package" 25 t)
         ("Version" 15 t)
         ("Status" 10 t)
         ("Description" 0 nil)])
  
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Package" nil))
  (tabulated-list-init-header)
  
  ;; Set up revert function
  (setq revert-buffer-function #'bfepm-ui-refresh-buffer)
  
  ;; Update mode line to show current view
  (bfepm-ui--update-mode-line))

(defun bfepm-ui-refresh-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the BFEPM package list buffer."
  (interactive)
  (if (eq bfepm-ui-current-view 'installed)
      (bfepm-ui-update-package-list)
    (bfepm-ui-update-available-package-list))
  (tabulated-list-print t))

(defun bfepm-ui-update-package-list ()
  "Update the tabulated list with current package information."
  (let ((packages (bfepm-core-get-installed-packages))
        (entries '()))
    
    (dolist (package-name packages)
      (let* ((version (bfepm-core-get-package-version package-name))
             (status (if (bfepm-core-package-installed-p package-name) "Installed" "Missing"))
             (description (bfepm-ui--get-package-description package-name))
             (entry (list package-name
                         (vector package-name
                                version
                                status
                                (or description "No description available")))))
        (push entry entries)))
    
    (setq tabulated-list-entries (nreverse entries))))

(defun bfepm-ui--string-trim (string)
  "Trim whitespace from STRING (compatibility function)."
  (replace-regexp-in-string "\\`[ \t\n\r]+" "" 
                            (replace-regexp-in-string "[ \t\n\r]+\\'" "" string)))

(defun bfepm-ui--simple-toml-parse (file)
  "Simple TOML parser to extract package names and versions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((packages '())
          (in-packages-section nil))
      (while (not (eobp))
        (let ((line (bfepm-ui--string-trim (buffer-substring-no-properties 
                                            (line-beginning-position) 
                                            (line-end-position)))))
          (cond
           ;; Check for [packages] section
           ((string= line "[packages]")
            (setq in-packages-section t))
           ;; Check for any other sections
           ((and (string-prefix-p "[" line)
                 (not (string= line "[packages]")))
            (setq in-packages-section nil))
           ;; Parse package lines in [packages] section
           ((and in-packages-section
                 (not (string-prefix-p "#" line))
                 (not (string= line ""))
                 (not (string-prefix-p "[packages." line))
                 (string-match "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*\"\\([^\"]+\\)\"" line))
            (let ((pkg-name (match-string 1 line))
                  (version (match-string 2 line)))
              (push (cons pkg-name version) packages)))))
        (forward-line 1))
      (reverse packages))))

(defun bfepm-ui--parse-config-file-packages ()
  "Parse packages from configuration file when config structure is not available."
  (let ((config-file (cond
                      ;; First check bfepm-config-file variable (demo sets this)
                      ((and (boundp 'bfepm-config-file) bfepm-config-file)
                       bfepm-config-file)
                      ;; Check for sample/bfepm.toml (demo environment)
                      ((file-exists-p "sample/bfepm.toml")
                       (expand-file-name "sample/bfepm.toml"))
                      ;; Default location
                      (t (expand-file-name "bfepm.toml" user-emacs-directory)))))
    (if (file-exists-p config-file)
        (condition-case nil
            (progn
              (message "[BFEPM UI] Reading packages from: %s" config-file)
              (bfepm-ui--simple-toml-parse config-file))
          (error 
           (message "[BFEPM UI] Could not parse config file: %s" config-file)
           '()))
      (progn
        (message "[BFEPM UI] No config file found. Searched: %s" config-file)
        '()))))

(defun bfepm-ui--get-config-packages ()
  "Get packages from configuration file."
  (let ((config (bfepm-core-get-config)))
    (if config
        ;; Extract packages from config structure
        (let ((packages (bfepm-config-packages config)))
          (mapcar (lambda (pkg)
                    (cons (bfepm-package-name pkg)
                          (or (bfepm-package-version pkg) "latest")))
                  packages))
      ;; Fallback: try to parse configuration file directly
      (bfepm-ui--parse-config-file-packages))))

(defun bfepm-ui--get-config-description (package-name)
  "Get description for PACKAGE-NAME from demo package descriptions or fallback."
  (when (boundp 'bfepm-demo-package-descriptions)
    (cadr (assoc package-name bfepm-demo-package-descriptions))))

(defun bfepm-ui-update-available-package-list ()
  "Update the tabulated list with available packages from configuration."
  (let ((config-packages (bfepm-ui--get-config-packages))
        (entries '()))
    
    (dolist (package-info config-packages)
      (let* ((package-name (car package-info))
             (version-spec (cdr package-info))
             (status (if (bfepm-core-package-installed-p package-name) 
                        "Installed" 
                      "Available"))
             (description (or (bfepm-ui--get-package-description package-name)
                             (bfepm-ui--get-config-description package-name)
                             "No description available"))
             (entry (list package-name
                         (vector package-name
                                version-spec
                                status
                                description))))
        (push entry entries)))
    
    (setq tabulated-list-entries (nreverse entries))))

(defun bfepm-ui--get-package-description (package-name)
  "Get description for PACKAGE-NAME from its main file."
  (let* ((package-dir (condition-case nil
                          (expand-file-name package-name (bfepm-core-get-packages-directory))
                        (error nil)))
         (main-file (when package-dir
                      (expand-file-name (format "%s.el" package-name) package-dir))))
    
    (when (and main-file (file-exists-p main-file))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents main-file)
            (goto-char (point-min))
            ;; Look for package header description
            (or (when (re-search-forward "^;;; Commentary:\\s-*$" nil t)
                  (forward-line 1)
                  (when (re-search-forward "^;;[ \t]*\\(.+\\)$" nil t)
                    (match-string 1)))
                ;; Fallback: look for first comment line
                (progn
                  (goto-char (point-min))
                  (when (re-search-forward "^;;[ \t]+\\(.+\\)$" nil t)
                    (match-string 1)))))
        (error nil))))




(defun bfepm-ui-toggle-view ()
  "Toggle between installed and available packages view."
  (interactive)
  (if (eq bfepm-ui-current-view 'installed)
      (bfepm-ui-show-available-external)
    (bfepm-ui-show-installed-external))))

(defun bfepm-ui-show-package-details ()
  "Show detailed information about the package at point."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (when package-name
      (bfepm-ui--show-package-info-buffer package-name))))

(defun bfepm-ui--show-package-info-buffer (package-name)
  "Show detailed package information for PACKAGE-NAME in a separate buffer."
  (let ((buffer (get-buffer-create (format "*BFEPM Package Info: %s*" package-name)))
        (package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
        (version (bfepm-core-get-package-version package-name)))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Package: %s\n" package-name))
        (insert (format "Version: %s\n" version))
        (insert (format "Directory: %s\n" package-dir))
        (insert "\n")
        
        ;; Show files in package directory
        (when (file-directory-p package-dir)
          (insert "Files:\n")
          (let ((files (directory-files package-dir nil "^[^.]")))
            (dolist (file files)
              (insert (format "  - %s\n" file))))
          (insert "\n"))
        
        ;; Show package description if available
        (let ((description (bfepm-ui--get-package-description package-name)))
          (when description
            (insert "Description:\n")
            (insert (format "  %s\n\n" description))))
        
        ;; Show load path status
        (let ((load-path-entry (expand-file-name package-name (bfepm-core-get-packages-directory))))
          (insert "Load Path Status:\n")
          (insert (format "  %s: %s\n"
                         load-path-entry
                         (if (member load-path-entry load-path) "Added" "Not added"))))
        
        (goto-char (point-min))
        (read-only-mode 1))
      
      (pop-to-buffer buffer))))

(defun bfepm-ui-install-package (&optional package-name)
  "Install PACKAGE-NAME.  If not provided, install package at point."
  (interactive)
  (let ((pkg-name (or package-name 
                     (tabulated-list-get-id)
                     (completing-read "Package name: " 
                                    (mapcar #'car (bfepm-ui--get-config-packages))
                                    nil nil))))
    (when pkg-name
      (condition-case err
          (progn
            (bfepm-package-install pkg-name)
            (bfepm-ui-refresh))
        (error
         (message "Failed to install %s: %s" pkg-name (error-message-string err)))))))

(defun bfepm-ui-remove-package ()
  "Remove the package at point."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (when package-name
      (when (yes-or-no-p (format "Remove package %s? " package-name))
        (condition-case err
            (progn
              (bfepm-package-remove package-name)
              (bfepm-ui-refresh))
          (error
           (message "Failed to remove %s: %s" package-name (error-message-string err))))))))

(defun bfepm-ui-update-package ()
  "Update the package at point."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (when package-name
      (condition-case err
          (progn
            (bfepm-package-update package-name)
            (bfepm-ui-refresh))
        (error
         (message "Failed to update %s: %s" package-name (error-message-string err)))))))

(defun bfepm-ui-update-all-packages ()
  "Update all installed packages."
  (interactive)
  (when (yes-or-no-p "Update all packages? ")
    (condition-case err
        (progn
          (bfepm-package-update-all)
          (bfepm-ui-refresh))
      (error
       (message "Failed to update packages: %s" (error-message-string err))))))

(defun bfepm-ui-refresh ()
  "Refresh the package list."
  (interactive)
  (revert-buffer))

(defun bfepm-ui-help ()
  "Show help for BFEPM UI commands."
  (interactive)
  (with-help-window "*BFEPM UI Help*"
    (princ "BFEPM Package Management UI\n\n")
    (princ "Key bindings:\n")
    (princ "  RET   - Show package details\n")
    (princ "  i     - Install package (at point or prompt)\n")
    (princ "  d     - Remove package at point\n")
    (princ "  u     - Update package at point\n")
    (princ "  U     - Update all packages\n")
    (princ "  g     - Refresh package list\n")
    (princ "  a     - Show available packages from config\n")
    (princ "  I     - Show installed packages\n")
    (princ "  t     - Toggle between installed/available view\n")
    (princ "  q     - Quit window\n")
    (princ "  ?     - Show this help\n\n")
    (princ "Views:\n")
    (princ "  Installed - Packages currently installed\n")
    (princ "  Available - Packages defined in configuration file\n\n")
    (princ "Package Status:\n")
    (princ "  Installed - Package is properly installed\n")
    (princ "  Available - Package can be installed from config\n")
    (princ "  Missing   - Package directory exists but may be corrupted\n")))

;;;###autoload
(defun bfepm-ui ()
  "Open the BFEPM package management interface."
  (interactive)
  (let ((buffer (get-buffer-create bfepm-ui-buffer-name)))
    (with-current-buffer buffer
      (bfepm-ui-mode)
      (setq bfepm-ui-current-view 'installed)
      (bfepm-ui--update-mode-line)
      (bfepm-ui-update-package-list)
      (tabulated-list-print))
    
    (switch-to-buffer buffer)
    (message "BFEPM Package Management - Press 'a' for available packages, '?' for help")))

;; Additional interactive functions for external use
;;;###autoload
(defun bfepm-ui-show-available-external ()
  "Switch to available packages view (external command)."
  (interactive)
  (bfepm-ui)
  (setq bfepm-ui-current-view 'available)
  (bfepm-ui--update-mode-line)
  (bfepm-ui-update-available-package-list)
  (tabulated-list-print)
  (message "Showing available packages from configuration"))

;;;###autoload
(defun bfepm-ui-show-installed-external ()
  "Switch to installed packages view (external command)."
  (interactive)
  (bfepm-ui))


(provide 'bfepm-ui)

;;; bfepm-ui.el ends here
