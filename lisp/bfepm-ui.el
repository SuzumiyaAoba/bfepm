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
    ;; Enhanced UI features
    (define-key map (kbd "s") 'bfepm-ui-show-package-status)
    (define-key map (kbd "m") 'bfepm-ui-mark-package)
    (define-key map (kbd "M") 'bfepm-ui-unmark-all)
    (define-key map (kbd "B i") 'bfepm-ui-install-marked)
    (define-key map (kbd "B d") 'bfepm-ui-remove-marked)
    (define-key map (kbd "/") 'bfepm-ui-filter-packages)
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
  (condition-case err
      (progn
        (if (eq bfepm-ui-current-view 'installed)
            (bfepm-ui-update-package-list)
          (bfepm-ui-update-available-package-list))
        (tabulated-list-print t))
    (error
     (message "BFEPM UI refresh buffer error: %s" (error-message-string err)))))

(defun bfepm-ui-update-package-list ()
  "Update the tabulated list with current package information."
  (let ((packages (bfepm-core-get-installed-packages))
        (entries '()))
    
    (dolist (package-name packages)
      (let* ((version (bfepm-core-get-package-version package-name))
             (status (if (bfepm-core-package-installed-p package-name) "Installed" "Missing"))
             (description (bfepm-ui--get-package-description package-name))
             (desc-or-default (or description "No description available")))
        (when (bfepm-ui--should-show-package-p package-name desc-or-default)
          (let ((entry (list package-name
                           (vector package-name
                                  version
                                  status
                                  desc-or-default))))
            (push entry entries)))))
    
    (setq tabulated-list-entries (nreverse entries))))

(defun bfepm-ui--string-trim (string)
  "Trim whitespace from STRING (compatibility function)."
  (if (fboundp 'string-trim)
      (string-trim string)
    ;; Optimized single-regex approach for compatibility
    (if (string-match "\\`[ \t\n\r]*\\(.*?\\)[ \t\n\r]*\\'" string)
        (match-string 1 string)
      string)))

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
  (cond
   ;; Check if demo packages are available (demo environment)
   ((and (boundp 'bfepm-demo-packages) bfepm-demo-packages)
    (message "[BFEPM UI] Using demo packages (%d packages)" (length bfepm-demo-packages))
    (mapcar (lambda (pkg-info)
              (let ((name (car pkg-info)))
                (cons name (if (fboundp 'bfepm-demo-get-package-version)
                              (bfepm-demo-get-package-version name)
                            "latest"))))
            bfepm-demo-packages))
   ;; Try to get from loaded config
   (t
    (let ((config (bfepm-core-get-config)))
      (if (and config (> (length (bfepm-config-packages config)) 0))
          ;; Extract packages from config structure
          (let ((packages (bfepm-config-packages config)))
            (mapcar (lambda (pkg)
                      (cons (bfepm-package-name pkg)
                            (or (bfepm-package-version pkg) "latest")))
                    packages))
        ;; Fallback: try to parse configuration file directly
        (bfepm-ui--parse-config-file-packages))))))

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
                             "No description available")))
        (when (bfepm-ui--should-show-package-p package-name description)
          (let ((entry (list package-name
                           (vector package-name
                                  version-spec
                                  status
                                  description))))
            (push entry entries)))))
    
    (setq tabulated-list-entries (nreverse entries))))

(defun bfepm-ui--get-package-description (package-name)
  "Get description for PACKAGE-NAME from its main file."
  (let ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
        (main-file nil)
        (description nil))
    
    (when (file-directory-p package-dir)
      ;; Find the main package file (package-name.el)
      (setq main-file (expand-file-name (concat package-name ".el") package-dir))
      
      ;; If main file doesn't exist, try to find any .el file
      (unless (file-exists-p main-file)
        (let ((el-files (directory-files package-dir t "\\.el$")))
          (when el-files
            (setq main-file (car el-files)))))
      
      ;; Extract description from the file
      (when (and main-file (file-exists-p main-file))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents main-file nil 0 4096) ; Read first 4KB for better coverage
              (goto-char (point-min))
              ;; Look for standard Emacs Lisp package header format (case-insensitive)
              ;; Format: ;;; package-name.el --- Description here
              (let ((case-fold-search t)) ; Enable case-insensitive search
                (when (re-search-forward 
                       (format "^;;;[[:space:]]*%s\\.el[[:space:]]*---[[:space:]]*\\(.+\\)$" 
                               (regexp-quote package-name)) nil t)
                  (setq description (match-string 1)))
                ;; Alternative: look for generic ;;; filename.el --- description
                (unless description
                  (goto-char (point-min))
                  (when (re-search-forward "^;;;[[:space:]]*[^[:space:]]+\\.el[[:space:]]*---[[:space:]]*\\(.+\\)$" nil t)
                    (setq description (match-string 1))))
                ;; Fallback: look for any comment line that might be a description
                (unless description
                  (goto-char (point-min))
                  (when (re-search-forward "^;;[[:space:]]*\\([A-Z][^.]*\\.[[:space:]]*\\)$" nil t)
                    (setq description (match-string 1)))))
              ;; Clean up description
              (when description
                (setq description (bfepm-ui--string-trim description))
                (when (string-match "^\\(.*?\\)[[:space:]]*-\\*-.*-\\*-[[:space:]]*$" description)
                  (setq description (match-string 1 description)))
                (setq description (bfepm-ui--string-trim description))))
          (error nil))))
    
    description))

(defun bfepm-ui-toggle-view ()
  "Toggle between installed and available packages view."
  (interactive)
  (if (eq bfepm-ui-current-view 'installed)
      (bfepm-ui-show-available-external)
    (bfepm-ui-show-installed-external)))

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
      (let ((inhibit-read-only t)
            (content-parts (list (format "Package: %s\n" package-name)
                                (format "Version: %s\n" version)
                                (format "Directory: %s\n" package-dir)
                                "\n")))
        (erase-buffer)
        
        ;; Build file list section
        (when (file-directory-p package-dir)
          (let ((files (directory-files package-dir nil "^[^.]")))
            (push "Files:\n" content-parts)
            (dolist (file files)
              (push (format "  - %s\n" file) content-parts))
            (push "\n" content-parts)))
        
        ;; Add description section
        (let ((description (bfepm-ui--get-package-description package-name)))
          (when description
            (push "Description:\n" content-parts)
            (push (format "  %s\n\n" description) content-parts)))
        
        ;; Add load path status
        (let ((load-path-entry (expand-file-name package-name (bfepm-core-get-packages-directory))))
          (push "Load Path Status:\n" content-parts)
          (push (format "  %s: %s\n"
                       load-path-entry
                       (if (member load-path-entry load-path) "Added" "Not added"))
                content-parts))
        
        ;; Insert all content at once for better performance
        (insert (apply 'concat (nreverse content-parts)))
        
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
      (message "🔄 Installing %s... (ASYNC: UI stays responsive during download/extraction)" pkg-name)
      (bfepm-package-install-async 
       pkg-name
       (lambda (success package-name error-msg)
         (if success
             (progn
               (message "✓ Successfully installed %s - refreshing package list" package-name)
               ;; Refresh UI asynchronously to avoid blocking
               (run-with-timer 0.1 nil 
                              (lambda () 
                                (when (get-buffer "*BFEPM*")
                                  (with-current-buffer "*BFEPM*"
                                    (when (eq major-mode 'bfepm-ui-mode)
                (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh))))))))
           (message "✗ Failed to install %s: %s" package-name error-msg)))))))

(defun bfepm-ui-remove-package ()
  "Remove the package at point."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (when package-name
      (when (yes-or-no-p (format "Remove package %s? " package-name))
        (condition-case err
            (progn
              (bfepm-package-remove package-name)
              (when (eq major-mode 'bfepm-ui-mode)
                (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh))))
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
            (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh)))
        (error
         (message "Failed to update %s: %s" package-name (error-message-string err)))))))

(defun bfepm-ui-update-all-packages ()
  "Update all installed packages."
  (interactive)
  (when (yes-or-no-p "Update all packages? ")
    (condition-case err
        (progn
          (bfepm-package-update-all)
          (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh)))
      (error
       (message "Failed to update packages: %s" (error-message-string err))))))

(defun bfepm-ui-refresh ()
  "Refresh the package list."
  (interactive)
  (condition-case err
      (when (and (buffer-live-p (current-buffer))
                 (eq major-mode 'bfepm-ui-mode))
        (revert-buffer))
    (error
     (message "BFEPM UI refresh error: %s" (error-message-string err)))))

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
    (princ "  ?     - Show this help\n")
    (princ "  s     - Show package status summary\n")
    (princ "  m     - Mark package for batch operations\n")
    (princ "  M     - Unmark all packages\n")
    (princ "  B i   - Install all marked packages\n")
    (princ "  B d   - Remove all marked packages\n")
    (princ "  /     - Filter packages by name/description\n\n")
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

;; UI/UX Enhancements

(defun bfepm-ui-show-package-status ()
  "Show detailed status information about packages in a summary buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*BFEPM Status*"))
        (installed-packages (bfepm-core-get-installed-packages))
        (available-packages (bfepm-ui--get-config-packages)))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== BFEPM Package Manager Status ===\n\n")
        
        ;; Summary section
        (insert (format "Installed Packages: %d\n" 
                       (length installed-packages)))
        (insert (format "Available in Config: %d\n" 
                       (length available-packages)))
        (insert (format "Packages Directory: %s\n" 
                       (bfepm-core-get-packages-directory)))
        (insert "\n")
        
        ;; Package health check
        (insert "=== Package Health Check ===\n")
        (let ((missing-count 0))
          (dolist (pkg-name installed-packages)
            (let ((pkg-dir (expand-file-name pkg-name (bfepm-core-get-packages-directory))))
              (unless (file-directory-p pkg-dir)
                (setq missing-count (1+ missing-count)))))
          
          (insert (format "Missing/Corrupted: %d\n" missing-count))
          (insert (format "Health Status: %s\n\n" 
                         (if (= missing-count 0) "* Good" "! Issues Found"))))
        
        ;; Quick actions guide
        (insert "=== Quick Actions ===\n")
        (insert "Press 'i' in package list to install\n")
        (insert "Press 'd' to remove packages\n")
        (insert "Press 'U' to update all packages\n")
        (insert "Press 't' to toggle view\n")
        (insert "Press 'g' to refresh\n")
        
        (goto-char (point-min))
        (read-only-mode 1))
      
      (pop-to-buffer buffer)
      (message "Package status summary displayed"))))

(defun bfepm-ui-mark-package ()
  "Mark package at point for batch operations."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (when package-name
      (if (get-text-property (point) 'bfepm-marked)
          (progn
            (remove-text-properties (line-beginning-position) (line-end-position) 
                                   '(bfepm-marked nil face nil))
            (message "Unmarked %s" package-name))
        (add-text-properties (line-beginning-position) (line-end-position)
                            '(bfepm-marked t face highlight))
        (message "Marked %s" package-name))
      (forward-line 1))))

(defun bfepm-ui-unmark-all ()
  "Unmark all marked packages."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (remove-text-properties (line-beginning-position) (line-end-position)
                             '(bfepm-marked nil face nil))
      (forward-line 1)))
  (message "All packages unmarked"))

(defun bfepm-ui-install-marked ()
  "Install all marked packages."
  (interactive)
  (let ((marked-packages '())
        (failed-packages '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'bfepm-marked)
          (let ((package-name (tabulated-list-get-id)))
            (when package-name
              (push package-name marked-packages))))
        (forward-line 1)))
    
    (if marked-packages
        (when (yes-or-no-p (format "Install %d marked packages? " (length marked-packages)))
          (dolist (package-name marked-packages)
            (condition-case err
                (progn
                  (bfepm-utils-message "Installing %s..." package-name)
                  (bfepm-package-install package-name))
              (error
               (push (cons package-name (error-message-string err)) failed-packages)
               (bfepm-utils-message "Failed to install %s: %s" 
                                   package-name (error-message-string err)))))
          (bfepm-ui-unmark-all)
          (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh))
          (if failed-packages
              (message "Batch installation completed with %d failures. Check *Messages* for details." 
                      (length failed-packages))
            (message "Batch installation completed successfully")))
      (message "No packages marked for installation"))))

(defun bfepm-ui-remove-marked ()
  "Remove all marked packages."
  (interactive)
  (let ((marked-packages '())
        (failed-packages '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'bfepm-marked)
          (let ((package-name (tabulated-list-get-id)))
            (when package-name
              (push package-name marked-packages))))
        (forward-line 1)))
    
    (if marked-packages
        (when (yes-or-no-p (format "Remove %d marked packages? " (length marked-packages)))
          (dolist (package-name marked-packages)
            (condition-case err
                (progn
                  (bfepm-utils-message "Removing %s..." package-name)
                  (bfepm-package-remove package-name))
              (error
               (push (cons package-name (error-message-string err)) failed-packages)
               (bfepm-utils-message "Failed to remove %s: %s" 
                                   package-name (error-message-string err)))))
          (bfepm-ui-unmark-all)
          (when (eq major-mode 'bfepm-ui-mode)
            (bfepm-ui-refresh))
          (if failed-packages
              (message "Batch removal completed with %d failures. Check *Messages* for details." 
                      (length failed-packages))
            (message "Batch removal completed successfully")))
      (message "No packages marked for removal"))))

(defvar bfepm-ui-filter-string ""
  "Current filter string for package list.")

(defun bfepm-ui-filter-packages (filter-string)
  "Filter packages by FILTER-STRING in name or description."
  (interactive "sFilter packages (empty to clear): ")
  (setq bfepm-ui-filter-string filter-string)
  (cond
   ((eq bfepm-ui-current-view 'installed)
    (bfepm-ui-update-package-list))
   ((eq bfepm-ui-current-view 'available)
    (bfepm-ui-update-available-package-list)))
  (tabulated-list-print t)
  (if (string-empty-p filter-string)
      (message "Filter cleared")
    (message "Filtering by: %s" filter-string)))

(defun bfepm-ui--should-show-package-p (package-name description)
  "Return t if PACKAGE-NAME or DESCRIPTION matches current filter."
  (if (string-empty-p bfepm-ui-filter-string)
      t
    (let ((filter-regex (regexp-quote bfepm-ui-filter-string)))
      (or (string-match-p filter-regex package-name)
          (and description (string-match-p filter-regex description))))))

(provide 'bfepm-ui)

;;; bfepm-ui.el ends here
