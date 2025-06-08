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

(defvar bfepm-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'bfepm-ui-show-package-details)
    (define-key map (kbd "i") 'bfepm-ui-install-package)
    (define-key map (kbd "d") 'bfepm-ui-remove-package)
    (define-key map (kbd "u") 'bfepm-ui-update-package)
    (define-key map (kbd "U") 'bfepm-ui-update-all-packages)
    (define-key map (kbd "g") 'bfepm-ui-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'bfepm-ui-help)
    map)
  "Keymap for BFEPM UI mode.")

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
  (setq revert-buffer-function #'bfepm-ui-refresh-buffer))

(defun bfepm-ui-refresh-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the BFEPM package list buffer."
  (interactive)
  (bfepm-ui-update-package-list)
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

(defun bfepm-ui--get-package-description (package-name)
  "Get description for PACKAGE-NAME from its main file."
  (let* ((package-dir (if (boundp 'bfepm-core-get-packages-directory)
                          (expand-file-name package-name (bfepm-core-get-packages-directory))
                        (expand-file-name package-name)))
         (main-file (expand-file-name (format "%s.el" package-name) package-dir)))
    
    (when (file-exists-p main-file)
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
        (error nil)))))

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

(defun bfepm-ui-install-package (package-name)
  "Install PACKAGE-NAME."
  (interactive "sPackage name: ")
  (condition-case err
      (progn
        (bfepm-package-install package-name)
        (bfepm-ui-refresh))
    (error
     (message "Failed to install %s: %s" package-name (error-message-string err)))))

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
  (bfepm-ui-refresh-buffer)
  (message "Package list refreshed"))

(defun bfepm-ui-help ()
  "Show help for BFEPM UI commands."
  (interactive)
  (with-help-window "*BFEPM UI Help*"
    (princ "BFEPM Package Management UI\n\n")
    (princ "Key bindings:\n")
    (princ "  RET   - Show package details\n")
    (princ "  i     - Install new package\n")
    (princ "  d     - Remove package at point\n")
    (princ "  u     - Update package at point\n")
    (princ "  U     - Update all packages\n")
    (princ "  g     - Refresh package list\n")
    (princ "  q     - Quit window\n")
    (princ "  ?     - Show this help\n\n")
    (princ "Package Status:\n")
    (princ "  Installed - Package is properly installed\n")
    (princ "  Missing   - Package directory exists but may be corrupted\n")))

;;;###autoload
(defun bfepm-ui ()
  "Open the BFEPM package management interface."
  (interactive)
  (let ((buffer (get-buffer-create bfepm-ui-buffer-name)))
    (with-current-buffer buffer
      (bfepm-ui-mode)
      (bfepm-ui-update-package-list)
      (tabulated-list-print))
    
    (switch-to-buffer buffer)
    (message "BFEPM Package Management - Press ? for help")))

(provide 'bfepm-ui)

;;; bfepm-ui.el ends here