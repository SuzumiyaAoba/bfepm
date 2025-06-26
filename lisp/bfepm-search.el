;;; bfepm-search.el --- Package search functionality for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides package search functionality for BFEPM.
;; It searches across MELPA and GNU ELPA archives to find packages
;; matching user queries with support for filtering and detailed results.

;;; Code:

(require 'bfepm-core)
(require 'bfepm-network)
(require 'bfepm-utils)
(require 'bfepm-version)

;; Search configuration and state
(defvar bfepm-search--cache nil
  "Cache for search results to avoid repeated archive fetches.")

(defvar bfepm-search--last-query ""
  "Last search query for caching and optimization.")

(defvar bfepm-search--last-results nil
  "Last search results for quick re-filtering.")

(defconst bfepm-search--archive-sources
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/"))
  "Default archive sources for package search.")

;; Core search data structure
(cl-defstruct bfepm-search-result
  "Structure representing a single search result."
  name                    ; Package name (string)
  version                 ; Version string
  description             ; Package description
  source                  ; Source archive (melpa/gnu)
  dependencies            ; List of dependencies
  kind                    ; Package type (tar/single)
  installed-p)            ; Whether package is installed

;; Main search functions

;;;###autoload
(defun bfepm-search (query &optional sources)
  "Search for packages matching QUERY across SOURCES.
SOURCES defaults to all available archives.
Returns a list of bfepm-search-result structures."
  (interactive "sSearch packages: ")
  (let ((sources (or sources bfepm-search--archive-sources)))
    (if (string-empty-p query)
        (progn
          (message "Empty search query")
          nil)
      (if (called-interactively-p 'any)
          (bfepm-search--display-results 
           (bfepm-search--search-sync query sources))
        (bfepm-search--search-sync query sources)))))

;;;###autoload
(defun bfepm-search-async (query callback &optional sources)
  "Search for packages matching QUERY asynchronously.
CALLBACK is called with (success results error-message) when complete.
SOURCES defaults to all available archives."
  (let ((sources (or sources bfepm-search--archive-sources)))
    (if (string-empty-p query)
        (funcall callback nil nil "Empty search query")
      (bfepm-search--search-async query sources callback))))

;; Synchronous search implementation

(defun bfepm-search--search-sync (query sources)
  "Perform synchronous search for QUERY across SOURCES."
  (let ((all-results '())
        (query-regex (bfepm-search--build-query-regex query)))
    
    (bfepm-utils-message "Searching for packages matching: %s" query)
    
    (dolist (source sources)
      (let* ((source-name (car source))
             (source-url (cdr source))
             (results (bfepm-search--search-source-sync 
                      query-regex source-name source-url)))
        (setq all-results (append all-results results))))
    
    (bfepm-utils-message "Found %d packages matching '%s'" 
                        (length all-results) query)
    
    ;; Cache results
    (setq bfepm-search--last-query query)
    (setq bfepm-search--last-results all-results)
    
    (bfepm-search--sort-results all-results query)))

(defun bfepm-search--search-source-sync (query-regex source-name source-url)
  "Search single SOURCE synchronously using QUERY-REGEX."
  (condition-case err
      (let* ((archive-contents (bfepm-search--fetch-archive-contents-sync source-url))
             (results '()))
        
        (dolist (package-entry archive-contents)
          (let* ((name (symbol-name (car package-entry)))
                 (info (cdr package-entry))
                 (result (bfepm-search--create-result-from-archive-entry 
                         name info source-name)))
            (when (bfepm-search--matches-query-p result query-regex)
              (push result results))))
        
        (bfepm-utils-message "Found %d matches in %s" 
                            (length results) source-name)
        results)
    (error
     (bfepm-utils-message "Failed to search %s: %s" 
                         source-name (error-message-string err))
     nil)))

(defun bfepm-search--fetch-archive-contents-sync (source-url)
  "Fetch archive contents synchronously from SOURCE-URL."
  (let ((archive-file (concat source-url "archive-contents")))
    (condition-case err
        (with-temp-buffer
          (bfepm-utils-message "Fetching archive contents from %s..." archive-file)
          (unless (fboundp 'url-insert-file-contents)
            (error "url-insert-file-contents not available"))
          (url-insert-file-contents archive-file)
          (goto-char (point-min))
          (let ((contents (read (current-buffer))))
            (unless (listp contents)
              (error "Invalid archive format"))
            contents))
      (error
       (bfepm-utils-error "Failed to fetch archive contents from %s: %s"
                          source-url (error-message-string err))))))

;; Asynchronous search implementation

(defun bfepm-search--search-async (query sources callback)
  "Perform asynchronous search for QUERY across SOURCES."
  (let ((query-regex (bfepm-search--build-query-regex query))
        (all-results '())
        (completed-sources 0)
        (total-sources (length sources)))
    
    (bfepm-utils-message "Starting async search for: %s" query)
    
    (if (= total-sources 0)
        (funcall callback t '() nil)
      
      (dolist (source sources)
        (let* ((source-name (car source))
               (source-url (cdr source)))
          (bfepm-search--search-source-async 
           query-regex source-name source-url
           (lambda (success results error-msg)
             (setq completed-sources (1+ completed-sources))
             
             (if success
                 (progn
                   (setq all-results (append all-results results))
                   (bfepm-utils-message "Completed search in %s (%d/%d)" 
                                       source-name completed-sources total-sources))
               (bfepm-utils-message "Failed to search %s: %s" 
                                   source-name error-msg))
             
             ;; Check if all sources completed
             (when (= completed-sources total-sources)
               (let ((sorted-results (bfepm-search--sort-results all-results query)))
                 (bfepm-utils-message "Async search completed: %d results" 
                                     (length sorted-results))
                 ;; Cache results
                 (setq bfepm-search--last-query query)
                 (setq bfepm-search--last-results sorted-results)
                 (funcall callback t sorted-results nil))))))))))

(defun bfepm-search--search-source-async (query-regex source-name source-url callback)
  "Search single SOURCE asynchronously using QUERY-REGEX."
  (bfepm-network-fetch-archive-contents-async
   source-url
   (lambda (success contents error-msg)
     (if success
         (condition-case err
             (let ((results '()))
               (dolist (package-entry contents)
                 (let* ((name (symbol-name (car package-entry)))
                        (info (cdr package-entry))
                        (result (bfepm-search--create-result-from-archive-entry 
                                name info source-name)))
                   (when (bfepm-search--matches-query-p result query-regex)
                     (push result results))))
               (funcall callback t results nil))
           (error
            (funcall callback nil nil (error-message-string err))))
       (funcall callback nil nil error-msg)))))

;; Search result processing

(defun bfepm-search--create-result-from-archive-entry (name info source-name)
  "Create search result from archive entry NAME and INFO."
  (let* ((info-list (if (vectorp info) (append info nil) info))
         (version (car info-list))
         (deps (cadr info-list))
         (description (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-version-normalize version))
         (installed-p (bfepm-core-package-installed-p name)))
    
    (make-bfepm-search-result
     :name name
     :version version-string
     :description (or description "No description available")
     :source source-name
     :dependencies deps
     :kind kind
     :installed-p installed-p)))

(defun bfepm-search--build-query-regex (query)
  "Build regex pattern from search QUERY."
  (let ((terms (split-string query "\\s-+" t)))
    (if (= (length terms) 1)
        ;; Single term - case insensitive match
        (regexp-quote (downcase query))
      ;; Multiple terms - all must match (AND logic)
      (mapconcat (lambda (term) (regexp-quote (downcase term))) terms ".*"))))

(defun bfepm-search--matches-query-p (result query-regex)
  "Check if RESULT matches QUERY-REGEX."
  (let ((name (downcase (bfepm-search-result-name result)))
        (desc (downcase (bfepm-search-result-description result))))
    (or (string-match-p query-regex name)
        (string-match-p query-regex desc))))

(defun bfepm-search--sort-results (results query)
  "Sort RESULTS by relevance to QUERY."
  (let ((query-lower (downcase query)))
    (sort results
          (lambda (a b)
            (let ((name-a (downcase (bfepm-search-result-name a)))
                  (name-b (downcase (bfepm-search-result-name b)))
                  (desc-a (downcase (bfepm-search-result-description a)))
                  (desc-b (downcase (bfepm-search-result-description b))))
              (cond
               ;; Exact name matches first
               ((string= name-a query-lower) t)
               ((string= name-b query-lower) nil)
               ;; Name starts with query
               ((string-prefix-p query-lower name-a) 
                (not (string-prefix-p query-lower name-b)))
               ((string-prefix-p query-lower name-b) nil)
               ;; Name contains query
               ((string-match-p query-lower name-a)
                (not (string-match-p query-lower name-b)))
               ((string-match-p query-lower name-b) nil)
               ;; Description contains query
               ((string-match-p query-lower desc-a)
                (not (string-match-p query-lower desc-b)))
               ;; Alphabetical fallback
               (t (string< name-a name-b))))))))

;; Search result display

(defun bfepm-search--display-results (results)
  "Display search RESULTS in a dedicated buffer."
  (if (null results)
      (message "No packages found matching search query")
    (let ((buffer (get-buffer-create "*BFEPM Search Results*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (bfepm-search--insert-results-header results)
          (bfepm-search--insert-results-table results)
          (bfepm-search--setup-results-buffer))
        (goto-char (point-min))
        (read-only-mode 1))
      
      (pop-to-buffer buffer)
      (message "Found %d packages - Press 'i' to install, 'RET' for details" 
               (length results)))))

(defun bfepm-search--insert-results-header (results)
  "Insert header information for search RESULTS."
  (insert (format "=== BFEPM Search Results ===\n\n"))
  (insert (format "Found %d packages matching your search\n" (length results)))
  (insert (format "Search performed at: %s\n\n" 
                  (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert "Key bindings:\n")
  (insert "  RET - Show package details\n")
  (insert "  i   - Install package\n")
  (insert "  q   - Quit\n")
  (insert "  r   - Refresh search\n\n")
  (insert (make-string 80 ?=) "\n\n"))

(defun bfepm-search--insert-results-table (results)
  "Insert formatted table of search RESULTS."
  (let ((name-width 25)
        (version-width 15)
        (source-width 8)
        (status-width 10))
    
    ;; Table header
    (insert (format "%-*s %-*s %-*s %-*s %s\n"
                   name-width "Package"
                   version-width "Version"
                   source-width "Source"
                   status-width "Status"
                   "Description"))
    (insert (format "%s %s %s %s %s\n"
                   (make-string name-width ?-)
                   (make-string version-width ?-)
                   (make-string source-width ?-)
                   (make-string status-width ?-)
                   (make-string 40 ?-)))
    
    ;; Table rows
    (dolist (result results)
      (let* ((name (bfepm-search-result-name result))
             (version (bfepm-search-result-version result))
             (source (bfepm-search-result-source result))
             (status (if (bfepm-search-result-installed-p result) 
                        "Installed" "Available"))
             (description (bfepm-search-result-description result))
             ;; Truncate long descriptions
             (desc-display (if (> (length description) 40)
                              (concat (substring description 0 37) "...")
                            description)))
        
        (let ((line-start (point)))
          (insert (format "%-*s %-*s %-*s %-*s %s\n"
                         name-width name
                         version-width version
                         source-width source
                         status-width status
                         desc-display))
          ;; Add text properties for interactive features
          (put-text-property line-start (point) 'bfepm-search-result result)
          (put-text-property line-start (point) 'bfepm-package-name name))))))

(defun bfepm-search--setup-results-buffer ()
  "Setup keybindings and mode for search results buffer."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'bfepm-search-show-package-details)
    (define-key map (kbd "i") 'bfepm-search-install-package)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "r") 'bfepm-search-refresh)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (use-local-map map)))

;; Interactive functions for search results buffer

(defun bfepm-search-show-package-details ()
  "Show detailed information about the package at point."
  (interactive)
  (let ((result (get-text-property (point) 'bfepm-search-result)))
    (if result
        (bfepm-search--show-package-details-buffer result)
      (message "No package at point"))))

(defun bfepm-search-install-package ()
  "Install the package at point."
  (interactive)
  (let ((package-name (get-text-property (point) 'bfepm-package-name)))
    (if package-name
        (if (bfepm-core-package-installed-p package-name)
            (message "Package %s is already installed" package-name)
          (progn
            (message "Installing %s..." package-name)
            (bfepm-package-install-async 
             package-name
             (lambda (success name error-msg)
               (if success
                   (message "✓ Successfully installed %s" name)
                 (message "✗ Failed to install %s: %s" name error-msg))))))
      (message "No package at point"))))

(defun bfepm-search-refresh ()
  "Refresh the current search results."
  (interactive)
  (if (string-empty-p bfepm-search--last-query)
      (call-interactively 'bfepm-search)
    (bfepm-search bfepm-search--last-query)))

(defun bfepm-search--show-package-details-buffer (result)
  "Show detailed information about RESULT in a separate buffer."
  (let* ((name (bfepm-search-result-name result))
         (buffer (get-buffer-create (format "*BFEPM Package: %s*" name))))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== Package Details: %s ===\n\n" name))
        
        (insert (format "Name: %s\n" name))
        (insert (format "Version: %s\n" (bfepm-search-result-version result)))
        (insert (format "Source: %s\n" (bfepm-search-result-source result)))
        (insert (format "Status: %s\n" 
                        (if (bfepm-search-result-installed-p result)
                            "Installed" "Available")))
        (insert (format "Type: %s\n" (bfepm-search-result-kind result)))
        (insert "\nDescription:\n")
        (insert (format "  %s\n\n" (bfepm-search-result-description result)))
        
        (when (bfepm-search-result-dependencies result)
          (insert "Dependencies:\n")
          (dolist (dep (bfepm-search-result-dependencies result))
            (insert (format "  - %s\n" (if (listp dep) (car dep) dep))))
          (insert "\n"))
        
        (insert "Actions:\n")
        (if (bfepm-search-result-installed-p result)
            (insert "  Package is already installed\n")
          (insert "  Press 'i' to install this package\n"))
        
        (goto-char (point-min))
        (read-only-mode 1)
        
        ;; Add keybindings
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "i") 
            (lambda () (interactive) 
              (bfepm-package-install name)))
          (define-key map (kbd "q") 'quit-window)
          (use-local-map map)))
      
      (pop-to-buffer buffer))))

;; Utility functions

(defun bfepm-search--get-package-description (package-name)
  "Get description for PACKAGE-NAME from its main file.
This is a fallback when bfepm-ui is not available."
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
                    (setq description (match-string 1)))))
              ;; Clean up description
              (when description
                (setq description (string-trim description))
                (when (string-match "^\\(.*?\\)[[:space:]]*-\\*-.*-\\*-[[:space:]]*$" description)
                  (setq description (match-string 1 description)))
                (setq description (string-trim description))))
          (error nil))))
    
    description))

(defun bfepm-search-installed-packages (query)
  "Search within installed packages matching QUERY."
  (interactive "sSearch installed packages: ")
  (let* ((installed (bfepm-core-get-installed-packages))
         (query-regex (bfepm-search--build-query-regex query))
         (matching '()))
    
    (dolist (package-name installed)
      (let* ((description (or (and (fboundp 'bfepm-ui--get-package-description)
                                   (bfepm-ui--get-package-description package-name))
                              (bfepm-search--get-package-description package-name)))
             (version (bfepm-core-get-package-version package-name))
             (result (make-bfepm-search-result
                     :name package-name
                     :version version
                     :description (or description "No description available")
                     :source "local"
                     :dependencies nil
                     :kind 'unknown
                     :installed-p t)))
        (when (bfepm-search--matches-query-p result query-regex)
          (push result matching))))
    
    (if (called-interactively-p 'any)
        (bfepm-search--display-results matching)
      matching)))

(provide 'bfepm-search)

;;; bfepm-search.el ends here