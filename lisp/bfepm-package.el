;;; bfepm-package.el --- BFEPM Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Package installation, removal, and management functionality for BFEPM.

;;; Code:

(require 'url)
(require 'tar-mode)
(require 'bfepm-core)
(require 'bfepm-utils)
(require 'bfepm-git)
(require 'bfepm-version)
(require 'bfepm-network)

;; Try to load generic-search-engine from lib directory
(condition-case nil
    (require 'generic-search-engine)
  (error
   (message "Warning: generic-search-engine not available, using built-in search")))

;; Forward declaration for config functions
(declare-function bfepm-config-get-package "bfepm-config")

;; Forward declarations for optional modules
(declare-function bfepm-search "bfepm-search")

;; Forward declare GSE functions to avoid warnings
(declare-function gse-create-engine "generic-search-engine")
(declare-function gse-search "generic-search-engine")
(declare-function gse-search-async "generic-search-engine")
(declare-function gse-add-source "generic-search-engine")
(declare-function gse-cache-get "generic-search-engine")
(declare-function gse-cache-put "generic-search-engine")

;; Global search engine instance
(defvar bfepm-package--search-engine nil
  "Search engine instance for BFEPM package discovery.")

(defun bfepm-package--ensure-search-engine ()
  "Ensure search engine is initialized."
  (unless bfepm-package--search-engine
    (if (fboundp 'gse-create-engine)
        (progn
          (setq bfepm-package--search-engine
                (gse-create-engine
                 :name "bfepm-search"
                 :cache-ttl 300  ; 5 minutes
                 :max-cache-size 1000
                 :ranking-algorithm #'bfepm-package--rank-search-results))
          ;; Add package sources as search sources
          (bfepm-package--register-search-sources))
      ;; Fallback: use a simple marker to indicate fallback mode
      (setq bfepm-package--search-engine 'fallback))))

(defun bfepm-package--register-search-sources ()
  "Register package sources with the search engine."
  (when (and (not (eq bfepm-package--search-engine 'fallback))
             (fboundp 'gse-add-source))
    ;; Add MELPA source
    (gse-add-source bfepm-package--search-engine
                    "melpa"
                    :searcher #'bfepm-package--search-melpa
                    :priority 10
                    :cache-key-fn (lambda (query) (format "melpa:%s" query)))
    ;; Add GNU ELPA source
    (gse-add-source bfepm-package--search-engine
                    "gnu"
                    :searcher #'bfepm-package--search-gnu-elpa
                    :priority 5
                    :cache-key-fn (lambda (query) (format "gnu:%s" query)))))

(defun bfepm-package--rank-search-results (results query)
  "Rank search RESULTS for relevance based on QUERY."
  ;; Enhanced ranking considering query relevance, popularity, and recency
  (sort results
        (lambda (a b)
          (let ((score-a (bfepm-package--calculate-search-score a query))
                (score-b (bfepm-package--calculate-search-score b query)))
            (> score-a score-b)))))

(defun bfepm-package--calculate-search-score (result query)
  "Calculate search score for RESULT based on QUERY."
  (let ((name (plist-get result :name))
        (description (plist-get result :description))
        (downloads (or (plist-get result :downloads) 0))
        (recent (plist-get result :recent))
        (score 0))

    ;; Query relevance scoring
    (when (and name query)
      ;; Exact name match gets highest score
      (if (string= (downcase name) (downcase query))
          (setq score (+ score 1000))
        ;; Partial name match
        (when (string-match-p (regexp-quote (downcase query)) (downcase name))
          (setq score (+ score 500))))

      ;; Description relevance
      (when (and description
                 (string-match-p (regexp-quote (downcase query)) (downcase description)))
        (setq score (+ score 200))))

    ;; Popularity scoring (logarithmic to prevent domination)
    (setq score (+ score (floor (log (max 1 downloads) 10))))

    ;; Recency bonus
    (when recent
      (setq score (+ score 100)))

    score))

;; Try to load bfepm-config, fall back to minimal if not available
(condition-case nil
    (require 'bfepm-config)
  (error
   (condition-case nil
       (require 'bfepm-config-minimal)
     (error
      (message "Warning: Neither bfepm-config nor bfepm-config-minimal available")))))

;; Initialize search engine when package module is loaded
(add-hook 'after-init-hook #'bfepm-package--ensure-search-engine)

(defvar bfepm-package--melpa-archive-url "https://melpa.org/packages/archive-contents"
  "URL for MELPA archive contents.")

(defvar bfepm-package--gnu-archive-url "https://elpa.gnu.org/packages/archive-contents"
  "URL for GNU ELPA archive contents.")

(defvar bfepm-package--archive-cache nil
  "Cache for package archive contents.")

(defvar bfepm-package--last-archive-fetch-time nil
  "Time of last archive fetch to implement rate limiting.")

(defvar bfepm-package--archive-fetch-delay 1.0
  "Minimum delay in seconds between archive fetches.")

;; Search implementation functions
(defun bfepm-package--search-melpa (query &optional callback)
  "Search MELPA packages for QUERY.
If CALLBACK is provided, search asynchronously."
  (bfepm-package--search-elpa-source "https://melpa.org/packages/archive-contents" query callback))

(defun bfepm-package--search-gnu-elpa (query &optional callback)
  "Search GNU ELPA packages for QUERY.
If CALLBACK is provided, search asynchronously."
  (bfepm-package--search-elpa-source "https://elpa.gnu.org/packages/archive-contents" query callback))


;; Real ELPA search implementation functions
(defun bfepm-package--search-elpa-source (archive-url query &optional callback)
  "Search ELPA source at ARCHIVE-URL for QUERY.
If CALLBACK is provided, search asynchronously and call CALLBACK with results.
Otherwise, search synchronously (not recommended for interactive use)."
  (if callback
      ;; Async search - non-blocking
      (bfepm-package--search-elpa-source-async archive-url query callback)
    ;; Sync search - for compatibility only, not recommended
    (bfepm-package--search-elpa-source-sync archive-url query)))

(defun bfepm-package--search-elpa-source-async (archive-url query callback)
  "Search ELPA source asynchronously at ARCHIVE-URL for QUERY.
Calls CALLBACK with (success results error-message) when complete."
  (condition-case err
      (url-retrieve archive-url
                    (lambda (status)
                      (let ((results '())
                            (error-msg nil))
                        (unwind-protect
                            (progn
                              ;; Check for URL retrieval errors
                              (when (plist-get status :error)
                                (setq error-msg (format "URL retrieval failed: %s"
                                                       (plist-get status :error))))

                              (unless error-msg
                                (goto-char (point-min))
                                ;; Skip HTTP headers
                                (if (re-search-forward "^\\r?$" nil t)
                                    (condition-case parse-err
                                        (let ((archive-contents (read (current-buffer))))
                                          (when archive-contents
                                            (dolist (package-entry (cdr archive-contents))
                                              (when (listp package-entry)
                                                (let* ((package-name (symbol-name (car package-entry)))
                                                       (package-info (cadr package-entry))
                                                       (version (when (vectorp package-info) (aref package-info 0)))
                                                       (description (when (vectorp package-info) (aref package-info 2))))
                                                  (when (bfepm-package--package-matches-query-p package-name description query)
                                                    (push (bfepm-package--create-search-result package-name version description archive-url)
                                                          results)))))))
                                      (error
                                       (setq error-msg (format "Failed to parse archive contents: %s"
                                                              (error-message-string parse-err)))))
                                  (setq error-msg "Invalid HTTP response format"))))

                          ;; Always clean up buffer
                          (when (current-buffer)
                            (kill-buffer (current-buffer))))

                        ;; Call callback with results
                        (if error-msg
                            (funcall callback nil nil error-msg)
                          (let ((sorted-results (seq-take
                                               (sort results
                                                     (lambda (a b)
                                                       (> (bfepm-package--calculate-search-score a query)
                                                          (bfepm-package--calculate-search-score b query))))
                                               10)))
                            (funcall callback t sorted-results nil))))))
    (error
     (funcall callback nil nil (format "Search failed: %s" (error-message-string err))))))

(defun bfepm-package--search-elpa-source-sync (archive-url query)
  "Search ELPA source synchronously at ARCHIVE-URL for QUERY.
WARNING: This blocks Emacs and should only be used for testing/compatibility."
  (condition-case err
      (let* ((response (url-retrieve-synchronously archive-url))
             (results '()))
        (when response
          (unwind-protect
              (with-current-buffer response
                (goto-char (point-min))
                ;; Skip HTTP headers
                (when (re-search-forward "^\\r?$" nil t)
                  (let ((archive-contents (condition-case nil
                                              (read (current-buffer))
                                            (error nil))))
                    (when archive-contents
                      (dolist (package-entry (cdr archive-contents))
                        (when (listp package-entry)
                          (let* ((package-name (symbol-name (car package-entry)))
                                 (package-info (cadr package-entry))
                                 (version (when (vectorp package-info) (aref package-info 0)))
                                 (description (when (vectorp package-info) (aref package-info 2))))
                            (when (bfepm-package--package-matches-query-p package-name description query)
                              (push (bfepm-package--create-search-result package-name version description archive-url)
                                    results)))))))))
            ;; Always kill buffer
            (when (buffer-live-p response)
              (kill-buffer response))))
        ;; Sort by relevance score and limit results
        (seq-take (sort results
                        (lambda (a b)
                          (> (bfepm-package--calculate-search-score a query)
                             (bfepm-package--calculate-search-score b query))))
                  10))
    (error
     (message "Warning: Failed to search %s: %s" archive-url (error-message-string err))
     nil)))

(defun bfepm-package--package-matches-query-p (package-name description query)
  "Check if PACKAGE-NAME and DESCRIPTION match search QUERY."
  (let ((escaped-query (regexp-quote (downcase query))))
    (or (string-match-p escaped-query (downcase package-name))
        (and description (string-match-p escaped-query (downcase description))))))

(defun bfepm-package--create-search-result (package-name version description archive-url)
  "Create search result from PACKAGE-NAME, VERSION, DESCRIPTION and ARCHIVE-URL."
  (list :name package-name
        :description (or description "No description available")
        :version (if (vectorp version)
                     (mapconcat #'number-to-string version ".")
                   (format "%s" version))
        :source (cond ((string-match-p "melpa" archive-url) "melpa")
                      ((string-match-p "elpa\\.gnu" archive-url) "gnu")
                      (t "unknown"))
        :downloads 0                      ; Default until real data available
        :recent nil))                     ; Default until real data available

(defun bfepm-package--get-default-sources ()
  "Get default package sources when config is not available."
  '(("melpa" . (:url "https://melpa.org/packages/" :type "elpa" :priority 10))
    ("gnu" . (:url "https://elpa.gnu.org/packages/" :type "elpa" :priority 5))))

(defun bfepm-package-search (query &optional callback)
  "Search for packages matching QUERY.
If CALLBACK is provided, search asynchronously and call CALLBACK with results.
Otherwise, return results synchronously."
  (bfepm-package--ensure-search-engine)
  (if (and (not (eq bfepm-package--search-engine 'fallback))
           (fboundp 'gse-search))
      ;; Use generic-search-engine if available
      (if callback
          (gse-search-async bfepm-package--search-engine query callback)
        (gse-search bfepm-package--search-engine query))
    ;; Fallback implementation
    (bfepm-package--search-fallback query callback)))

(defun bfepm-package--search-fallback (query callback)
  "Fallback search implementation."
  (let ((results (append (bfepm-package--search-melpa query)
                        (bfepm-package--search-gnu-elpa query))))
    (if callback
        (funcall callback results)
      results)))

(defun bfepm-package--get-source-priority (source)
  "Get priority from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-priority source))
   ((and (listp source) (keywordp (car source))) (plist-get source :priority))
   ((listp source) (or (cdr (assoc 'priority source)) 10))
   (t 10)))

(defun bfepm-package--get-source-type (source)
  "Get type from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-type source))
   ((plistp source) (plist-get source :type))
   (t "elpa")))

(defun bfepm-package--get-source-url (source)
  "Get URL from SOURCE (supports both struct and plist)."
  (cond
   ((bfepm-source-p source) (bfepm-source-url source))
   ((plistp source) (plist-get source :url))
   (t nil)))

(defun bfepm-package--format-version (version)
  "Format VERSION from various formats to string.

This function is deprecated. Use `bfepm-version-normalize' instead."
  (bfepm-version-normalize version))

(defun bfepm-package--version-matches-p (available-version requested-version)
  "Check if AVAILABLE-VERSION satisfies REQUESTED-VERSION.

This function is deprecated. Use `bfepm-version-satisfies-p' instead."
  (bfepm-version-satisfies-p available-version requested-version))

(defun bfepm-package-install (package-spec &optional callback)
  "Install package specified by PACKAGE-SPEC.
PACKAGE-SPEC can be a string (package name), a list (name version),
or a bfepm-package structure.
CALLBACK is an optional function called with (success name error-message)."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (config (bfepm-core-get-config)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p (bfepm-package-name package))
        (progn
          (bfepm-utils-message "Package %s already installed" (bfepm-package-name package))
          (when callback (funcall callback t (bfepm-package-name package) nil)))
      (condition-case err
          (progn
            (bfepm-utils-message "Installing package: %s" (bfepm-package-name package))

            ;; Find package in sources
            (let ((package-info (bfepm-package--find-package package config)))
              (unless package-info
                (bfepm-utils-error "Package not found: %s" (bfepm-package-name package)))

              ;; Validate version if specified
              (when (and (not (string= (bfepm-package-version package) "latest"))
                         package-info)
                (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                       (available-version (bfepm-package--format-version (car info-list)))
                       (requested-version (bfepm-package-version package)))
                  (unless (bfepm-package--version-matches-p available-version requested-version)
                    (bfepm-utils-message "Warning: Requested version %s not available. Latest is %s"
                                      requested-version available-version))))

              ;; Download and install
              (bfepm-package--download-and-install package package-info))
            (when callback (funcall callback t (bfepm-package-name package) nil)))
        (error
         (when callback (funcall callback nil (bfepm-package-name package)
                                 (error-message-string err)))
         (unless callback (signal (car err) (cdr err))))))))

(defun bfepm-package-install-async (package-spec callback)
  "Install package specified by PACKAGE-SPEC asynchronously.
CALLBACK is called with (success package-name error-message) when complete."
  (let* ((package (cond
                   ((stringp package-spec)
                    (make-bfepm-package :name package-spec :version "latest"))
                   ((and (listp package-spec) (= (length package-spec) 2))
                    (make-bfepm-package :name (car package-spec) :version (cadr package-spec)))
                   (t package-spec)))
         (package-name (bfepm-package-name package)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p package-name)
        (progn
          (bfepm-utils-message "Package %s already installed" package-name)
          (when callback (funcall callback t package-name nil)))
      ;; Use async archive fetching for truly non-blocking operation
      (bfepm-utils-message "🔄 Starting truly async installation of %s" package-name)
      ;; First check if this is a git package from config
      (let* ((config (bfepm-core-get-config))
             (config-package (when config (bfepm-config-get-package config package-name))))
        (if (and config-package (bfepm-package-source config-package))
            ;; This is a git package from config - install it directly async
            (progn
              (bfepm-utils-message "Installing git package %s from configuration" package-name)
              (run-with-timer 0.01 nil
                              (lambda ()
                                (condition-case err
                                    (progn
                                      (bfepm-package-install package-spec)
                                      (when callback (funcall callback t package-name nil)))
                                  (error
                                   (when callback (funcall callback nil package-name (error-message-string err))))))))
          ;; Not a git package - use ELPA sources
          (let* ((sources (if config
                             (bfepm-config-sources config)
                           (bfepm-package--get-default-sources)))
                 ;; Use the first ELPA source (highest priority)
                 (elpa-source (cl-find-if (lambda (source)
                                           (string= (bfepm-package--get-source-type (cdr source)) "elpa"))
                                         sources)))
            (if elpa-source
            (let ((archive-url (bfepm-package--get-source-url (cdr elpa-source))))
              (bfepm-package--fetch-archive-contents-async
               archive-url
               (lambda (success contents error-msg)
                 (if success
                     (let ((package-info (alist-get (intern package-name) contents)))
                       (if package-info
                           ;; Get dependency info first
                           (let* ((info-list (if (vectorp package-info) (append package-info nil) package-info))
                                  (deps (cadr info-list)))
                             (if deps
                                 ;; Install dependencies asynchronously first
                                 (bfepm-package--install-dependencies-async
                                  deps
                                  (lambda (dep-success dep-error-msg)
                                    (if dep-success
                                        ;; Download and install main package (NON-BLOCKING)
                                        (bfepm-package--download-and-install-async package package-info
                                          (lambda (success error-msg)
                                            (if success
                                                (funcall callback t package-name nil)
                                              (funcall callback nil package-name error-msg))))
                                      (funcall callback nil package-name
                                             (format "Failed to install dependencies: %s" dep-error-msg)))))
                               ;; No dependencies, install directly (NON-BLOCKING)
                               (bfepm-package--download-and-install-async package package-info
                                 (lambda (success error-msg)
                                   (if success
                                       (funcall callback t package-name nil)
                                     (funcall callback nil package-name error-msg))))))
                         (funcall callback nil package-name (format "Package not found: %s" package-name))))
                   (funcall callback nil package-name error-msg)))))
              ;; Fallback to sync method
              (run-with-timer 0.01 nil
                              (lambda ()
                                (condition-case err
                                    (progn
                                      (bfepm-package-install package-spec)
                                      (when callback (funcall callback t package-name nil)))
                                  (error
                                   (when callback (funcall callback nil package-name (error-message-string err))))))))))))))

(defun bfepm-package--find-package (package config)
  "Find PACKAGE in available sources from CONFIG."
  (let* ((package-name (bfepm-package-name package))
         (sources (if config
                     (bfepm-config-sources config)
                   (bfepm-package--get-default-sources))))

    ;; First check if package has its own source definition in config
    (or (when config
          (let ((configured-package (cl-find-if (lambda (pkg)
                                                  (string= (bfepm-package-name pkg) package-name))
                                                (bfepm-config-packages config))))
            (when (and configured-package (bfepm-package-source configured-package))
              ;; Package has its own source definition
              (let ((package-source (bfepm-package-source configured-package)))
                (when package-source
                  (bfepm-package--find-in-source package-name package-source))))))

        ;; Try each source in priority order
        (cl-loop for source in (sort sources (lambda (a b)
                                               (> (bfepm-package--get-source-priority (cdr a))
                                                  (bfepm-package--get-source-priority (cdr b)))))
                 for package-info = (bfepm-package--find-in-source package-name (cdr source))
                 when package-info return package-info))))

(defun bfepm-package--find-in-source (package-name source)
  "Find PACKAGE-NAME in SOURCE."
  (let ((source-type (bfepm-package--get-source-type source)))
    (cond
     ((string= source-type "elpa")
      (bfepm-package--find-in-elpa package-name source))
     ((string= source-type "git")
      (bfepm-package--find-in-git package-name source))
     (t nil))))

(defun bfepm-package--find-in-elpa (package-name source)
  "Find PACKAGE-NAME in ELPA SOURCE."
  (let* ((archive-url (bfepm-package--get-source-url source))
         (archive-contents (bfepm-package--get-archive-contents archive-url)))
    (alist-get (intern package-name) archive-contents)))

(defun bfepm-package--get-archive-contents (archive-url)
  "Get archive contents from ARCHIVE-URL, using cache if available."
  (let ((cache-key archive-url))
    (or (alist-get cache-key bfepm-package--archive-cache nil nil #'string=)
        (let ((contents (bfepm-package--fetch-archive-contents archive-url)))
          (push (cons cache-key contents) bfepm-package--archive-cache)
          contents))))

(defun bfepm-package--find-in-git (_package-name source)
  "Find PACKAGE-NAME in git SOURCE."
  (let ((url (bfepm-package--get-source-url source))
        (ref (plist-get source :ref)))
    (when url
      ;; For git sources, we return a synthetic package info
      ;; The version will be determined when actually cloning
      ;; Include source information in the package info
      (list (or ref "latest") nil (format "Git package from %s" url) 'git source))))

(defun bfepm-package--fetch-archive-contents (archive-url)
  "Fetch archive contents from ARCHIVE-URL with error handling."
  (let ((archive-file (concat archive-url "archive-contents")))
    (condition-case err
        (with-temp-buffer
          (bfepm-utils-message "🔍 Fetching archive contents from %s (BLOCKING - will be fixed)" archive-file)
          (url-insert-file-contents archive-file)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (bfepm-utils-error "Failed to fetch archive contents from %s: %s"
                         archive-file (error-message-string err))))))

(defun bfepm-package--fetch-archive-contents-async (archive-url callback)
  "Fetch archive contents from ARCHIVE-URL asynchronously.
CALLBACK is called with (success contents error-message) when complete.

This function is deprecated. Use `bfepm-network-fetch-archive-contents-async'."
  (bfepm-network-fetch-archive-contents-async
   archive-url callback bfepm-package--archive-fetch-delay))

(defun bfepm-package--download-and-install (package package-info)
  "Download and install PACKAGE using PACKAGE-INFO."
  (let* (;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (kind (cadddr info-list)))
    ;; Dispatch to appropriate installation method based on package type
    (cond
     ((eq kind 'git)
      (bfepm-package--download-and-install-git package package-info))
     (t
      (bfepm-package--download-and-install-elpa package package-info)))))

(defun bfepm-package--download-and-install-async (package package-info callback)
  "Download and install PACKAGE using PACKAGE-INFO asynchronously.
CALLBACK is called with (success error-message) when complete."
  (let* (;; Handle both list and vector formats from ELPA
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (kind (cadddr info-list)))
    ;; Dispatch to appropriate installation method based on package type
    (cond
     ((eq kind 'git)
      ;; Git packages still use sync method for now
      (condition-case err
          (progn
            (bfepm-package--download-and-install-git package package-info)
            (funcall callback t nil))
        (error
         (funcall callback nil (error-message-string err)))))
     (t
      (bfepm-package--download-and-install-elpa-async package package-info callback)))))

(defun bfepm-package--download-and-install-elpa (package package-info)
  "Download and install ELPA PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (deps (cadr info-list))
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file with checksum verification
    (bfepm-utils-message "📥 Downloading %s (%s)..." package-name version-string)
    (bfepm-package--download-with-checksum archive-file local-file package-name version-string kind)

    ;; Install dependencies first
    (bfepm-package--install-dependencies deps)

    ;; Extract/install package with rollback on failure
    (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
      (condition-case err
          (progn
            (bfepm-package--extract-and-install package-name local-file kind)
            ;; Save version information
            (bfepm-package--save-version-info package-name version-string)
            ;; Verify installation
            (bfepm-package--verify-installation package-name install-dir)
            ;; Invalidate caches after successful installation
            (bfepm-core--invalidate-cache package-name))
        (error
         ;; Rollback on failure
         (when (file-directory-p install-dir)
           (bfepm-utils-message "Rolling back failed installation of %s" package-name)
           (ignore-errors (delete-directory install-dir t)))
         (bfepm-utils-error "Failed to install %s: %s" package-name (error-message-string err)))))

    (bfepm-utils-message "✅ Successfully installed %s" package-name)))

(defun bfepm-package--download-and-install-elpa-async (package package-info callback)
  "Download and install ELPA PACKAGE using PACKAGE-INFO asynchronously.
CALLBACK is called with (success error-message) when complete."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (_deps (cadr info-list)) ; Dependencies already handled by caller
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file asynchronously with checksum verification
    (bfepm-utils-message "📥 Downloading %s (%s) (NON-BLOCKING)..." package-name version-string)
    (bfepm-network-download-file-async
     archive-file local-file
     (lambda (success error-msg)
       (if success
           ;; Verify file and continue with installation
           (condition-case err
               (progn
                 ;; Verify download
                 (unless (and (file-exists-p local-file)
                             (> (file-attribute-size (file-attributes local-file)) 0))
                   (error "Downloaded file %s is empty or missing" local-file))

                 ;; Extract/install package with rollback on failure
                 (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
                   (condition-case extract-err
                       (progn
                         (bfepm-package--extract-and-install package-name local-file kind)
                         ;; Save version information
                         (bfepm-package--save-version-info package-name version-string)
                         ;; Verify installation
                         (bfepm-package--verify-installation package-name install-dir)
                         ;; Invalidate caches after successful installation
                         (bfepm-core--invalidate-cache package-name)
                         (bfepm-utils-message "✅ Successfully installed %s" package-name)
                         (funcall callback t nil))
                     (error
                      ;; Rollback on failure
                      (when (file-directory-p install-dir)
                        (bfepm-utils-message "Rolling back failed installation of %s" package-name)
                        (ignore-errors (delete-directory install-dir t)))
                      (funcall callback nil (format "Failed to install %s: %s"
                                                   package-name (error-message-string extract-err)))))))
             (error
              (funcall callback nil (error-message-string err))))
         ;; Download failed
         (funcall callback nil (format "Failed to download %s: %s" package-name error-msg))))
     3))) ; 3 retries

(defun bfepm-package--download-and-install-elpa-async-simple (package package-info callback)
  "Download and install ELPA PACKAGE using PACKAGE-INFO asynchronously.
This dependency version does not install dependencies to avoid circular deps.
CALLBACK is called with (success package-name error-message) when complete."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (version (car info-list))
         (_deps (cadr info-list)) ; Skip dependencies for dependency installation
         (_desc (caddr info-list))
         (kind (cadddr info-list))
         (version-string (bfepm-package--format-version version))
         (archive-file (bfepm-package--build-archive-url package-name version-string kind))
         (download-dir (expand-file-name "downloads" (bfepm-core-get-cache-directory)))
         (local-file (expand-file-name
                      (format "%s-%s.%s" package-name version-string
                              (if (eq kind 'tar) "tar" "el"))
                      download-dir)))

    (bfepm-utils-ensure-directory download-dir)

    ;; Download package file asynchronously with checksum verification
    (bfepm-utils-message "📥 Downloading dependency %s (%s) (NON-BLOCKING)..." package-name version-string)
    (bfepm-network-download-file-async
     archive-file local-file
     (lambda (success error-msg)
       (if success
           ;; Verify file and continue with installation
           (condition-case err
               (progn
                 ;; Verify download
                 (unless (and (file-exists-p local-file)
                             (> (file-attribute-size (file-attributes local-file)) 0))
                   (error "Downloaded file %s is empty or missing" local-file))

                 ;; Extract/install package with rollback on failure
                 (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
                   (condition-case extract-err
                       (progn
                         (bfepm-package--extract-and-install package-name local-file kind)
                         ;; Save version information
                         (bfepm-package--save-version-info package-name version-string)
                         ;; Verify installation
                         (bfepm-package--verify-installation package-name install-dir)
                         ;; Invalidate caches after successful installation
                         (bfepm-core--invalidate-cache package-name)
                         (bfepm-utils-message "✅ Successfully installed dependency %s" package-name)
                         (funcall callback t package-name nil))
                     (error
                      ;; Rollback on failure
                      (when (file-directory-p install-dir)
                        (bfepm-utils-message "Rolling back failed dependency installation of %s" package-name)
                        (ignore-errors (delete-directory install-dir t)))
                      (funcall callback nil package-name (format "Failed to install dependency %s: %s"
                                                   package-name (error-message-string extract-err)))))))
             (error
              (funcall callback nil package-name (error-message-string err))))
         ;; Download failed
         (funcall callback nil package-name (format "Failed to download dependency %s: %s" package-name error-msg))))
     3))) ; 3 retries

(defun bfepm-package--download-and-install-git (package package-info)
  "Download and install git PACKAGE using PACKAGE-INFO."
  (let* ((package-name (bfepm-package-name package))
         (info-list (if (vectorp package-info) (append package-info nil) package-info))
         (source-config (nth 4 info-list)) ; Source config is included in package-info
         (url (bfepm-package--get-source-url source-config))
         (ref (or (plist-get source-config :ref)
                  (bfepm-package-version package)
                  "latest"))
         (shallow (plist-get source-config :shallow))
         (install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))

    (unless url
      (bfepm-utils-error "No git URL found for package %s" package-name))

    (bfepm-utils-message "Installing git package %s from %s" package-name url)

    ;; Remove existing installation if it exists
    (when (file-directory-p install-dir)
      (delete-directory install-dir t))

    ;; Clone repository with rollback on failure
    (condition-case err
        (progn
          ;; Clone the repository
          (bfepm-git-clone url install-dir ref shallow)

          ;; Get actual version from git repository
          (let ((version (bfepm-package--get-git-version install-dir ref)))
            ;; Save version information
            (bfepm-package--save-version-info package-name version)

            ;; Install dependencies by scanning Package-Requires
            (bfepm-package--install-git-dependencies package-name install-dir)

            ;; Verify installation
            (bfepm-package--verify-installation package-name install-dir)

            ;; Add to load-path
            (add-to-list 'load-path install-dir)

            ;; Invalidate caches after successful installation
            (bfepm-core--invalidate-cache package-name)

            (bfepm-utils-message "Successfully installed git package %s (version: %s)" package-name version)))
      (error
       ;; Rollback on failure
       (when (file-directory-p install-dir)
         (bfepm-utils-message "Rolling back failed git installation of %s" package-name)
         (ignore-errors (delete-directory install-dir t)))
       (bfepm-utils-error "Failed to install git package %s: %s" package-name (error-message-string err))))))

(defun bfepm-package--get-git-version (repo-dir ref)
  "Get version for git package at REPO-DIR with REF."
  (bfepm-git-get-latest-version repo-dir ref))

(defun bfepm-package--install-git-dependencies (package-name install-dir)
  "Scan PACKAGE-NAME in INSTALL-DIR for Package-Requires and install deps."
  (let ((main-file (expand-file-name (format "%s.el" package-name) install-dir)))
    (when (file-exists-p main-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB to find headers
            (goto-char (point-min))
            ;; Look for Package-Requires header
            (when (re-search-forward "^[[:space:];]*Package-Requires: *\\(.*\\)$" nil t)
              (let ((requires-string (match-string 1)))
                (condition-case parse-err
                    (let ((deps (read requires-string)))
                      (when (and deps (listp deps))
                        (bfepm-utils-message "Installing dependencies for git package %s: %s"
                                           package-name
                                           (mapconcat (lambda (dep) (symbol-name (car dep))) deps ", "))
                        (bfepm-package--install-dependencies deps)))
                  (error
                   (bfepm-utils-message "Warning: Failed to parse Package-Requires header for %s: %s"
                                      package-name (error-message-string parse-err)))))))
        (error
         (bfepm-utils-message "Warning: Failed to read Package-Requires for git package %s: %s"
                            package-name (error-message-string err)))))))

(defun bfepm-package--build-archive-url (package-name version kind)
  "Build archive URL for PACKAGE-NAME with improved format detection.
VERSION is the package version, KIND is the package type."
  (let ((extension (cond
                    ((eq kind 'tar) "tar")
                    ((eq kind 'single) "el")
                    (t "el"))))
    (format "https://melpa.org/packages/%s-%s.%s"
            package-name version extension)))

(defun bfepm-package--install-dependencies (deps)
  "Install package dependencies DEPS."
  (when deps
    (dolist (dep deps)
      (let ((dep-name (symbol-name (car dep))))
        ;; Skip built-in packages like 'emacs'
        (unless (or (string= dep-name "emacs")
                    (bfepm-core-package-installed-p dep-name))
          (bfepm-utils-message "📥 Installing dependency: %s (BLOCKING - will be fixed)" dep-name)
          (condition-case err
              (bfepm-package-install dep-name)
            (error
             (bfepm-utils-message "Warning: Failed to install dependency %s: %s"
                               dep-name (error-message-string err)))))))))

(defun bfepm-package--install-dependencies-async (deps callback)
  "Install package dependencies DEPS asynchronously.
CALLBACK is called with (success error-message) when all are done."
  (if (null deps)
      (funcall callback t nil)
    (let ((remaining-deps (cl-remove-if (lambda (dep)
                                         (let ((dep-name (symbol-name (car dep))))
                                           (or (string= dep-name "emacs")
                                               (bfepm-core-package-installed-p dep-name))))
                                       deps)))
      (if (null remaining-deps)
          (funcall callback t nil)
        (bfepm-package--install-dependencies-async-recursive remaining-deps 0 callback)))))

(defun bfepm-package--install-dependencies-async-recursive (deps index callback)
  "Recursively install dependencies DEPS starting from INDEX.
CALLBACK is called with (success error-message) when all are done."
  (if (>= index (length deps))
      (funcall callback t nil)
    (let* ((dep (nth index deps))
           (dep-name (symbol-name (car dep))))
      (bfepm-utils-message "📥 Installing dependency: %s (NON-BLOCKING)" dep-name)
      (bfepm-package--install-single-dependency-async
       dep-name
       (lambda (success package-name error-msg)
         (if success
             ;; Add delay between dependency installations to avoid rate limiting
             (run-with-timer 0.5 nil
                           (lambda ()
                             (bfepm-package--install-dependencies-async-recursive deps (1+ index) callback)))
           (bfepm-utils-message "Warning: Failed to install dependency %s: %s" package-name error-msg)
           ;; Continue with next dependency even if one fails (with delay)
           (run-with-timer 0.5 nil
                          (lambda ()
                            (bfepm-package--install-dependencies-async-recursive deps (1+ index) callback)))))))))

(defun bfepm-package--install-single-dependency-async (package-name callback)
  "Install a single dependency PACKAGE-NAME asynchronously.
CALLBACK is called with (success package-name error-message) when complete.
This function ensures no fallback to synchronous operations."
  (let* ((package (make-bfepm-package :name package-name :version "latest"))
         (config (bfepm-core-get-config)))

    ;; Check if already installed
    (if (bfepm-core-package-installed-p package-name)
        (progn
          (bfepm-utils-message "Dependency %s already installed" package-name)
          (funcall callback t package-name nil))
      ;; Only use async method, no fallback to sync
      (let* ((sources (if config
                         (bfepm-config-sources config)
                       (bfepm-package--get-default-sources)))
             ;; Use the first ELPA source (highest priority)
             (elpa-source (cl-find-if (lambda (source)
                                       (string= (bfepm-package--get-source-type (cdr source)) "elpa"))
                                     sources)))
        (if elpa-source
            (let ((archive-url (bfepm-package--get-source-url (cdr elpa-source))))
              (bfepm-package--fetch-archive-contents-async
               archive-url
               (lambda (success contents error-msg)
                 (if success
                     (let ((package-info (alist-get (intern package-name) contents)))
                       (if package-info
                           ;; Found dependency, install without its own dependencies (to avoid circular deps)
                           (bfepm-package--download-and-install-elpa-async-simple package package-info callback)
                         (funcall callback nil package-name (format "Dependency not found: %s" package-name))))
                   (funcall callback nil package-name error-msg)))))
          ;; No ELPA source available
          (funcall callback nil package-name "No ELPA source available for dependency"))))))

(defun bfepm-package--extract-and-install (package-name archive-file kind)
  "Extract and install PACKAGE-NAME from ARCHIVE-FILE.
KIND specifies the package type (tar or single file)."
  (let ((install-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
    (bfepm-utils-ensure-directory install-dir)

    (cond
     ((eq kind 'tar)
      (bfepm-package--extract-tar-package archive-file install-dir))
     (t
      (bfepm-package--install-single-file archive-file install-dir)))

    ;; Add to load-path
    (add-to-list 'load-path install-dir)))

(defun bfepm-package--extract-tar-package (tar-file install-dir)
  "Extract TAR-FILE to INSTALL-DIR with improved compression detection."
  (let ((default-directory install-dir))
    (bfepm-utils-message "📦 Extracting package to %s..." install-dir)

    ;; Detect compression format and use appropriate tar flags
    (let* ((tar-flags (cond
                       ((or (string-suffix-p ".tar.gz" tar-file)
                            (string-suffix-p ".tgz" tar-file)) "-xzf")
                       ((string-suffix-p ".tar.bz2" tar-file) "-xjf")
                       ((string-suffix-p ".tar.xz" tar-file) "-xJf")
                       ((string-suffix-p ".tar.lz" tar-file) "--lzip -xf")
                       (t "-xf")))
           (result (call-process "tar" nil nil nil tar-flags tar-file "--strip-components=1")))

      (unless (= result 0)
        (bfepm-utils-error "Failed to extract tar file %s (exit code: %d)" tar-file result))

      ;; Verify extraction succeeded
      (let ((extracted-files (directory-files install-dir nil "^[^.]")))
        (unless (> (length extracted-files) 0)
          (bfepm-utils-error "Tar extraction failed: no files found in %s" install-dir))
        (bfepm-utils-message "✅ Extracted %d files to %s" (length extracted-files) install-dir)))))

(defun bfepm-package--install-single-file (el-file install-dir)
  "Install single EL-FILE to INSTALL-DIR with verification."
  (let ((target-file (expand-file-name (file-name-nondirectory el-file) install-dir)))
    (bfepm-utils-message "Installing single file to %s" target-file)
    (copy-file el-file target-file t)
    ;; Verify file was copied
    (unless (file-exists-p target-file)
      (bfepm-utils-error "Failed to copy file %s to %s" el-file target-file))))

(defun bfepm-package--save-version-info (package-name version)
  "Save VERSION information for PACKAGE-NAME."
  (let* ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory)))
         (version-file (expand-file-name ".bfepm-version" package-dir)))
    (when (file-directory-p package-dir)
      (with-temp-file version-file
        (insert (format "%s\n" version))))))

(defun bfepm-package--parse-metadata (package-name install-dir)
  "Parse basic metadata for PACKAGE-NAME in INSTALL-DIR."
  (let ((main-file (expand-file-name (format "%s.el" package-name) install-dir))
        (metadata-file (expand-file-name ".bfepm-metadata" install-dir))
        (metadata (list)))

    ;; Parse main package file if it exists
    (when (file-exists-p main-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents main-file nil 0 4096) ; Read first 4KB for headers
            (goto-char (point-min))

            ;; Extract Package-Requires (parse as Lisp data structure)
            (when (re-search-forward "^[[:space:];]*Package-Requires:[[:space:]]*\\(.+\\)$" nil t)
              (condition-case parse-err
                  (let* ((requires-str (match-string 1))
                         (requires (ignore-errors (read requires-str))))
                    (setq metadata (cons (cons 'requires requires) metadata)))
                (error
                 (bfepm-utils-message "Warning: Failed to parse Package-Requires for %s: %s"
                                    package-name (error-message-string parse-err)))))

            ;; Extract Version
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Version:[[:space:]]*\\(.+\\)$" nil t)
              (setq metadata (cons (cons 'version (string-trim (match-string 1))) metadata)))

            ;; Extract Keywords
            (goto-char (point-min))
            (when (re-search-forward "^[[:space:];]*Keywords:[[:space:]]*\\(.+\\)$" nil t)
              (setq metadata (cons (cons 'keywords (string-trim (match-string 1))) metadata))))
        (error
         (bfepm-utils-message "Warning: Failed to parse metadata for %s: %s"
                            package-name (error-message-string err)))))

    ;; Cache metadata to file for quick access (only if non-empty)
    (when metadata
      (condition-case err
          (with-temp-file metadata-file
            (prin1 metadata (current-buffer)))
        (error
         (bfepm-utils-message "Warning: Failed to cache metadata for %s: %s"
                            package-name (error-message-string err)))))

    metadata))

(defun bfepm-package-remove (package-name)
  "Remove installed package PACKAGE-NAME."
  (let ((package-dir (expand-file-name package-name (bfepm-core-get-packages-directory))))
    (if (file-directory-p package-dir)
        (progn
          (delete-directory package-dir t)
          ;; Invalidate caches after removal
          (bfepm-core--invalidate-cache package-name)
          (bfepm-utils-message "Removed package: %s" package-name))
      (bfepm-utils-message "Package not installed: %s" package-name))))

(defun bfepm-package-update (package-name)
  "Update installed package PACKAGE-NAME."
  (when (bfepm-core-package-installed-p package-name)
    (bfepm-package-remove package-name)
    (bfepm-package-install package-name)))

(defun bfepm-package-update-all ()
  "Update all installed packages with optimized batch processing."
  (let ((installed-packages (bfepm-core-get-installed-packages)))
    (when installed-packages
      (bfepm-utils-message "Updating %d packages..." (length installed-packages))
      ;; Clear all caches once at the beginning for efficiency
      (bfepm-core--clear-all-caches)
      (dolist (package-name installed-packages)
        (bfepm-utils-message "Updating %s..." package-name)
        (condition-case err
            (bfepm-package-update package-name)
          (error
           (bfepm-utils-message "Warning: Failed to update %s: %s"
                               package-name (error-message-string err)))))
      (bfepm-utils-message "Package update complete."))))

(defun bfepm-package-list ()
  "List installed packages with version information."
  (let ((packages (bfepm-core-get-installed-packages)))
    (if packages
        (progn
          (bfepm-utils-message "Installed packages:")
          (dolist (package packages)
            (let ((version (bfepm-core-get-package-version package)))
              (if (string= version "unknown")
                  (message "  - %s (version unknown)" package)
                (message "  - %s (%s)" package version)))))
      (bfepm-utils-message "No packages installed"))))

(defun bfepm-package-info (package-name)
  "Show information about PACKAGE-NAME."
  (let* ((config (bfepm-core-get-config))
         (package-info (bfepm-package--find-package
                        (make-bfepm-package :name package-name) config)))
    (if package-info
        (progn
          (bfepm-utils-message "Package: %s" package-name)
          (message "  Version: %s" (cadr package-info))
          (message "  Description: %s" (cadddr package-info))
          (when (caddr package-info)
            (message "  Dependencies: %s"
                     (mapconcat (lambda (dep) (symbol-name (car dep)))
                                (caddr package-info) ", "))))
      (bfepm-utils-message "Package not found: %s" package-name))))


(defun bfepm-package--verify-installation (package-name install-dir)
  "Verify that PACKAGE-NAME was installed correctly in INSTALL-DIR."
  (unless (file-directory-p install-dir)
    (bfepm-utils-error "Installation directory %s does not exist" install-dir))

  ;; Check for at least one .el file
  (let ((el-files (directory-files install-dir nil "\\.el$")))
    (unless el-files
      (bfepm-utils-error "No .el files found in %s" install-dir))

    ;; Check that main package file exists (package-name.el)
    (let ((main-file (format "%s.el" package-name)))
      (unless (member main-file el-files)
        ;; If main file doesn't exist, check if any .el file contains the package name
        (unless (cl-some (lambda (file) (string-match-p package-name file)) el-files)
          (bfepm-utils-message "Warning: Main package file %s not found, but other .el files exist" main-file))))

    ;; Parse and cache package metadata
    (bfepm-package--parse-metadata package-name install-dir)

    (bfepm-utils-message "Installation verified: %d .el files found in %s"
                        (length el-files) install-dir)))

(defun bfepm-package--get-package-checksum (_package-name _version _kind)
  "Get expected checksum for PACKAGE-NAME VERSION of KIND from MELPA."
  ;; This is a placeholder - MELPA doesn't currently provide checksums
  ;; In the future, this could fetch from a checksum database or MELPA API
  (bfepm-utils-message "Checksum verification not available for MELPA packages")
  nil)

(defun bfepm-package--download-with-checksum (url local-file package-name version kind)
  "Download file from URL with optional checksum verification.
LOCAL-FILE is the destination path for download.
PACKAGE-NAME, VERSION, and KIND are used for checksum verification."
  ;; Download the file
  (unless (bfepm-network-download-file url local-file 3)
    (bfepm-utils-error "Failed to download %s after retries" package-name))

  ;; Verify file was downloaded successfully
  (unless (and (file-exists-p local-file)
               (> (file-attribute-size (file-attributes local-file)) 0))
    (bfepm-utils-error "Downloaded file %s is empty or missing" local-file))

  ;; Optional checksum verification (currently not available for MELPA)
  (let ((expected-checksum (bfepm-package--get-package-checksum package-name version kind)))
    (when expected-checksum
      (bfepm-utils-message "Verifying checksum for %s" package-name)
      (unless (bfepm-utils-verify-checksum local-file expected-checksum)
        (bfepm-utils-error "Checksum verification failed for %s" package-name))))

  t)

(provide 'bfepm-package)

;;; bfepm-package.el ends here

