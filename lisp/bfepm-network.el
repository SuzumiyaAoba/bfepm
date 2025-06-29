;;; bfepm-network.el --- Network and HTTP operations for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides network operations including async file downloads,
;; HTTP requests, and archive content fetching. It centralizes all network
;; functionality with proper error handling, retry logic, and rate limiting.

;;; Code:

(require 'url)

;; Try to load generic-http-client from lib directory
(condition-case nil
    (require 'generic-http-client)
  (error
   (message "Warning: generic-http-client not available, falling back to url.el")))

;; Global HTTP client instance
(defvar bfepm-network--http-client nil
  "Global HTTP client instance for BFEPM network operations.")

(defun bfepm-network--ensure-client ()
  "Ensure HTTP client is initialized."
  (unless bfepm-network--http-client
    (if (fboundp 'ghc-create-client)
        (setq bfepm-network--http-client
              (ghc-create-client 
               :user-agent "BFEPM/1.0 (Better Fast Emacs Package Manager)"
               :timeout 30
               :retry-count 3
               :retry-strategy 'exponential
               :rate-limit 2))  ; 2 requests per second
      ;; Fallback: use a simple marker to indicate fallback mode
      (setq bfepm-network--http-client 'fallback))))

;; Constants for network operations
(defconst bfepm-network--default-max-retries 3
  "Default maximum number of retry attempts for network operations.")

(defconst bfepm-network--default-connectivity-timeout 5
  "Default timeout in seconds for connectivity checks.")

(defconst bfepm-network--performance-test-threshold 0.1
  "Maximum acceptable time in seconds for performance tests.")

(defvar bfepm-network--last-request-time nil
  "Time of last network request for rate limiting.")

(defvar bfepm-network--request-delay 0.5
  "Minimum delay in seconds between network requests.")

;; Forward declare GHC functions to avoid warnings
(declare-function ghc-create-client "generic-http-client")
(declare-function ghc-get "generic-http-client")
(declare-function ghc-get-async "generic-http-client")
(declare-function ghc-download-file "generic-http-client")
(declare-function ghc-download-file-async "generic-http-client")
(declare-function ghc-response-success-p "generic-http-client")
(declare-function ghc-response-body "generic-http-client")
(declare-function ghc-response-error "generic-http-client")

(defcustom bfepm-network-connectivity-test-url "https://httpbin.org/status/200"
  "URL used to test network connectivity."
  :type 'string
  :group 'bfepm)

(defun bfepm-network-http-get (url)
  "Make HTTP GET request to URL and return response body.
This is a synchronous operation that blocks until completion."
  (bfepm-network--ensure-client)
  (if (and (not (eq bfepm-network--http-client 'fallback))
           (fboundp 'ghc-get))
      ;; Use generic-http-client if available
      (let ((response (ghc-get bfepm-network--http-client url)))
        (if (ghc-response-success-p response)
            (ghc-response-body response)
          (error "[BFEPM Network] HTTP GET failed for %s: %s" 
                 url (ghc-response-error response))))
    ;; Fallback to url.el
    (with-temp-buffer
      (condition-case err
          (progn
            (url-insert-file-contents url)
            (buffer-string))
        (error
         (error "[BFEPM Network] HTTP GET failed for %s: %s" url (error-message-string err)))))))

(defun bfepm-network-download-file (url local-file &optional max-retries)
  "Download file from URL to LOCAL-FILE with retry logic.
MAX-RETRIES defaults to 3. This is a synchronous operation."
  (bfepm-network--ensure-client)
  (bfepm-network--ensure-directory (file-name-directory local-file))
  (if (and (not max-retries) ; Fallback if max-retries is specified, as ghc-client uses its own config
           (not (eq bfepm-network--http-client 'fallback))
           (fboundp 'ghc-download-file))
      ;; Use generic-http-client if available
      (let ((response (ghc-download-file bfepm-network--http-client url local-file)))
        (if (ghc-response-success-p response)
            (progn
              (message "[BFEPM Network] Downloaded to %s" local-file)
              t)
          (error "[BFEPM Network] Failed to download %s: %s" 
                 url (ghc-response-error response))))
    ;; Fallback implementation
    (bfepm-network--download-file-fallback url local-file max-retries)))


(defun bfepm-network-download-file-async (url local-file callback &optional max-retries)
  "Download file from URL to LOCAL-FILE asynchronously.
CALLBACK is called with (success error-message) when complete.
MAX-RETRIES defaults to 3."
  (bfepm-network--ensure-client)
  (bfepm-network--ensure-directory (file-name-directory local-file))
  (if (and (not max-retries) ; Fallback if max-retries is specified, as ghc-client uses its own config
           (not (eq bfepm-network--http-client 'fallback))
           (fboundp 'ghc-download-file-async))
      ;; Use generic-http-client if available
      (ghc-download-file-async bfepm-network--http-client url local-file
                              (lambda (response)
                                (if (ghc-response-success-p response)
                                    (progn
                                      (message "[BFEPM Network] Downloaded to %s" local-file)
                                      (funcall callback t nil))
                                  (funcall callback nil (ghc-response-error response)))))
    ;; Fallback implementation
    (bfepm-network--download-file-async-fallback url local-file callback max-retries)))


(defun bfepm-network-fetch-archive-contents-async (archive-url callback &optional rate-limit-delay)
  "Fetch archive contents from ARCHIVE-URL asynchronously with rate limiting.
CALLBACK is called with (success contents error-message) when complete.
RATE-LIMIT-DELAY overrides the default delay between requests."
  (bfepm-network--ensure-client)
  (let ((archive-file (concat archive-url "archive-contents"))
        (current-time (float-time))
        (delay (or rate-limit-delay bfepm-network--request-delay)))
    
    ;; Apply rate limiting
    (if (and bfepm-network--last-request-time
             (< (- current-time bfepm-network--last-request-time) delay))
        (let ((wait-time (- delay (- current-time bfepm-network--last-request-time))))
          (message "[BFEPM Network] ⏱️ Rate limiting: waiting %.1fs before fetching archive contents" wait-time)
          (run-with-timer wait-time nil
                          (lambda ()
                            (bfepm-network-fetch-archive-contents-async archive-url callback rate-limit-delay))))
      (progn
        (setq bfepm-network--last-request-time current-time)
        (if (and (not (eq bfepm-network--http-client 'fallback))
                 (fboundp 'ghc-get-async))
            ;; Use generic-http-client if available
            (ghc-get-async bfepm-network--http-client archive-file
                          (lambda (response)
                            (if (ghc-response-success-p response)
                                (condition-case err
                                    (let ((contents (car (read-from-string (ghc-response-body response)))))
                                      (funcall callback t contents nil))
                                  (error
                                   (funcall callback nil nil (format "Failed to parse archive contents: %s" (error-message-string err)))))
                              (funcall callback nil nil (ghc-response-error response)))))
          ;; Fallback to url-retrieve
          (url-retrieve 
           archive-file
           (lambda (status)
             (condition-case err
                 (progn
                   (when (plist-get status :error)
                     (error "Archive fetch failed: %s" (plist-get status :error)))
                   
                   (goto-char (point-min))
                   (when (re-search-forward "^$" nil t)
                     (forward-char 1))
                   
                   (let ((contents (read (current-buffer))))
                     (funcall callback t contents nil)))
               (error
                (funcall callback nil nil (error-message-string err))))
             (kill-buffer (current-buffer)))
           nil t))))))

(defun bfepm-network-http-get-async (url callback)
  "Make asynchronous HTTP GET request to URL.
CALLBACK is called with (success response-body error-message) when complete."
  (bfepm-network--ensure-client)
  (if (and (not (eq bfepm-network--http-client 'fallback))
           (fboundp 'ghc-get-async))
      ;; Use generic-http-client if available
      (ghc-get-async bfepm-network--http-client url
                    (lambda (response)
                      (if (ghc-response-success-p response)
                          (funcall callback t (ghc-response-body response) nil)
                        (funcall callback nil nil (ghc-response-error response)))))
    ;; Fallback implementation
    (bfepm-network--http-get-async-fallback url callback)))

(defun bfepm-network-set-rate-limit (delay)
  "Set the minimum DELAY in seconds between network requests.
This affects all rate-limited operations."
  (setq bfepm-network--request-delay delay))

(defun bfepm-network-get-rate-limit ()
  "Get the current rate limit delay in seconds."
  bfepm-network--request-delay)

(defun bfepm-network-reset-rate-limit ()
  "Reset the rate limiting timer, allowing immediate next request."
  (setq bfepm-network--last-request-time nil))

(defun bfepm-network--ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-network-validate-url (url)
  "Validate that URL is a properly formed HTTP/HTTPS URL.
Returns the normalized URL or signals an error."
  (unless (stringp url)
    (error "[BFEPM Network] URL must be a string"))
  
  (when (string= url "")
    (error "[BFEPM Network] URL cannot be empty"))
  
  ;; Upgrade HTTP to HTTPS if needed
  (let ((normalized-url 
         (if (string-prefix-p "http://" url)
             (progn
               (message "[BFEPM Network] Upgraded HTTP to HTTPS")
               (replace-regexp-in-string "^http://" "https://" url))
           url)))
    
    ;; Validate HTTPS URLs
    (unless (string-prefix-p "https://" normalized-url)
      (error "[BFEPM Network] Only HTTPS URLs are supported: %s" normalized-url))
    
    normalized-url))

(defun bfepm-network-build-url (base-url &rest path-components)
  "Build a URL from BASE-URL and PATH-COMPONENTS.
Properly handles URL path joining and encoding."
  (let ((url (bfepm-network-validate-url base-url)))
    ;; Remove trailing slash from base URL
    (when (string-suffix-p "/" url)
      (setq url (substring url 0 -1)))
    
    ;; Add path components
    (dolist (component path-components)
      (when (and component (not (string= component "")))
        ;; URL encode the component
        (let ((encoded-component (url-hexify-string (format "%s" component))))
          (setq url (concat url "/" encoded-component)))))
    
    url))

(defun bfepm-network-check-connectivity (&optional timeout)
  "Check network connectivity by making a request to a known endpoint.
TIMEOUT defaults to 5 seconds. Returns t if connected, nil otherwise."
  (bfepm-network--ensure-client)
  (let ((timeout-seconds (or timeout bfepm-network--default-connectivity-timeout))
        (test-url bfepm-network-connectivity-test-url)
        (connected nil))
    
    (condition-case nil
        (with-timeout (timeout-seconds)
          (if (and (not (eq bfepm-network--http-client 'fallback))
                   (fboundp 'ghc-get))
              ;; Use generic-http-client if available
              (let ((response (ghc-get bfepm-network--http-client test-url)))
                (setq connected (ghc-response-success-p response)))
            ;; Fallback to url.el
            (with-temp-buffer
              (url-insert-file-contents test-url)
              (setq connected t))))
      (error nil))
    
    connected))

;;; Fallback implementations using url.el

(defun bfepm-network--download-file-fallback (url local-file &optional max-retries)
  "Fallback download implementation using url.el."
  (let ((retries (or max-retries bfepm-network--default-max-retries))
        (attempt 0)
        (success nil))
    (while (and (< attempt retries) (not success))
      (setq attempt (1+ attempt))
      (message "[BFEPM Network] Downloading from %s... (attempt %d/%d)" url attempt retries)
      (condition-case err
          (progn
            (condition-case nil
                (url-copy-file url local-file t)
              (wrong-number-of-arguments
               (condition-case nil
                   (url-copy-file url local-file)
                 (wrong-number-of-arguments
                  (bfepm-network--download-with-url-retrieve-fallback url local-file)))))
            (when (and (file-exists-p local-file)
                       (> (file-attribute-size (file-attributes local-file)) 0))
              (message "[BFEPM Network] Downloaded to %s" local-file)
              (setq success t)))
        (error
         (message "[BFEPM Network] Download attempt %d failed: %s" attempt (error-message-string err))
         (when (= attempt retries)
           (error "[BFEPM Network] Failed to download %s after %d attempts: %s"
                  url retries (error-message-string err))))))
    success))

(defun bfepm-network--download-with-url-retrieve-fallback (url local-file)
  "Download URL to LOCAL-FILE using url-retrieve as fallback method."
  (let ((buffer (url-retrieve-synchronously url t)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^\\r?$" nil t)
            (write-region (point) (point-max) local-file))
        (kill-buffer buffer)))))

(defun bfepm-network--download-file-async-fallback (url local-file callback &optional max-retries)
  "Fallback async download using url-retrieve."
  (let ((retries (or max-retries bfepm-network--default-max-retries))
        (attempt 0))
    (bfepm-network--download-attempt-fallback url local-file callback retries attempt)))

(defun bfepm-network--download-attempt-fallback (url local-file callback retries attempt)
  "Internal fallback function for async download attempts."
  (setq attempt (1+ attempt))
  (message "[BFEPM Network] Starting async download from %s... (attempt %d/%d)" url attempt retries)
  (url-retrieve
   url
   (lambda (status)
     (condition-case err
         (progn
           (when (plist-get status :error)
             (error "Download failed: %s" (plist-get status :error)))
           
           (goto-char (point-min))
           (when (re-search-forward "^$" nil t)
             (forward-char 1))
           
           (let ((content (buffer-substring (point) (point-max))))
             (with-temp-file local-file
               (set-buffer-file-coding-system 'binary)
               (insert content)))
           
           (if (and (file-exists-p local-file)
                    (> (file-attribute-size (file-attributes local-file)) 0))
               (progn
                 (message "[BFEPM Network] Downloaded to %s" local-file)
                 (funcall callback t nil))
             (if (< attempt retries)
                 (bfepm-network--download-attempt-fallback url local-file callback retries attempt)
               (funcall callback nil (format "Failed to download %s after %d attempts" url retries)))))
       (error
        (message "[BFEPM Network] Download attempt %d failed: %s" attempt (error-message-string err))
        (if (< attempt retries)
            (bfepm-network--download-attempt-fallback url local-file callback retries attempt)
          (funcall callback nil (format "Failed to download %s after %d attempts: %s"
                                       url retries (error-message-string err))))))
     (kill-buffer (current-buffer)))
   nil t))

(defun bfepm-network--http-get-async-fallback (url callback)
  "Fallback HTTP GET async implementation using url-retrieve."
  (url-retrieve
   url
   (lambda (status)
     (condition-case err
         (progn
           (when (plist-get status :error)
             (error "HTTP request failed: %s" (plist-get status :error)))
           
           (goto-char (point-min))
           (when (re-search-forward "^$" nil t)
             (forward-char 1))
           
           (let ((response-body (buffer-substring (point) (point-max))))
             (funcall callback t response-body nil)))
       (error
        (funcall callback nil nil (error-message-string err))))
     (kill-buffer (current-buffer)))
   nil t))

(provide 'bfepm-network)

;;; bfepm-network.el ends here