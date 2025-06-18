;;; bfepm-network.el --- Network and HTTP operations for BFEPM -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides network operations including async file downloads,
;; HTTP requests, and archive content fetching. It centralizes all network
;; functionality with proper error handling, retry logic, and rate limiting.

;;; Code:

(require 'url)

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

(defcustom bfepm-network-connectivity-test-url "https://httpbin.org/status/200"
  "URL used to test network connectivity."
  :type 'string
  :group 'bfepm)

(defun bfepm-network-http-get (url)
  "Make HTTP GET request to URL and return response body.
This is a synchronous operation that blocks until completion."
  (with-temp-buffer
    (condition-case err
        (progn
          (url-insert-file-contents url)
          (buffer-string))
      (error
       (error "[BFEPM Network] HTTP GET failed for %s: %s" url (error-message-string err))))))

(defun bfepm-network-download-file (url local-file &optional max-retries)
  "Download file from URL to LOCAL-FILE with retry logic.
MAX-RETRIES defaults to 3. This is a synchronous operation."
  (let ((retries (or max-retries bfepm-network--default-max-retries))
        (attempt 0)
        (success nil))
    (bfepm-network--ensure-directory (file-name-directory local-file))
    (while (and (< attempt retries) (not success))
      (setq attempt (1+ attempt))
      (message "[BFEPM Network] Downloading from %s... (attempt %d/%d)" url attempt retries)
      (condition-case err
          (progn
            (url-copy-file url local-file t)
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

(defun bfepm-network-download-file-async (url local-file callback &optional max-retries)
  "Download file from URL to LOCAL-FILE asynchronously.
CALLBACK is called with (success error-message) when complete.
MAX-RETRIES defaults to 3."
  (let ((retries (or max-retries bfepm-network--default-max-retries))
        (attempt 0))
    ;; Ensure parent directory exists
    (bfepm-network--ensure-directory (file-name-directory local-file))
    (bfepm-network--download-attempt url local-file callback retries attempt)))

(defun bfepm-network--download-attempt (url local-file callback retries attempt)
  "Internal function for async download attempts using `url-retrieve'.
URL is the source to download from, LOCAL-FILE is the destination.
CALLBACK is called when download completes or fails.
RETRIES specifies maximum retry attempts, ATTEMPT tracks current attempt."
  (setq attempt (1+ attempt))
  (message "[BFEPM Network] 📥 Starting async download from %s... (attempt %d/%d)" url attempt retries)
  (url-retrieve
   url
   (lambda (status)
     (condition-case err
         (progn
           ;; Check for errors in status
           (when (plist-get status :error)
             (error "Download failed: %s" (plist-get status :error)))
           
           ;; Move past HTTP headers to find the response body
           (goto-char (point-min))
           (when (re-search-forward "^$" nil t)
             (forward-char 1))
           
           ;; Write response body to file
           (let ((content (buffer-substring (point) (point-max))))
             (with-temp-file local-file
               (set-buffer-file-coding-system 'binary)
               (insert content)))
           
           ;; Verify file was downloaded successfully
           (if (and (file-exists-p local-file)
                    (> (file-attribute-size (file-attributes local-file)) 0))
               (progn
                 (message "[BFEPM Network] Downloaded to %s" local-file)
                 (funcall callback t nil))
             (if (< attempt retries)
                 (bfepm-network--download-attempt url local-file callback retries attempt)
               (funcall callback nil (format "Failed to download %s after %d attempts" url retries)))))
       (error
        (message "[BFEPM Network] Download attempt %d failed: %s" attempt (error-message-string err))
        (if (< attempt retries)
            (bfepm-network--download-attempt url local-file callback retries attempt)
          (funcall callback nil (format "Failed to download %s after %d attempts: %s"
                                       url retries (error-message-string err))))))
     ;; Clean up the buffer after processing
     (kill-buffer (current-buffer)))
   nil t))

(defun bfepm-network-fetch-archive-contents-async (archive-url callback &optional rate-limit-delay)
  "Fetch archive contents from ARCHIVE-URL asynchronously with rate limiting.
CALLBACK is called with (success contents error-message) when complete.
RATE-LIMIT-DELAY overrides the default delay between requests."
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
        (url-retrieve 
         archive-file
         (lambda (status)
           (condition-case err
               (progn
                 ;; Check for HTTP errors
                 (when (plist-get status :error)
                   (error "Archive fetch failed: %s" (plist-get status :error)))
                 
                 ;; Skip HTTP headers
                 (goto-char (point-min))
                 (when (re-search-forward "^$" nil t)
                   (forward-char 1))
                 
                 ;; Parse the archive contents
                 (let ((contents (read (current-buffer))))
                   (funcall callback t contents nil)))
             (error
              (funcall callback nil nil (error-message-string err))))
           ;; Clean up buffer
           (kill-buffer (current-buffer)))
         nil t)))))

(defun bfepm-network-http-get-async (url callback)
  "Make asynchronous HTTP GET request to URL.
CALLBACK is called with (success response-body error-message) when complete."
  (url-retrieve
   url
   (lambda (status)
     (condition-case err
         (progn
           ;; Check for HTTP errors
           (when (plist-get status :error)
             (error "HTTP request failed: %s" (plist-get status :error)))
           
           ;; Skip HTTP headers
           (goto-char (point-min))
           (when (re-search-forward "^$" nil t)
             (forward-char 1))
           
           ;; Get response body
           (let ((response-body (buffer-substring (point) (point-max))))
             (funcall callback t response-body nil)))
       (error
        (funcall callback nil nil (error-message-string err))))
     ;; Clean up buffer
     (kill-buffer (current-buffer)))
   nil t))

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
  (let ((timeout-seconds (or timeout bfepm-network--default-connectivity-timeout))
        (test-url bfepm-network-connectivity-test-url)
        (connected nil))
    
    (condition-case nil
        (with-timeout (timeout-seconds)
          (with-temp-buffer
            (url-insert-file-contents test-url)
            (setq connected t)))
      (error nil))
    
    connected))

(provide 'bfepm-network)

;;; bfepm-network.el ends here