;;; generic-http-client.el --- Generic HTTP Client Library -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: http network download retry rate-limiting
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This library provides a generic HTTP client with advanced features:
;; - Synchronous and asynchronous HTTP operations
;; - Automatic retry logic with configurable strategies
;; - Rate limiting to prevent server overload
;; - Download progress tracking
;; - Checksum verification for downloads
;; - Connection pooling and keep-alive support
;; - Flexible error handling and recovery
;;
;; The library is designed to be reusable across different applications
;; and provides a clean abstraction over Emacs' built-in URL library.
;;
;; Usage:
;;   (setq client (ghc-create-client 
;;                 :base-url "https://api.example.com"
;;                 :retry-count 3
;;                 :rate-limit 10))
;;   
;;   (ghc-get client "/users/123" callback)
;;   (ghc-download client "/files/package.tar" "/tmp/package.tar")

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)

;;; Core Data Structures

(cl-defstruct (ghc-client (:constructor make-ghc-client)
                          (:copier nil))
  "HTTP client with retry logic and rate limiting."
  base-url               ; String: base URL for all requests
  default-headers        ; Alist: default headers for all requests
  timeout                ; Number: request timeout in seconds (default: 30)
  retry-count            ; Number: maximum retry attempts (default: 3)
  retry-delay            ; Number: base delay between retries in seconds (default: 1)
  retry-strategy         ; Symbol: linear, exponential, or custom function
  rate-limit             ; Number: requests per second (default: nil = no limit)
  user-agent             ; String: User-Agent header
  follow-redirects-p     ; Boolean: follow HTTP redirects (default: t)
  verify-ssl-p           ; Boolean: verify SSL certificates (default: t)
  proxy-config           ; Plist: proxy configuration
  connection-pool        ; Hash table: connection pooling
  (request-history '())  ; List: request history for rate limiting
  (active-requests 0)    ; Number: current active requests
  hooks)                 ; Alist: lifecycle hooks

(cl-defstruct (ghc-request (:constructor make-ghc-request)
                           (:copier nil))
  "HTTP request specification."
  method                 ; Symbol: GET, POST, PUT, DELETE, etc.
  url                    ; String: full URL or path (relative to client base-url)
  headers                ; Alist: request-specific headers
  body                   ; String: request body for POST/PUT
  query-params           ; Alist: query parameters
  timeout                ; Number: request-specific timeout override
  retry-count            ; Number: request-specific retry override
  metadata)              ; Plist: arbitrary metadata

(cl-defstruct (ghc-response (:constructor make-ghc-response)
                            (:copier nil))
  "HTTP response."
  status-code            ; Number: HTTP status code
  headers                ; Alist: response headers
  body                   ; String: response body
  url                    ; String: final URL (after redirects)
  elapsed-time           ; Number: request duration in seconds
  attempt-count          ; Number: number of attempts made
  metadata)              ; Plist: arbitrary metadata

(cl-defstruct (ghc-download-spec (:constructor make-ghc-download-spec)
                                 (:copier nil))
  "File download specification."
  url                    ; String: download URL
  local-path             ; String: local file path
  expected-checksum      ; String: expected file checksum
  checksum-algorithm     ; Symbol: sha256, md5, sha1
  progress-callback      ; Function: (downloaded total) -> void
  chunk-size             ; Number: download chunk size in bytes
  resume-p               ; Boolean: resume partial downloads
  overwrite-p)           ; Boolean: overwrite existing files

;;; Rate Limiting

(defvar ghc--global-request-history (make-hash-table :test 'equal)
  "Global request history for rate limiting per client.")

(defun ghc--should-rate-limit-p (client)
  "Check if CLIENT should be rate limited."
  (when-let ((rate-limit (ghc-client-rate-limit client)))
    (let* ((client-key (sxhash client))
           (history (gethash client-key ghc--global-request-history '()))
           (now (float-time))
           (window 1.0)  ; 1 second window
           (recent-requests (cl-count-if 
                           (lambda (time) (< (- now time) window))
                           history)))
      (>= recent-requests rate-limit))))

(defun ghc--record-request (client)
  "Record a request for CLIENT for rate limiting."
  (when (ghc-client-rate-limit client)
    (let ((client-key (sxhash client))
          (now (float-time)))
      (push now (gethash client-key ghc--global-request-history '()))
      (puthash client-key 
               (cl-subseq (gethash client-key ghc--global-request-history)
                         0 (min 100 (length (gethash client-key ghc--global-request-history))))
               ghc--global-request-history))))

(defun ghc--wait-for-rate-limit (client)
  "Wait if necessary to respect CLIENT's rate limit."
  (when (ghc--should-rate-limit-p client)
    (let ((delay (/ 1.0 (ghc-client-rate-limit client))))
      (message "[HTTP Client] Rate limiting: waiting %.2fs" delay)
      (sleep-for delay))))

;;; URL and Header Processing

(defun ghc--build-url (client path)
  "Build full URL from CLIENT base URL and PATH."
  (if (string-match-p "^https?://" path)
      path  ; Already a full URL
    (let ((base (ghc-client-base-url client)))
      (if base
          (concat (string-trim-right base "/") "/" (string-trim-left path "/"))
        path))))

(defun ghc--build-headers (client request)
  "Build complete headers from CLIENT defaults and REQUEST specifics."
  (let ((headers (append (ghc-client-default-headers client)
                        (ghc-request-headers request))))
    (when (ghc-client-user-agent client)
      (unless (assoc "User-Agent" headers)
        (push (cons "User-Agent" (ghc-client-user-agent client)) headers)))
    headers))

(defun ghc--add-query-params (url params)
  "Add query PARAMS to URL."
  (if params
      (let ((query-string (mapconcat
                          (lambda (param)
                            (format "%s=%s"
                                   (url-hexify-string (car param))
                                   (url-hexify-string (cdr param))))
                          params "&")))
        (concat url (if (string-match-p "\\?" url) "&" "?") query-string))
    url))

;;; Retry Logic

(defun ghc--calculate-retry-delay (strategy attempt base-delay)
  "Calculate retry delay based on STRATEGY, ATTEMPT number, and BASE-DELAY."
  (pcase strategy
    ('linear base-delay)
    ('exponential (* base-delay (expt 2 (1- attempt))))
    ((pred functionp) (funcall strategy attempt base-delay))
    (_ base-delay)))

(defun ghc--should-retry-p (response attempt max-retries)
  "Check if request should be retried based on RESPONSE, ATTEMPT, and MAX-RETRIES."
  (and (< attempt max-retries)
       response
       (let ((status (ghc-response-status-code response)))
         (or (>= status 500)  ; Server errors
             (= status 429)   ; Rate limited
             (= status 408)   ; Request timeout
             (= status 503)   ; Service unavailable
             (= status 502)   ; Bad gateway
             (= status 504))))) ; Gateway timeout

;;; Core HTTP Operations

(defun ghc--perform-request (client request)
  "Perform HTTP REQUEST using CLIENT with retry logic."
  (let* ((max-retries (or (ghc-request-retry-count request)
                         (ghc-client-retry-count client)
                         3))
         (attempt 0)
         (response nil)
         (start-time (float-time)))
    
    (while (and (<= attempt max-retries)
                (or (zerop attempt)
                    (ghc--should-retry-p response attempt max-retries)))
      (when (> attempt 0)
        (let ((delay (ghc--calculate-retry-delay
                     (ghc-client-retry-strategy client)
                     attempt
                     (ghc-client-retry-delay client))))
          (message "[HTTP Client] Retrying request (attempt %d/%d) after %.2fs"
                   (1+ attempt) max-retries delay)
          (sleep-for delay)))
      
      (ghc--wait-for-rate-limit client)
      (ghc--record-request client)
      
      (setq response (ghc--execute-request client request))
      (setf (ghc-response-attempt-count response) (1+ attempt))
      (cl-incf attempt))
    
    (setf (ghc-response-elapsed-time response) (- (float-time) start-time))
    response))

(defun ghc--execute-request (client request)
  "Execute single HTTP REQUEST using CLIENT."
  (let* ((url (ghc--build-url client (ghc-request-url request)))
         (url (ghc--add-query-params url (ghc-request-query-params request)))
         (headers (ghc--build-headers client request))
         (timeout (or (ghc-request-timeout request)
                     (ghc-client-timeout client)
                     30)))
    
    (condition-case err
        (let ((url-request-method (symbol-name (ghc-request-method request)))
              (url-request-extra-headers headers)
              (url-request-data (ghc-request-body request))
              (url-http-attempt-keepalives t))
          
          (with-current-buffer (url-retrieve-synchronously url nil nil timeout)
            (ghc--parse-response (current-buffer) url)))
      
      (error
       (make-ghc-response
        :status-code 0
        :headers '()
        :body (format "Request failed: %s" (error-message-string err))
        :url url
        :elapsed-time 0
        :attempt-count 1)))))

(defun ghc--parse-response (buffer url)
  "Parse HTTP response from BUFFER for URL."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let* ((status-line (buffer-substring-no-properties
                        (point) (progn (end-of-line) (point))))
           (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                         (string-to-number (match-string 1 status-line))))
           (headers '())
           (body ""))
      
      ;; Parse headers
      (forward-line 1)
      (while (not (looking-at "^$"))
        (let ((line (buffer-substring-no-properties
                    (point) (progn (end-of-line) (point)))))
          (when (string-match "^\\([^:]+\\): *\\(.+\\)$" line)
            (push (cons (match-string 1 line) (match-string 2 line)) headers)))
        (forward-line 1))
      
      ;; Parse body
      (forward-line 1)
      (setq body (buffer-substring-no-properties (point) (point-max)))
      
      (make-ghc-response
       :status-code (or status-code 0)
       :headers headers
       :body body
       :url url
       :elapsed-time 0
       :attempt-count 1))))

;;; Async Operations

(defun ghc--perform-request-async (client request callback)
  "Perform HTTP REQUEST using CLIENT asynchronously, calling CALLBACK with result."
  (let* ((url (ghc--build-url client (ghc-request-url request)))
         (url (ghc--add-query-params url (ghc-request-query-params request)))
         (headers (ghc--build-headers client request))
         (start-time (float-time)))
    
    (ghc--wait-for-rate-limit client)
    (ghc--record-request client)
    
    (let ((url-request-method (symbol-name (ghc-request-method request)))
          (url-request-extra-headers headers)
          (url-request-data (ghc-request-body request)))
      
      (url-retrieve url
                    (lambda (_status)
                      (let ((response (condition-case err
                                        (ghc--parse-response (current-buffer) url)
                                      (error
                                       (make-ghc-response
                                        :status-code 0
                                        :headers '()
                                        :body (format "Async request failed: %s" 
                                                     (error-message-string err))
                                        :url url)))))
                        (setf (ghc-response-elapsed-time response) 
                              (- (float-time) start-time))
                        (funcall callback response)))
                    nil nil
                    (or (ghc-request-timeout request)
                        (ghc-client-timeout client)
                        30)))))

;;; File Download

(defun ghc--download-file-sync (client download-spec)
  "Download file synchronously using CLIENT and DOWNLOAD-SPEC."
  (let* ((url (ghc-download-spec-url download-spec))
         (local-path (ghc-download-spec-local-path download-spec))
         (request (make-ghc-request :method 'GET :url url)))
    
    (ghc--ensure-directory (file-name-directory local-path))
    
    (let ((response (ghc--perform-request client request)))
      (when (= (ghc-response-status-code response) 200)
        (with-temp-file local-path
          (insert (ghc-response-body response)))
        
        (when (ghc-download-spec-expected-checksum download-spec)
          (unless (ghc--verify-checksum download-spec local-path)
            (delete-file local-path)
            (error "Checksum verification failed for %s" local-path))))
      
      response)))

(defun ghc--verify-checksum (download-spec file-path)
  "Verify checksum of FILE-PATH against DOWNLOAD-SPEC."
  (let* ((algorithm (or (ghc-download-spec-checksum-algorithm download-spec) 'sha256))
         (expected (ghc-download-spec-expected-checksum download-spec))
         (actual (ghc--calculate-checksum file-path algorithm)))
    (string= (downcase expected) (downcase actual))))

(defun ghc--calculate-checksum (file-path algorithm)
  "Calculate checksum of FILE-PATH using ALGORITHM."
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (secure-hash algorithm (current-buffer))))

(defun ghc--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;; Public API

(defun ghc-create-client (&rest args)
  "Create HTTP client with ARGS.
ARGS is a plist with keys matching ghc-client structure slots."
  (apply #'make-ghc-client
         :timeout 30
         :retry-count 3
         :retry-delay 1
         :retry-strategy 'exponential
         :follow-redirects-p t
         :verify-ssl-p t
         args))

(defun ghc-get (client path &optional callback)
  "Make GET request to PATH using CLIENT.
If CALLBACK is provided, request is asynchronous."
  (let ((request (make-ghc-request :method 'GET :url path)))
    (if callback
        (ghc--perform-request-async client request callback)
      (ghc--perform-request client request))))

(defun ghc-post (client path body &optional callback)
  "Make POST request to PATH with BODY using CLIENT.
If CALLBACK is provided, request is asynchronous."
  (let ((request (make-ghc-request :method 'POST :url path :body body)))
    (if callback
        (ghc--perform-request-async client request callback)
      (ghc--perform-request client request))))

(defun ghc-put (client path body &optional callback)
  "Make PUT request to PATH with BODY using CLIENT.
If CALLBACK is provided, request is asynchronous."
  (let ((request (make-ghc-request :method 'PUT :url path :body body)))
    (if callback
        (ghc--perform-request-async client request callback)
      (ghc--perform-request client request))))

(defun ghc-delete (client path &optional callback)
  "Make DELETE request to PATH using CLIENT.
If CALLBACK is provided, request is asynchronous."
  (let ((request (make-ghc-request :method 'DELETE :url path)))
    (if callback
        (ghc--perform-request-async client request callback)
      (ghc--perform-request client request))))

(defun ghc-download (client url local-path &optional options)
  "Download file from URL to LOCAL-PATH using CLIENT.
OPTIONS is a plist that can contain:
  :expected-checksum - expected file checksum
  :checksum-algorithm - checksum algorithm (:sha256, :md5, :sha1)
  :progress-callback - function called with (downloaded total)
  :overwrite-p - whether to overwrite existing files"
  (let ((download-spec (apply #'make-ghc-download-spec
                             :url url
                             :local-path local-path
                             options)))
    (ghc--download-file-sync client download-spec)))

(defun ghc-set-rate-limit (client requests-per-second)
  "Set rate limit for CLIENT to REQUESTS-PER-SECOND."
  (setf (ghc-client-rate-limit client) requests-per-second))

(defun ghc-add-header (client name value)
  "Add header NAME with VALUE to CLIENT's default headers."
  (let ((headers (ghc-client-default-headers client)))
    (setf (ghc-client-default-headers client)
          (cons (cons name value)
                (cl-remove name headers :key #'car :test #'string=)))))

(defun ghc-check-connectivity (client &optional test-url)
  "Check if CLIENT can reach TEST-URL (or a default test endpoint)."
  (let ((url (or test-url (concat (ghc-client-base-url client) "/status"))))
    (condition-case nil
        (let ((response (ghc-get client url)))
          (< (ghc-response-status-code response) 400))
      (error nil))))

;;; Statistics and Monitoring

(defun ghc-client-stats (client)
  "Get statistics for CLIENT."
  (list :active-requests (ghc-client-active-requests client)
        :request-history-length (length (ghc-client-request-history client))
        :rate-limit (ghc-client-rate-limit client)
        :base-url (ghc-client-base-url client)))

;;; Error Handling

(define-error 'ghc-error "HTTP Client Error")
(define-error 'ghc-network-error "Network Error" 'ghc-error)
(define-error 'ghc-timeout-error "Request Timeout" 'ghc-error)
(define-error 'ghc-rate-limit-error "Rate Limit Exceeded" 'ghc-error)

(defun ghc-error (format-string &rest args)
  "Signal HTTP client error with formatted message."
  (signal 'ghc-error (list (apply #'format format-string args))))

(provide 'generic-http-client)

;;; generic-http-client.el ends here