;;; generic-search-engine.el --- Generic Multi-Source Search Engine -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: search aggregation caching ranking
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This library provides a generic search engine that can aggregate
;; results from multiple sources with intelligent caching, relevance
;; ranking, and asynchronous operation support.
;;
;; Key Features:
;; - Multi-source search aggregation
;; - Configurable search adapters for different data sources
;; - Intelligent caching with TTL and invalidation strategies
;; - Relevance-based ranking algorithms
;; - Async and sync search operations
;; - Query preprocessing and normalization
;; - Result filtering and transformation
;; - Progress tracking for long searches
;; - Extensible plugin architecture
;;
;; Supported Use Cases:
;; - Package repository search (MELPA, PyPI, npm, etc.)
;; - Document search across multiple systems
;; - API aggregation and search
;; - File system search with multiple backends
;; - Database search across multiple tables/systems
;;
;; Usage:
;;   (setq engine (gse-create-engine :name "package-search"))
;;   (gse-add-source engine package-source-melpa)
;;   (gse-add-source engine package-source-gnu)
;;   (gse-search engine "query" callback)

;;; Code:

(require 'cl-lib)

;;; Core Data Structures

(cl-defstruct (gse-engine (:constructor make-gse-engine)
                          (:copier nil))
  "Generic search engine with multi-source support."
  name                   ; String: engine identifier
  sources                ; List: registered search sources
  query-preprocessor     ; Function: (query) -> normalized-query
  result-aggregator      ; Function: (results-list) -> merged-results
  ranking-algorithm      ; Function: (results query) -> ranked-results
  cache-manager          ; gse-cache-manager: result caching
  search-filters         ; List: result filtering functions
  result-transformers    ; List: result transformation functions
  progress-tracker       ; Function: (completed total) -> void
  max-concurrent         ; Number: maximum concurrent source searches
  timeout                ; Number: search timeout in seconds
  hooks                  ; Alist: lifecycle hooks
  metadata)              ; Plist: engine metadata

(cl-defstruct (gse-source (:constructor make-gse-source)
                          (:copier nil))
  "Search source adapter."
  name                   ; String: source identifier
  type                   ; Symbol: source type (http, database, file, etc.)
  priority               ; Number: source priority (higher = more important)
  search-fn              ; Function: (query options) -> results
  search-async-fn        ; Function: (query options callback) -> void
  availability-checker   ; Function: () -> available-p
  rate-limit             ; Number: requests per second limit
  timeout                ; Number: source-specific timeout
  cache-policy           ; Plist: source-specific caching
  metadata)              ; Plist: source metadata

(cl-defstruct (gse-result (:constructor make-gse-result)
                          (:copier nil))
  "Search result representation."
  id                     ; String: unique result identifier
  title                  ; String: result title/name
  description            ; String: result description
  url                    ; String: result URL or identifier
  source                 ; String: source name
  score                  ; Number: relevance score (0.0-1.0)
  type                   ; Symbol: result type
  metadata               ; Plist: result-specific data
  timestamp              ; Time: when result was found
  highlighted-fields)    ; Alist: (field . highlighted-content)

(cl-defstruct (gse-query (:constructor make-gse-query)
                         (:copier nil))
  "Search query with options."
  text                   ; String: query text
  filters                ; Alist: search filters
  sort-by                ; Symbol: sort criteria
  max-results            ; Number: maximum results to return
  offset                 ; Number: result offset for pagination
  include-sources        ; List: sources to search (nil = all)
  exclude-sources        ; List: sources to exclude
  timeout                ; Number: query-specific timeout
  metadata)              ; Plist: query metadata

(cl-defstruct (gse-cache-manager (:constructor make-gse-cache-manager)
                                 (:copier nil))
  "Cache management for search results."
  cache-store            ; Hash table: result cache
  ttl                    ; Number: time-to-live in seconds
  max-size               ; Number: maximum cache entries
  key-generator          ; Function: (query) -> cache-key
  invalidation-strategy  ; Function: (entry) -> should-invalidate-p
  persistence-backend    ; Function: save/load cache to/from disk
  compression-p          ; Boolean: compress cached data
  metrics)               ; Hash table: cache performance metrics

;;; Query Processing

(defun gse--normalize-query (engine query-text)
  "Normalize QUERY-TEXT using ENGINE's preprocessor."
  (if-let ((preprocessor (gse-engine-query-preprocessor engine)))
      (funcall preprocessor query-text)
    (string-trim (downcase query-text))))

(defun gse--build-search-query (query-text options)
  "Build gse-query from QUERY-TEXT and OPTIONS."
  (make-gse-query
   :text query-text
   :filters (plist-get options :filters)
   :sort-by (plist-get options :sort-by)
   :max-results (or (plist-get options :max-results) 100)
   :offset (or (plist-get options :offset) 0)
   :include-sources (plist-get options :include-sources)
   :exclude-sources (plist-get options :exclude-sources)
   :timeout (plist-get options :timeout)
   :metadata (plist-get options :metadata)))

(defun gse--validate-query (query)
  "Validate search QUERY."
  (unless (gse-query-p query)
    (error "Invalid query object"))
  (when (string-empty-p (string-trim (gse-query-text query)))
    (error "Query text cannot be empty"))
  t)

;;; Source Management

(defun gse-add-source (engine source)
  "Add SOURCE to ENGINE."
  (unless (gse-source-p source)
    (error "Invalid source object"))
  (let ((sources (gse-engine-sources engine)))
    (setf (gse-engine-sources engine)
          (cons source (cl-remove (gse-source-name source) sources
                                 :key #'gse-source-name :test #'string=)))))

(defun gse-remove-source (engine source-name)
  "Remove source named SOURCE-NAME from ENGINE."
  (setf (gse-engine-sources engine)
        (cl-remove source-name (gse-engine-sources engine)
                  :key #'gse-source-name :test #'string=)))

(defun gse-get-source (engine source-name)
  "Get source named SOURCE-NAME from ENGINE."
  (cl-find source-name (gse-engine-sources engine)
          :key #'gse-source-name :test #'string=))

(defun gse--filter-sources (engine query)
  "Filter sources for ENGINE based on QUERY constraints."
  (let ((all-sources (gse-engine-sources engine))
        (include (gse-query-include-sources query))
        (exclude (gse-query-exclude-sources query)))
    (cond
     (include (cl-intersection all-sources include
                              :key #'gse-source-name :test #'string=))
     (exclude (cl-set-difference all-sources exclude
                                :key #'gse-source-name :test #'string=))
     (t all-sources))))

(defun gse--check-source-availability (source)
  "Check if SOURCE is available for searching."
  (if-let ((checker (gse-source-availability-checker source)))
      (funcall checker)
    t))  ; Assume available if no checker

;;; Caching System

(defun gse--get-cache-key (cache-manager query)
  "Generate cache key for QUERY using CACHE-MANAGER."
  (if-let ((generator (gse-cache-manager-key-generator cache-manager)))
      (funcall generator query)
    (secure-hash 'sha256 (format "%S" query))))

(defun gse--cache-get (cache-manager cache-key)
  "Get cached result for CACHE-KEY from CACHE-MANAGER."
  (when-let ((cache-store (gse-cache-manager-cache-store cache-manager)))
    (let ((entry (gethash cache-key cache-store)))
      (when (and entry (not (gse--cache-entry-expired-p cache-manager entry)))
        (gse--update-cache-metrics cache-manager :hits)
        (cdr entry)))))  ; Return cached data

(defun gse--cache-put (cache-manager cache-key results)
  "Store RESULTS for CACHE-KEY in CACHE-MANAGER."
  (when-let ((cache-store (gse-cache-manager-cache-store cache-manager)))
    (let ((entry (cons (float-time) results)))
      (puthash cache-key entry cache-store)
      (gse--maybe-cleanup-cache cache-manager)
      (gse--update-cache-metrics cache-manager :misses))))

(defun gse--cache-entry-expired-p (cache-manager entry)
  "Check if cache ENTRY is expired according to CACHE-MANAGER policy."
  (let ((timestamp (car entry))
        (ttl (gse-cache-manager-ttl cache-manager)))
    (and ttl (> (- (float-time) timestamp) ttl))))

(defun gse--maybe-cleanup-cache (cache-manager)
  "Clean up cache if it exceeds size limits."
  (when-let* ((cache-store (gse-cache-manager-cache-store cache-manager))
              (max-size (gse-cache-manager-max-size cache-manager))
              (current-size (hash-table-count cache-store)))
    (when (> current-size max-size)
      (gse--cleanup-cache-entries cache-manager))))

(defun gse--cleanup-cache-entries (cache-manager)
  "Remove old cache entries to free space."
  (let* ((cache-store (gse-cache-manager-cache-store cache-manager))
         (entries (hash-table-keys cache-store))
         (sorted-entries (sort entries
                              (lambda (a b)
                                (< (car (gethash a cache-store))
                                   (car (gethash b cache-store))))))
         (to-remove (/ (length entries) 4)))  ; Remove 25% of entries
    (dotimes (i to-remove)
      (remhash (nth i sorted-entries) cache-store))))

(defun gse--update-cache-metrics (cache-manager metric)
  "Update cache METRIC in CACHE-MANAGER."
  (when-let ((metrics (gse-cache-manager-metrics cache-manager)))
    (let ((current (gethash metric metrics 0)))
      (puthash metric (1+ current) metrics))))

;;; Synchronous Search

(defun gse-search (engine query-text &optional options)
  "Search for QUERY-TEXT using ENGINE with OPTIONS.
Returns list of gse-result objects."
  (let* ((normalized-query (gse--normalize-query engine query-text))
         (query (gse--build-search-query normalized-query options))
         (cache-manager (gse-engine-cache-manager engine)))
    
    (gse--validate-query query)
    
    ;; Check cache first
    (if-let* ((cache-manager)
              (cache-key (gse--get-cache-key cache-manager query))
              (cached-results (gse--cache-get cache-manager cache-key)))
        cached-results
      
      ;; Perform search
      (let* ((sources (gse--filter-sources engine query))
             (available-sources (cl-remove-if-not #'gse--check-source-availability sources))
             (all-results '()))
        
        (when (null available-sources)
          (error "No available sources for search"))
        
        ;; Search each source
        (dolist (source available-sources)
          (when-let ((results (gse--search-source-sync source query)))
            (setq all-results (append all-results results))))
        
        ;; Process results
        (let ((processed-results (gse--process-results engine all-results query)))
          ;; Cache results
          (when cache-manager
            (gse--cache-put cache-manager cache-key processed-results))
          
          processed-results)))))

(defun gse--search-source-sync (source query)
  "Search SOURCE synchronously with QUERY."
  (condition-case err
      (when-let ((search-fn (gse-source-search-fn source)))
        (let ((results (funcall search-fn (gse-query-text query) query)))
          (gse--enrich-results-with-source results source)))
    (error
     (message "Search failed for source %s: %s" 
              (gse-source-name source) (error-message-string err))
     nil)))

;;; Asynchronous Search

(defun gse-search-async (engine query-text callback &optional options)
  "Search for QUERY-TEXT using ENGINE asynchronously.
CALLBACK is called with (success results error-message)."
  (let* ((normalized-query (gse--normalize-query engine query-text))
         (query (gse--build-search-query normalized-query options))
         (cache-manager (gse-engine-cache-manager engine)))
    
    (condition-case err
        (progn
          (gse--validate-query query)
          
          ;; Check cache first
          (if-let* ((cache-manager)
                    (cache-key (gse--get-cache-key cache-manager query))
                    (cached-results (gse--cache-get cache-manager cache-key)))
              (funcall callback t cached-results nil)
            
            ;; Perform async search
            (gse--search-async-internal engine query cache-manager cache-key callback)))
      (error
       (funcall callback nil nil (error-message-string err))))))

(defun gse--search-async-internal (engine query cache-manager cache-key callback)
  "Internal async search implementation."
  (let* ((sources (gse--filter-sources engine query))
         (available-sources (cl-remove-if-not #'gse--check-source-availability sources))
         (total-sources (length available-sources))
         (search-state (make-hash-table :test 'equal)))
    
    (when (= total-sources 0)
      (funcall callback nil nil "No available sources for search")
      (cl-return))
    
    ;; Initialize search state
    (puthash 'results '() search-state)
    (puthash 'completed 0 search-state)
    (puthash 'finished-p nil search-state)
    
    ;; Search each source asynchronously
    (dolist (source available-sources)
      (gse--search-source-async source query
        (lambda (success results _error-msg)
          (unless (gethash 'finished-p search-state)
            (let ((completed (1+ (gethash 'completed search-state))))
              (puthash 'completed completed search-state)
              
              (when success
                (puthash 'results 
                        (append (gethash 'results search-state) results)
                        search-state))
              
              ;; Update progress
              (when-let ((tracker (gse-engine-progress-tracker engine)))
                (funcall tracker completed total-sources))
              
              ;; Check if all sources completed
              (when (= completed total-sources)
                (puthash 'finished-p t search-state)
                (let ((final-results (gse--process-results 
                                     engine 
                                     (gethash 'results search-state) 
                                     query)))
                  ;; Cache results
                  (when cache-manager
                    (gse--cache-put cache-manager cache-key final-results))
                  
                  (funcall callback t final-results nil))))))))))

(defun gse--search-source-async (source query callback)
  "Search SOURCE asynchronously with QUERY, calling CALLBACK with results."
  (condition-case err
      (if-let ((async-fn (gse-source-search-async-fn source)))
          (funcall async-fn (gse-query-text query) query
                   (lambda (success results error-msg)
                     (if success
                         (let ((enriched (gse--enrich-results-with-source results source)))
                           (funcall callback t enriched nil))
                       (funcall callback nil nil error-msg))))
        ;; Fall back to sync search in a timer
        (run-with-timer 0.01 nil
                       (lambda ()
                         (let ((results (gse--search-source-sync source query)))
                           (funcall callback (not (null results)) results nil)))))
    (error
     (funcall callback nil nil (error-message-string err)))))

;;; Result Processing

(defun gse--process-results (engine results query)
  "Process search RESULTS using ENGINE for QUERY."
  (let ((processed results))
    
    ;; Apply filters
    (dolist (filter (gse-engine-search-filters engine))
      (setq processed (cl-remove-if-not filter processed)))
    
    ;; Apply transformers
    (dolist (transformer (gse-engine-result-transformers engine))
      (setq processed (mapcar transformer processed)))
    
    ;; Rank results
    (when-let ((ranker (gse-engine-ranking-algorithm engine)))
      (setq processed (funcall ranker processed query)))
    
    ;; Apply pagination
    (let ((offset (gse-query-offset query))
          (max-results (gse-query-max-results query)))
      (when (or (> offset 0) max-results)
        (setq processed (cl-subseq processed 
                                  offset 
                                  (when max-results 
                                    (min (+ offset max-results) 
                                         (length processed)))))))
    
    processed))

(defun gse--enrich-results-with-source (results source)
  "Enrich RESULTS with SOURCE information."
  (mapcar (lambda (result)
           (setf (gse-result-source result) (gse-source-name source))
           result)
          results))

;;; Ranking Algorithms

(defun gse-default-ranking-algorithm (results query)
  "Default ranking algorithm for RESULTS based on QUERY."
  (let ((query-text (downcase (gse-query-text query))))
    (sort results
          (lambda (a b)
            (> (gse--calculate-relevance-score a query-text)
               (gse--calculate-relevance-score b query-text))))))

(defun gse--calculate-relevance-score (result query-text)
  "Calculate relevance score for RESULT given QUERY-TEXT."
  (let* ((title (downcase (gse-result-title result)))
         (description (downcase (gse-result-description result)))
         (score 0.0))
    
    ;; Exact title match
    (when (string= title query-text)
      (setq score (+ score 1.0)))
    
    ;; Title starts with query
    (when (string-prefix-p query-text title)
      (setq score (+ score 0.8)))
    
    ;; Title contains query
    (when (string-match-p query-text title)
      (setq score (+ score 0.6)))
    
    ;; Description contains query
    (when (string-match-p query-text description)
      (setq score (+ score 0.3)))
    
    ;; Add source priority bonus
    (setq score (+ score (* 0.1 (or (gse-result-score result) 0))))
    
    score))

;;; Public API

(defun gse-create-engine (&rest args)
  "Create search engine with ARGS."
  (let ((engine (apply #'make-gse-engine
                      :sources '()
                      :ranking-algorithm #'gse-default-ranking-algorithm
                      :max-concurrent 5
                      :timeout 30
                      args)))
    
    ;; Create default cache manager if not provided
    (unless (gse-engine-cache-manager engine)
      (setf (gse-engine-cache-manager engine)
            (make-gse-cache-manager
             :cache-store (make-hash-table :test 'equal)
             :ttl 300  ; 5 minutes
             :max-size 1000
             :metrics (make-hash-table :test 'equal))))
    
    engine))

(defun gse-create-source (&rest args)
  "Create search source with ARGS."
  (apply #'make-gse-source args))

(defun gse-create-result (&rest args)
  "Create search result with ARGS."
  (apply #'make-gse-result 
         :timestamp (current-time)
         :score 0.0
         args))

(defun gse-add-filter (engine filter-fn)
  "Add FILTER-FN to ENGINE's result filters."
  (let ((filters (gse-engine-search-filters engine)))
    (setf (gse-engine-search-filters engine)
          (append filters (list filter-fn)))))

(defun gse-add-transformer (engine transformer-fn)
  "Add TRANSFORMER-FN to ENGINE's result transformers."
  (let ((transformers (gse-engine-result-transformers engine)))
    (setf (gse-engine-result-transformers engine)
          (append transformers (list transformer-fn)))))

(defun gse-get-stats (engine)
  "Get statistics for ENGINE."
  (let ((cache-manager (gse-engine-cache-manager engine)))
    (list :sources (length (gse-engine-sources engine))
          :cache-size (when cache-manager
                       (hash-table-count (gse-cache-manager-cache-store cache-manager)))
          :cache-metrics (when cache-manager
                          (gse-cache-manager-metrics cache-manager)))))

;;; Error Handling

(define-error 'gse-error "Generic Search Engine Error")
(define-error 'gse-source-error "Search Source Error" 'gse-error)
(define-error 'gse-query-error "Search Query Error" 'gse-error)

(defun gse-error (format-string &rest args)
  "Signal search engine error with formatted message."
  (signal 'gse-error (list (apply #'format format-string args))))

(provide 'generic-search-engine)

;;; generic-search-engine.el ends here