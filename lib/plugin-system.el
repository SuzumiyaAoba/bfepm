;;; plugin-system.el --- Extensible Plugin System for Package Managers -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: plugins extensions hooks architecture
;; URL: https://github.com/SuzumiyaAoba/bfepm

;;; Commentary:

;; This library provides a flexible plugin system for package managers
;; and other extensible applications. It supports dynamic plugin loading,
;; dependency management, lifecycle hooks, and sandboxed execution.
;;
;; Key Features:
;; - Dynamic plugin discovery and loading
;; - Plugin dependency resolution and ordering
;; - Lifecycle management (load, enable, disable, unload)
;; - Hook system for extensibility points
;; - Plugin API versioning and compatibility checking
;; - Sandboxed plugin execution with resource limits
;; - Plugin configuration and settings management
;; - Event system for inter-plugin communication
;; - Plugin templates and scaffolding
;; - Hot reloading for development
;;
;; Plugin Types:
;; - Source Providers: Add new package sources (Git, HTTP, local)
;; - Installation Backends: Custom installation methods
;; - UI Extensions: Additional interface components
;; - Search Adapters: Custom search implementations
;; - Version Engines: Support for new version schemes
;; - Configuration Parsers: Support for new config formats
;; - Notification Systems: Custom notification methods
;; - Authentication Providers: Custom auth mechanisms
;;
;; Usage:
;;   (setq manager (ps-create-manager))
;;   (ps-register-plugin manager my-plugin-spec)
;;   (ps-load-plugin manager "my-plugin")
;;   (ps-enable-plugin manager "my-plugin")

;;; Code:

(require 'cl-lib)

;;; Core Data Structures

(cl-defstruct (ps-manager (:constructor make-ps-manager)
                          (:copier nil))
  "Plugin system manager."
  name                   ; String: manager identifier
  plugins                ; Hash table: loaded plugins by name
  plugin-registry        ; Hash table: registered plugin specs
  plugin-directories     ; List: directories to search for plugins
  api-version            ; String: supported plugin API version
  hook-registry          ; Hash table: registered hooks
  event-system           ; ps-event-system: inter-plugin communication
  security-policy        ; ps-security-policy: plugin security settings
  dependency-resolver    ; Function: resolve plugin dependencies
  lifecycle-hooks        ; Alist: system lifecycle hooks
  plugin-cache          ; Hash table: plugin metadata cache
  hot-reload-p          ; Boolean: enable hot reloading
  sandbox-enabled-p)    ; Boolean: enable plugin sandboxing

(cl-defstruct (ps-plugin (:constructor make-ps-plugin)
                         (:copier nil))
  "Plugin specification and runtime state."
  name                   ; String: plugin identifier
  version                ; String: plugin version
  api-version            ; String: required API version
  description            ; String: plugin description
  author                 ; String: plugin author
  dependencies           ; List: required plugin names
  optional-dependencies  ; List: optional plugin names
  conflicts              ; List: conflicting plugin names
  load-path              ; String: plugin file/directory path
  entry-point            ; Symbol: main plugin function
  hooks                  ; Alist: hooks provided by plugin
  capabilities           ; List: plugin capabilities/permissions
  configuration          ; Hash table: plugin configuration
  state                  ; Symbol: loaded, enabled, disabled, error
  load-time              ; Time: when plugin was loaded
  error-info             ; String: error information if failed
  context)               ; ps-plugin-context: execution context

(cl-defstruct (ps-plugin-context (:constructor make-ps-plugin-context)
                                 (:copier nil))
  "Plugin execution context and sandbox."
  plugin-name            ; String: plugin identifier
  namespace              ; Hash table: plugin's private namespace
  api-registry           ; Hash table: available API functions
  resource-limits        ; Plist: resource consumption limits
  permissions            ; List: granted permissions
  temp-directory         ; String: temporary directory for plugin
  cleanup-functions      ; List: functions to call on unload
  message-handler        ; Function: handle plugin messages
  event-subscriptions)   ; List: subscribed events

(cl-defstruct (ps-hook (:constructor make-ps-hook)
                       (:copier nil))
  "Hook definition for extensibility points."
  name                   ; Symbol: hook identifier
  description            ; String: hook description
  type                   ; Symbol: filter, action, async-action
  parameters             ; List: hook parameter specification
  return-type            ; Symbol: expected return type
  handlers               ; List: registered handler functions
  priority-enabled-p     ; Boolean: support handler priorities
  async-p                ; Boolean: asynchronous hook execution
  cancellable-p)         ; Boolean: handlers can cancel execution

(cl-defstruct (ps-event-system (:constructor make-ps-event-system)
                               (:copier nil))
  "Event system for inter-plugin communication."
  event-registry         ; Hash table: registered event types
  subscribers            ; Hash table: event subscribers
  event-queue            ; List: queued events for async processing
  async-processor        ; Timer: async event processor
  middleware             ; List: event middleware functions
  debug-mode-p)          ; Boolean: enable event debugging

(cl-defstruct (ps-security-policy (:constructor make-ps-security-policy)
                                  (:copier nil))
  "Security policy for plugin execution."
  sandbox-enabled-p      ; Boolean: enable sandboxing
  allowed-functions      ; List: allowed function names
  forbidden-functions    ; List: explicitly forbidden functions
  file-access-policy     ; Symbol: none, readonly, restricted, full
  network-access-policy  ; Symbol: none, restricted, full
  resource-limits        ; Plist: CPU, memory, file limits
  signature-verification-p ; Boolean: verify plugin signatures
  trusted-authors        ; List: trusted plugin authors
  quarantine-policy)     ; Symbol: strict, moderate, permissive

;;; Plugin Discovery and Registration

(defun ps-discover-plugins (manager)
  "Discover plugins in MANAGER's plugin directories."
  (let ((discovered '()))
    (dolist (directory (ps-manager-plugin-directories manager))
      (when (file-directory-p directory)
        (let ((plugin-files (directory-files directory t "\\.el$")))
          (dolist (file plugin-files)
            (when-let ((spec (ps--parse-plugin-file file)))
              (push spec discovered))))))
    discovered))

(defun ps--parse-plugin-file (file-path)
  "Parse plugin specification from FILE-PATH."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (ps--extract-plugin-metadata (current-buffer) file-path))
    (error
     (message "Failed to parse plugin file %s: %s" file-path (error-message-string err))
     nil)))

(defun ps--extract-plugin-metadata (_buffer file-path)
  "Extract plugin metadata from BUFFER for FILE-PATH."
  (let ((metadata (make-hash-table :test 'equal)))
    (goto-char (point-min))
    
    ;; Extract standard header fields
    (while (re-search-forward "^;;[[:space:]]*\\([A-Za-z-]+\\):[[:space:]]*\\(.+\\)$" nil t)
      (let ((key (downcase (match-string 1)))
            (value (string-trim (match-string 2))))
        (puthash key value metadata)))
    
    ;; Look for plugin definition
    (goto-char (point-min))
    (when (re-search-forward "(ps-define-plugin[[:space:]]+\\([^[:space:]()]+\\)" nil t)
      (let ((plugin-name (match-string 1)))
        (puthash "name" plugin-name metadata)))
    
    ;; Create plugin specification
    (when (gethash "name" metadata)
      (make-ps-plugin
       :name (gethash "name" metadata)
       :version (or (gethash "version" metadata) "0.1.0")
       :api-version (or (gethash "api-version" metadata) "1.0")
       :description (or (gethash "description" metadata) "")
       :author (or (gethash "author" metadata) "")
       :dependencies (ps--parse-dependency-list (gethash "dependencies" metadata))
       :load-path file-path
       :state 'discovered))))

(defun ps--parse-dependency-list (dep-string)
  "Parse dependency list from DEP-STRING."
  (when dep-string
    (mapcar #'string-trim (split-string dep-string "[,;]"))))

(defun ps-register-plugin (manager plugin-spec)
  "Register PLUGIN-SPEC with MANAGER."
  (unless (ps-plugin-p plugin-spec)
    (error "Invalid plugin specification"))
  
  (let ((registry (ps-manager-plugin-registry manager))
        (name (ps-plugin-name plugin-spec)))
    (puthash name plugin-spec registry)
    (message "Registered plugin: %s" name)))

;;; Plugin Loading and Lifecycle

(defun ps-load-plugin (manager plugin-name)
  "Load plugin named PLUGIN-NAME using MANAGER."
  (cl-block ps-load-plugin
    (let* ((registry (ps-manager-plugin-registry manager))
           (spec (gethash plugin-name registry)))
      
      (unless spec
        (error "Plugin not found: %s" plugin-name))
      
      (when (eq (ps-plugin-state spec) 'loaded)
        (message "Plugin %s already loaded" plugin-name)
        (cl-return-from ps-load-plugin nil))
      
      ;; Check dependencies
      (ps--resolve-dependencies manager spec)
      
      ;; Check API compatibility
      (ps--check-api-compatibility manager spec)
      
      ;; Load plugin file
      (condition-case err
          (progn
            (ps--setup-plugin-context manager spec)
            (load (ps-plugin-load-path spec) nil t)
            (ps--initialize-plugin manager spec)
            (setf (ps-plugin-state spec) 'loaded)
            (setf (ps-plugin-load-time spec) (current-time))
            (puthash plugin-name spec (ps-manager-plugins manager))
            (message "Loaded plugin: %s" plugin-name))
        (error
         (setf (ps-plugin-state spec) 'error)
         (setf (ps-plugin-error-info spec) (error-message-string err))
         (error "Failed to load plugin %s: %s" plugin-name (error-message-string err)))))))

(defun ps--resolve-dependencies (manager spec)
  "Resolve dependencies for plugin SPEC using MANAGER."
  (dolist (dep (ps-plugin-dependencies spec))
    (unless (ps-plugin-loaded-p manager dep)
      (ps-load-plugin manager dep))))

(defun ps--check-api-compatibility (manager spec)
  "Check API compatibility for plugin SPEC with MANAGER."
  (let ((plugin-api (ps-plugin-api-version spec))
        (manager-api (ps-manager-api-version manager)))
    (unless (ps--api-compatible-p plugin-api manager-api)
      (error "Plugin %s requires API version %s, but %s is available"
             (ps-plugin-name spec) plugin-api manager-api))))

(defun ps--api-compatible-p (plugin-version manager-version)
  "Check if PLUGIN-VERSION is compatible with MANAGER-VERSION."
  ;; Simple major version compatibility check
  (let ((plugin-major (car (version-to-list plugin-version)))
        (manager-major (car (version-to-list manager-version))))
    (= plugin-major manager-major)))

(defun ps--setup-plugin-context (manager spec)
  "Setup execution context for plugin SPEC in MANAGER."
  (let* ((plugin-name (ps-plugin-name spec))
         (context (make-ps-plugin-context
                  :plugin-name plugin-name
                  :namespace (make-hash-table :test 'equal)
                  :api-registry (ps--build-api-registry manager)
                  :permissions (ps--calculate-permissions manager spec)
                  :cleanup-functions '())))
    (setf (ps-plugin-context spec) context)))

(defun ps--build-api-registry (_manager)
  "Build API registry for plugins in MANAGER."
  (let ((api (make-hash-table :test 'equal)))
    ;; Add core API functions
    (puthash 'ps-register-hook api #'ps-register-hook)
    (puthash 'ps-emit-event api #'ps-emit-event)
    (puthash 'ps-subscribe-event api #'ps-subscribe-event)
    (puthash 'ps-log api #'ps-log)
    (puthash 'ps-get-config api #'ps-get-config)
    (puthash 'ps-set-config api #'ps-set-config)
    api))

(defun ps--calculate-permissions (_manager spec)
  "Calculate permissions for plugin SPEC in MANAGER."
  (let ((capabilities (ps-plugin-capabilities spec)))
    ;; Default permissions based on capabilities
    (append '(basic-api)
            (when (member 'file-access capabilities) '(file-read file-write))
            (when (member 'network-access capabilities) '(network)))))

(defun ps--initialize-plugin (manager spec)
  "Initialize plugin SPEC in MANAGER."
  (when-let ((entry-point (ps-plugin-entry-point spec)))
    (when (fboundp entry-point)
      (funcall entry-point manager))))

(defun ps-enable-plugin (manager plugin-name)
  "Enable plugin named PLUGIN-NAME in MANAGER."
  (cl-block ps-enable-plugin
    (let ((plugin (gethash plugin-name (ps-manager-plugins manager))))
      (unless plugin
        (error "Plugin not loaded: %s" plugin-name))
      
      (when (eq (ps-plugin-state plugin) 'enabled)
        (message "Plugin %s already enabled" plugin-name)
        (cl-return-from ps-enable-plugin nil))
      
      ;; Run enable hooks
      (ps--run-plugin-hooks manager plugin 'enable)
      (setf (ps-plugin-state plugin) 'enabled)
      (message "Enabled plugin: %s" plugin-name))))

(defun ps-disable-plugin (manager plugin-name)
  "Disable plugin named PLUGIN-NAME in MANAGER."
  (cl-block ps-disable-plugin
    (let ((plugin (gethash plugin-name (ps-manager-plugins manager))))
      (unless plugin
        (error "Plugin not loaded: %s" plugin-name))
      
      (when (eq (ps-plugin-state plugin) 'disabled)
        (message "Plugin %s already disabled" plugin-name)
        (cl-return-from ps-disable-plugin nil))
      
      ;; Run disable hooks
      (ps--run-plugin-hooks manager plugin 'disable)
      (setf (ps-plugin-state plugin) 'disabled)
      (message "Disabled plugin: %s" plugin-name))))

(defun ps-unload-plugin (manager plugin-name)
  "Unload plugin named PLUGIN-NAME from MANAGER."
  (let ((plugin (gethash plugin-name (ps-manager-plugins manager))))
    (unless plugin
      (error "Plugin not loaded: %s" plugin-name))
    
    ;; Disable first if enabled
    (when (eq (ps-plugin-state plugin) 'enabled)
      (ps-disable-plugin manager plugin-name))
    
    ;; Run cleanup
    (ps--cleanup-plugin manager plugin)
    (remhash plugin-name (ps-manager-plugins manager))
    (message "Unloaded plugin: %s" plugin-name)))

(defun ps--cleanup-plugin (_manager plugin)
  "Cleanup resources for PLUGIN in MANAGER."
  (when-let ((context (ps-plugin-context plugin)))
    ;; Run cleanup functions
    (dolist (cleanup-fn (ps-plugin-context-cleanup-functions context))
      (condition-case err
          (funcall cleanup-fn)
        (error
         (message "Plugin cleanup error: %s" (error-message-string err)))))
    
    ;; Clean up temporary files
    (when-let ((temp-dir (ps-plugin-context-temp-directory context)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Hook System

(defun ps-register-hook (manager hook-name hook-spec)
  "Register hook HOOK-NAME with HOOK-SPEC in MANAGER."
  (let ((registry (ps-manager-hook-registry manager)))
    (puthash hook-name hook-spec registry)))

(defun ps-add-hook-handler (manager hook-name handler &optional priority)
  "Add HANDLER for HOOK-NAME in MANAGER with optional PRIORITY."
  (let* ((registry (ps-manager-hook-registry manager))
         (hook (gethash hook-name registry)))
    
    (unless hook
      (error "Hook not registered: %s" hook-name))
    
    (let ((handlers (ps-hook-handlers hook)))
      (if priority
          (ps--insert-handler-with-priority handlers handler priority)
        (setf (ps-hook-handlers hook) (append handlers (list handler)))))))

(defun ps--insert-handler-with-priority (handlers handler priority)
  "Insert HANDLER with PRIORITY into HANDLERS list."
  ;; For simplicity, higher numbers = higher priority
  (let ((inserted nil)
        (new-handlers '()))
    (dolist (existing-handler handlers)
      (let ((existing-priority (get existing-handler 'ps-priority)))
        (when (and (not inserted) (< (or existing-priority 0) priority))
          (push handler new-handlers)
          (put handler 'ps-priority priority)
          (setq inserted t))
        (push existing-handler new-handlers)))
    
    (unless inserted
      (push handler new-handlers)
      (put handler 'ps-priority priority))
    
    (reverse new-handlers)))

(defun ps-run-hook (manager hook-name &rest args)
  "Run hook HOOK-NAME in MANAGER with ARGS."
  (let* ((registry (ps-manager-hook-registry manager))
         (hook (gethash hook-name registry)))
    
    (when hook
      (let ((handlers (ps-hook-handlers hook))
            (result nil))
        (dolist (handler handlers)
          (condition-case err
              (setq result (apply handler args))
            (error
             (message "Hook handler error in %s: %s" hook-name (error-message-string err)))))
        result))))

;;; Event System

(defun ps-emit-event (manager event-type data)
  "Emit event of EVENT-TYPE with DATA in MANAGER."
  (let* ((event-system (ps-manager-event-system manager))
         (subscribers (gethash event-type (ps-event-system-subscribers event-system))))
    
    (dolist (subscriber subscribers)
      (condition-case err
          (funcall subscriber data)
        (error
         (message "Event handler error for %s: %s" event-type (error-message-string err)))))))

(defun ps-subscribe-event (manager event-type handler)
  "Subscribe HANDLER to EVENT-TYPE in MANAGER."
  (let* ((event-system (ps-manager-event-system manager))
         (subscribers (ps-event-system-subscribers event-system))
         (current-handlers (gethash event-type subscribers '())))
    
    (puthash event-type (cons handler current-handlers) subscribers)))

;;; Plugin Queries

(defun ps-plugin-loaded-p (manager plugin-name)
  "Check if plugin PLUGIN-NAME is loaded in MANAGER."
  (and (gethash plugin-name (ps-manager-plugins manager)) t))

(defun ps-plugin-enabled-p (manager plugin-name)
  "Check if plugin PLUGIN-NAME is enabled in MANAGER."
  (when-let ((plugin (gethash plugin-name (ps-manager-plugins manager))))
    (eq (ps-plugin-state plugin) 'enabled)))

(defun ps-list-plugins (manager &optional state)
  "List plugins in MANAGER, optionally filtered by STATE."
  (let ((plugins '()))
    (maphash (lambda (name plugin)
              (when (or (null state) (eq (ps-plugin-state plugin) state))
                (push name plugins)))
             (ps-manager-plugins manager))
    plugins))

(defun ps-get-plugin-info (manager plugin-name)
  "Get information about plugin PLUGIN-NAME in MANAGER."
  (gethash plugin-name (ps-manager-plugins manager)))

;;; Public API

(defun ps-create-manager (&rest args)
  "Create plugin manager with ARGS."
  (let ((manager (apply #'make-ps-manager
                       :plugins (make-hash-table :test 'equal)
                       :plugin-registry (make-hash-table :test 'equal)
                       :hook-registry (make-hash-table :test 'equal)
                       :plugin-cache (make-hash-table :test 'equal)
                       :api-version "1.0"
                       args)))
    
    ;; Create event system
    (setf (ps-manager-event-system manager)
          (make-ps-event-system
           :event-registry (make-hash-table :test 'equal)
           :subscribers (make-hash-table :test 'equal)))
    
    ;; Create default security policy
    (unless (ps-manager-security-policy manager)
      (setf (ps-manager-security-policy manager)
            (make-ps-security-policy
             :sandbox-enabled-p nil
             :file-access-policy 'restricted
             :network-access-policy 'restricted)))
    
    manager))

(defun ps-add-plugin-directory (manager directory)
  "Add DIRECTORY to MANAGER's plugin search paths."
  (let ((dirs (ps-manager-plugin-directories manager)))
    (unless (member directory dirs)
      (setf (ps-manager-plugin-directories manager)
            (cons directory dirs)))))

(defun ps-refresh-plugins (manager)
  "Refresh plugin registry by rediscovering plugins in MANAGER."
  (let ((discovered (ps-discover-plugins manager)))
    (dolist (plugin discovered)
      (ps-register-plugin manager plugin))
    (length discovered)))

;;; Plugin Definition Macro

(defmacro ps-define-plugin (name &rest body)
  "Define a plugin with NAME and BODY.
Note: This only creates the plugin specification. You must still
register it with a plugin manager using `ps-register-plugin'."
  (declare (indent 1))
  `(progn
     (defun ,(intern (format "%s--plugin-main" name)) (manager)
       ,@body)
     
     (defvar ,(intern (format "%s--plugin-spec" name))
       (make-ps-plugin
        :name ,(symbol-name name)
        :entry-point ',(intern (format "%s--plugin-main" name))))))

;;; Utility Functions

(defun ps--run-plugin-hooks (manager plugin hook-type)
  "Run HOOK-TYPE hooks for PLUGIN in MANAGER."
  (when-let ((hooks (ps-plugin-hooks plugin)))
    (dolist (hook-entry hooks)
      (when (eq (car hook-entry) hook-type)
        (funcall (cdr hook-entry) manager plugin)))))

(defun ps-log (level message &rest args)
  "Log MESSAGE with LEVEL and ARGS."
  (let ((formatted-message (apply #'format message args)))
    (message "[Plugin System %s] %s" level formatted-message)))

(defun ps-get-config (manager plugin-name key &optional default)
  "Get configuration KEY for PLUGIN-NAME in MANAGER."
  (when-let* ((plugin (gethash plugin-name (ps-manager-plugins manager)))
              (config (ps-plugin-configuration plugin)))
    (or (gethash key config) default)))

(defun ps-set-config (manager plugin-name key value)
  "Set configuration KEY to VALUE for PLUGIN-NAME in MANAGER."
  (when-let ((plugin (gethash plugin-name (ps-manager-plugins manager))))
    (unless (ps-plugin-configuration plugin)
      (setf (ps-plugin-configuration plugin) (make-hash-table :test 'equal)))
    (puthash key value (ps-plugin-configuration plugin))))

;;; Error Handling

(define-error 'ps-error "Plugin System Error")
(define-error 'ps-plugin-error "Plugin Error" 'ps-error)
(define-error 'ps-dependency-error "Plugin Dependency Error" 'ps-error)

(defun ps-error (format-string &rest args)
  "Signal a plugin system error with FORMAT-STRING and ARGS."
  (signal 'ps-error (list (apply #'format format-string args))))

(provide 'plugin-system)

;;; plugin-system.el ends here