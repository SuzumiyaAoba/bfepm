# BFEPM Framework API Reference

This document provides a comprehensive reference for BFEPM's framework APIs, both for end users and developers extending the system.

## 📚 Table of Contents

- [Core Framework APIs](#-core-framework-apis)
- [HTTP Client API](#-http-client-api)
- [Version Engine API](#-version-engine-api)
- [Search Engine API](#-search-engine-api)
- [Configuration Framework API](#-configuration-framework-api)
- [Plugin System API](#-plugin-system-api)
- [Package Manager Framework API](#-package-manager-framework-api)
- [Error Handling](#-error-handling)
- [Data Structures](#-data-structures)

## 🏗️ Core Framework APIs

### Framework Status and Management

#### `bfepm-framework-status`
```elisp
(bfepm-framework-status) => plist
```
Returns the status of all framework components.

**Returns:** Property list with framework component statuses
```elisp
(:http-client enhanced 
 :version-engine advanced 
 :search-engine multi-source 
 :config framework
 :plugins 3)
```

#### `bfepm-framework-available-p`
```elisp
(bfepm-framework-available-p COMPONENT) => boolean
```
Check if a specific framework component is available.

**Parameters:**
- `COMPONENT` (symbol): Component name (`:http-client`, `:version-engine`, `:search-engine`, `:config`)

**Returns:** `t` if component is available, `nil` otherwise

#### `bfepm-framework-reload`
```elisp
(bfepm-framework-reload &optional COMPONENTS) => boolean
```
Reload framework components.

**Parameters:**
- `COMPONENTS` (optional list): Specific components to reload, or `nil` for all

**Returns:** `t` if successful, `nil` if any component failed to load

#### `bfepm-framework-diagnose`
```elisp
(bfepm-framework-diagnose) => list
```
Diagnose framework component issues.

**Returns:** List of diagnostic messages and recommendations

## 🌐 HTTP Client API

### Client Configuration

#### `ghc-create-client`
```elisp
(ghc-create-client &key BASE-URL TIMEOUT RETRY-COUNT RETRY-STRATEGY RATE-LIMIT USER-AGENT PROXY) => http-client
```
Create an HTTP client with specified configuration.

**Parameters:**
- `BASE-URL` (string): Base URL for all requests
- `TIMEOUT` (number): Request timeout in seconds (default: 30)
- `RETRY-COUNT` (number): Maximum retry attempts (default: 3)
- `RETRY-STRATEGY` (symbol): Retry strategy (`:linear`, `:exponential`, `:exponential-jitter`)
- `RATE-LIMIT` (number): Maximum requests per second (default: 5)
- `USER-AGENT` (string): User agent string
- `PROXY` (string): Proxy URL

### HTTP Operations

#### `ghc-get`
```elisp
(ghc-get CLIENT URL &optional HEADERS) => response
```
Perform HTTP GET request.

**Parameters:**
- `CLIENT` (http-client): HTTP client instance
- `URL` (string): Request URL
- `HEADERS` (alist): Additional headers

**Returns:** Response object with status, headers, and body

#### `ghc-get-async`
```elisp
(ghc-get-async CLIENT URL CALLBACK &optional HEADERS) => request-id
```
Perform asynchronous HTTP GET request.

**Parameters:**
- `CLIENT` (http-client): HTTP client instance
- `URL` (string): Request URL
- `CALLBACK` (function): Callback function `(lambda (response) ...)`
- `HEADERS` (alist): Additional headers

**Returns:** Request ID for tracking

#### `ghc-download-file`
```elisp
(ghc-download-file CLIENT URL FILENAME &optional CALLBACK) => boolean
```
Download file from URL.

**Parameters:**
- `CLIENT` (http-client): HTTP client instance
- `URL` (string): File URL
- `FILENAME` (string): Local filename to save
- `CALLBACK` (optional function): Progress callback

**Returns:** `t` if successful, `nil` otherwise

### Response Handling

#### `ghc-response-success-p`
```elisp
(ghc-response-success-p RESPONSE) => boolean
```
Check if response indicates success (200-299 status).

#### `ghc-response-status-code`
```elisp
(ghc-response-status-code RESPONSE) => number
```
Get HTTP status code from response.

#### `ghc-response-body`
```elisp
(ghc-response-body RESPONSE) => string
```
Get response body as string.

#### `ghc-response-headers`
```elisp
(ghc-response-headers RESPONSE) => alist
```
Get response headers as association list.

#### `ghc-response-error`
```elisp
(ghc-response-error RESPONSE) => string
```
Get error message from failed response.

## 🏷️ Version Engine API

### Engine Management

#### `vce-create-engine`
```elisp
(vce-create-engine &key NAME STRICT-MODE-P) => version-engine
```
Create a version constraint engine.

**Parameters:**
- `NAME` (string): Engine identifier
- `STRICT-MODE-P` (boolean): Enable strict parsing mode

### Version Parsing and Comparison

#### `vce-parse-version`
```elisp
(vce-parse-version ENGINE VERSION-STRING &optional FORMAT) => parsed-version
```
Parse version string using engine.

**Parameters:**
- `ENGINE` (version-engine): Engine instance
- `VERSION-STRING` (string): Version to parse
- `FORMAT` (optional symbol): Specific format to use

#### `vce-compare-versions`
```elisp
(vce-compare-versions ENGINE V1 V2) => number
```
Compare two versions.

**Returns:** -1 if V1 < V2, 0 if equal, 1 if V1 > V2

#### `vce-version<`, `vce-version>`, `vce-version=`
```elisp
(vce-version< ENGINE V1 V2) => boolean
(vce-version> ENGINE V1 V2) => boolean
(vce-version= ENGINE V1 V2) => boolean
```
Version comparison predicates.

### Constraint Satisfaction

#### `vce-satisfies-p`
```elisp
(vce-satisfies-p ENGINE VERSION CONSTRAINT) => boolean
```
Check if version satisfies constraint.

**Parameters:**
- `ENGINE` (version-engine): Engine instance
- `VERSION` (string): Version to check
- `CONSTRAINT` (string): Constraint specification

**Examples:**
```elisp
(vce-satisfies-p engine "1.2.5" "^1.2.0")     ; => t
(vce-satisfies-p engine "20240615" "~20240601") ; => t
```

#### `vce-find-best-match`
```elisp
(vce-find-best-match ENGINE VERSIONS CONSTRAINT) => string
```
Find best version from list that satisfies constraint.

**Parameters:**
- `VERSIONS` (list): Available versions
- `CONSTRAINT` (string): Version constraint

#### `vce-sort-versions`
```elisp
(vce-sort-versions ENGINE VERSIONS &optional DESCENDING-P) => list
```
Sort versions in ascending or descending order.

### Format Registration

#### `vce-register-format`
```elisp
(vce-register-format ENGINE NAME FORMAT-SPEC) => boolean
```
Register custom version format.

**Parameters:**
- `NAME` (symbol): Format identifier
- `FORMAT-SPEC` (vce-version-format): Format specification

#### `vce-register-operator`
```elisp
(vce-register-operator ENGINE OPERATOR HANDLER) => boolean
```
Register custom constraint operator.

**Parameters:**
- `OPERATOR` (symbol): Operator identifier
- `HANDLER` (function): Handler function

## 🔍 Search Engine API

### Engine Management

#### `gse-create-engine`
```elisp
(gse-create-engine &key NAME CACHE-TTL MAX-CACHE-SIZE RANKING-ALGORITHM) => search-engine
```
Create search engine.

**Parameters:**
- `NAME` (string): Engine identifier
- `CACHE-TTL` (number): Cache time-to-live in seconds
- `MAX-CACHE-SIZE` (number): Maximum cache entries
- `RANKING-ALGORITHM` (function): Custom ranking function

#### `gse-add-source`
```elisp
(gse-add-source ENGINE SOURCE-NAME &key SEARCHER PRIORITY CACHE-KEY-FN) => boolean
```
Add search source to engine.

**Parameters:**
- `SOURCE-NAME` (string): Source identifier
- `SEARCHER` (function): Search function
- `PRIORITY` (number): Source priority (higher = preferred)
- `CACHE-KEY-FN` (function): Cache key generation function

### Search Operations

#### `gse-search`
```elisp
(gse-search ENGINE QUERY &optional OPTIONS) => list
```
Perform synchronous search across all sources.

**Parameters:**
- `QUERY` (string): Search query
- `OPTIONS` (plist): Search options

**Returns:** List of search results

#### `gse-search-async`
```elisp
(gse-search-async ENGINE QUERY CALLBACK &optional OPTIONS) => request-id
```
Perform asynchronous search.

**Parameters:**
- `CALLBACK` (function): Callback function `(lambda (success results error) ...)`

#### `gse-search-source`
```elisp
(gse-search-source ENGINE SOURCE-NAME QUERY &optional OPTIONS) => list
```
Search specific source only.

### Cache Management

#### `gse-cache-get`
```elisp
(gse-cache-get ENGINE CACHE-KEY) => cached-value
```
Get value from search cache.

#### `gse-cache-put`
```elisp
(gse-cache-put ENGINE CACHE-KEY VALUE &optional TTL) => boolean
```
Store value in search cache.

#### `gse-cache-clear`
```elisp
(gse-cache-clear ENGINE &optional SOURCE-NAME) => boolean
```
Clear cache for engine or specific source.

### Result Processing

#### `gse-create-result`
```elisp
(gse-create-result &key ID TITLE DESCRIPTION URL SOURCE TYPE METADATA) => search-result
```
Create standardized search result.

#### `gse-rank-results`
```elisp
(gse-rank-results ENGINE RESULTS QUERY) => list
```
Rank search results by relevance.

## ⚙️ Configuration Framework API

### Loader Management

#### `gcf-create-loader`
```elisp
(gcf-create-loader &key NAME SUPPORTED-FORMATS FORMAT-PRIORITY FALLBACK-LOADER DEFAULT-FACTORY SCHEMA) => config-loader
```
Create configuration loader.

**Parameters:**
- `SUPPORTED-FORMATS` (alist): Format name to parser function mapping
- `FORMAT-PRIORITY` (list): Preferred format order
- `FALLBACK-LOADER` (function): Fallback loading function
- `DEFAULT-FACTORY` (function): Default configuration factory
- `SCHEMA` (config-schema): Configuration schema for validation

### Configuration Operations

#### `gcf-load-config`
```elisp
(gcf-load-config LOADER FILE) => config
```
Load configuration from file.

#### `gcf-save-config`
```elisp
(gcf-save-config LOADER CONFIG FILE) => boolean
```
Save configuration to file.

#### `gcf-validate-config`
```elisp
(gcf-validate-config LOADER CONFIG) => boolean
```
Validate configuration against schema.

### Schema Management

#### `gcf-create-schema`
```elisp
(gcf-create-schema &key NAME VERSION REQUIRED-FIELDS OPTIONAL-FIELDS FIELDS) => config-schema
```
Create configuration schema.

#### `gcf-add-validator`
```elisp
(gcf-add-validator LOADER VALIDATOR-FUNCTION) => boolean
```
Add custom validator to loader.

### Format Support

#### `gcf-parse-toml`
```elisp
(gcf-parse-toml FILE) => config
```
Parse TOML configuration file.

#### `gcf-parse-json`
```elisp
(gcf-parse-json FILE) => config
```
Parse JSON configuration file.

#### `gcf-parse-sexp`
```elisp
(gcf-parse-sexp FILE) => config
```
Parse S-expression configuration file.

## 🔌 Plugin System API

### Plugin Definition

#### `ps-define-plugin`
```elisp
(ps-define-plugin NAME DOCSTRING &rest BODY) => plugin
```
Define a plugin.

**Parameters:**
- `NAME` (symbol): Plugin identifier
- `DOCSTRING` (string): Plugin description
- `BODY` (forms): Plugin initialization code

### Plugin Management

#### `ps-create-manager`
```elisp
(ps-create-manager &key NAME SECURITY-POLICY) => plugin-manager
```
Create plugin manager.

#### `ps-load-plugin`
```elisp
(ps-load-plugin MANAGER PLUGIN-NAME) => boolean
```
Load specific plugin.

#### `ps-unload-plugin`
```elisp
(ps-unload-plugin MANAGER PLUGIN-NAME) => boolean
```
Unload plugin.

#### `ps-list-plugins`
```elisp
(ps-list-plugins MANAGER) => list
```
List available plugins.

#### `ps-plugin-status`
```elisp
(ps-plugin-status MANAGER PLUGIN-NAME) => plist
```
Get plugin status information.

### Plugin Discovery

#### `ps-add-plugin-directory`
```elisp
(ps-add-plugin-directory MANAGER DIRECTORY) => boolean
```
Add directory to plugin search path.

#### `ps-refresh-plugins`
```elisp
(ps-refresh-plugins MANAGER) => number
```
Scan for new plugins in search directories.

### Hook System

#### `ps-register-hook`
```elisp
(ps-register-hook MANAGER HOOK-TYPE HOOK-SPEC) => boolean
```
Register plugin hook.

#### `ps-run-hooks`
```elisp
(ps-run-hooks MANAGER HOOK-TYPE &rest ARGS) => list
```
Execute all hooks of specified type.

### Security

#### `ps-create-sandbox`
```elisp
(ps-create-sandbox &key ALLOWED-FUNCTIONS PROHIBITED-FUNCTIONS MAX-MEMORY MAX-TIME) => sandbox
```
Create plugin execution sandbox.

## 📦 Package Manager Framework API

### Package Manager Creation

#### `pmf-create-package-manager`
```elisp
(pmf-create-package-manager NAME &key VERSION-ENGINE CONFIG-LOADER SEARCH-ENGINE INSTALLATION-BACKEND SOURCE-MANAGER HOOKS) => package-manager
```
Create package manager instance.

### Lifecycle Management

#### `pmf-lifecycle-hook`
```elisp
(pmf-lifecycle-hook MANAGER HOOK-TYPE &rest ARGS) => any
```
Execute package manager lifecycle hook.

**Hook Types:**
- `before-install`, `after-install`
- `before-remove`, `after-remove`  
- `before-update`, `after-update`

### Source Management

#### `pmf-source-manager-add-source`
```elisp
(pmf-source-manager-add-source MANAGER SOURCE) => boolean
```
Add package source to manager.

#### `make-pmf-source`
```elisp
(make-pmf-source &key NAME TYPE URL PRIORITY METADATA) => source
```
Create package source specification.

### Installation Backend

#### `make-pmf-installation-backend`
```elisp
(make-pmf-installation-backend &key NAME DOWNLOAD-MANAGER EXTRACTION-HANDLERS INSTALL-FN REMOVE-FN ROLLBACK-FN) => backend
```
Create installation backend.

## ❗ Error Handling

### Exception Types

All framework libraries use standardized exception types:

#### `bfepm-error`
Base error type for all BFEPM errors.

#### `bfepm-network-error`
Network operation failures.

#### `bfepm-version-error`
Version parsing or constraint errors.

#### `bfepm-config-error`
Configuration loading or validation errors.

#### `bfepm-plugin-error`
Plugin loading or execution errors.

### Error Handling Functions

#### `bfepm-error`
```elisp
(bfepm-error FORMAT-STRING &rest ARGS) => no-return
```
Signal BFEPM error with formatted message.

#### `bfepm-handle-error`
```elisp
(bfepm-handle-error ERROR-TYPE HANDLER-FUNCTION) => boolean
```
Register error handler for specific error type.

### Error Recovery

#### `bfepm-with-error-recovery`
```elisp
(bfepm-with-error-recovery RECOVERY-FUNCTION &rest BODY) => any
```
Execute body with automatic error recovery.

## 📊 Data Structures

### HTTP Client Structures

#### `ghc-client`
```elisp
(cl-defstruct ghc-client
  base-url timeout retry-count retry-strategy rate-limit
  user-agent proxy headers auth-config)
```

#### `ghc-response`
```elisp
(cl-defstruct ghc-response
  status-code headers body error-message request-time)
```

### Version Engine Structures

#### `vce-engine`
```elisp
(cl-defstruct vce-engine
  name version-formats constraint-operators default-format
  strict-mode-p cache hooks)
```

#### `vce-parsed-version`
```elisp
(cl-defstruct vce-parsed-version
  original format components prerelease build-metadata
  normalized metadata)
```

#### `vce-constraint`
```elisp
(cl-defstruct vce-constraint
  original operator version parameters metadata)
```

### Search Engine Structures

#### `gse-engine`
```elisp
(cl-defstruct gse-engine
  name sources cache ranking-algorithm max-concurrent
  timeout default-options)
```

#### `gse-result`
```elisp
(cl-defstruct gse-result
  id title description url source type score metadata)
```

### Configuration Framework Structures

#### `gcf-loader`
```elisp
(cl-defstruct gcf-loader
  name supported-formats format-priority fallback-loader
  default-factory schema validators)
```

#### `gcf-schema`
```elisp
(cl-defstruct gcf-schema
  name version required-fields optional-fields fields
  custom-validators)
```

### Plugin System Structures

#### `ps-plugin`
```elisp
(cl-defstruct ps-plugin
  name version description author hooks commands
  dependencies conflicts status)
```

#### `ps-manager`
```elisp
(cl-defstruct ps-manager
  name loaded-plugins available-plugins plugin-directories
  security-policy sandbox-config)
```

### Package Manager Framework Structures

#### `pmf-package-manager`
```elisp
(cl-defstruct pmf-package-manager
  name version-engine config-loader search-engine
  installation-backend source-manager hooks metadata)
```

#### `pmf-source`
```elisp
(cl-defstruct pmf-source
  name type url priority authentication cache-config
  metadata)
```

## 🔗 Integration Examples

### Complete Framework Integration

```elisp
;; Create fully integrated package manager
(defun create-enhanced-bfepm ()
  "Create BFEPM instance with all framework enhancements."
  (let* ((http-client (ghc-create-client 
                       :timeout 60 :retry-count 5 :rate-limit 5))
         (version-engine (vce-create-engine :name "bfepm-enhanced"))
         (config-loader (gcf-create-loader
                        :supported-formats '(("toml" . gcf-parse-toml)
                                           ("json" . gcf-parse-json))))
         (search-engine (gse-create-engine :name "bfepm-search")))
    
    ;; Configure version engine
    (vce-register-format version-engine 'melpa-date ...)
    
    ;; Configure search engine
    (gse-add-source search-engine "melpa" ...)
    (gse-add-source search-engine "gnu" ...)
    
    ;; Create package manager
    (pmf-create-package-manager "bfepm"
      :version-engine version-engine
      :config-loader config-loader
      :search-engine search-engine
      :installation-backend (create-bfepm-backend http-client))))
```

This API reference provides comprehensive coverage of all framework APIs. For practical usage examples, see the [Framework Guide](FRAMEWORK-GUIDE.md).