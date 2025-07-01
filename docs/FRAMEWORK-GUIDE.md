# BFEPM Framework Integration Guide

This guide explains how to leverage BFEPM's powerful framework abstraction layer to enhance your package management experience.

## 🎯 What are Framework Libraries?

BFEPM v0.2.0 introduces a sophisticated framework abstraction layer consisting of 7 specialized libraries that enhance core functionality while maintaining complete backward compatibility.

### 🏗️ Framework Architecture

```
Framework Libraries (lib/)
├── generic-http-client.el        # Advanced HTTP operations
├── version-constraint-engine.el  # Sophisticated version handling
├── generic-search-engine.el      # Multi-source search
├── generic-config-framework.el   # Multi-format configuration
├── package-manager-framework.el  # Generic package manager abstractions
├── plugin-system.el              # Extensible plugin architecture
└── bfepm-framework-integration.el # Integration layer
```

## 🚀 Quick Start

### 1. Check Framework Status

```elisp
;; See which framework libraries are active
(bfepm-framework-status)
; => (:http-client enhanced :version-engine advanced :search-engine multi-source :config framework)

;; Check if specific frameworks are available
(bfepm-framework-available-p 'http-client)     ; => t
(bfepm-framework-available-p 'version-engine)  ; => t
```

### 2. Basic Usage (No Configuration Required)

Framework integration is automatic! Your existing BFEPM configuration works immediately with enhanced performance:

```toml
# Your existing bfepm.toml works unchanged but faster
[packages]
company = "latest"
magit = "^3.3.0"
lsp-mode = { version = "^8.0", optional = true }
```

## 🌟 Enhanced Features

### ⚡ Advanced HTTP Operations

**Before (Built-in):**
- Basic retry logic
- Simple timeout handling
- No rate limiting

**After (Framework-Enhanced):**
- Exponential backoff with jitter
- Smart retry strategies
- Configurable rate limiting
- Connection pooling
- Corporate proxy support

```elisp
;; Configure advanced HTTP behavior
(setq bfepm-network-config
  '(:timeout 60
    :retry-count 5
    :retry-strategy exponential
    :rate-limit 3                    ; requests per second
    :user-agent "MyEmacs-BFEPM/1.0"
    :max-concurrent 5))
```

### 🏷️ Sophisticated Version Handling

**Before (Built-in):**
- Basic semantic versioning
- MELPA date versions

**After (Framework-Enhanced):**
- Multiple version formats
- Complex constraint operators
- Cross-format comparison
- Custom version schemes

```elisp
;; Advanced version constraints
(bfepm-version-satisfies-p "1.2.5" "^1.2.0")     ; => t (caret constraint)
(bfepm-version-satisfies-p "20240615" "^20240601") ; => t (MELPA date)

;; Find best version match
(bfepm-version-find-best-match 
  '("1.1.0" "1.2.3" "1.3.0" "2.0.0") 
  "^1.2.0")
; => "1.3.0"

;; Custom version format
(bfepm-version-register-format 'build-number
  :parser #'my-build-number-parser
  :comparator #'my-build-number-comparator
  :pattern "^build-[0-9]+$")
```

### 🔍 Multi-Source Search

**Before (Built-in):**
- Sequential source queries
- Basic relevance scoring

**After (Framework-Enhanced):**
- Parallel source queries
- Intelligent result ranking
- Cached search results
- Aggregated result deduplication

```elisp
;; Search across all sources with intelligent ranking
(bfepm-search "helm")
; => Returns ranked results from MELPA, GNU ELPA, and other sources

;; Async search with callback
(bfepm-search-async "company" 
  (lambda (results)
    (message "Found %d packages" (length results))))

;; Add custom search source
(bfepm-search-add-source "internal"
  :url "https://packages.mycompany.com/archive-contents"
  :priority 15
  :cache-ttl 1800)
```

### ⚙️ Multi-Format Configuration

**Before (Built-in):**
- TOML only
- Simple fallback

**After (Framework-Enhanced):**
- TOML, JSON, S-expressions
- Automatic format detection
- Schema validation
- Migration utilities

```elisp
;; Load configuration from any supported format
(bfepm-config-load "bfepm.toml")   ; TOML
(bfepm-config-load "bfepm.json")   ; JSON  
(bfepm-config-load "bfepm.el")     ; S-expressions

;; Validate configuration against schema
(bfepm-config-validate config)

;; Convert between formats
(bfepm-config-convert "bfepm.toml" "bfepm.json")
```

## 🎛️ Advanced Configuration

### HTTP Client Customization

```elisp
;; Corporate environment configuration
(setq bfepm-http-client-config
  '(:base-url nil
    :timeout 120
    :retry-count 5
    :retry-strategy exponential-jitter
    :backoff-base 1.5
    :rate-limit 2
    :max-concurrent 3
    :user-agent "Corporate-Emacs/29.1 BFEPM/0.2.0"
    :proxy "http://proxy.company.com:8080"
    :proxy-auth (:username "user" :password-var "PROXY_PASSWORD")
    :ssl-verify t
    :headers (("X-Custom-Header" . "value"))))
```

### Version Engine Configuration

```elisp
;; Add custom version format for internal packages
(bfepm-version-register-format 'internal
  :name 'internal
  :parser (lambda (version-string)
            (when (string-match "^v\\([0-9]+\\)\\.\\([0-9]+\\)\\-\\([0-9]+\\)$" version-string)
              (list (string-to-number (match-string 1 version-string))
                    (string-to-number (match-string 2 version-string))
                    (string-to-number (match-string 3 version-string)))))
  :comparator #'bfepm-version--compare-internal
  :pattern "^v[0-9]+\\.[0-9]+\\-[0-9]+$")

;; Use custom constraints
[packages]
my-internal-package = { version = "@>=v1.2-100", format = "internal" }
```

### Search Engine Configuration

```elisp
;; Configure search behavior
(setq bfepm-search-config
  '(:parallel-queries t
    :max-concurrent 5
    :timeout 30
    :cache-ttl 1800
    :result-limit 100
    :ranking-algorithm enhanced
    :deduplicate-results t))

;; Custom ranking function
(setq bfepm-search-ranking-function
  (lambda (results query)
    (sort results
          (lambda (a b)
            (> (my-custom-score a query)
               (my-custom-score b query))))))
```

## 🔌 Plugin System (Experimental)

### Creating a Plugin

```elisp
;; Define a custom plugin
(bfepm-define-plugin dockerfile-packages
  "Support for Dockerfile-based package installation."
  :version "1.0.0"
  :author "Your Name <email@example.com>"
  :description "Install packages from Dockerfiles"
  
  ;; Register new source type
  (bfepm-register-source-type 'dockerfile
    :installer #'dockerfile-package-installer
    :searcher #'dockerfile-package-searcher
    :validator #'dockerfile-package-validator)
  
  ;; Add custom commands
  (bfepm-register-command 'install-dockerfile
    #'bfepm-install-dockerfile-command
    "Install package from Dockerfile")
  
  ;; Register hooks
  (bfepm-register-hook 'before-install
    #'dockerfile-before-install-hook))

;; Plugin implementation functions
(defun dockerfile-package-installer (package)
  "Install PACKAGE from Dockerfile."
  ;; Custom installation logic
  )

(defun dockerfile-package-searcher (query)
  "Search for packages matching QUERY in Dockerfile sources."
  ;; Custom search logic
  )
```

### Loading Plugins

```elisp
;; Load plugin directory
(bfepm-plugin-load-directory "~/.emacs.d/bfepm-plugins/")

;; Load specific plugin
(bfepm-plugin-load 'dockerfile-packages)

;; List available plugins
(bfepm-plugin-list)
; => (dockerfile-packages git-lfs-packages private-registry)

;; Plugin status
(bfepm-plugin-status 'dockerfile-packages)
; => (:status loaded :version "1.0.0" :hooks 1 :commands 1)
```

## 📊 Performance Monitoring

### Benchmarking Framework vs Built-in

```elisp
;; Benchmark search performance
(bfepm-benchmark-search "helm" 10)
; => Framework: 0.82s avg, Built-in: 2.15s avg (62% improvement)

;; Benchmark version resolution
(bfepm-benchmark-version-resolution 100)
; => Framework: 0.03s avg, Built-in: 0.08s avg (63% improvement)

;; Network operation statistics
(bfepm-network-stats)
; => (:requests 150 :success-rate 98% :avg-response-time 0.45s :cache-hits 23%)
```

### Performance Tuning

```elisp
;; Optimize for fast networks
(setq bfepm-performance-profile 'fast-network
      bfepm-http-client-config
      '(:timeout 15
        :retry-count 2
        :rate-limit 10
        :max-concurrent 8))

;; Optimize for slow/unreliable networks
(setq bfepm-performance-profile 'slow-network
      bfepm-http-client-config
      '(:timeout 120
        :retry-count 5
        :rate-limit 1
        :max-concurrent 2
        :retry-strategy exponential-jitter))

;; Memory optimization for large package sets
(setq bfepm-cache-config
      '(:max-memory-usage 50000000  ; 50MB
        :cache-ttl 3600             ; 1 hour
        :eviction-strategy lru
        :compression t))
```

## 🛡️ Security Features

### Safe Plugin Execution

```elisp
;; Configure plugin sandboxing
(setq bfepm-plugin-security
  '(:sandbox-mode t
    :allowed-functions (message format require)
    :prohibited-functions (delete-file shell-command)
    :max-memory 10000000           ; 10MB limit
    :max-execution-time 30         ; 30 seconds
    :network-access limited))      ; Only to registered sources
```

### Network Security

```elisp
;; Secure HTTP configuration
(setq bfepm-http-security
  '(:ssl-verify t
    :ssl-min-version tlsv1.2
    :certificate-checking strict
    :follow-redirects limited       ; Max 3 redirects
    :max-response-size 100000000    ; 100MB limit
    :timeout 60
    :user-agent-required t))
```

## 🔧 Troubleshooting

### Common Issues

#### Framework Libraries Not Loading

```elisp
;; Check library availability
(bfepm-framework-diagnose)
; => Reports missing libraries and load errors

;; Force reload frameworks
(bfepm-framework-reload)

;; Check load path
(member (expand-file-name "lib" bfepm-directory) load-path)
```

#### Performance Issues

```elisp
;; Profile framework operations
(bfepm-profile-enable)
(bfepm-search "helm")
(bfepm-profile-report)
; => Shows timing breakdown by component

;; Check cache status
(bfepm-cache-status)
; => (:hits 45 :misses 12 :size 1.2MB :efficiency 78%)

;; Clear caches if needed
(bfepm-cache-clear)
```

#### Plugin Issues

```elisp
;; Debug plugin loading
(bfepm-plugin-debug-mode t)
(bfepm-plugin-load 'my-plugin)

;; Check plugin sandbox violations
(bfepm-plugin-security-log)
; => Lists any security violations or sandbox escapes
```

### Debug Mode

```elisp
;; Enable comprehensive debugging
(setq bfepm-debug-mode t
      bfepm-debug-components '(http-client version-engine search-engine config))

;; View debug log
(bfepm-debug-log)

;; Save debug session
(bfepm-debug-save-session "~/bfepm-debug.log")
```

## 📈 Migration Guide

### From v0.1.x to v0.2.x

Framework integration is completely automatic! Your existing configuration continues to work without changes, but gains enhanced performance and new capabilities.

**No Action Required:**
- Existing `bfepm.toml` files work unchanged
- All existing commands (`bfepm-install`, `bfepm-update`, etc.) work as before
- Performance automatically improves

**Optional Enhancements:**
```elisp
;; Take advantage of new features
(setq bfepm-search-parallel t)           ; Enable parallel search
(setq bfepm-network-rate-limit 5)        ; Configure rate limiting
(setq bfepm-cache-enabled t)             ; Enable intelligent caching
```

### Customizing Framework Behavior

```elisp
;; Prefer framework features when available
(setq bfepm-framework-preference 'prefer-framework)

;; Use built-in implementations only
(setq bfepm-framework-preference 'prefer-builtin)

;; Automatic (default) - use framework when available, fallback when not
(setq bfepm-framework-preference 'automatic)
```

## 🎓 Advanced Examples

### Enterprise Configuration

```elisp
;; Complete enterprise setup
(setq bfepm-enterprise-config
  '(:http-client
    (:proxy "http://proxy.company.com:8080"
     :timeout 120
     :retry-count 5
     :rate-limit 2
     :ssl-verify t
     :certificate-file "/etc/ssl/certs/company-ca.pem")
    
    :sources
    (("melpa" . (:url "https://melpa.org/packages/" :priority 10))
     ("gnu" . (:url "https://elpa.gnu.org/packages/" :priority 5))
     ("company-internal" . (:url "https://packages.company.com/" :priority 20)))
    
    :search-engine
    (:parallel-queries t
     :cache-ttl 3600
     :max-results 50)
    
    :security
    (:plugin-sandboxing t
     :network-restrictions t
     :package-verification t)))

(bfepm-apply-enterprise-config bfepm-enterprise-config)
```

### Development Environment

```elisp
;; Optimized for development
(setq bfepm-dev-config
  '(:http-client
    (:timeout 30
     :retry-count 3
     :rate-limit 10
     :max-concurrent 8)
    
    :search-engine
    (:parallel-queries t
     :cache-ttl 1800
     :result-ranking enhanced)
    
    :version-engine
    (:constraint-checking strict
     :cross-format-comparison t
     :custom-formats (build-number snapshot))
    
    :plugins
    (:auto-load t
     :sandboxing relaxed
     :development-mode t)))

(bfepm-apply-dev-config bfepm-dev-config)
```

This framework integration represents a significant evolution in BFEPM's capabilities while maintaining the simplicity and reliability that users expect. The graceful degradation design ensures that BFEPM works in all environments, providing enhanced functionality when possible and reliable baseline functionality always.