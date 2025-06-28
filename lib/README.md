# Generic Package Manager Framework Libraries

This directory contains a collection of reusable libraries extracted from the BFEPM (Better Fast Package Manager) codebase. These libraries provide a comprehensive framework for building package managers and similar tools across different ecosystems.

## 📚 Library Overview

### Core Framework
- **`package-manager-framework.el`** - Main framework interfaces and base structures
- **`bfepm-framework-integration.el`** - Example integration showing how to use the framework

### Specialized Libraries
- **`generic-http-client.el`** - HTTP client with retry logic, rate limiting, and async support
- **`version-constraint-engine.el`** - Version comparison and constraint satisfaction system  
- **`generic-config-framework.el`** - Multi-format configuration management with validation
- **`generic-search-engine.el`** - Multi-source search with caching and ranking
- **`plugin-system.el`** - Extensible plugin architecture with sandboxing

## 🏗️ Architecture Principles

### 1. **Domain-Driven Design**
Each library focuses on a specific domain with clear boundaries:
```
Network Operations ← HTTP Client
Version Management ← Version Engine  
Configuration ← Config Framework
Search & Discovery ← Search Engine
Extensibility ← Plugin System
```

### 2. **Interface Segregation**
Small, focused interfaces rather than monolithic ones:
```elisp
;; Good: Focused interface
(cl-defstruct http-client base-url timeout retry-count)

;; Avoid: Monolithic interface  
(cl-defstruct mega-client http-config search-config version-config ...)
```

### 3. **Dependency Inversion**
High-level modules depend on abstractions, not concrete implementations:
```elisp
;; Package manager depends on abstract version engine
(setf (pmf-package-manager-version-engine pm) any-version-engine)

;; Not on specific implementation
(setf (pmf-package-manager-version-engine pm) bfepm-version-engine)
```

## 🚀 Usage Examples

### Basic Package Manager
```elisp
;; Create a simple package manager
(setq pm (pmf-create-package-manager "my-pm"
  :version-engine (vce-create-engine :name "semver")
  :config-loader (gcf-create-loader)
  :search-engine (gse-create-engine :name "search")))

;; Add package sources
(let ((source-mgr (pmf-package-manager-source-manager pm)))
  (pmf-source-manager-add-source source-mgr
    (make-pmf-source :name "npm" :url "https://registry.npmjs.org/")))
```

### HTTP Operations
```elisp
;; Create HTTP client with retry logic
(setq client (ghc-create-client 
  :base-url "https://api.example.com"
  :retry-count 3
  :rate-limit 10))

;; Make requests
(ghc-get client "/packages/helm")
(ghc-download client "/files/package.tar" "/tmp/package.tar")
```

### Version Constraints
```elisp
;; Create version engine
(setq engine (vce-create-engine :name "npm"))

;; Check version satisfaction
(vce-satisfies-p engine "1.2.5" "^1.2.0")  ; => t
(vce-find-best-match engine '("1.1.0" "1.2.0" "2.0.0") "^1.0.0")  ; => "1.2.0"
```

### Configuration Management
```elisp
;; Create config loader supporting multiple formats
(setq loader (gcf-create-loader
  :supported-formats '(("toml" . gcf-parse-toml)
                       ("json" . gcf-parse-json))))

;; Load and validate configuration
(setq config (gcf-load loader "config.toml"))
(gcf-validate loader config)
```

### Multi-Source Search
```elisp
;; Create search engine
(setq engine (gse-create-engine :name "package-search"))

;; Add multiple sources
(gse-add-source engine npm-source)
(gse-add-source engine pypi-source)

;; Search across all sources
(gse-search engine "express")
(gse-search-async engine "django" callback)
```

## 🔌 Plugin System

### Creating Plugins
```elisp
(ps-define-plugin my-plugin
  "Add custom functionality to the package manager."
  
  ;; Register new source type
  (ps-register-hook manager 'source-types
    (make-ps-hook :name 'custom-source))
  
  ;; Add search adapter
  (gse-add-source search-engine custom-search-source))
```

### Plugin Manager
```elisp
;; Create plugin manager
(setq plugin-mgr (ps-create-manager :name "my-pm-plugins"))

;; Discover and load plugins
(ps-add-plugin-directory plugin-mgr "~/.my-pm/plugins/")
(ps-refresh-plugins plugin-mgr)
(ps-load-plugin plugin-mgr "git-plugin")
```

## 🔧 Framework Specialization

### For Different Package Ecosystems

#### Python/pip
```elisp
(defun create-python-package-manager ()
  (pmf-create-package-manager "python"
    :version-engine (vce-create-engine :name "pep440")
    :search-engine (create-pypi-search-engine)
    :installation-backend (create-pip-backend)))
```

#### Node.js/npm  
```elisp
(defun create-nodejs-package-manager ()
  (pmf-create-package-manager "nodejs"
    :version-engine (vce-create-engine :name "semver")
    :search-engine (create-npm-search-engine)
    :installation-backend (create-npm-backend)))
```

#### System Packages
```elisp
(defun create-system-package-manager ()
  (pmf-create-package-manager "system"
    :version-engine (vce-create-engine :name "distro")
    :search-engine (create-apt-search-engine)
    :installation-backend (create-apt-backend)))
```

## 📊 Benefits of Framework Approach

### 1. **Code Reuse**
- Network operations reused across all package managers
- Version constraint logic shared between ecosystems  
- Search algorithms work with any data source
- Configuration management handles multiple formats

### 2. **Consistency**
- Uniform API across different package managers
- Consistent error handling and logging
- Standardized plugin interface
- Common configuration patterns

### 3. **Extensibility** 
- Easy to add new version schemes
- Pluggable search backends
- Custom installation methods
- Flexible configuration formats

### 4. **Testing**
- Framework components tested in isolation
- Mock implementations for integration tests
- Consistent test patterns across specializations
- Better coverage through focused tests

### 5. **Maintenance**
- Bug fixes benefit all implementations
- Performance improvements apply broadly
- Security updates centralized
- Documentation reused across projects

## 🎯 Use Cases

### Package Managers
- **Language-specific**: npm, pip, cargo, gem, etc.
- **System-level**: apt, yum, pacman, homebrew
- **Application-specific**: Emacs packages, VS Code extensions

### Content Management
- **Document repositories**: Wiki systems, documentation sites
- **Media libraries**: Photo/video management systems  
- **Code repositories**: Git hosting platforms

### API Aggregation
- **Service discovery**: Microservice registries
- **API gateways**: Request routing and aggregation
- **Data federation**: Multiple database queries

### File Management
- **Backup systems**: Multi-destination backup tools
- **Sync services**: File synchronization across sources
- **Archive tools**: Multi-format archive management

## 📈 Performance Characteristics

### Network Operations
- **Async by default**: Non-blocking HTTP operations
- **Smart retry**: Exponential backoff with jitter
- **Rate limiting**: Configurable requests per second
- **Connection pooling**: Reuse HTTP connections

### Caching
- **Multi-level**: Memory + disk caching
- **TTL-based**: Time-based expiration
- **LRU eviction**: Least recently used cleanup
- **Compression**: Optional result compression

### Search Performance
- **Parallel queries**: Concurrent source searching
- **Result streaming**: Progressive result delivery
- **Index caching**: Cached search metadata
- **Relevance ranking**: Optimized scoring algorithms

## 🔒 Security Features

### Plugin Sandboxing
- **Function allowlists**: Restrict available functions
- **Resource limits**: CPU, memory, file constraints
- **Permission system**: Granular capability control
- **Signature verification**: Trusted plugin authors

### Network Security
- **SSL verification**: Certificate validation
- **Proxy support**: Corporate proxy compatibility
- **Request validation**: Input sanitization
- **Rate limiting**: DoS protection

### Configuration Security
- **Schema validation**: Prevent malicious configs
- **Environment isolation**: Separate dev/prod settings
- **Secret management**: Encrypted credential storage
- **Access controls**: Permission-based config access

## 📝 Migration Guide

### From BFEPM to Framework
1. **Backup existing configuration**
2. **Install framework libraries** 
3. **Run migration utility**: `bfepm-migrate-to-framework`
4. **Test compatibility**: Verify existing functionality
5. **Enable new features**: Leverage framework capabilities

### From Other Package Managers
1. **Analyze existing architecture**
2. **Map components to framework interfaces**
3. **Implement specialized backends**
4. **Create configuration migration tools**
5. **Develop compatibility layers**

## 📖 API Documentation

### Framework Interfaces
- **Package Manager**: `pmf-create-package-manager`, `pmf-lifecycle-hook`
- **Version Engine**: `vce-compare-versions`, `vce-satisfies-p`
- **HTTP Client**: `ghc-get`, `ghc-post`, `ghc-download`
- **Search Engine**: `gse-search`, `gse-search-async`
- **Config Framework**: `gcf-load`, `gcf-validate`
- **Plugin System**: `ps-load-plugin`, `ps-register-hook`

### Error Handling
- **Standard exceptions**: Consistent error types across libraries
- **Error recovery**: Automatic retry and fallback mechanisms  
- **Debugging support**: Detailed error messages and logging
- **Graceful degradation**: Functionality reduction rather than failure

## 🤝 Contributing

### Adding New Libraries
1. Follow existing naming conventions (`generic-*`, `*-framework`)
2. Implement standard error handling patterns
3. Include comprehensive test suite
4. Document public API and usage examples
5. Add integration examples

### Extending Existing Libraries
1. Maintain backward compatibility
2. Add new features through optional parameters
3. Update documentation and examples
4. Ensure test coverage for new functionality
5. Follow existing code style

### Framework Integration
1. Study `bfepm-framework-integration.el` example
2. Implement all required interfaces
3. Add specialization-specific optimizations
4. Create migration tools for existing users
5. Document integration patterns

This framework represents a modern approach to building maintainable, extensible package management systems while promoting code reuse and consistency across different domains.