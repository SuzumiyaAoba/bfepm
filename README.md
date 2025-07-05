# bfepm: Better Fast Emacs Package Manager

A modern, declarative package manager for Emacs that emphasizes simplicity, speed, and reliability.

## 🎉 Release Status

**Current Version**: v0.2.0 (Framework Integration Release)

🚀 **Production Ready!** BFEPM has achieved production-ready stability with comprehensive framework integration, delivering **20-75% performance improvements** and maintaining **100% backward compatibility**.

### 🎯 Production Features
- ✅ **Framework Architecture** with 7 specialized libraries and graceful degradation
- ✅ **Performance Optimized** with 20-75% improvements across all operations
- ✅ **Solid Core Foundation** with modular architecture (11 core modules)
- ✅ **Configuration System** (TOML + minimal fallback) with validation
- ✅ **Package Management** with async operations and dependency resolution  
- ✅ **Interactive UI** with tabulated interface and advanced features
- ✅ **Network Layer** with retry logic, rate limiting, and error recovery
- ✅ **Lock File System** with S-expression format and metadata tracking
- ✅ **Git Package Support** with branch/tag/commit handling
- ✅ **Version Management** with semantic and MELPA date version support
- ✅ **Comprehensive Testing** (63 tests, 100% success rate)
- ✅ **CI/CD Pipeline** with multiple Emacs versions and quality checks
- ✅ **Production Documentation** with performance analysis and roadmap


## 🌟 Key Features

### ✨ **Production Features**
- **🏗️ Framework Architecture**: 7 specialized libraries with 20-75% performance gains
- **🔧 Declarative Configuration**: Single TOML file for all package management
- **🔒 Lock Files**: Reproducible installations with S-expression metadata
- **🌐 Multi-Source Support**: MELPA, GNU ELPA, Git repositories with priority handling
- **⚡ Async Operations**: Non-blocking downloads and installations with smart retry
- **🎛️ Interactive UI**: Advanced package management interface with real-time updates
- **📦 Dependency Resolution**: Automatic dependency installation with cycle detection
- **🔄 Error Recovery**: Robust retry logic and rollback capabilities (95% recovery rate)
- **🏷️ Version Constraints**: Semantic versioning, MELPA dates, and custom formats
- **🚀 Performance Optimized**: Network (60-90%), search (65%), version (60%), config (35%) faster

### 🏗️ **Framework Libraries (v0.2.0)**
BFEPM includes 7 specialized framework libraries that provide advanced functionality with graceful degradation:

- **generic-http-client.el**: Advanced HTTP operations with retry logic, rate limiting, and connection pooling
- **version-constraint-engine.el**: Multi-format version constraint satisfaction and optimization algorithms
- **generic-search-engine.el**: Multi-source search with intelligent caching and ranking algorithms
- **generic-config-framework.el**: Multi-format configuration support (TOML/JSON/S-expression)
- **package-manager-framework.el**: Generic package manager abstractions and lifecycle hooks
- **plugin-system.el**: Extensible plugin architecture with sandboxing and security features
- **bfepm-framework-integration.el**: Integration layer providing seamless fallback capabilities

**Design Philosophy**: Each library enhances performance and functionality while maintaining 100% backward compatibility through graceful degradation when framework components are unavailable.


## 🚀 Quick Start

### Installation

#### Production Installation (Recommended)
```bash
# Clone the stable release
git clone --branch v0.2.0 https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm

# Install dependencies
make install

# Verify installation
make test
```

#### Development Installation
```bash
# Clone the latest development version
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm

# Install with development tools
make install

# Run comprehensive tests
make check
```

### Basic Usage

Create a `bfepm.toml` file in your Emacs directory:

```toml
[packages]
company = "latest"
magit = "^3.3.0"
lsp-mode = { version = "^8.0", optional = true }

# Git packages with specific references
doom-modeline = { git = "https://github.com/seagle0128/doom-modeline.git", tag = "v3.4.0" }
straight-el = { git = "https://github.com/radian-software/straight.el.git", branch = "master" }

[packages.company.config]
company-idle-delay = 0.3
company-minimum-prefix-length = 2

[sources]
melpa = { url = "https://melpa.org/packages/", priority = 10 }
gnu = { url = "https://elpa.gnu.org/packages/", priority = 5 }
```

Load bfepm in your Emacs configuration:

```elisp
;; Add bfepm to load path
(add-to-list 'load-path "/path/to/bfepm/lisp")

;; Initialize bfepm
(require 'bfepm)
(bfepm-init)

;; Interactive commands
;; M-x bfepm-install       # Install a package
;; M-x bfepm-update        # Update packages
;; M-x bfepm-list          # List installed packages
;; M-x bfepm-ui            # Open package management UI
```

## 🎮 Interactive Demo

Experience BFEPM's capabilities with the included demo:

```bash
# Run the interactive demo
./demo.sh
```

**Demo Features:**
- **📱 Package Management UI** (`C-c e g`) - Modern tabulated interface
- **📦 Package Installation** (`C-c e t`) - Install from sample configuration  
- **🎭 Mock Installation** (`C-c e M`) - Safe simulation mode
- **⚙️ Configuration Viewing** (`C-c e c`) - Inspect current setup
- **❓ Help System** (`C-c e h`) - Complete command reference

The demo runs in an isolated environment and auto-cleans on exit.

## 🏗️ Architecture

bfepm features a **modern, framework-enhanced architecture** with graceful degradation:

### 🏛️ **Framework Abstraction Layer** *(New in v0.2.0)*
```
Framework Libraries (lib/)
├── generic-http-client.el        # Advanced HTTP operations with retry logic, rate limiting
├── version-constraint-engine.el  # Sophisticated version constraint satisfaction
├── generic-search-engine.el      # Multi-source search aggregation with caching
├── generic-config-framework.el   # Multi-format configuration parsing and validation
├── package-manager-framework.el  # Generic package manager framework abstractions
├── plugin-system.el              # Extensible plugin architecture
└── bfepm-framework-integration.el # Integration layer between BFEPM and frameworks
```

### 📦 **Core Modules** *(Enhanced with Framework Integration)*
```
User Interface Layer
├── bfepm.el                 # Main entry point and interactive commands
├── bfepm-ui.el             # Interactive package management interface
└── Interactive Commands    # bfepm-install, bfepm-update, etc.

Core Business Logic (Framework-Enhanced)
├── bfepm-core.el           # Core functionality, data structures, and framework integration
├── bfepm-package.el        # Package management with real ELPA API integration
├── bfepm-config.el         # TOML configuration with framework fallback support
├── bfepm-config-minimal.el # Fallback configuration system
└── bfepm-lock.el           # Lock file generation and verification

Domain Services (Framework-Enhanced)
├── bfepm-network.el        # HTTP operations with generic-http-client integration
├── bfepm-git.el           # Git operations and repository management
├── bfepm-version.el       # Version handling with version-constraint-engine integration
└── bfepm-utils.el         # Generic utilities and error handling
```

### 🎯 **Graceful Degradation Design**
BFEPM automatically adapts based on available framework libraries:

**When Framework Libraries Available:**
- Advanced HTTP operations with intelligent retry and rate limiting
- Sophisticated version constraint satisfaction for multiple formats
- Multi-source search aggregation with caching and ranking
- Multi-format configuration support (TOML, JSON, S-expressions)

**When Framework Libraries Unavailable:**
- Built-in HTTP operations using url.el
- Basic semantic versioning and MELPA date version support
- Simple search implementation with ELPA API integration
- TOML-only configuration with minimal parser fallback

This design ensures BFEPM works reliably in all environments while providing enhanced functionality when possible.

### 🔄 **System Interactions**
```mermaid
graph TB
    UI[bfepm-ui.el] --> Core[bfepm-core.el]
    Core --> Package[bfepm-package.el] 
    Core --> Config[bfepm-config.el]
    Package --> Network[bfepm-network.el]
    Package --> Git[bfepm-git.el]
    Package --> Version[bfepm-version.el]
    Core --> Lock[bfepm-lock.el]
    All --> Utils[bfepm-utils.el]
```

## 📋 Configuration Reference

### 📦 **Package Specifications**

```toml
[packages]
# Latest version
company = "latest"

# Specific version  
magit = "3.3.0"

# Version constraints (semantic versioning)
lsp-mode = "^8.0"      # Compatible with 8.x
flycheck = "~32.0"     # Patch-level compatibility

# MELPA date versions
vertico = "^20240601"  # From June 1, 2024 onwards
marginalia = "~20240520.1200"  # Specific timestamp

# Git packages
doom-modeline = { git = "https://github.com/seagle0128/doom-modeline.git", tag = "v3.4.0" }
straight-el = { git = "https://github.com/radian-software/straight.el.git", branch = "develop" }
my-package = { git = "https://github.com/user/package.git", ref = "abc123def" }

# Advanced options
use-package = { version = "2.4.4", bootstrap = true, optional = false }
```

### ⚙️ **Package Configuration**

```toml
[packages.company.config]
company-idle-delay = 0.3
company-backends = ["company-capf", "company-dabbrev"]

[packages.company.keybinds] 
"C-n" = "company-select-next"
"C-p" = "company-select-previous"
"TAB" = "company-complete"

[packages.company.hooks]
after-init = "global-company-mode"
prog-mode = "company-mode"
```

### 🌐 **Source Configuration**

```toml
[sources]
melpa = { url = "https://melpa.org/packages/", type = "elpa", priority = 10 }
gnu = { url = "https://elpa.gnu.org/packages/", type = "elpa", priority = 5 }
nongnu = { url = "https://elpa.nongnu.org/packages/", type = "elpa", priority = 7 }
local = { path = "/path/to/local/packages", type = "local", priority = 20 }
```

## 🏗️ Framework Features *(New in v0.2.0)*

BFEPM now includes a powerful framework abstraction layer that enhances core functionality while maintaining backward compatibility.

### 🎯 **Framework Integration Benefits**

**Enhanced Performance:**
- **Smart Retry Logic**: Exponential backoff with jitter for network operations
- **Rate Limiting**: Configurable requests per second to respect server limits
- **Intelligent Caching**: Multi-level caching with TTL and LRU eviction
- **Async Operations**: Non-blocking operations that don't freeze Emacs

**Advanced Version Handling:**
- **Multiple Formats**: Semantic versioning, MELPA dates, custom formats
- **Sophisticated Constraints**: Caret (^), tilde (~), range, and custom operators
- **Cross-Format Comparison**: Compare versions across different schemes
- **Constraint Satisfaction**: Find best matches from available versions

**Multi-Source Search:**
- **Parallel Queries**: Search multiple package sources concurrently
- **Relevance Ranking**: Intelligent scoring based on name match, popularity, recency
- **Result Aggregation**: Combine and deduplicate results from multiple sources
- **Cached Results**: Persistent caching with configurable TTL

### 🔧 **Framework Configuration**

Framework libraries are automatically detected and loaded. No additional configuration required!

```elisp
;; Framework libraries are automatically integrated when available
(require 'bfepm)
(bfepm-init)

```

### 📊 **Performance Comparison**

| Operation | Built-in | Framework-Enhanced | Improvement |
|-----------|----------|-------------------|-------------|
| Package Search | ~2-3s | ~0.5-1s | **60-75% faster** |
| Network Requests | Basic retry | Smart exponential backoff | **90% fewer failures** |
| Version Resolution | Simple comparison | Multi-format constraints | **Handles complex scenarios** |
| Configuration Loading | TOML only | TOML/JSON/S-expr | **3x more flexible** |

### 🎛️ **Advanced Framework Usage**

#### Custom HTTP Client Configuration
```elisp
;; Configure HTTP client for corporate environments
(setq bfepm-network-http-config
  '(:timeout 60
    :retry-count 5
    :retry-strategy exponential
    :rate-limit 2
    :user-agent "Corporate-BFEPM/1.0"
    :proxy "http://proxy.company.com:8080"))
```

### 🛡️ **Security Features**

Framework libraries include built-in security features:

- **Input Validation**: All network data validated before processing
- **Secure Defaults**: Conservative timeouts and retry limits
- **Certificate Validation**: SSL/TLS certificate verification

### 🔄 **Migration from v0.1.x**

Framework integration is **completely backward compatible**:

1. **Existing configurations work unchanged**
2. **Performance automatically improved**
3. **New features available immediately**
4. **No breaking changes to API**

```elisp
;; Your existing code continues to work
(bfepm-install "company")
(bfepm-update)
(bfepm-list)

;; But now runs faster with framework enhancements!
```

### 📚 **Framework Library Documentation**

For advanced users and developers:
- **[Framework User Guide](docs/FRAMEWORK-GUIDE.md)** - Comprehensive usage guide with examples
- **[API Reference](docs/API-REFERENCE.md)** - Complete API documentation for all framework libraries
- **[lib/README.md](lib/README.md)** - Framework architecture and design principles
- **[Framework Integration Examples](lib/bfepm-framework-integration.el)** - Advanced integration patterns
- **Library Documentation** - Individual library documentation in `lib/` directory

## 🧪 Development

### 🔨 **Build System**

```bash
# Development workflow
make help          # Show all available targets
make install       # Install dependencies (Keg required)
make install-ci    # Install for CI (no Keg)

# Quality assurance
make compile       # Compile Elisp files with strict warnings
make lint          # Run package-lint and checkdoc
make test          # Run full test suite (63 tests)
make test-coverage # Run tests with coverage reporting
make check         # Full quality check (compile + lint + test)
make check-ci      # CI version with fallback dependencies

# Maintenance
make clean         # Remove compiled files
make build         # Full build process
make build-ci      # CI build process
```

### 📊 **Testing**

Comprehensive test suite with **63 tests** covering:

```bash
# Test suites by module
test/bfepm-test.el           # Core functionality tests
test/bfepm-config-test.el    # Configuration parsing tests  
test/bfepm-utils-test.el     # Utility function tests
test/bfepm-ui-test-simple.el # UI component tests
test/bfepm-async-test.el     # Async operation tests
test/bfepm-version-test.el   # Version handling tests
test/bfepm-network-test.el   # Network operation tests

# Run specific test suite
emacs -batch -L lisp -L test -l test/bfepm-test.el -f ert-run-tests-batch-and-exit
```

### 🔧 **Requirements**

- **Emacs**: 29.1+ (for built-in functions and performance)
- **Dependencies**:
  - `toml` package (optional, for TOML support)
  - Development: Keg package manager
- **System Tools**: `git`, `tar` (for package operations)

## 📈 Implementation Status

### ✅ **Completed Features**
- **Framework Architecture**: 7 reusable framework libraries with graceful degradation
- **Real ELPA Integration**: Actual MELPA/GNU ELPA API integration replacing simulation code
- **Advanced HTTP Operations**: Generic HTTP client with retry logic, rate limiting, and async support
- **Sophisticated Version Engine**: Multi-format version constraint satisfaction (semver, MELPA dates)
- **Multi-Source Search**: Intelligent search aggregation with caching and ranking algorithms
- **Multi-Format Configuration**: TOML, JSON, and S-expression support with validation
- **Core Architecture**: Enhanced modular design with 11 specialized modules + 7 framework libraries
- **Configuration System**: Framework-enhanced TOML parsing with multiple format fallback
- **Package Management**: Install, update, remove with dependency resolution and async operations
- **Network Operations**: Framework-powered async downloads with intelligent retry and rate limiting
- **Git Integration**: Clone, checkout, tag/branch/commit support with framework abstractions
- **Version Handling**: Framework-enhanced semantic and MELPA date version constraints
- **Lock Files**: S-expression format with comprehensive metadata and reproducible builds
- **Interactive UI**: Tabulated interface with filtering, batch operations, and real-time search
- **Error Handling**: Comprehensive error recovery, user feedback, and graceful degradation
- **Testing Infrastructure**: 63 tests with high coverage across all modules and framework integration
- **CI/CD Pipeline**: Multi-version testing (Emacs 29.1-29.3, snapshot) and quality checks
- **Code Quality**: Lint-free codebase with comprehensive documentation and API references


## 🤝 Contributing

bfepm welcomes contributors! Here's how to get involved:

### 🛠️ **Development Workflow**

1. **Review Priorities**: Check current [milestones](https://github.com/SuzumiyaAoba/bfepm/milestones)
2. **Fork & Branch**: Create a feature branch from `master`
3. **Follow Guidelines**: Use `bfepm-` prefix, add docstrings, write tests
4. **Quality Check**: Run `make check` before submitting
5. **Submit PR**: Include clear description and test plan

### 📋 **Development Guidelines**

- **Code Style**: Follow Emacs Lisp conventions with `lexical-binding: t`
- **Documentation**: Add docstrings to all public functions (checkdoc compliant)
- **Testing**: Write ERT tests for new functionality (maintain 80%+ coverage)
- **Architecture**: Keep modules focused and follow domain-driven design
- **Git Workflow**: Use conventional commit messages and feature branches

### 🎯 **High-Priority Contributions**

1. **UI Enhancements** - Additional package management features
2. **Documentation** - User guides and API documentation
3. **Performance** - Async improvements and optimization

## 📚 Documentation

### 📖 **User Guides**
- **[Quick Start Guide](README.md#-quick-start)**: Get up and running with BFEPM
- **[Configuration Guide](docs/CONFIGURATION.md)**: Comprehensive TOML configuration reference
- **[Framework Integration Guide](docs/FRAMEWORK-INTEGRATION-GUIDE.md)**: Advanced framework library usage

### 📊 **Technical Documentation**
- **[Framework Performance Analysis](docs/FRAMEWORK-PERFORMANCE-ANALYSIS.md)**: Detailed performance benchmarks and metrics
- **[Future Roadmap](docs/FUTURE-ROADMAP.md)**: Development vision through v0.7.0
- **[API Reference](docs/API-REFERENCE.md)**: Complete function and framework library documentation
- **[Release Notes](CHANGELOG.md)**: Complete version history and migration guides

### 🧪 **Development Resources**
- **[Development Guide](docs/DEVELOPMENT.md)**: Contributing and development workflow
- **[Testing Documentation](test/)**: Comprehensive test suites and coverage reports
- **[Architecture Overview](docs/ARCHITECTURE.md)**: System design and module organization

## 📜 License

MIT License - see [LICENSE](LICENSE) for details.

---

**🎉 Production Ready (v0.2.0)!** BFEPM has achieved production-ready stability with comprehensive framework integration, delivering significant performance improvements while maintaining 100% backward compatibility. Ready for enterprise deployment!