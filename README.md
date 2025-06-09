# bfepm: Better Fast Emacs Package Manager

A modern, declarative package manager for Emacs that emphasizes simplicity, speed, and reliability.

## ⚠️ Development Status

**Current Version**: v0.1.0-alpha (Development Phase)

bfepm is currently in active development. Core functionality is implemented and tested, but not yet ready for production use.

### 🎯 Current Status
- ✅ Core foundation with data structures
- ✅ Configuration loading (TOML + minimal fallback)
- ✅ Basic package management operations
- ✅ Interactive package management UI
- ✅ Package installation with retry logic and error recovery
- ✅ Comprehensive test suite (33 tests)
- ✅ CI/CD pipeline
- 🚧 Working on: Dependency resolution and lock file improvements
- 📋 Next: Multi-source support and profile system

See [Implementation Roadmap](docs/IMPLEMENTATION_ROADMAP.md) for detailed progress and plans.

## 🌟 Planned Features

- **Declarative Configuration**: Single TOML file for all package management
- **Lock Files**: Reproducible package installations across environments
- **Multi-Source Support**: MELPA, GNU ELPA, Git repositories, and custom sources
- **Fast Operations**: Optimized for performance with lazy loading
- **Profile System**: Different configurations for different use cases
- **Dependency Resolution**: Automatic conflict detection and resolution

## 🚀 Quick Start

### Installation (Development)

```bash
# Clone the repository
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm

# Install dependencies using Keg
make install

# Run tests to verify setup
make test

# Try the interactive demo
./demo.sh
```

### Basic Usage (Preview)

Create a `bfepm.toml` file in your Emacs directory:

```toml
[packages]
company = "latest"
magit = "^3.3.0"
lsp-mode = { version = "^8.0", optional = true }

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
;; M-x bfepm-install
;; M-x bfepm-update
;; M-x bfepm-list

;; Package Management UI
;; M-x bfepm-ui-show
```

## 🎮 Interactive Demo

Try BFEPM with the included interactive demo:

```bash
# Run the interactive demo
./demo.sh
```

**Demo Features:**
- **Package Management UI** (`C-c e g`) - Interactive tabulated interface
- **Package Installation** (`C-c e t`) - Install packages from sample configuration
- **Mock Installation** (`C-c e M`) - Safe simulation mode for testing
- **Configuration Viewing** (`C-c e c`) - Check current setup
- **Help System** (`C-c e h`) - View all available commands

The demo runs in a temporary environment and automatically cleans up on exit.
```

## 📋 Configuration Format

### Package Specifications

```toml
[packages]
# Latest version
company = "latest"

# Specific version
magit = "3.3.0"

# Version constraint (semver)
lsp-mode = "^8.0"      # Compatible with 8.x
flycheck = "~32.0"     # Compatible with 32.x

# Advanced configuration
use-package = { version = "2.4.4", bootstrap = true, optional = false }
```

### Package Configuration

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

### Profile System (Planned)

```toml
[profiles]
default = ["base"]
development = ["base", "lsp", "debug"]
writing = ["base", "org"]

[profiles.development.packages]
company = "latest"
lsp-mode = "latest"
flycheck = "latest"
```

## 🏗️ Architecture

bfepm follows a modular, layered architecture:

```
User Interface Layer
├── Interactive Commands (bfepm-install, bfepm-update, etc.)
├── CLI Interface (planned)
└── Configuration API

Core Layer
├── Package Management (bfepm-package.el)
├── Configuration Management (bfepm-config.el)
├── Dependency Resolution (planned)
└── Profile Management (planned)

Service Layer
├── Source Management (planned)
├── Download Manager (planned)
├── Cache Manager (planned)
└── Lock File Manager (bfepm-lock.el)

Storage Layer
├── File System Operations
├── Package Registry
└── Lock Files
```

### Current Modules

- **`lisp/bfepm.el`** - Main entry point and interactive commands
- **`lisp/bfepm-core.el`** - Core functionality and data structures
- **`lisp/bfepm-config.el`** - TOML configuration parsing
- **`lisp/bfepm-config-minimal.el`** - Fallback configuration without TOML
- **`lisp/bfepm-package.el`** - Package management operations
- **`lisp/bfepm-utils.el`** - Utility functions and error handling
- **`lisp/bfepm-lock.el`** - Lock file generation and management

## 🧪 Development

### Building and Testing

```bash
# Install dependencies
make install

# Build project
make build

# Run all tests
make test

# Run tests with coverage
make test-coverage

# Run linting
make lint

# Run all checks (compile + lint + test)
make check
```

### Requirements

- **Emacs**: 29.1+ (for built-in functions)
- **Dependencies**: 
  - `toml` package (optional, for TOML support)
  - `async` package (planned, for async operations)
- **Development Tools**: Keg package manager

### Testing

The project includes comprehensive tests using ERT (Emacs Regression Testing):

```bash
# Run specific test suite
emacs -batch -L lisp -L test -l test/bfepm-test.el -f ert-run-tests-batch-and-exit

# Current test coverage: 31 tests across all modules
```

## 📚 Documentation

- [Architecture Design](docs/ARCHITECTURE.md) - System architecture and component design
- [Implementation Roadmap](docs/IMPLEMENTATION_ROADMAP.md) - Current status and future plans
- [Concept Document](docs/CONCEPT.md) - Project vision and goals
- [Legacy Implementation Plan](docs/IMPLEMENTATION_PLAN.md) - Original development plan

## 🤝 Contributing

bfepm is in active development and welcomes contributors!

### How to Contribute

1. **Check the [Implementation Roadmap](docs/IMPLEMENTATION_ROADMAP.md)** for current priorities
2. **Fork the repository** and create a feature branch
3. **Follow the development workflow**:
   ```bash
   git checkout -b feature/your-feature
   # Make changes
   make test  # Ensure tests pass
   make lint  # Check code style
   ```
4. **Add tests** for new functionality
5. **Update documentation** as needed
6. **Submit a pull request** with a clear description

### Development Guidelines

- Follow Emacs Lisp conventions and use `lexical-binding: t`
- Add docstrings to all public functions
- Write tests for new functionality using ERT
- Keep functions focused and modular
- Use the `bfepm-` prefix for all public functions

## 📊 Project Status

### Recent Accomplishments
- ✅ Established solid foundation with comprehensive test suite
- ✅ Implemented modular architecture with clear separation of concerns
- ✅ Set up robust CI/CD pipeline with multiple Emacs versions
- ✅ Created flexible configuration system with TOML support

### Next Milestones
- 🎯 **v0.1.0** (2 weeks): Fix build system, enhance package installation
- 🎯 **v0.2.0** (5 weeks): Multi-source support, dependency resolution
- 🎯 **v1.0.0** (11 weeks): Production-ready release

### Community
- **Issues**: Report bugs and request features on GitHub
- **Discussions**: Join development discussions in GitHub Issues
- **Documentation**: Help improve documentation and examples

## 📜 License

MIT License - see [LICENSE](LICENSE) for details.

---

**Note**: This project is under active development. APIs and configurations may change before the v1.0.0 release. We welcome feedback and contributions!