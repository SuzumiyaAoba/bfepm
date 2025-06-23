# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building and Testing
```bash
# Show all available targets
make help

# Install dependencies using Keg
make install

# Install dependencies for CI (without Keg)
make install-ci

# Build the project
make build

# Build for CI environment
make build-ci

# Run tests with ERT
make test

# Run linting (package-lint + checkdoc)
make lint

# Compile Emacs Lisp files
make compile

# Full check (compile + lint + test)
make check

# Run checks with CI dependencies
make check-ci

# Test with coverage reporting
make test-coverage

# Clean compiled files
make clean
```

### Running Individual Tests
```bash
# Run a specific test file
emacs -batch -L lisp -L test \
  --eval "(require 'ert)" \
  -l test/bfepm-test.el \
  -f ert-run-tests-batch-and-exit

# Run a specific test by pattern
emacs -batch -L lisp -L test \
  --eval "(require 'ert)" \
  -l test/bfepm-test.el \
  --eval "(ert-run-tests-batch-and-exit \"test-pattern\")"

# Check package dependencies
keg install
```

## Architecture Overview

bfepm is a modern Emacs Lisp package manager with a **domain-driven, modular architecture** consisting of 11 specialized modules:

### 📦 **Core Modules (4 modules)**
- **bfepm.el**: Main entry point with interactive commands and public API
- **bfepm-core.el**: Core functionality, data structures, and system initialization
- **bfepm-config.el**: TOML configuration parsing, validation, and management
- **bfepm-config-minimal.el**: Fallback configuration system without TOML dependencies

### 🔧 **Domain Services (5 modules)**
- **bfepm-package.el**: Package installation, removal, dependency resolution, and lifecycle management
- **bfepm-network.el**: HTTP operations, downloads, retry logic, and rate limiting
- **bfepm-git.el**: Git operations, repository management, and version control
- **bfepm-version.el**: Version comparison, constraint handling, and semantic versioning
- **bfepm-lock.el**: Lock file generation, verification, and reproducible installations

### 🎮 **User Interface (2 modules)**
- **bfepm-ui.el**: Interactive tabulated package management interface with advanced features
- **bfepm-utils.el**: Generic utilities, error handling, and cross-cutting concerns

### 🏗️ **System Architecture**
```
User Interface Layer
├── bfepm.el                 # Interactive commands (bfepm-install, bfepm-update, etc.)
├── bfepm-ui.el             # Advanced package management UI
└── Public API              # User-facing functions

Core Business Logic
├── bfepm-core.el           # Data structures and core functionality  
├── bfepm-package.el        # Package lifecycle management
├── bfepm-config.el         # Configuration system
├── bfepm-config-minimal.el # Fallback configuration
└── bfepm-lock.el           # Reproducible installations

Domain Services  
├── bfepm-network.el        # Network operations and downloads
├── bfepm-git.el           # Git repository management
├── bfepm-version.el       # Version handling and constraints
└── bfepm-utils.el         # Generic utilities and error handling
```

### 📊 **Data Structures**
- `bfepm-package`: Core package representation with name, version, source, dependencies, config, and status
- `bfepm-config`: Configuration structure containing packages, sources, profiles, and global settings
- `bfepm-source`: Package source definition with URL, type, and priority
- `bfepm-lock`: Lock file structure ensuring deterministic package versions

### 📁 **File Organization**
```
~/.emacs.d/
├── bfepm.toml        # Main TOML configuration
├── bfepm.lock        # S-expression lock file
└── bfepm/
    ├── packages/     # Installed packages directory
    ├── cache/        # Download and metadata cache
    └── profiles/     # Profile-based configurations
```

### ⚙️ **Configuration Format**
bfepm uses TOML for human-readable configuration with support for:
- Package specifications with semantic version constraints
- Multiple package sources (MELPA, GNU ELPA, Git repositories)
- Git packages with branch/tag/commit support
- Package-specific configuration and keybindings
- Profile-based configuration management
- Source priority and fallback strategies

### 🎯 **Key Design Principles**
1. **Domain-Driven Design**: Clear separation of concerns across 11 specialized modules
2. **Declarative Configuration**: Single TOML file for all package management
3. **Reproducible Builds**: S-expression lock files ensure consistent environments
4. **Async Operations**: Non-blocking downloads and installations
5. **Error Recovery**: Comprehensive retry logic and rollback capabilities
6. **Modular Architecture**: Loosely coupled modules with clear interfaces

### 🧪 **Testing Strategy**
- **Comprehensive Coverage**: 63 tests across 7 test suites covering all modules
- **ERT Framework**: Emacs standard testing framework for robust test execution
- **Unit Tests**: Test individual functions and data structures with `ert-deftest`
- **Integration Tests**: Test component interactions and workflows
- **Async Testing**: Specialized tests for async operations and callbacks
- **Coverage Reporting**: Using built-in testcover for test coverage analysis
- **CI/CD Integration**: Multi-version testing and quality checks

### 📈 **Current Status**
- **Code Quality**: 11 modules, 63 tests, lint-free codebase
- **Architecture**: Mature domain-driven design with proper separation of concerns
- **Features**: Core functionality complete, approaching production readiness
- **Performance**: Async operations, intelligent caching, retry logic

## Git Workflow Guidelines

### Task Workflow
Before starting any task, follow this workflow:

1. **Check Current Branch Status**
   - Verify current branch is appropriate for the task
   - Switch to master if needed: `git checkout master`
   - Pull latest changes: `git pull origin master`

2. **Create Feature Branch**
   - Use descriptive branch names: `feature/description-of-work`
   - Example: `git checkout -b feature/add-dependency-resolution`

3. **Complete Task and Commit**
   - Make changes and test thoroughly
   - Run `make check` to ensure quality
   - Stage relevant files: `git add <files>`
   - Create descriptive commit with proper format

4. **Push and Create PR**
   - Push branch: `git push -u origin <branch-name>`
   - Create PR with comprehensive description
   - Include test plan and summary of changes

### Commit Message Format
```
type: brief description of changes
(Common types include: `feat` for new features, `fix` for bug fixes, `docs` for documentation changes, `refactor` for code restructuring without functional changes, `style` for formatting, `test` for adding or improving tests, `chore` for build process or auxiliary tool changes)

- Detailed explanation of what was changed
- Why the changes were made
- Impact on the system

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Branch Management
- **Feature branches**: `feature/description`
- **Bug fixes**: `fix/issue-description`
- **Documentation**: `docs/topic`
- **Refactoring**: `refactor/component-name`

### Pre-commit Checklist
- [ ] All tests pass (`make test`)
- [ ] Code compiles without warnings (`make compile`)
- [ ] Linting passes (`make lint`)
- [ ] Documentation updated if needed
- [ ] CLAUDE.md updated for workflow changes

## Development Guidelines

### 🏗️ **Code Architecture Patterns**
- **Domain Separation**: Keep domain-specific functions in appropriate modules
  - Network operations → `bfepm-network.el`
  - Git operations → `bfepm-git.el`
  - Version handling → `bfepm-version.el`
  - Package management → `bfepm-package.el`
  - Generic utilities only → `bfepm-utils.el`
- **Async Callbacks**: Use consistent 3-argument callback pattern: `(success package-name error-msg)`
- **Error Handling**: Comprehensive error recovery with `bfepm-utils-error` and condition-case
- **Module Dependencies**: Maintain clear dependency hierarchy to avoid circular dependencies

### 📝 **Coding Standards**
- Follow existing naming conventions (bfepm- prefix for all functions)
- Use cl-defstruct for data structures with proper field validation
- Include comprehensive error handling with bfepm-utils-error
- Add docstrings to all public functions (checkdoc compliant)
- Write tests for new functionality using ERT (`ert-deftest`)
- Ensure lexical binding is enabled (`-*- lexical-binding: t -*-`)
- Run `make check` before committing changes
- Maintain test coverage above 80% when possible
- Always follow the Git workflow guidelines above

### 🔄 **Async Programming Patterns**
- All async operations use callback pattern with consistent signatures
- Network operations are non-blocking with retry logic
- UI refreshes use timers to avoid blocking the main thread
- Error handling includes proper cleanup for failed async operations

### 🧪 **Testing Best Practices**
- Test both sync and async code paths
- Use temporary files and directories for file system tests
- Mock external dependencies (network, git) when possible
- Include edge cases and error conditions in tests
- Verify cleanup in test teardown (unwind-protect)

### 🏷️ **Version and Dependency Management**
- Support both semantic versioning (^1.2.3) and MELPA date versions (^20240601)
- Handle git packages with branch/tag/commit specifications
- Implement proper dependency resolution with cycle detection
- Use lock files for reproducible builds across environments