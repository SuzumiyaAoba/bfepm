# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building and Testing
```bash
# Install dependencies using Keg
make install

# Build the project
make build

# Run tests with ERT
make test

# Run linting (package-lint + checkdoc)
make lint

# Compile Emacs Lisp files
make compile

# Full check (compile + lint + test)
make check

# Test with coverage reporting
make test-coverage
```

### Package Management
```bash
# Install dependencies via Keg
keg install

# Run tests with ERT
emacs -batch -L . -L test \
  --eval "(require 'ert)" \
  -l test/bfepm-test.el \
  -f ert-run-tests-batch-and-exit

# Byte-compile all files
emacs -batch -L . -f batch-byte-compile *.el

# Package linting
emacs -batch -L . \
  --eval "(require 'package-lint)" \
  --eval "(package-lint-batch-and-exit)" \
  bfepm.el
```

## Architecture Overview

bfepm is an Emacs Lisp package manager with a modular, layered architecture:

### Core Components
- **bfepm.el**: Main entry point with interactive commands
- **bfepm-core.el**: Core functionality, data structures, and initialization
- **bfepm-config.el**: TOML configuration file parsing and validation
- **bfepm-package.el**: Package installation, removal, and management
- **bfepm-utils.el**: Utility functions for downloads, version comparison, and file operations
- **bfepm-lock.el**: Lock file generation and verification for reproducible installs

### Data Structures
- `bfepm-package`: Represents a package with name, version, source, dependencies, config, and status
- `bfepm-config`: Configuration structure containing packages, sources, profiles, and global settings
- `bfepm-source`: Package source definition with URL, type, and priority
- `bfepm-lock`: Lock file structure ensuring deterministic package versions

### File Organization
```
~/.emacs.d/
├── bfepm.toml        # Main configuration
├── bfepm.lock        # Version lock file
└── bfepm/
    ├── packages/     # Installed packages
    ├── cache/        # Download and metadata cache
    └── profiles/     # Profile configurations
```

### Configuration Format
bfepm uses TOML for configuration with support for:
- Package specifications with version constraints
- Multiple package sources (MELPA, GNU ELPA, Git repos)
- Profile-based configuration management
- Package-specific configuration and keybindings

### Key Design Principles
1. **Declarative Configuration**: Single TOML file for all package management
2. **Reproducible Builds**: Lock files ensure consistent environments
3. **Lazy Loading**: Packages loaded only when needed
4. **Profile Support**: Different configurations for different use cases

### Testing Strategy
- **ERT**: Emacs standard testing framework for robust test execution
- **Unit Tests**: Test individual functions and data structures with `ert-deftest`
- **Integration Tests**: Test component interactions and workflows
- **Coverage Reporting**: Using undercover.el for test coverage analysis
- **Mock-friendly**: Utilities designed for easy testing and isolation

When working with this codebase:
- Follow existing naming conventions (bfepm- prefix for all functions)
- Use cl-defstruct for data structures  
- Include comprehensive error handling with bfepm-utils-error
- Add docstrings to all public functions (checkdoc compliant)
- Write tests for new functionality using ERT (`ert-deftest`)
- Ensure lexical binding is enabled (`-*- lexical-binding: t -*-`)
- Run `make check` before committing changes
- Maintain test coverage above 80% when possible