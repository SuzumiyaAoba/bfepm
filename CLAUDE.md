# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building and Testing
```bash
# Install dependencies
make install

# Build the project
make build

# Run tests
make test

# Run linting
make lint

# Compile Emacs Lisp files
make compile

# Full check (lint + test)
make check
```

### Package Management
```bash
# Install dependencies via Cask
cask install

# Run tests via Cask
cask exec buttercup -L .

# Byte-compile all files
cask exec emacs -batch -L . -f batch-byte-compile *.el
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
- **buttercup**: BDD-style testing framework
- **Unit Tests**: Test individual functions and data structures
- **Integration Tests**: Test component interactions
- **Mock-friendly**: Utilities designed for easy testing

When working with this codebase:
- Follow existing naming conventions (bfepm- prefix for all functions)
- Use cl-defstruct for data structures
- Include comprehensive error handling with bfepm-utils-error
- Add docstrings to all public functions
- Write tests for new functionality using buttercup