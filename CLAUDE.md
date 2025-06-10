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

bfepm is an Emacs Lisp package manager with a modular, layered architecture:

### Core Components
- **bfepm.el**: Main entry point with interactive commands (lisp/bfepm.el)
- **bfepm-core.el**: Core functionality, data structures, and initialization (lisp/bfepm-core.el)
- **bfepm-config.el**: TOML configuration file parsing and validation (lisp/bfepm-config.el)
- **bfepm-config-minimal.el**: Fallback configuration without TOML dependency (lisp/bfepm-config-minimal.el)
- **bfepm-package.el**: Package installation, removal, and management (lisp/bfepm-package.el)
- **bfepm-utils.el**: Utility functions for downloads, version comparison, and file operations (lisp/bfepm-utils.el)
- **bfepm-lock.el**: Lock file generation and verification for reproducible installs (lisp/bfepm-lock.el)
- **bfepm-ui.el**: Interactive tabulated package management interface (lisp/bfepm-ui.el)

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
- **ERT**: Emacs standard testing framework for robust test execution (35 tests)
- **Unit Tests**: Test individual functions and data structures with `ert-deftest`
- **Integration Tests**: Test component interactions and workflows
- **Coverage Reporting**: Using built-in testcover for test coverage analysis
- **Mock-friendly**: Utilities designed for easy testing and isolation
- **CI/CD Integration**: Streamlined pipeline using Makefile targets

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

When working with this codebase:
- Follow existing naming conventions (bfepm- prefix for all functions)
- Use cl-defstruct for data structures  
- Include comprehensive error handling with bfepm-utils-error
- Add docstrings to all public functions (checkdoc compliant)
- Write tests for new functionality using ERT (`ert-deftest`)
- Ensure lexical binding is enabled (`-*- lexical-binding: t -*-`)
- Run `make check` before committing changes
- Maintain test coverage above 80% when possible
- Always follow the Git workflow guidelines above