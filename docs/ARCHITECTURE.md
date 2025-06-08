# bfepm Architecture Design

## Current Implementation Status (2025-06-08)

### Completed Components
- ✅ **Core Foundation**: Basic data structures and initialization
- ✅ **Configuration Management**: TOML and minimal config loading
- ✅ **Package Management**: Basic install/remove operations
- ✅ **Utilities**: Common functions and error handling
- ✅ **Lock File Support**: Basic lock file generation
- ✅ **Testing Framework**: Comprehensive test suite (31 tests)
- ✅ **Build System**: Makefile, CI/CD with GitHub Actions
- ✅ **Project Structure**: lisp/ directory organization

### Current File Structure
```
bfepm/
├── lisp/                        # Source code (moved from root)
│   ├── bfepm.el                # Main entry point
│   ├── bfepm-core.el           # Core functionality
│   ├── bfepm-config.el         # Full TOML configuration
│   ├── bfepm-config-minimal.el # Fallback configuration
│   ├── bfepm-package.el        # Package management
│   ├── bfepm-utils.el          # Utilities
│   └── bfepm-lock.el           # Lock file support
├── test/                       # Test suite
│   ├── bfepm-test.el
│   ├── bfepm-config-test.el
│   └── bfepm-utils-test.el
├── sample/                     # Sample configurations
├── docs/                       # Documentation
├── Keg                        # Package dependencies
├── Makefile                   # Build system
└── .github/workflows/         # CI/CD
```

## Overall System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     User Interface Layer                    │
├─────────────────┬─────────────────┬─────────────────────────┤
│   Emacs Lisp    │   CLI Tool      │    Web Interface        │
│   Interactive   │   (bfepm command) │    (Optional)           │
│   Functions     │                 │                         │
└─────────────────┴─────────────────┴─────────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────┐
│                       Core Layer                            │
├─────────────────┬─────────────────┬─────────────────────────┤
│  Package API    │  Profile API    │   Configuration API     │
│                 │                 │                         │
│ • install       │ • switch        │ • load                  │
│ • update        │ • create        │ • validate              │
│ • remove        │ • list          │ • merge                 │
│ • list          │                 │                         │
└─────────────────┴─────────────────┴─────────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────┐
│                     Service Layer                           │
├─────────────────┬─────────────────┬─────────────────────────┤
│ Dependency      │ Download        │    Cache               │
│ Resolver        │ Manager         │    Manager             │
│                 │                 │                         │
│ • resolve       │ • fetch         │ • get                   │
│ • validate      │ • verify        │ • set                   │
│ • lock          │ • extract       │ • invalidate            │
└─────────────────┴─────────────────┴─────────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────┐
│                     Storage Layer                           │
├─────────────────┬─────────────────┬─────────────────────────┤
│  File System    │   Registry      │     Lock File           │
│                 │   (Sources)     │                         │
│ • packages/     │ • melpa         │ • bfepm.lock              │
│ • cache/        │ • gnu-elpa      │ • versions              │
│ • profiles/     │ • git repos     │ • checksums             │
└─────────────────┴─────────────────┴─────────────────────────┘
```

## Core Component Design

### 1. Configuration Manager

```elisp
;; bfepm-config.el
(defstruct bfepm-config
  packages          ; Package list
  profiles         ; Profile definitions
  sources          ; Package sources
  global-settings) ; Global settings

(defun bfepm-config-load (file)
  "Load configuration file and return bfepm-config structure")

(defun bfepm-config-validate (config)
  "Validate configuration validity")

(defun bfepm-config-merge (configs)
  "Merge multiple configurations")
```

### 2. Package Manager

```elisp
;; bfepm-package.el
(defstruct bfepm-package
  name             ; Package name
  version          ; Version
  source           ; Source information
  dependencies     ; Dependencies
  config           ; Package configuration
  status)          ; Installation status

(defun bfepm-package-install (package-spec)
  "Install package")

(defun bfepm-package-update (package-name &optional version)
  "Update package")

(defun bfepm-package-remove (package-name)
  "Remove package")

(defun bfepm-package-list (&optional filter)
  "List installed packages")
```

### 3. Dependency Resolver

```elisp
;; bfepm-deps.el
(defstruct bfepm-dependency-graph
  nodes            ; Package nodes
  edges            ; Dependency edges
  resolved)        ; Resolved order

(defun bfepm-deps-resolve (packages)
  "Resolve dependencies and determine installation order")

(defun bfepm-deps-check-conflicts (packages)
  "Check conflicts between packages")

(defun bfepm-deps-generate-lock (resolved-packages)
  "Generate lock file")
```

### 4. Profile Manager

```elisp
;; bfepm-profile.el
(defstruct bfepm-profile
  name             ; Profile name
  includes         ; Inherited profiles
  packages         ; Package list
  config           ; Profile-specific configuration
  active)          ; Active status

(defun bfepm-profile-switch (profile-name)
  "Switch profile")

(defun bfepm-profile-create (name &optional base-profile)
  "Create new profile")

(defun bfepm-profile-list ()
  "List available profiles")
```

### 5. Download Manager

```elisp
;; bfepm-download.el
(defun bfepm-download-package (package source)
  "Download package (asynchronous)")

(defun bfepm-download-verify (package checksum)
  "Verify integrity of downloaded package")

(defun bfepm-download-extract (archive target-dir)
  "Extract archive")
```

### 6. Cache Manager

```elisp
;; bfepm-cache.el
(defun bfepm-cache-get (key)
  "Get value from cache")

(defun bfepm-cache-set (key value &optional ttl)
  "Save value to cache")

(defun bfepm-cache-invalidate (pattern)
  "Invalidate cache")

(defun bfepm-cache-cleanup ()
  "Remove expired cache")
```

## File System Structure

```
~/.emacs.d/
├── bfepm.toml                   # Main configuration file
├── bfepm.lock                   # Lock file
├── bfepm/
│   ├── packages/              # Installed packages
│   │   ├── company/
│   │   ├── magit/
│   │   └── ...
│   ├── cache/                 # Cache directory
│   │   ├── metadata/          # Package metadata
│   │   ├── downloads/         # Downloaded files
│   │   └── indices/           # Repository indices
│   ├── profiles/              # Profile configurations
│   │   ├── base.toml
│   │   ├── development.toml
│   │   └── writing.toml
│   └── logs/                  # Log files
│       ├── install.log
│       ├── update.log
│       └── error.log
```

## Configuration File Specification

### bfepm.toml

```toml
[meta]
version = "1.0.0"
created = "2024-01-01T00:00:00Z"
updated = "2024-01-15T12:30:00Z"

[sources]
melpa = { url = "https://melpa.org/packages/", priority = 10 }
gnu = { url = "https://elpa.gnu.org/packages/", priority = 5 }
melpa-stable = { url = "https://stable.melpa.org/packages/", priority = 7 }

[packages]
company = "latest"
magit = "3.3.0"
lsp-mode = { version = "^8.0", optional = true }
use-package = { version = "2.4.4", bootstrap = true }

[packages.company.config]
company-idle-delay = 0.3
company-minimum-prefix-length = 2
company-backends = ["company-capf", "company-dabbrev"]

[packages.company.keybinds]
"C-n" = "company-select-next"
"C-p" = "company-select-previous"
"TAB" = "company-complete"

[packages.company.hooks]
after-init = "global-company-mode"
prog-mode = "company-mode"

[profiles]
default = ["base"]
development = ["base", "lsp", "debug"]
writing = ["base", "org"]

[profiles.development.packages]
company = "latest"
lsp-mode = "latest"
flycheck = "latest"

[global]
auto-update = false
parallel-downloads = 4
startup-timeout = 30
backup-before-update = true
```

### bfepm.lock

```toml
[meta]
version = "1.0.0"
generated = "2024-01-15T12:30:00Z"
bfepm-version = "0.1.0"

[packages.company]
version = "0.9.13"
source = "melpa"
checksum = "sha256:abc123..."
dependencies = ["cl-lib"]

[packages.company.dependencies.cl-lib]
version = "0.6.1"
source = "gnu"
checksum = "sha256:def456..."

[resolution]
strategy = "conservative"
conflicts = []
warnings = []
```

## Data Flow

### Package Installation Flow

```
bfepm-package-install
    ↓
bfepm-config-validate
    ↓
bfepm-deps-resolve
    ↓
bfepm-download-package (async)
    ↓
bfepm-download-verify
    ↓
bfepm-download-extract
    ↓
bfepm-package-configure
    ↓
bfepm-deps-generate-lock
```

### Profile Switching Flow

```
bfepm-profile-switch
    ↓
bfepm-profile-deactivate-current
    ↓
bfepm-config-merge (new profile)
    ↓
bfepm-package-sync
    ↓
bfepm-profile-activate
```

## Concurrency Strategy

### Asynchronous Downloads

```elisp
(defun bfepm-download-packages-async (packages callback)
  "Download multiple packages in parallel"
  (let ((futures (mapcar #'bfepm-download-package-future packages)))
    (bfepm-async-all futures callback)))
```

### Promise-based API

```elisp
(defun bfepm-package-install-async (package-spec)
  "Asynchronous package installation"
  (bfepm-promise-new
   (lambda (resolve reject)
     (bfepm-async-run
      (lambda ()
        (condition-case err
            (resolve (bfepm-package-install-sync package-spec))
          (error (reject err))))))))
```

## Error Handling

### Error Classification

1. **Configuration Errors**: Configuration file syntax errors, invalid values
2. **Network Errors**: Download failures, timeouts
3. **Dependency Errors**: Dependency conflicts, circular dependencies
4. **Installation Errors**: File system errors, permission issues

### Recovery Strategies

```elisp
(defun bfepm-error-recovery (error-type error-data)
  "Recovery processing based on error type"
  (pcase error-type
    ('network-error (bfepm-retry-with-backoff error-data))
    ('dependency-conflict (bfepm-suggest-resolution error-data))
    ('installation-error (bfepm-rollback-changes error-data))
    (_ (bfepm-log-error error-data))))
```

## Security Considerations

### Package Verification

```elisp
(defun bfepm-verify-package-signature (package signature)
  "Verify package digital signature")

(defun bfepm-check-package-permissions (package)
  "Check permissions required by package")
```

### Sandbox Execution

```elisp
(defun bfepm-sandbox-eval (code)
  "Execute code in restricted environment")
```

## Performance Optimization

### Lazy Loading Strategy

```elisp
(defun bfepm-autoload-package (package trigger)
  "Auto-load package on specified trigger")

(defun bfepm-defer-package-config (package config)
  "Defer application of package configuration")
```

### Cache Strategy

- **Metadata Cache**: Persistence of package information
- **Download Cache**: Reuse of same versions
- **Dependency Cache**: Storage of resolution results

This design achieves a scalable and maintainable package manager.