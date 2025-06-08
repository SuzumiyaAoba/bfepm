# bfepm - Better Fast Emacs Package Manager

bfepm is a package manager for Emacs that provides simple and reliable package installation.

## Features

- **Declarative Configuration**: Centralized management with TOML configuration files
- **Version Management**: Support for version constraints (exact, compatible, patch-level)
- **Multiple Sources**: Support for MELPA, GNU ELPA, and MELPA Stable
- **Dependency Resolution**: Automatic dependency resolution and installation
- **Simple Installation**: Focus on package installation without configuration management

## Installation

### Dependencies

bfepm requires:

- Emacs 29.1 or later
- toml.el (optional, for full TOML parsing - automatically falls back to minimal parser if not available)

**Note**: bfepm works without any external dependencies. If toml.el is not available, bfepm uses its built-in minimal configuration system.

### Manual Installation

```bash
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm
make install
```

### Nix Installation (Recommended for Development)

If you have [Nix](https://nixos.org/) installed with flakes enabled:

```bash
# Clone and enter development environment
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm
nix develop

# Or run directly
nix run github:SuzumiyaAoba/bfepm#demo
```

See [Nix Setup Guide](docs/nix-setup.md) for detailed instructions.

## Basic Usage

### Initialization

Add the following to your Emacs configuration file (`init.el`):

```elisp
(add-to-list 'load-path "/path/to/bfepm")
(require 'bfepm)
(bfepm-init)
```

### Configuration File

bfepm uses `~/.emacs.d/bfepm.toml` as its configuration file. See `sample/bfepm.toml` for a complete example with 25 popular packages.

```toml
# Example bfepm.toml

[meta]
version = "1.0.0"

[sources]
melpa = { url = "https://melpa.org/packages/", priority = 10 }
gnu = { url = "https://elpa.gnu.org/packages/", priority = 5 }
melpa-stable = { url = "https://stable.melpa.org/packages/", priority = 7 }

[packages]
company = "latest"
vertico = "^20250601"          # Compatible version
consult = "~20250520.1200"     # Patch level
marginalia = "20250515.800"    # Exact version
magit = "latest"
which-key = "latest"
```

## Available Commands

### Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x bfepm-install` | Install a package |
| `M-x bfepm-remove` | Remove a package |
| `M-x bfepm-update` | Update packages (all packages if no argument) |
| `M-x bfepm-list` | List installed packages |
| `M-x bfepm-init` | Initialize bfepm |

### Package Management

#### Installing Packages

```elisp
;; Interactive
M-x bfepm-install RET company RET

;; Programmatically
(bfepm-install "company")
```

#### Removing Packages

```elisp
;; Interactive
M-x bfepm-remove RET company RET

;; Programmatically
(bfepm-remove "company")
```

#### Updating Packages

```elisp
;; Update specific package
M-x bfepm-update RET company RET

;; Update all packages
M-x bfepm-update RET RET
```

#### Listing Installed Packages

```elisp
M-x bfepm-list
```

### Lock File Operations

#### Generate Lock File

```elisp
(bfepm-lock-generate)
```

#### Install from Lock File

```elisp
(bfepm-lock-install)
```

#### Verify Lock File

```elisp
(bfepm-lock-verify)
```

## Configuration File Details

### Package Specifications

#### Basic Specifications

```toml
[packages]
company = "latest"             # Latest version
vertico = "^20250601"          # Compatible version (>=20250601 <20260000)
consult = "~20250520.1200"     # Patch level (>=20250520.1200 <20250521.0000)
marginalia = "20250515.800"    # Exact version
magit = "latest"               # Always get latest
which-key = "latest"           # Popular utility package
```

#### Version Specifications for MELPA Packages

bfepm supports MELPA's date-based versioning system:

```toml
[packages]
# Exact date-time version
company = "20250426.1319"

# Compatible version - any version from this date forward (same year)
vertico = "^20250601"          # >=20250601, <20260000

# Patch level - same day, newer time allowed
consult = "~20250520.1200"     # >=20250520.1200, <20250521.0000

# Latest available
orderless = "latest"
```

### Package Sources

```toml
[sources]
melpa = { 
  url = "https://melpa.org/packages/", 
  type = "elpa", 
  priority = 10 
}
gnu = { 
  url = "https://elpa.gnu.org/packages/", 
  type = "elpa", 
  priority = 5 
}
my-repo = { 
  url = "https://github.com/user/repo.git", 
  type = "git", 
  priority = 15 
}
```

### Global Settings

```toml
[global]
auto-update = false         # Enable/disable auto-update
parallel-downloads = 4      # Number of parallel downloads
startup-timeout = 30        # Startup timeout (seconds)
backup-before-update = true # Backup before updates
```

## File Structure

bfepm uses the following directory structure:

```
~/.emacs.d/
├── bfepm.toml            # Main configuration file
├── bfepm.lock            # Lock file
└── bfepm/
    ├── packages/         # Installed packages
    │   ├── company/
    │   ├── magit/
    │   └── ...
    ├── cache/            # Cache directory
    │   ├── metadata/     # Package metadata
    │   ├── downloads/    # Downloaded files
    │   └── indices/      # Repository indices
    └── logs/             # Log files
        ├── install.log
        ├── update.log
        └── error.log
```

**Sample Files**: The repository includes `sample/bfepm.toml` with 25 popular packages and `sample/demo-init.el` for testing.

## Quick Demo

### Using Nix (Easiest)

```bash
# Interactive demo with sample configurations
nix run github:SuzumiyaAoba/bfepm#demo

# Or locally
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm
nix run .#demo
```

### Manual Demo

```bash
# Clone and test
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm

# Run sample test script
./sample/test-script.sh

# Or start Emacs with demo configuration
emacs -Q -L . -l sample/demo-init.el
# Then use: C-c e h (help), C-c e c (show config), C-c e M (mock install), C-c e l (list packages)
# Demo uses sample/bfepm.toml with 25 popular packages
```

## Development

### Build and Test

#### Using Make

```bash
# Install dependencies
make install

# Build
make build

# Run tests
make test

# Lint
make lint

# Full check
make check
```

#### Using Nix (Recommended)

```bash
# Enter development environment
nix develop

# Run all checks
nix flake check

# Build package
nix build

# Run demo
nix run .#demo

# Run tests
nix run .#test
```

### Dependency Management

bfepm uses Cask for managing development dependencies:

```bash
# Install dependencies with Cask
cask install

# Run tests
cask exec buttercup -L .
```

## Version Specifications

bfepm supports the following version specification formats:

| Format | Description | Example |
|--------|-------------|---------|
| `latest` | Latest version | `"latest"` |
| `x.y.z` | Exact match | `"1.2.3"` |
| `^x.y.z` | Compatible version | `"^1.2.3"` (>=1.2.3 <2.0.0) |
| `~x.y.z` | Patch level | `"~1.2.3"` (>=1.2.3 <1.3.0) |

## Lock Files

The lock file (`bfepm.lock`) records the exact versions and checksums of installed packages:

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
```

## Troubleshooting

### Common Issues

#### TOML Configuration Issues

If you see warnings about "bfepm-config module not loaded":

1. **Full TOML Support**: Install `toml.el` package for complete TOML parsing
2. **Minimal Mode**: EPM automatically falls back to minimal configuration parser
3. **Check Status**: Use `M-x bfepm-demo-show-config` in demo mode to verify configuration loading

```elisp
;; Check which configuration module is loaded
(featurep 'bfepm-config)         ; Full TOML support
(featurep 'bfepm-config-minimal) ; Minimal parser (fallback)
```

#### Package Installation Fails

1. Check network connectivity
2. Verify package source URLs are correct
3. Check log files (`~/.emacs.d/bfepm/logs/error.log`)
4. Test with demo mode: `emacs -Q -L . -l sample/demo-init.el`

#### Configuration File Issues

```elisp
;; Check configuration status
(bfepm-demo-show-config)

;; Verify configuration file exists
(file-exists-p bfepm-config-file)
```

#### Dependency Conflicts

```elisp
;; Check dependencies
(bfepm-package-info "package-name")
```

#### Demo Mode for Testing

```bash
# Safe testing environment with sample configuration
emacs -Q -L . -l sample/demo-init.el

# Key commands in demo:
# C-c e h  - Show help
# C-c e M  - Mock install packages (safe)
# C-c e c  - Show configuration status
# C-c e l  - List installed packages
```

## Current Limitations

The current version (v0.1.0) has the following limitations:

- Profile functionality is not yet implemented
- Asynchronous downloads are partially implemented
- Package search functionality is not implemented
- CLI tools are not implemented

## API Reference

### Core Functions

#### `bfepm-install (package-spec)`
Install a package specified by PACKAGE-SPEC.

#### `bfepm-remove (package-name)`
Remove the specified package.

#### `bfepm-update (&optional package-name)`
Update a specific package or all packages if no argument is provided.

#### `bfepm-list ()`
List all installed packages.

#### `bfepm-init ()`
Initialize bfepm in the current Emacs session.

### Configuration Functions

#### `bfepm-config-load (file)`
Load bfepm configuration from a TOML file.

#### `bfepm-config-validate (config)`
Validate bfepm configuration structure.

### Lock File Functions

#### `bfepm-lock-generate ()`
Generate a lock file from currently installed packages.

#### `bfepm-lock-verify ()`
Verify that installed packages match the lock file.

#### `bfepm-lock-install ()`
Install packages from the lock file.

### Utility Functions

#### `bfepm-utils-version-compare (v1 v2)`
Compare two version strings.

#### `bfepm-utils-version-satisfies-p (version requirement)`
Check if a version satisfies a requirement specification.

## Contributing

Bug reports and feature requests are welcome via GitHub Issues.

### Development Setup

1. Clone the repository
2. Install development dependencies: `make install`
3. Run tests: `make test`
4. Follow the coding conventions in existing files

### Coding Guidelines

- Use the `bfepm-` prefix for all public functions
- Include comprehensive error handling with `bfepm-utils-error`
- Add docstrings to all public functions
- Write tests for new functionality using buttercup
- Follow existing naming conventions

## License

[License information to be added]

## Related Links

- [Design Document](CONCEPT.md)
- [Architecture](ARCHITECTURE.md)
- [Implementation Plan](IMPLEMENTATION_PLAN.md)