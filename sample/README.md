# EPM Sample Configurations

This directory contains demonstration files for EPM (Emacs Package Manager).

## Files Overview

### Demo Files

#### `demo-init.el`
An example Emacs initialization file showing:
- How to load and initialize EPM
- Demo functions for testing EPM functionality
- Key bindings for interactive commands
- Package list loaded from sample/epm.toml

#### `epm.toml`
Sample EPM configuration file containing:
- 25 popular Emacs packages with real version specifications
- MELPA date-based and semantic version examples
- Multiple package sources (MELPA, GNU ELPA, MELPA Stable)

**Note**: This file demonstrates EPM's configuration format. EPM automatically detects TOML support:
- With `toml.el`: Full TOML parsing support
- Without `toml.el`: Falls back to minimal built-in parser

#### `test-script.sh`
A shell script for testing EPM functionality in batch mode.

## Quick Start

### 1. Using Nix (Recommended)

```bash
# Run interactive terminal demo
nix run github:SuzumiyaAoba/epm#demo

# Or locally
git clone https://github.com/SuzumiyaAoba/epm.git
cd epm
nix run .#demo
```

### 2. Manual Testing

```bash
# Start Emacs with EPM demo
emacs -Q -L . -l sample/demo-init.el
```

### 3. Interactive Demo Commands

Once EPM is loaded in the demo, you can use these key bindings:

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c e h` | `epm-demo-help` | Show help and available commands |
| `C-c e c` | `epm-demo-show-config` | Show current configuration |
| `C-c e 1` | `epm-demo-install-package` | Install selected package |
| `C-c e t` | `epm-demo-install-popular-packages` | Install packages from sample/epm.toml |
| `C-c e M` | `epm-demo-install-popular-mock` | Mock install packages |
| `C-c e P` | `epm-demo-show-package-list` | Show available packages |
| `C-c e l` | `epm-demo-list-packages` | List installed packages |
| `C-c e s` | `epm-demo-install-with-version` | Test version specification |
| `C-c e I` | `epm-install` | Install any package (interactive) |
| `C-c e R` | `epm-remove` | Remove a package |

## Configuration Examples

See the `sample/epm.toml` file for package configuration examples.

### Version Specifications

EPM supports MELPA's date-based versioning and semantic versioning:

```toml
[packages]
# MELPA date-based versions
company = "20250426.1319"      # Exact date-time version
vertico = "^20250601"          # Compatible version (>=20250601 <20260000)
consult = "~20250520.1200"     # Patch level (>=20250520.1200 <20250521.0000)

# Semantic versions
magit = "^3.3.0"               # Compatible version (>=3.3.0 <4.0.0)
org = "~9.5.0"                 # Patch level (>=9.5.0 <9.6.0)

# Latest available
which-key = "latest"           # Always get latest
```

## Testing

Use the demo functions to test EPM functionality:

1. **Check Status**: `C-c e c` - Verify EPM configuration and module loading
2. **View Packages**: `C-c e P` - See all 25 packages from sample/epm.toml
3. **Mock Install**: `C-c e M` - Safe simulation of package installation
4. **Single Package**: `C-c e 1` - Install a selected package interactively
5. **Multiple Packages**: `C-c e t` - Install package set from sample/epm.toml
6. **List Installed**: `C-c e l` - Check what packages are installed
7. **Version Testing**: `C-c e s` - Test version specification handling

### Configuration Status Check

Use `C-c e c` to verify:
- EPM module loading status
- Configuration file detection
- TOML parsing capability (full vs minimal)
- Directory setup

## Expected Directory Structure

During demo, EPM uses temporary directories:

```
/tmp/epm-demo-xxx/
├── packages/             # Installed packages
│   ├── company/
│   └── vertico/
└── cache/
    └── downloads/        # Downloaded packages
```

For more information, see the main [README.md](../README.md).