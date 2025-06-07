# bfepm Sample Configurations

This directory contains demonstration files for bfepm (Better Fast Emacs Package Manager).

## Files Overview

### Demo Files

#### `demo-init.el`
An example Emacs initialization file showing:
- How to load and initialize bfepm
- Demo functions for testing bfepm functionality
- Key bindings for interactive commands
- Package list loaded from sample/bfepm.toml

#### `bfepm.toml`
Sample bfepm configuration file containing:
- 25 popular Emacs packages with real version specifications
- MELPA date-based and semantic version examples
- Multiple package sources (MELPA, GNU ELPA, MELPA Stable)

**Note**: This file demonstrates bfepm's configuration format. bfepm automatically detects TOML support:
- With `toml.el`: Full TOML parsing support
- Without `toml.el`: Falls back to minimal built-in parser

#### `test-script.sh`
A shell script for testing bfepm functionality in batch mode.

## Quick Start

### 1. Using Nix (Recommended)

```bash
# Run interactive terminal demo
nix run github:SuzumiyaAoba/bfepm#demo

# Or locally
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm
nix run .#demo
```

### 2. Manual Testing

```bash
# Start Emacs with bfepm demo
emacs -Q -L . -l sample/demo-init.el
```

### 3. Interactive Demo Commands

Once bfepm is loaded in the demo, you can use these key bindings:

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c e h` | `bfepm-demo-help` | Show help and available commands |
| `C-c e c` | `bfepm-demo-show-config` | Show current configuration |
| `C-c e 1` | `bfepm-demo-install-package` | Install selected package |
| `C-c e t` | `bfepm-demo-install-popular-packages` | Install packages from sample/bfepm.toml |
| `C-c e M` | `bfepm-demo-install-popular-mock` | Mock install packages |
| `C-c e P` | `bfepm-demo-show-package-list` | Show available packages |
| `C-c e l` | `bfepm-demo-list-packages` | List installed packages |
| `C-c e s` | `bfepm-demo-install-with-version` | Test version specification |
| `C-c e I` | `bfepm-install` | Install any package (interactive) |
| `C-c e R` | `bfepm-remove` | Remove a package |

## Configuration Examples

See the `sample/bfepm.toml` file for package configuration examples.

### Version Specifications

bfepm supports MELPA's date-based versioning and semantic versioning:

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

Use the demo functions to test bfepm functionality:

1. **Check Status**: `C-c e c` - Verify bfepm configuration and module loading
2. **View Packages**: `C-c e P` - See all 25 packages from sample/bfepm.toml
3. **Mock Install**: `C-c e M` - Safe simulation of package installation
4. **Single Package**: `C-c e 1` - Install a selected package interactively
5. **Multiple Packages**: `C-c e t` - Install package set from sample/bfepm.toml
6. **List Installed**: `C-c e l` - Check what packages are installed
7. **Version Testing**: `C-c e s` - Test version specification handling

### Configuration Status Check

Use `C-c e c` to verify:
- bfepm module loading status
- Configuration file detection
- TOML parsing capability (full vs minimal)
- Directory setup

## Expected Directory Structure

During demo, bfepm uses temporary directories:

```
/tmp/bfepm-demo-xxx/
├── packages/             # Installed packages
│   ├── company/
│   └── vertico/
└── cache/
    └── downloads/        # Downloaded packages
```

For more information, see the main [README.md](../README.md).