# BFEPM User Interface Guide

## Overview

BFEPM provides an interactive package management interface built with Emacs' tabulated-list-mode. The UI allows you to view, install, remove, and manage packages through an intuitive table-based interface.

## Main UI Commands

### Entry Points

- **`M-x bfepm-ui`** - Main package management interface (shows installed packages)
- **`M-x bfepm-ui-show-available-external`** - Show available packages from configuration
- **`M-x bfepm-ui-show-installed-external`** - Show installed packages

## Interface Layout

The package management UI displays packages in a tabulated format with the following columns:

| Column | Description |
|--------|-------------|
| Package | Package name |
| Version | Version specification or installed version |
| Status | Installation status (Installed/Available/Missing) |
| Description | Package description (when available) |

## Navigation and Controls

### Basic Navigation

| Key | Action |
|-----|--------|
| `↑`/`↓` | Move between packages |
| `RET` | Show detailed package information |
| `q` | Quit the package management window |
| `g` | Refresh package list |
| `?` | Show help |

### Package Management

| Key | Action |
|-----|--------|
| `i` | Install package (at point or prompt for name) |
| `d` | Remove package at point |
| `u` | Update package at point |
| `U` | Update all installed packages |

### View Switching

| Key | Action |
|-----|--------|
| `t` | Toggle between installed/available view |
| `a` | Show available packages from configuration |
| `I` | Show installed packages |

## Package Status Indicators

- **Installed**: Package is properly installed and available
- **Available**: Package is defined in configuration but not yet installed
- **Missing**: Package directory exists but may be corrupted

## Detailed Package Information

Press `RET` on any package to view detailed information including:

- Package name and version
- Installation directory
- Files in package directory
- Package description (when available)
- Load path status

## Configuration Integration

The UI automatically detects packages from:

1. **Demo Environment**: Uses `bfepm-demo-packages` when available
2. **Loaded Configuration**: Extracts from loaded bfepm configuration
3. **Configuration File**: Parses TOML directly when structure not available

### Supported Configuration Sources

- `sample/bfepm.toml` (demo environment)
- `~/.emacs.d/bfepm.toml` (default location)
- Custom configuration file path (via `bfepm-config-file` variable)

## Demo Environment

The demo environment provides additional functionality:

### Demo Key Bindings

| Key | Action |
|-----|--------|
| `C-c e g` | Open package management UI (available packages) |
| `C-c e G` | Direct access to available packages view |
| `C-c e h` | Show demo help |

### Demo Features

- **Mock Installation**: Safe simulation of package installation
- **Sample Packages**: Pre-configured set of popular Emacs packages
- **Temporary Environment**: Uses temporary directories (auto-cleanup)

## Error Handling

The UI includes robust error handling for:

- **Installation Failures**: Displays error messages without crashing
- **Missing Configuration**: Gracefully handles missing configuration files
- **Network Issues**: Handles download failures during installation
- **Permission Problems**: Reports file system permission issues

## Customization

### Configuration Variables

- **`bfepm-ui-buffer-name`**: Name of main UI buffer (default: `*BFEPM Packages*`)
- **`bfepm-ui-available-buffer-name`**: Name of available packages buffer
- **`bfepm-ui-current-view`**: Current view mode (`installed` or `available`)

### Mode Variables

- **`tabulated-list-format`**: Column specifications
- **`tabulated-list-padding`**: Spacing between columns
- **`tabulated-list-sort-key`**: Default sort column

## Integration with Main BFEPM Commands

The UI integrates seamlessly with main BFEPM commands:

```elisp
;; These work from anywhere
(bfepm-install "package-name")
(bfepm-remove "package-name")
(bfepm-update)
(bfepm-list)

;; UI-specific commands
(bfepm-ui)                           ; Main UI
(bfepm-ui-show-available-external)   ; Available packages
```

## Architecture

### UI Components

- **`bfepm-ui-mode`**: Main mode derived from `tabulated-list-mode`
- **Package List Management**: Dynamic package discovery and display
- **View Switching**: Toggle between installed and available packages
- **Action Handlers**: Install, remove, update operations

### Data Flow

1. **Package Discovery**: Scan configuration and installed packages
2. **List Generation**: Create tabulated entries with package information
3. **Display Update**: Refresh table with current data
4. **User Actions**: Handle key presses and execute operations

### External Dependencies

The UI module has minimal dependencies:

- **`tabulated-list`**: Built-in Emacs table interface
- **`bfepm-core`**: Core package management functions
- **`bfepm-package`**: Package installation/removal operations
- **`bfepm-utils`**: Utility functions

## Performance Considerations

- **Lazy Loading**: Package descriptions loaded on demand
- **Efficient Updates**: Only refresh when necessary
- **Memory Management**: Clean up temporary data structures
- **Responsive UI**: Non-blocking operations where possible

## Troubleshooting

### Common Issues

1. **Empty Package List**: Check configuration file exists and is valid
2. **Installation Failures**: Verify network connectivity and permissions
3. **Display Issues**: Try refreshing with `g` key
4. **Mode Conflicts**: Ensure no conflicting key bindings

### Debug Information

Use these commands for debugging:

```elisp
;; Check current configuration
(bfepm-core-get-config)

;; Verify package discovery
(bfepm-ui--get-config-packages)

;; Test package installation
(bfepm-ui-install-package "test-package")
```

## Best Practices

1. **Regular Updates**: Use `U` to update all packages periodically
2. **Configuration Management**: Keep `bfepm.toml` under version control
3. **Backup**: Backup package configurations before major changes
4. **Testing**: Use demo environment for testing new packages

The BFEPM UI provides a modern, intuitive interface for Emacs package management while maintaining the flexibility and power of the underlying system.