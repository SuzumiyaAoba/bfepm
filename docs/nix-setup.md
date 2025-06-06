# Nix Development Environment for bfepm

This document explains how to set up and use the Nix-based development environment for bfepm.

## Prerequisites

### Install Nix

1. **Install Nix** (if not already installed):
   ```bash
   sh <(curl -L https://nixos.org/nix/install) --daemon
   ```

2. **Enable Flakes** (add to `~/.config/nix/nix.conf` or `/etc/nix/nix.conf`):
   ```
   experimental-features = nix-command flakes
   ```

3. **Optional: Install direnv** for automatic environment loading:
   ```bash
   # On macOS with Homebrew
   brew install direnv
   
   # On NixOS
   nix-env -iA nixpkgs.direnv
   ```

## Quick Start

### Option 1: Using Nix Flakes (Recommended)

```bash
# Clone the repository
git clone https://github.com/SuzumiyaAoba/bfepm.git
cd bfepm

# Enter development environment
nix develop

# Or run specific commands
nix run .#demo    # Run demo environment
nix run .#test    # Run tests
```

### Option 2: Using direnv (Automatic)

```bash
# In the project directory
direnv allow

# Environment will be automatically loaded when entering the directory
```

### Option 3: Legacy nix-shell

```bash
# For systems without flakes support
nix-shell
```

## Available Development Environments

### Default Environment
The main development environment with all tools and dependencies:

```bash
nix develop
# or
nix develop .#default
```

**Includes:**
- Emacs with required packages (toml.el, async.el, buttercup)
- GNU Make
- Development scripts
- Git

### Minimal Environment
Lightweight environment with just Emacs and Make:

```bash
nix develop .#minimal
```

**Use case:** Quick testing or when you want to manage dependencies manually.

### Testing Environment
Focused on running tests:

```bash
nix develop .#testing
```

**Includes:**
- Emacs with testing dependencies
- Test runner scripts

## Available Commands

Once in the development environment, you can use:

### Development Scripts

```bash
bfepm-dev   # Show development information and available commands
bfepm-test  # Run the test suite
bfepm-demo  # Start interactive demo environment
```

### Standard Make Commands

```bash
make install    # Install dependencies (when using Cask)
make build      # Build project
make test       # Run tests
make lint       # Run linting
make check      # Run all checks
```

### Direct Emacs Commands

```bash
# Run bfepm demo with sample configuration
emacs -Q -L . -l sample/demo-init.el

# Run bfepm directly
emacs -Q -L . -l bfepm.el

# Run tests directly
emacs -batch -L . -L test -l buttercup -f buttercup-run-discover test/

# Byte-compile files
emacs -batch -L . -f batch-byte-compile *.el
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: cachix/install-nix-action@v20
        with:
          extra_nix_config: |
            experimental-features = nix-command flakes
      - name: Run tests
        run: nix flake check
      - name: Build package
        run: nix build
```

### Local Testing Matrix

Test against multiple Emacs versions:

```bash
nix-build nix/ci.nix -A testMatrix
```

## Package Building

### Build the EPM package

```bash
# Build default package
nix build

# Build and run
nix run

# Install to user profile
nix profile install
```

### Package outputs

The built package includes:
- Compiled Emacs Lisp files
- Wrapper script `bfepm-emacs` for easy execution
- All required dependencies

## Advanced Usage

### Custom Emacs Configuration

You can customize the Emacs environment by modifying the `flake.nix`:

```nix
emacsWithPackages = pkgs.emacsWithPackages (epkgs: with epkgs; [
  # Required dependencies
  toml
  async
  
  # Add your additional packages here
  company
  magit
  # ... other packages
]);
```

### Development with Different Emacs Versions

```bash
# Override Emacs version
nix develop --override-input nixpkgs github:NixOS/nixpkgs/emacs-28

# Or specify in flake.nix for permanent change
```

### Cross-platform Development

The Nix flake supports multiple systems:
- x86_64-linux
- aarch64-linux  
- x86_64-darwin
- aarch64-darwin

## Troubleshooting

### Common Issues

#### Flakes not enabled
```
error: experimental Nix feature 'flakes' is disabled
```
**Solution:** Enable flakes in your Nix configuration.

#### Missing dependencies
```
error: attribute 'toml' missing
```
**Solution:** Update your nixpkgs channel or use the pinned version in flake.lock.

#### Permission denied on macOS
```
error: refusing to create Nix store
```
**Solution:** Install Nix with `--daemon` flag or check macOS security settings.

### Debug Commands

```bash
# Check flake status
nix flake show

# Verify package builds
nix build --dry-run

# Check development environment
nix develop --command env

# Update flake inputs
nix flake update
```

## Integration with Editor/IDE

### VS Code
Install the Nix IDE extension and use direnv for automatic environment loading.

### Emacs
The development environment provides a properly configured Emacs with all dependencies.

### Vim/Neovim
Use direnv for automatic environment activation when entering the project directory.

## Performance Tips

1. **Use binary cache:** Nix will download pre-built packages when available
2. **Enable flakes:** Faster evaluation and better caching
3. **Use direnv:** Automatic environment loading reduces manual setup
4. **Garbage collection:** Run `nix-collect-garbage` periodically to clean up old builds

## Security Considerations

- The flake pins exact versions of all dependencies for reproducibility
- All inputs are cryptographically verified
- No network access required during builds (after initial download)
- Isolated build environment prevents interference with system packages