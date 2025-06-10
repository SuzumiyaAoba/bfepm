# Build System Documentation

## Overview

bfepm uses a Makefile-based build system that provides comprehensive targets for development, testing, and CI/CD operations. The build system is designed to work both locally with Keg and in CI environments without external dependencies.

## Available Targets

Run `make help` to see all available targets:

```bash
$ make help
Available targets:
  all           - Build the project (default)
  install       - Install dependencies using Keg
  install-ci    - Install dependencies for CI (without Keg)
  build         - Compile Emacs Lisp files
  build-ci      - Install CI deps and build
  test          - Run test suite
  test-coverage - Run tests with coverage reporting
  lint          - Run package-lint and checkdoc
  package-lint  - Run package-lint on main file
  checkdoc      - Run checkdoc on all files
  compile       - Byte compile files
  check         - Run compile + lint + test
  check-ci      - Run check using CI dependencies
  clean         - Remove compiled files
  release       - Run all checks for release preparation
  help          - Show this help message
```

## Development Workflow

### Local Development

```bash
# Initial setup
make install          # Install dependencies with Keg
make build           # Compile all files
make test            # Run test suite

# Development cycle
make clean           # Clean compiled files
make check           # Full check (compile + lint + test)
```

### CI/CD Environment

```bash
# CI workflow
make install-ci      # Install dependencies without Keg
make check-ci        # Complete CI checks
make test-coverage   # Generate coverage reports
```

## Target Details

### Installation Targets

- **`install`**: Uses Keg to install dependencies (toml, async packages)
- **`install-ci`**: Installs dependencies directly via package.el for CI environments

### Build Targets

- **`build`**: Compiles all Emacs Lisp files with appropriate warning levels
- **`build-ci`**: Combines dependency installation and build for CI
- **`compile`**: Alias for build target

### Testing Targets

- **`test`**: Runs complete ERT test suite (35 tests)
- **`test-coverage`**: Runs tests with coverage analysis using testcover
- **`check`**: Complete quality check (compile + lint + test)
- **`check-ci`**: CI-specific quality check with dependency installation

### Quality Targets

- **`lint`**: Runs both package-lint and checkdoc
- **`package-lint`**: Lints main package file for MELPA compliance
- **`checkdoc`**: Validates documentation strings

### Utility Targets

- **`clean`**: Removes all compiled .elc files
- **`help`**: Shows available targets with descriptions
- **`release`**: Prepares for release (runs all checks)

## File Structure

The build system expects the following structure:

```
bfepm/
├── Makefile          # Build configuration
├── Keg               # Dependency specification
├── lisp/             # Source files
│   ├── bfepm.el
│   ├── bfepm-core.el
│   ├── bfepm-config.el
│   ├── bfepm-config-minimal.el
│   ├── bfepm-package.el
│   ├── bfepm-utils.el
│   ├── bfepm-lock.el
│   └── bfepm-ui.el
├── test/             # Test files
│   ├── bfepm-test.el
│   ├── bfepm-config-test.el
│   ├── bfepm-utils-test.el
│   └── bfepm-ui-test-simple.el
└── sample/           # Demo files
    ├── bfepm.toml
    └── demo-init.el
```

## Compilation Strategy

The build system uses a two-stage compilation approach:

1. **Strict Compilation**: Most files compiled with `byte-compile-error-on-warn t`
2. **Relaxed UI Compilation**: UI file compiled with warnings but no errors (due to forward references)

This ensures code quality while accommodating the complex dependencies in the UI module.

## CI Integration

The CI pipeline (.github/workflows/test.yml) leverages Makefile targets:

```yaml
# Main test job
- name: Run full test suite
  run: make check-ci

# Coverage job  
- name: Run tests with coverage
  run: make install-ci test-coverage
```

This approach:
- Eliminates duplication between CI and local builds
- Ensures consistency across environments
- Simplifies CI configuration maintenance
- Allows local reproduction of CI failures

## Environment Variables

- **`EMACS`**: Emacs executable (default: `emacs`)
- **`KEG`**: Keg executable (default: `keg`)

## Error Handling

The build system includes robust error handling:

- Compilation warnings are treated as errors for most files
- Dependencies are installed with error tolerance
- Test failures are properly reported
- Coverage generation continues even if some files fail

## Performance Considerations

- Parallel compilation where possible
- Incremental builds (only recompile changed files)
- Efficient dependency resolution
- Minimal CI overhead

## Troubleshooting

### Common Issues

1. **Keg not found**: Install Keg or use `make install-ci`
2. **Compilation warnings**: Fix warnings or check if file should use relaxed compilation
3. **Test failures**: Run `make test` locally to debug
4. **Coverage issues**: Ensure all source files have proper headers

### Debug Commands

```bash
# Verbose compilation
EMACS="emacs --debug-init" make build

# Test specific file
emacs -batch -L lisp -L test -l test/bfepm-test.el -f ert-run-tests-batch-and-exit

# Check dependencies
make install-ci
```

This build system provides a solid foundation for bfepm development while maintaining simplicity and reliability.