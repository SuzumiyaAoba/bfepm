#!/bin/bash
# EPM Demo Test Script
# This script demonstrates EPM functionality

set -e

echo "EPM Demo Test Script"
echo "==================="

# Check if we're in the right directory
if [ ! -f "epm.el" ]; then
    echo "Error: Please run this script from the EPM root directory"
    exit 1
fi

# Check if Nix is available and recommend it
if command -v nix >/dev/null 2>&1 && [ -f "../flake.nix" ]; then
    echo "✅ Nix detected - will use EPM development environment"
    echo "💡 For the best experience, you can also run: nix run .#demo"
else
    echo "⚠️  Nix not available - using system Emacs"
    echo "📦 Note: Some tests may fail due to missing dependencies (toml.el, async.el)"
    echo "💡 For full functionality, install Nix and run: nix run github:SuzumiyaAoba/epm#demo"
fi
echo ""

# Create a temporary demo directory
DEMO_DIR=$(mktemp -d)
echo "Creating demo environment in: $DEMO_DIR"

# Copy EPM files to demo directory
cp *.el "$DEMO_DIR/"
cp -r sample "$DEMO_DIR/"

# Change to demo directory
cd "$DEMO_DIR"

echo ""
echo "Demo Environment Setup Complete"
echo "==============================="
echo "Location: $DEMO_DIR"
echo ""

# Function to run Emacs commands
run_emacs_command() {
    local cmd="$1"
    local desc="$2"
    echo "Testing: $desc"
    echo "Command: $cmd"
    echo "----------------------------------------"
    
    # Try to use Nix environment if available, otherwise use regular Emacs
    if command -v nix >/dev/null 2>&1 && [ -f "../flake.nix" ]; then
        echo "🔧 Using Nix environment..."
        if (cd .. && nix develop --command emacs -batch -Q -L "$DEMO_DIR" --eval "$cmd") 2>&1; then
            echo "✅ SUCCESS: $desc"
        else
            echo "❌ FAILED: $desc"
        fi
    else
        echo "⚠️  Using system Emacs (dependencies may be missing)..."
        if emacs -batch -Q -L . --eval "$cmd" 2>&1; then
            echo "✅ SUCCESS: $desc"
        else
            echo "❌ FAILED: $desc (This may fail due to missing dependencies like toml.el)"
        fi
    fi
    echo ""
}

# Test 1: Basic utility functions (no dependencies)
echo "TEST 1: Basic Utility Functions"
echo "==============================="
run_emacs_command '
(progn
  (add-to-list '\''load-path ".")
  (require '\''epm-utils)
  (message "EPM utils loaded successfully")
  (message "Version comparison: 1.0.0 vs 1.0.0 = %d" (epm-utils-version-compare "1.0.0" "1.0.0"))
  (message "Version comparison: 1.0.1 vs 1.0.0 = %d" (epm-utils-version-compare "1.0.1" "1.0.0")))' "Basic Utility Functions"

# Test 2: Package structure creation (no config loading)
echo "TEST 2: Package Structure"
echo "========================="
run_emacs_command '
(progn
  (add-to-list '\''load-path ".")
  (require '\''epm-core)
  (let ((pkg (make-epm-package :name "test" :version "1.0.0")))
    (message "Package structure: name=%s version=%s" 
             (epm-package-name pkg) 
             (epm-package-version pkg))))' "Package Structure Creation"

# Test 3: Version satisfies function
echo "TEST 3: Version Satisfaction"
echo "============================"
run_emacs_command '
(progn
  (add-to-list '\''load-path ".")
  (require '\''epm-utils)
  (message "Testing version satisfaction:")
  (message "1.0.0 satisfies latest: %s" (epm-utils-version-satisfies-p "1.0.0" "latest"))
  (message "1.0.0 satisfies 1.0.0: %s" (epm-utils-version-satisfies-p "1.0.0" "1.0.0"))
  (message "1.0.1 satisfies ^1.0.0: %s" (epm-utils-version-satisfies-p "1.0.1" "^1.0.0")))' "Version Satisfaction"

# Test 4: EPM Core Functions (minimal, avoid config loading)
echo "TEST 4: EPM Core Functions"
echo "=========================="
run_emacs_command '
(progn
  (add-to-list '\''load-path ".")
  (require '\''epm-core)
  (message "Testing core functions...")
  ;; Set up basic variables to avoid initialization
  (setq epm-directory (expand-file-name "epm" user-emacs-directory))
  (message "Package installed check (dummy): %s" (epm-core-package-installed-p "non-existent"))
  (message "Core module loaded successfully"))' "EPM Core Functions"

# Test 5: Full EPM Loading (will fail without toml.el but shows the attempt)
echo "TEST 5: Full EPM Loading (Expected to fail without dependencies)"
echo "================================================================="
run_emacs_command '
(progn
  (add-to-list '\''load-path ".")
  (condition-case err
      (progn
        (require '\''epm)
        (message "EPM loaded successfully"))
    (error 
     (message "EPM loading failed as expected: %s" (error-message-string err)))))' "Full EPM Loading"

# Test 5: Directory structure
echo "TEST 5: Directory Structure"
echo "==========================="
echo "Checking EPM directory structure..."

if [ -d "$HOME/.emacs.d/epm" ]; then
    echo "✅ EPM directory exists: $HOME/.emacs.d/epm"
    ls -la "$HOME/.emacs.d/epm" 2>/dev/null || echo "Directory is empty (expected for new installation)"
else
    echo "ℹ️  EPM directory not yet created (will be created on first package installation)"
fi

# Display sample configurations
echo ""
echo "SAMPLE CONFIGURATIONS"
echo "===================="
echo ""

echo "📁 Available sample configurations:"
for config in sample/*.toml; do
    if [ -f "$config" ]; then
        echo "  - $(basename "$config")"
        echo "    $(head -n3 "$config" | tail -n1 | sed 's/^# //')"
    fi
done

echo ""
echo "📖 To test interactively:"
echo "  1. emacs -Q -L . -l sample/demo-init.el"
echo "  2. Use C-c e c to show config"
echo "  3. Use C-c e i to install company"
echo "  4. Use C-c e l to list packages"

echo ""
echo "🧪 Recommended: Full EPM demo with Nix:"
echo "  nix run .#demo (interactive terminal demo)"
echo "  nix run .#test (comprehensive test suite)"

echo ""
echo "📚 Configuration examples:"
echo "  - Minimal setup: sample/minimal.toml"
echo "  - Full features: sample/epm.toml"
echo "  - Development: sample/development.toml"

echo ""
echo "🗂️  Demo files location: $DEMO_DIR"
echo "💡 Run 'rm -rf $DEMO_DIR' to clean up when done"

echo ""
echo "Demo test script completed!"