#!/bin/bash
# BFEPM Demo Script
set -e

echo "BFEPM Demo Environment"
echo "======================"

# Create temporary directory for demo
DEMO_DIR=$(mktemp -d)
echo "Demo directory: $DEMO_DIR"

# Copy all source files
cp ./*.el "$DEMO_DIR/" 2>/dev/null || true

# Copy sample files
mkdir -p "$DEMO_DIR/sample"
cp ./sample/*.toml "$DEMO_DIR/sample/" 2>/dev/null || true
cp ./sample/demo-init.el "$DEMO_DIR/sample/" 2>/dev/null || true

echo ""
echo "✅ Demo environment created at: $DEMO_DIR"
echo ""
echo "📋 Available sample configurations:"
if [ -f "$DEMO_DIR/sample/bfepm.toml" ]; then
  echo "  - sample/bfepm.toml     (Basic setup)"
fi
echo ""
echo "🚀 Starting BFEPM Interactive Demo..."
echo "======================================"
echo ""
echo "Available commands in Emacs:"
echo "  C-c e c  - Show configuration"
echo "  C-c e i  - Install company package (demo)"
echo "  C-c e l  - List packages"
echo "  C-c e I  - Interactive package install"
echo "  C-c e R  - Remove package"
echo "  C-c e U  - Update packages"
echo ""
echo "💡 Press C-x C-c to exit Emacs and return to shell"
echo ""

# Change to demo directory
cd "$DEMO_DIR"

# Start Emacs in terminal mode with demo configuration
echo "🚀 Starting Emacs with BFEPM demo..."
emacs -nw -Q -L . -l sample/demo-init.el || {
  echo ""
  echo "📋 Emacs exited. Demo completed."
}

# Cleanup
echo ""
echo "🧹 Cleaning up demo environment..."
rm -rf "$DEMO_DIR"
echo "Demo environment cleaned up."