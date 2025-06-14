#!/bin/bash
# BFEPM Demo Script
set -e

echo "BFEPM Demo Environment"
echo "======================"

# Create temporary directory for demo
DEMO_DIR=$(mktemp -d)
echo "Demo directory: $DEMO_DIR"

# Copy all source files from lisp directory
mkdir -p "$DEMO_DIR/lisp"
cp ./lisp/*.el "$DEMO_DIR/lisp/" 2>/dev/null || true
# Also copy to root for backward compatibility with demo-init.el
cp ./lisp/*.el "$DEMO_DIR/" 2>/dev/null || true

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
echo "  === Package Management UI ==="
echo "  C-c e g  - Open BFEPM package management UI (NEW!)"
echo "  === Quick Install ==="
echo "  C-c e i  - Install company package (demo)"
echo "  C-c e 1  - Install selected package"
echo "  C-c e t  - Install packages from sample/bfepm.toml"
echo "  C-c e M  - Mock install packages (safe demo)"
echo "  === Management ==="
echo "  C-c e c  - Show configuration"
echo "  C-c e l  - List packages"
echo "  C-c e I  - Interactive package install"
echo "  C-c e R  - Remove package"
echo "  C-c e U  - Update packages"
echo "  C-c e h  - Show all commands"
echo ""
echo "💡 Press C-x C-c to exit Emacs and return to shell"
echo ""

# Change to demo directory
cd "$DEMO_DIR"

# Start Emacs in terminal mode with demo configuration
echo "🚀 Starting Emacs with BFEPM demo..."
if [ -t 0 ]; then
  emacs -nw -Q -L . -L lisp --eval "(setq load-prefer-newer t)" -l sample/demo-init.el || {
    echo ""
    echo "📋 Emacs exited. Demo completed."
  }
else
  echo "⚠️  Warning: Not running in a terminal. Starting Emacs in GUI mode..."
  emacs -Q -L . -L lisp --eval "(setq load-prefer-newer t)" -l sample/demo-init.el || {
    echo ""
    echo "📋 Emacs exited. Demo completed."
  }
fi

# Cleanup
echo ""
echo "🧹 Cleaning up demo environment..."
rm -rf "$DEMO_DIR"
echo "Demo environment cleaned up."