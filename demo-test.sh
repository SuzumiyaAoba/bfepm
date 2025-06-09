#!/bin/bash
# BFEPM Demo Test Script (Non-interactive)
set -e

echo "BFEPM Demo Test Environment"
echo "============================"

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

# Change to demo directory
cd "$DEMO_DIR"

# Test BFEPM loading in batch mode
echo "🧪 Testing BFEPM loading..."
emacs -batch -Q -L . -L lisp \
  --eval "(progn 
    (message \"Loading BFEPM modules...\")
    (require 'bfepm-utils)
    (message \"✅ bfepm-utils loaded\")
    (require 'bfepm-core)
    (message \"✅ bfepm-core loaded\")
    (require 'bfepm)
    (message \"✅ BFEPM main module loaded\")
    (require 'bfepm-ui)
    (message \"✅ BFEPM UI loaded\")
    (message \"🎉 All BFEPM modules loaded successfully!\"))" || {
  echo "❌ BFEPM loading failed"
  exit 1
}

echo ""
echo "🧪 Testing demo initialization..."
emacs -batch -Q -L . -L lisp \
  -l sample/demo-init.el \
  --eval "(progn
    (message \"Demo initialization complete\")
    (message \"Available packages: %d\" (length bfepm-demo-packages))
    (message \"BFEPM directory: %s\" (if (boundp 'bfepm-directory) bfepm-directory \"Not set\"))
    (bfepm-demo-show-config)
    (bfepm-demo-list-packages)
    (message \"✅ Demo test completed successfully!\"))" || {
  echo "❌ Demo initialization failed"
  exit 1
}

echo ""
echo "🧪 Testing BFEPM UI functionality..."
emacs -batch -Q -L . -L lisp \
  -l sample/demo-init.el \
  --eval "(progn
    (message \"Testing UI mode initialization...\")
    (with-temp-buffer
      (bfepm-ui-mode)
      (message \"✅ UI mode initialized: %s\" major-mode)
      (message \"✅ Tabulated list format: %d columns\" (length tabulated-list-format)))
    (message \"Testing package list update...\")
    (cl-letf (((symbol-function 'bfepm-core-get-installed-packages)
               (lambda () '(\"demo-package-1\" \"demo-package-2\")))
              ((symbol-function 'bfepm-core-get-package-version)
               (lambda (pkg) \"1.0.0\"))
              ((symbol-function 'bfepm-core-package-installed-p)
               (lambda (_) t))
              ((symbol-function 'bfepm-ui--get-package-description)
               (lambda (pkg) (format \"Description for %s\" pkg))))
      (with-temp-buffer
        (bfepm-ui-mode)
        (bfepm-ui-update-package-list)
        (message \"✅ Package list updated: %d entries\" (length tabulated-list-entries))))
    (message \"✅ UI functionality test completed!\"))" || {
  echo "❌ UI functionality test failed"
  exit 1
}

# Cleanup
echo ""
echo "🧹 Cleaning up demo environment..."
rm -rf "$DEMO_DIR"
echo "✅ Demo test completed successfully!"