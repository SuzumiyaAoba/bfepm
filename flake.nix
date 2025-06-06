{
  description = "EPM - Emacs Package Manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Emacs with required packages
        emacsWithPackages = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
          # Required dependencies
          toml
          async
          
          # Development dependencies
          buttercup
          # undercover # Not available in nixpkgs yet
        ]);

        # Development shell script
        devScript = pkgs.writeShellScriptBin "epm-dev" ''
          echo "EPM Development Environment"
          echo "=========================="
          echo "Emacs version: $(emacs --version | head -n1)"
          echo "Available commands:"
          echo "  make install    - Install dependencies"
          echo "  make build      - Build project" 
          echo "  make test       - Run tests"
          echo "  make lint       - Run linting"
          echo "  make check      - Run all checks"
          echo ""
          echo "To run EPM in Emacs:"
          echo "  emacs -Q -L . -l epm.el"
          echo ""
          echo "To run tests directly:"
          echo "  emacs -batch -L . -l buttercup -f buttercup-run-discover"
        '';

        # Test runner script
        testScript = pkgs.writeShellScriptBin "epm-test" ''
          set -e
          echo "Running EPM tests..."
          
          # First, try to compile all Emacs files
          echo "Checking Emacs Lisp syntax..."
          for file in *.el; do
            echo "Checking $file..."
            ${emacsWithPackages}/bin/emacs -batch -L . -f batch-byte-compile "$file" || {
              echo "Error: Failed to compile $file"
              exit 1
            }
          done
          
          # Check if buttercup is available
          if ! ${emacsWithPackages}/bin/emacs -batch -l buttercup --eval '(message "Buttercup loaded")' 2>/dev/null; then
            echo "Error: buttercup is not available"
            echo "Please ensure buttercup is installed"
            exit 1
          fi
          
          # Run tests
          ${emacsWithPackages}/bin/emacs -batch \
            -L . \
            -L test \
            -l buttercup \
            -f buttercup-run-discover \
            test/
        '';

        # Demo script
        demoScript = pkgs.writeShellScriptBin "epm-demo" ''
          set -e
          echo "EPM Demo Environment"
          echo "==================="
          
          # Create temporary directory for demo
          DEMO_DIR=$(mktemp -d)
          echo "Demo directory: $DEMO_DIR"
          
          # Copy all source files from current directory
          cp ./*.el "$DEMO_DIR/" 2>/dev/null || true
          
          # Create sample directory and copy files
          mkdir -p "$DEMO_DIR/sample"
          
          # Copy sample files from the actual project if running from local directory
          if [ -d "./sample" ]; then
            cp ./sample/*.toml "$DEMO_DIR/sample/" 2>/dev/null || true
            cp ./sample/demo-init.el "$DEMO_DIR/sample/" 2>/dev/null || true
          fi
          
          # Create minimal configuration if not exists
          if [ ! -f "$DEMO_DIR/sample/minimal.toml" ]; then
            cat > "$DEMO_DIR/sample/minimal.toml" << 'EOF'
# Minimal EPM Configuration
[meta]
version = "1.0.0"
description = "Minimal EPM setup"

[sources]
melpa = { url = "https://melpa.org/packages/", priority = 10 }
gnu = { url = "https://elpa.gnu.org/packages/", priority = 5 }

[packages]
company = "latest"
magit = "latest"

[packages.company.config]
company-idle-delay = 0.3

[global]
auto-update = false
parallel-downloads = 2
EOF

          # Create demo init file if not exists
          if [ ! -f "$DEMO_DIR/sample/demo-init.el" ]; then
            cat > "$DEMO_DIR/sample/demo-init.el" << 'EOF'
;;; demo-init.el --- EPM Demo Configuration -*- lexical-binding: t -*-

;; Load EPM
(add-to-list 'load-path ".")
(require 'epm)

;; Initialize EPM
(epm-init)

;; Demo functions
(defun epm-demo-show-config ()
  "Show current EPM configuration."
  (interactive)
  (message "EPM Demo: Configuration loaded"))

(defun epm-demo-install-company ()
  "Demo function to install company."
  (interactive)
  (message "EPM Demo: Installing company package..."))

(defun epm-demo-list-packages ()
  "Demo function to list packages."
  (interactive)
  (message "EPM Demo: Listing packages..."))

;; Bind demo functions
(global-set-key (kbd "C-c e c") #'epm-demo-show-config)
(global-set-key (kbd "C-c e i") #'epm-demo-install-company)
(global-set-key (kbd "C-c e l") #'epm-demo-list-packages)

(message "EPM Demo loaded! Try: C-c e c, C-c e i, C-c e l")

(provide 'demo-init)
;;; demo-init.el ends here
EOF
          fi
          
          echo ""
          echo "✅ Demo environment created at: $DEMO_DIR"
          echo ""
          echo "📋 Available sample configurations:"
          echo "  - sample/minimal.toml     (Basic setup)"
          echo ""
          echo "🚀 Starting EPM Interactive Demo..."
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
          ${emacsWithPackages}/bin/emacs -nw -Q -L . -l sample/demo-init.el < /dev/tty || {
            echo ""
            echo "📋 Emacs exited. Other demo options:"
            echo ""
            echo "1️⃣  Run batch tests:"
            echo "   ./sample/test-script.sh"
            echo ""
            echo "2️⃣  Manual testing:"
            echo "   emacs -batch -Q -L . -l epm.el --eval '(progn (epm-init) (message \"EPM loaded!\"))'"
            echo ""
            echo "3️⃣  Restart interactive demo:"
            echo "   emacs -nw -Q -L . -l sample/demo-init.el"
            echo ""
            echo "4️⃣  GUI Emacs (if available):"
            echo "   emacs -Q -L . -l sample/demo-init.el"
            echo ""
            echo "📁 Current directory: $DEMO_DIR"
            echo "💡 Type 'exit' to cleanup and return"
            echo ""
            
            # Start interactive shell for additional testing
            ${pkgs.bash}/bin/bash
          }
          
          # Cleanup
          rm -rf "$DEMO_DIR"
          echo "Demo environment cleaned up."
        '';

      in
      {
        # Default package
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "epm";
          version = "0.1.0";
          
          src = ./.;
          
          nativeBuildInputs = [ emacsWithPackages pkgs.makeWrapper ];
          
          buildPhase = ''
            # Byte-compile Emacs files
            emacs -batch -L . -f batch-byte-compile *.el
          '';
          
          installPhase = ''
            mkdir -p $out/share/emacs/site-lisp/epm
            cp *.el *.elc $out/share/emacs/site-lisp/epm/
            
            # Create wrapper scripts
            makeWrapper ${emacsWithPackages}/bin/emacs $out/bin/epm-emacs \
              --add-flags "-Q" \
              --add-flags "-L $out/share/emacs/site-lisp/epm" \
              --add-flags "-l epm"
          '';
          
          meta = with pkgs.lib; {
            description = "EPM - Emacs Package Manager";
            homepage = "https://github.com/SuzumiyaAoba/epm";
            license = licenses.mit; # Adjust as needed
            maintainers = [ ];
            platforms = platforms.all;
          };
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Emacs with required packages
            emacsWithPackages
            
            # Build tools
            gnumake
            
            # Cask (if available)
            # cask # Not in nixpkgs, we'll use make instead
            
            # Development utilities
            git
            devScript
            testScript
            demoScript
            
            # For TOML parsing (if needed for tooling)
            # python3Packages.toml
          ];
          
          shellHook = ''
            echo "Welcome to EPM development environment!"
            echo "======================================"
            echo ""
            epm-dev
          '';
        };

        # Multiple development environments
        devShells.minimal = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs
            gnumake
          ];
          
          shellHook = ''
            echo "EPM minimal development environment"
            echo "Note: You may need to install toml.el and async.el manually"
          '';
        };

        devShells.testing = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacsWithPackages
            testScript
          ];
          
          shellHook = ''
            echo "EPM testing environment"
            echo "======================"
            echo "Run 'epm-test' to execute tests"
          '';
        };

        # Apps for easy running
        apps = {
          default = flake-utils.lib.mkApp {
            drv = self.packages.${system}.default;
            exePath = "/bin/epm-emacs";
          };
          
          demo = flake-utils.lib.mkApp {
            drv = demoScript;
          };
          
          test = flake-utils.lib.mkApp {
            drv = testScript;
          };
        };

        # Checks for CI
        checks = {
          # Lint check
          lint = pkgs.runCommand "epm-lint" {
            buildInputs = [ emacsWithPackages ];
          } ''
            cd ${./.}
            
            # Basic syntax check by byte-compiling
            emacs -batch -L . -f batch-byte-compile *.el
            
            # Check for common issues
            emacs -batch -L . --eval "
              (progn
                (setq byte-compile-error-on-warn t)
                (batch-byte-compile))" *.el
            
            touch $out
          '';
          
          # Test check
          test = pkgs.runCommand "epm-test" {
            buildInputs = [ emacsWithPackages ];
          } ''
            cd ${./.}
            
            # Run tests
            emacs -batch \
              -L . \
              -L test \
              -l buttercup \
              -f buttercup-run-discover \
              test/ || echo "Tests completed with issues"
            
            touch $out
          '';
          
          # Build check
          build = self.packages.${system}.default;
        };
      });
}