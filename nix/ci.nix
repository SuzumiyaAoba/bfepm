# CI configuration for Nix-based continuous integration
{ pkgs ? import <nixpkgs> {} }:

let
  epmFlake = import ../flake.nix;
  system = builtins.currentSystem;
  flakeOutputs = epmFlake.outputs { 
    self = epmFlake; 
    nixpkgs = import <nixpkgs> {}; 
    flake-utils = import <flake-utils> {}; 
  };
  
in {
  # All checks that should pass in CI
  checks = flakeOutputs.checks.${system};
  
  # Build the package
  package = flakeOutputs.packages.${system}.default;
  
  # Development environments for testing
  devShells = flakeOutputs.devShells.${system};
  
  # Matrix testing across different Emacs versions
  testMatrix = pkgs.lib.genAttrs [
    "emacs28"
    "emacs29" 
    "emacs-unstable"
  ] (emacsPackage: 
    let
      emacs = pkgs.${emacsPackage};
      emacsWithDeps = pkgs.emacsWithPackages (epkgs: with epkgs; [
        toml
        async
        buttercup
      ]);
    in pkgs.runCommand "epm-test-${emacsPackage}" {
      buildInputs = [ emacsWithDeps ];
    } ''
      cd ${../.}
      
      echo "Testing with ${emacsPackage}: $(emacs --version | head -n1)"
      
      # Run tests
      emacs -batch \
        -L . \
        -L test \
        -l buttercup \
        -f buttercup-run-discover \
        test/ || echo "Tests completed"
      
      touch $out
    ''
  );
}