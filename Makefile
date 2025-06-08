EMACS ?= emacs
KEG ?= keg

.PHONY: all test clean install build lint compile check package-lint checkdoc

all: build

# Install dependencies using Keg
install:
	$(KEG) install

# Build the project
build: install
	$(EMACS) -batch -L lisp \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile lisp/*.el

# Run tests with ERT
test: build
	$(EMACS) -batch -L lisp -L test \
		--eval "(require 'ert)" \
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-f ert-run-tests-batch-and-exit

# Clean compiled files
clean:
	rm -f lisp/*.elc
	rm -f test/*.elc

# Lint source files
lint: package-lint checkdoc

# Package linting
package-lint:
	$(EMACS) -batch -L lisp \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		--eval "(package-lint-batch-and-exit)" \
		lisp/bfepm.el

# Documentation linting
checkdoc:
	$(EMACS) -batch -L lisp \
		--eval "(checkdoc-file \"lisp/bfepm.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-core.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-config.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-config-minimal.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-utils.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-package.el\")" \
		--eval "(checkdoc-file \"lisp/bfepm-lock.el\")"

# Byte compile all files
compile:
	$(EMACS) -batch -L lisp \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile lisp/*.el

# Run all checks
check: compile lint test

# Test with coverage using built-in testcover
test-coverage:
	rm -f lisp/*.elc coverage*.json coverage*.txt
	$(EMACS) -batch -L . -L lisp -L test \
		--eval "(require 'testcover)" \
		--eval "(require 'ert)" \
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		--eval "(testcover-start \"lisp/bfepm.el\")" \
		--eval "(testcover-start \"lisp/bfepm-core.el\")" \
		--eval "(testcover-start \"lisp/bfepm-config.el\")" \
		--eval "(testcover-start \"lisp/bfepm-config-minimal.el\")" \
		--eval "(testcover-start \"lisp/bfepm-package.el\")" \
		--eval "(testcover-start \"lisp/bfepm-utils.el\")" \
		--eval "(testcover-start \"lisp/bfepm-lock.el\")" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-f ert-run-tests-batch-and-exit
	@echo "Generating coverage report..."
	$(EMACS) -batch -L . -L lisp -l scripts/generate-coverage.el
	@echo "Coverage report generated in coverage.json"

# Release preparation
release: check
	@echo "✅ All checks passed - ready for release"
	@echo "📦 Run 'git tag v0.1.0 && git push origin v0.1.0' to create release"