EMACS ?= emacs
KEG ?= keg

.PHONY: all test clean install build lint compile check package-lint checkdoc

all: build

# Install dependencies using Keg
install:
	$(KEG) install

# Build the project
build: install
	$(KEG) build

# Run tests with ERT
test: build
	$(EMACS) -batch -L . -L test \
		--eval "(require 'ert)" \
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-f ert-run-tests-batch-and-exit

# Clean compiled files
clean:
	rm -f *.elc
	rm -f test/*.elc

# Lint source files
lint: package-lint checkdoc

# Package linting
package-lint:
	$(EMACS) -batch -L . \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		--eval "(package-lint-batch-and-exit)" \
		bfepm.el

# Documentation linting
checkdoc:
	$(EMACS) -batch -L . \
		--eval "(checkdoc-file \"bfepm.el\")" \
		--eval "(checkdoc-file \"bfepm-core.el\")" \
		--eval "(checkdoc-file \"bfepm-config.el\")" \
		--eval "(checkdoc-file \"bfepm-config-minimal.el\")" \
		--eval "(checkdoc-file \"bfepm-utils.el\")" \
		--eval "(checkdoc-file \"bfepm-package.el\")" \
		--eval "(checkdoc-file \"bfepm-lock.el\")"

# Byte compile all files
compile:
	$(EMACS) -batch -L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile *.el

# Run all checks
check: compile lint test

# Test with coverage (requires undercover)
test-coverage:
	$(EMACS) -batch -L . -L test \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'undercover) (package-refresh-contents) (package-install 'undercover))" \
		--eval "(require 'undercover)" \
		--eval "(undercover \"*.el\" (:exclude \"test/*.el\"))" \
		--eval "(require 'ert)" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-f ert-run-tests-batch-and-exit

# Release preparation
release: check
	@echo "✅ All checks passed - ready for release"
	@echo "📦 Run 'git tag v0.1.0 && git push origin v0.1.0' to create release"