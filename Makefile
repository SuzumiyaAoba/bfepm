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

# Test with coverage (requires undercover)
test-coverage:
	rm -f lisp/*.elc coverage-final.json
	$(EMACS) -batch -L lisp -L test \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'undercover) (package-refresh-contents) (package-install 'undercover))" \
		--eval "(require 'undercover)" \
		--eval "(undercover \"lisp/bfepm.el\" \"lisp/bfepm-core.el\" \"lisp/bfepm-config.el\" \"lisp/bfepm-config-minimal.el\" \"lisp/bfepm-package.el\" \"lisp/bfepm-utils.el\" \"lisp/bfepm-lock.el\" (:exclude \"test/*.el\") (:report-format 'codecov) (:report-file \"coverage-final.json\") (:send-report nil))" \
		--eval "(require 'ert)" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-f ert-run-tests-batch-and-exit

# Release preparation
release: check
	@echo "✅ All checks passed - ready for release"
	@echo "📦 Run 'git tag v0.1.0 && git push origin v0.1.0' to create release"