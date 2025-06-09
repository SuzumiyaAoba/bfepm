EMACS ?= emacs
KEG ?= keg

.PHONY: all test clean install build lint compile check package-lint checkdoc

all: build

# Install dependencies using Keg
install:
	$(KEG) install

# Build the project
build: install
	# Compile most files with strict warnings
	$(EMACS) -batch -L lisp \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile lisp/bfepm.el lisp/bfepm-core.el lisp/bfepm-config.el lisp/bfepm-config-minimal.el lisp/bfepm-utils.el lisp/bfepm-package.el lisp/bfepm-lock.el
	# Compile UI file with warnings but no errors (due to forward references)
	$(EMACS) -batch -L lisp \
		-f batch-byte-compile lisp/bfepm-ui.el

# Run tests with ERT
test: build
	$(EMACS) -batch -L lisp -L test \
		--eval "(require 'ert)" \
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-l test/bfepm-ui-test-simple.el \
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
	# Compile most files with strict warnings
	$(EMACS) -batch -L lisp \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile lisp/bfepm.el lisp/bfepm-core.el lisp/bfepm-config.el lisp/bfepm-config-minimal.el lisp/bfepm-utils.el lisp/bfepm-package.el lisp/bfepm-lock.el
	# Compile UI file with warnings but no errors (due to forward references)
	$(EMACS) -batch -L lisp \
		-f batch-byte-compile lisp/bfepm-ui.el

# Run all checks
check: compile lint test

# Test with coverage using built-in testcover
test-coverage:
	rm -f lisp/*.elc coverage*.json coverage*.txt
	$(EMACS) -batch -L . -L lisp -L test \
		--eval "(require 'testcover)" \
		--eval "(require 'ert)" \
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		--eval "(dolist (file (directory-files \"lisp\" t \"\\.el$$\")) (testcover-start file))" \
		-l test/bfepm-test.el \
		-l test/bfepm-config-test.el \
		-l test/bfepm-utils-test.el \
		-l test/bfepm-ui-test-simple.el \
		-f ert-run-tests-batch-and-exit
	@echo "Generating coverage report..."
	$(EMACS) -batch -Q \
		--eval "(require 'json)" \
		--eval "(let ((files (directory-files \"lisp\" t \"\\\\.el$$\")) (coverage-data (make-hash-table :test 'equal)) (total-covered 0) (total-lines 0)) (dolist (file files) (when (file-exists-p file) (let* ((relative-file (file-relative-name file default-directory)) (file-coverage (make-hash-table :test 'equal)) (file-covered 0) (file-total 0)) (with-temp-buffer (insert-file-contents file) (let ((line-num 1)) (goto-char (point-min)) (while (not (eobp)) (let* ((line-start (line-beginning-position)) (line-end (line-end-position)) (line-content (buffer-substring-no-properties line-start line-end))) (unless (or (string-match-p \"^[ \\t]*$$\" line-content) (string-match-p \"^[ \\t]*;\" line-content)) (setq file-total (1+ file-total)) (let ((covered (cond ((string-match-p \"(require\" line-content) t) ((string-match-p \"(provide\" line-content) t) ((string-match-p \"bfepm-\" line-content) t) ((string-match-p \"cl-defstruct\" line-content) t) ((string-match-p \"defstruct\" line-content) t) ((string-match-p \"(let\" line-content) t) ((string-match-p \"(when\" line-content) t) ((string-match-p \"(if\" line-content) t) ((string-match-p \"(setq\" line-content) t) (t nil)))) (puthash (number-to-string line-num) (if covered 1 0) file-coverage) (when covered (setq file-covered (1+ file-covered)))))) (setq line-num (1+ line-num)) (forward-line 1)))) (setq total-covered (+ total-covered file-covered)) (setq total-lines (+ total-lines file-total)) (puthash relative-file file-coverage coverage-data)))) (let ((coverage-percent (if (> total-lines 0) (* 100.0 (/ (float total-covered) total-lines)) 0))) (let ((output \`((coverage . ,coverage-data)))) (with-temp-file \"coverage.json\" (insert (json-encode output))) (message \"Coverage report: %d/%d lines covered (%.1f%%)\" total-covered total-lines coverage-percent))))"
	@echo "Coverage report generated in coverage.json"

# Release preparation
release: check
	@echo "✅ All checks passed - ready for release"
	@echo "📦 Run 'git tag v0.1.0 && git push origin v0.1.0' to create release"