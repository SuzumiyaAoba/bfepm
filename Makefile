EMACS ?= emacs
CASK ?= cask

.PHONY: all test clean install build

all: build

install:
	$(CASK) install

build: install
	$(CASK) build

test: build
	$(CASK) exec buttercup -L .

clean:
	$(CASK) clean-elc
	rm -f *.elc

lint:
	$(CASK) exec $(EMACS) -batch -l package -f package-initialize \
		--eval "(require 'elisp-lint)" \
		--eval "(elisp-lint-files-batch '(\"epm.el\" \"epm-core.el\" \"epm-config.el\" \"epm-package.el\" \"epm-utils.el\"))"

compile:
	$(CASK) exec $(EMACS) -batch -L . -f batch-byte-compile *.el

check: lint test

release: check
	@echo "Ready for release"