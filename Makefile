SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

INIT="(progn \
(require 'package) \
(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
(package-initialize) \
(package-refresh-contents))"

LINT="(progn \
(unless (package-installed-p 'package-lint) \
(package-install 'package-lint)) \
(require 'package-lint) \
(package-lint-batch-and-exit))"

DOCSTR-FILES := $(wildcard ./docstr-*.el) \
				$(wildcard langs/*.el)

TEST-FILES := test/bootstrap.el $(shell ls test/docstr-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
	@$(CASK) install
	@$(CASK) build

# TODO: Add `checkdoc` and `lint` here when they pass
ci: clean install compile

install:
	@$(CASK) install

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . -L langs \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(DOCSTR-FILES)

lint:
	@echo "package linting..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval $(INIT) \
		--eval $(LINT) \
		$(DOCSTR-FILES)

clean:
	rm -rf .cask *.elc

.PHONY: build ci compile lint clean
