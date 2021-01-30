SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

DOCSTR-FILES := $(wildcard ./docstr-*.el) \
				$(wildcard clients/*.el)

TEST-FILES := test/bootstrap.el $(shell ls test/docstr-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

ci: CASK=
ci: compile clean

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-l test/bootstrap.el \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(DOCSTR-FILES)

clean:
	rm -rf .cask *.elc

.PHONY: build ci compile clean
