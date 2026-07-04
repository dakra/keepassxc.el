EMACS ?= emacs
SODIUM_DIR ?= ../sodium.el
LOAD_PATH = -L . -L $(SODIUM_DIR) -L test
ELS = keepassxc.el keepassxc-auth-source.el

.PHONY: all compile checkdoc lint test clean

all: compile checkdoc test

compile: clean
	$(EMACS) -Q --batch $(LOAD_PATH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(ELS)

checkdoc:
	$(EMACS) -Q --batch -l scripts/checkdoc-batch.el $(ELS)

lint:
	$(EMACS) -Q --batch $(LOAD_PATH) -l scripts/package-lint-batch.el $(ELS)

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/keepassxc-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
