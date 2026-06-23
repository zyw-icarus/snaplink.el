EMACS ?= emacs

.PHONY: test compile lint

test:
	$(EMACS) -Q --batch -L . -l snaplink-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile snaplink.el

lint:
	$(EMACS) -Q --batch -L . -l package-lint -f package-lint-batch-and-exit snaplink.el
