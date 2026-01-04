EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .

.PHONY: test
test:
	$(EMACS) $(EMACSFLAGS) -l ert \
		-l f90-ts-mode.el \
		-l tests/f90-ts-mode-tests.el \
		-f ert-run-tests-batch-and-exit

.PHONY: create
create:
	$(EMACS) $(EMACSFLAGS) -l ert \
		-l f90-ts-mode.el \
		-l tests/f90-ts-mode-tests.el \
		-f  f90-ts-mode-tests-generate-compare
