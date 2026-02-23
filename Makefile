EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .
LOAD = -l ert -l f90-ts-mode.el -l test/f90-ts-mode-test.el

.PHONY: test
test:
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
	--eval '(setq ert-batch-print-length nil ert-batch-print-level nil)' \
	--eval '(ert-run-tests-batch-and-exit "^f90-ts-mode/")'

.PHONY: test-extra
test-extra:
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
		--eval '(ert-run-tests-batch-and-exit "^f90-ts-mode-extra/")'

.PHONY: test-all
test-all:
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
		--eval '(ert-run-tests-batch-and-exit "^f90-ts-mode")'
