EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .
LOAD = -l ert -l f90-ts-mode.el -l test/f90-ts-mode-test.el
ERTFLAGS = --eval '(setq ert-batch-print-length nil ert-batch-print-level nil)'

define run-ert
	@$(EMACS) $(EMACSFLAGS) $(LOAD) $(ERTFLAGS) \
		--eval '(ert-run-tests-batch-and-exit "$(1)")'
endef

INDENT_BY_LINE_TESTS := $(shell \
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
		--eval '(with-temp-file "/tmp/ert-tests.txt" \
		          (mapc (lambda (s) (insert (format "%s\n" (ert-test-name s)))) \
		                (ert-select-tests "^f90-ts-mode-extra/indent-by-line/" t)))' \
		2>/dev/null && cat /tmp/ert-tests.txt)

INDENT_BY_LINE_TARGETS := $(foreach t,$(INDENT_BY_LINE_TESTS),\
	test-p-ibl-$(subst /,-,$(subst f90-ts-mode-extra/indent-by-line/,,$(t))))

define make-indent-by-line-target
.PHONY: test-p-ibl-$(subst /,-,$(subst f90-ts-mode-extra/indent-by-line/,,$(1)))
test-p-ibl-$(subst /,-,$(subst f90-ts-mode-extra/indent-by-line/,,$(1))):
	$$(call run-ert,^$(1)$$)
endef

$(foreach t,$(INDENT_BY_LINE_TESTS),$(eval $(call make-indent-by-line-target,$(t))))

.PHONY: test
test:
	$(call run-ert,^f90-ts-mode/)

.PHONY: test-extra
test-extra:
	$(call run-ert,^f90-ts-mode-extra/)

.PHONY: test-all
test-all:
	$(call run-ert,^f90-ts-mode)

.PHONY: test-parallel
test-parallel: test-p-main \
               test-p-extra-font-lock \
               test-p-extra-indent-by-region \
               $(INDENT_BY_LINE_TARGETS)

.PHONY: test-p-main
test-p-main:
	$(call run-ert,^f90-ts-mode/)

.PHONY: test-p-extra-font-lock
test-p-extra-font-lock:
	$(call run-ert,^f90-ts-mode-extra/font-lock/)

.PHONY: test-p-extra-indent-by-region
test-p-extra-indent-by-region:
	$(call run-ert,^f90-ts-mode-extra/indent-by-region/)
