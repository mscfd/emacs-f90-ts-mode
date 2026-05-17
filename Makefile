EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .

# to allow log instructions (as no-ops) run with: make test-target-name DEV=1
ifdef DEV
EMACSFLAGS += --eval "(setq f90-ts-allow-log t)"
endif

LOAD = \
	--eval "(setq debug-on-error t)" \
	--eval "(progn \
	  (require 'treesit) \
	  (setq treesit-language-source-alist nil) \
	  (add-to-list 'treesit-extra-load-path \"~/.emacs.d/tree-sitter\") \
	  (unless (treesit-language-available-p 'fortran) \
	    (princ \"Fortran grammar missing\\n\") \
	    (kill-emacs 1)))" \
	-l ert \
	-l f90-ts-mode.el \
	-l test/f90-ts-mode-test.el

ERTFLAGS = --eval '(setq ert-batch-print-length nil ert-batch-print-level nil)'

SRCS = f90-ts-mode.el test/f90-ts-mode-test.el

# for single threaded execution (all tests with one emacs call)
define run-ert-single
	@$(EMACS) $(EMACSFLAGS) $(LOAD) $(ERTFLAGS) \
		--eval '(ert-run-tests-batch-and-exit "$(1)")'
endef

# buffering and filtering for multithreaded execution, otherwise emacs
# produces too much noise
define run-ert-parallel
	@out=$$($(EMACS) $(EMACSFLAGS) $(LOAD) $(ERTFLAGS) \
		--eval '(ert-run-tests-batch-and-exit "^$(1)$$")' 2>&1); \
	rc=$$?; \
	if [ $$rc -ne 0 ]; then \
		echo "$$out" | grep -v \
			-e '^Running [0-9]' \
			-e '^Ran [0-9]'; \
	else \
		echo "$$out" | grep '^ *passed'; \
	fi; \
	exit $$rc
endef


# ----------------------------------------------------------------------
# test discovery  (all tests, one target each)
# ----------------------------------------------------------------------

ALL_TESTS := $(shell \
	set -e; \
	tmp=$$(mktemp); \
	if ! $(EMACS) $(EMACSFLAGS) $(LOAD) \
	  --eval '(with-temp-file "'"$$tmp"'" \
	            (mapc (lambda (s) (insert (format "%s\n" (ert-test-name s)))) \
	                  (ert-select-tests "^f90-ts-mode-test-" t)))'; \
	then \
	  echo "ERROR: test discovery failed (likely elisp syntax error)" >&2; \
	  exit 1; \
	fi; \
	cat $$tmp; rm -f $$tmp )

# Mangle test names into make-safe target names:
#   f90-ts-mode-test-std--foo  ->  _tp-std--foo
#   f90-ts-mode-test-extra--bar--baz  ->  _tp-extra--bar--baz
ALL_TEST_TARGETS := $(foreach t,$(ALL_TESTS),\
	_tp-$(subst f90-ts-mode-test-,,$(t)))

# generated parallel targets use run-ert-parallel to filter out noise
define make-test-target
.PHONY: _tp-$(subst f90-ts-mode-test-,,$(1))
_tp-$(subst f90-ts-mode-test-,,$(1)):
	$$(call run-ert-parallel,$(1))
endef

$(foreach t,$(ALL_TESTS),$(eval $(call make-test-target,$(t))))

# ----------------------------------------------------------------------
# checkdoc
# ----------------------------------------------------------------------

.PHONY: test-checkdoc
test-checkdoc:
	@for file in $(SRCS); do \
		$(EMACS) $(EMACSFLAGS) \
			--eval "(require 'checkdoc)" \
			--eval "(with-current-buffer (find-file-noselect \"$$file\") \
			  (checkdoc-current-buffer t))" \
			--eval "(when (get-buffer checkdoc-diagnostic-buffer) \
			  (with-current-buffer checkdoc-diagnostic-buffer \
			    (let ((content (string-trim (buffer-string)))) \
			      (if (string-match-p \"\\.el:[0-9]+:\" content) \
			        (progn \
			          (princ (concat content \"\\n\") 'external-debugging-output) \
			          (kill-emacs 1)) \
			        (princ (concat \"checkdoc: $$file passed\\n\") \
			          'external-debugging-output)))))" || exit 1; \
	done

# ----------------------------------------------------------------------
# byte compile
# ----------------------------------------------------------------------

.PHONY: test-byte-compile
test-byte-compile:
	@$(EMACS) $(EMACSFLAGS) \
		--eval "(progn \
		  (require 'treesit) \
		  (setq treesit-language-source-alist nil) \
		  (add-to-list 'treesit-extra-load-path \"~/.emacs.d/tree-sitter\") \
		  (add-to-list 'load-path \"test\"))" \
		--eval "(setq byte-compile-error-on-warn t)" \
		--eval "(byte-compile-file \"f90-ts-mode.el\")" \
		--eval "(load-file \"f90-ts-mode.el\")" \
		--eval "(byte-compile-file \"test/f90-ts-mode-test.el\")" \
		--eval "(when (get-buffer \"*Compile-Log*\") \
		  (with-current-buffer \"*Compile-Log*\" \
		    (let ((content (string-trim (buffer-string)))) \
		      (when (string-match-p \"Error:\" content) \
		        (kill-emacs 1)))))" \
	2>&1; rc=$$? ; rm -f $(foreach f,$(SRCS),$(f:%.el=%.elc)) ; \
	if [ $$rc -eq 0 ]; then \
		echo "byte-compile: all files passed"; \
	fi ; exit $$rc

# ----------------------------------------------------------------------
# standard ERT
# ----------------------------------------------------------------------

.PHONY: test-ert-std
test-ert-std:
	$(call run-ert-single,^f90-ts-mode-test-std--)

.PHONY: test-ert-extra
test-ert-extra:
	$(call run-ert-single,^f90-ts-mode-test-extra--)

.PHONY: test-ert-all
test-ert-all:
	$(call run-ert-single,^f90-ts-mode-test)

# ----------------------------------------------------------------------
# parallel execution  (one job per test)
# ----------------------------------------------------------------------

.PHONY: test-ert-parallel
test-ert-parallel: $(ALL_TEST_TARGETS)
	@echo "ERT RESULT: SUCCESS ($(words $(ALL_TEST_TARGETS)) tests)"
# ----------------------------------------------------------------------
# internal parallel targets
# ----------------------------------------------------------------------

.PHONY: _test-ert-p-main
_test-ert-p-main:
	$(call run-ert,^f90-ts-mode-test-std)

.PHONY: _test-ert-p-extra-font-lock
_test-ert-p-extra-font-lock:
	$(call run-ert,^f90-ts-mode-test-extra--font-lock--)

.PHONY: _test-ert-p-extra-indent-by-region
_test-ert-p-extra-indent-by-region:
	$(call run-ert,^f90-ts-mode-test-extra--indent-by-region--)
