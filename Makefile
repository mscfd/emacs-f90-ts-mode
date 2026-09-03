EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .

# Path to this Makefile.  Used by the recursive make invocation that
# creates the individual parallel ERT targets.
MAKEFILE := $(abspath $(firstword $(MAKEFILE_LIST)))

# To allow log instructions (as no-ops) run with:
#   make test-target-name DEV=1
ifdef DEV
EMACSFLAGS += --eval "(setq f90-ts-allow-log t)"
endif


# ----------------------------------------------------------------------
# Tree-sitter setup
#
# This only configures where grammars are loaded from.  It does not
# require the Fortran grammar to exist.
# ----------------------------------------------------------------------

TREE_SITTER_LOAD = \
	--eval "(progn \
	  (require 'treesit) \
	  (setq treesit-language-source-alist nil) \
	  (add-to-list 'treesit-extra-load-path \"~/.emacs.d/tree-sitter\"))"


# ----------------------------------------------------------------------
# ERT loading
#
# Unlike TREE_SITTER_LOAD, this requires the Fortran grammar.
# This variable is only used by ERT targets.
# ----------------------------------------------------------------------

LOAD = \
	$(TREE_SITTER_LOAD) \
	--eval "(unless (treesit-language-available-p 'fortran) \
	  (princ \"Fortran grammar missing\\n\") \
	  (kill-emacs 1))" \
	-l ert \
	-l f90-ts-mode.el \
	-l test/f90-ts-mode-test.el

ERTFLAGS = \
	--eval '(setq ert-batch-print-length nil ert-batch-print-level nil)'

SRCS = f90-ts-mode.el test/f90-ts-mode-test.el


# ----------------------------------------------------------------------
# ERT runners
# ----------------------------------------------------------------------

# Single-threaded execution: all selected tests in one Emacs process.
define run-ert-single
	@$(EMACS) $(EMACSFLAGS) $(LOAD) $(ERTFLAGS) \
		--eval '(ert-run-tests-batch-and-exit "$(1)")'
endef


# One Emacs process for one test.
#
# Buffering/filtering is intentional: running many Emacs processes in
# parallel otherwise produces a lot of interleaved output.
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
# Test all targets
# ----------------------------------------------------------------------

.PHONY: test-all
test-all: test-checkdoc test-byte-compile test-ert-all
	@echo "test-all: all checks passed"


.PHONY: test-all-parallel
test-all-parallel: test-checkdoc test-byte-compile test-ert-parallel
	@echo "test-all-parallel: all checks passed"


# ----------------------------------------------------------------------
# Lazy ERT test discovery
#
# There is deliberately NO:
#
#   ALL_TESTS := $(shell ...)
#
# here.
#
# Discovery only happens when test-ert-discover or test-ert-parallel is
# actually invoked.
#
# Missing Fortran grammar means an empty test set.
# ----------------------------------------------------------------------

.PHONY: test-ert-discover
test-ert-discover:
	@set -e; \
	grammar_rc=0; \
	$(EMACS) $(EMACSFLAGS) \
		$(TREE_SITTER_LOAD) \
		--eval "(if (treesit-language-available-p 'fortran) \
		          (kill-emacs 0) \
		        (kill-emacs 42))" \
		>/dev/null 2>&1 || grammar_rc=$$?; \
	\
	if [ $$grammar_rc -eq 42 ]; then \
		exit 0; \
	elif [ $$grammar_rc -ne 0 ]; then \
		echo "ERROR: Tree-sitter grammar availability check failed" >&2; \
		exit $$grammar_rc; \
	fi; \
	\
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
		--eval '(mapc (lambda (s) (princ (format "%s\n" (ert-test-name s)))) (ert-select-tests "^f90-ts-mode-test-" t))'


# ----------------------------------------------------------------------
# Parallel ERT
#
# Discovery is performed at recipe execution time.
#
# A temporary Makefile is generated containing one target per ERT test.
# A recursive make then executes those targets in parallel.
#
# If no grammar exists, discovery produces zero tests and this target
# succeeds.
# ----------------------------------------------------------------------

.PHONY: test-ert-parallel
test-ert-parallel:
	@set -e; \
	tmp_tests=$$(mktemp); \
	tmp_make=$$(mktemp); \
	trap 'rm -f "$$tmp_tests" "$$tmp_make"' EXIT; \
	\
	grammar_rc=0; \
	$(EMACS) $(EMACSFLAGS) \
		$(TREE_SITTER_LOAD) \
		--eval "(if (treesit-language-available-p 'fortran) \
		          (kill-emacs 0) \
		        (kill-emacs 42))" \
		>/dev/null 2>&1 || grammar_rc=$$?; \
	\
	if [ $$grammar_rc -eq 42 ]; then \
		: > "$$tmp_tests"; \
	elif [ $$grammar_rc -ne 0 ]; then \
		echo "ERROR: Tree-sitter grammar availability check failed" >&2; \
		exit $$grammar_rc; \
	else \
		if ! $(EMACS) $(EMACSFLAGS) $(LOAD) \
			--eval '(with-temp-file "'"$$tmp_tests"'" (mapc (lambda (s) (insert (format "%s\n" (ert-test-name s)))) (ert-select-tests "^f90-ts-mode-test-" t)))'; \
		then \
			echo "ERROR: ERT test discovery failed (likely elisp syntax error)" >&2; \
			exit 1; \
		fi; \
	fi; \
	\
	count=$$(wc -l < "$$tmp_tests"); \
	\
	if [ "$$count" -eq 0 ]; then \
		echo "ERT RESULT: SUCCESS (0 tests)"; \
		exit 0; \
	fi; \
	\
	{ \
		echo "# Automatically generated by test-ert-parallel"; \
		echo "# Do not edit."; \
		echo; \
		while IFS= read -r test; do \
			target=$${test#f90-ts-mode-test-}; \
			printf '.PHONY: _tp-%s\n' "$$target"; \
			printf '_tp-%s:\n' "$$target"; \
			printf '\t$$(call run-ert-parallel,%s)\n\n' "$$test"; \
		done < "$$tmp_tests"; \
	} > "$$tmp_make"; \
	\
	targets=$$(sed 's/^f90-ts-mode-test-/_tp-/' "$$tmp_tests" | tr '\n' ' '); \
	\
	$(MAKE) -f "$(MAKEFILE)" -f "$$tmp_make" $$targets; \
	\
	echo "ERT RESULT: SUCCESS ($$count tests)"


# ----------------------------------------------------------------------
# checkdoc
#
# Does not require the Fortran grammar.
# ----------------------------------------------------------------------

.PHONY: test-checkdoc
test-checkdoc:
	@for file in $(SRCS); do \
		$(EMACS) $(EMACSFLAGS) \
			--eval "(require 'checkdoc)" \
			$(TREE_SITTER_LOAD) \
			-l f90-ts-mode.el \
			--eval "(with-current-buffer (find-file-noselect \"$$file\") \
			  (checkdoc-current-buffer t))" \
			--eval "(when (get-buffer checkdoc-diagnostic-buffer) \
			  (with-current-buffer checkdoc-diagnostic-buffer \
			    (let ((content (string-trim (buffer-string)))) \
			      (if (string-match-p \"\\.el:[0-9]+:\" content) \
			        (progn \
			          (princ (concat content \"\\n\") \
			            'external-debugging-output) \
			          (kill-emacs 1)) \
			        (princ (concat \"checkdoc: $$file passed\\n\") \
			          'external-debugging-output)))))" \
		|| exit 1; \
	done


# ----------------------------------------------------------------------
# Byte compile
#
# Does not require the Fortran grammar.
# ----------------------------------------------------------------------

.PHONY: test-byte-compile
test-byte-compile:
	@$(EMACS) $(EMACSFLAGS) \
		$(TREE_SITTER_LOAD) \
		--eval "(add-to-list 'load-path \"test\")" \
		--eval "(setq byte-compile-error-on-warn t)" \
		--eval "(byte-compile-file \"f90-ts-mode.el\")" \
		--eval "(load-file \"f90-ts-mode.el\")" \
		--eval "(byte-compile-file \"test/f90-ts-mode-test.el\")" \
		--eval "(when (get-buffer \"*Compile-Log*\") \
		  (with-current-buffer \"*Compile-Log*\" \
		    (let ((content (string-trim (buffer-string)))) \
		      (when (string-match-p \"Error:\" content) \
		        (kill-emacs 1)))))" \
	2>&1; \
	rc=$$?; \
	rm -f $(foreach f,$(SRCS),$(f:%.el=%.elc)); \
	if [ $$rc -eq 0 ]; then \
		echo "byte-compile: all files passed"; \
	fi; \
	exit $$rc


# ----------------------------------------------------------------------
# Standard ERT
#
# These targets intentionally require the grammar.  They run ERT
# directly rather than using lazy discovery.
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
# Legacy internal parallel targets
#
# These are retained for compatibility.  They use the same parallel
# runner as the dynamically generated targets.
# ----------------------------------------------------------------------

.PHONY: _test-ert-p-main
_test-ert-p-main:
	$(call run-ert-parallel,^f90-ts-mode-test-std)


.PHONY: _test-ert-p-extra-font-lock
_test-ert-p-extra-font-lock:
	$(call run-ert-parallel,^f90-ts-mode-test-extra--font-lock--)


.PHONY: _test-ert-p-extra-indent-by-region
_test-ert-p-extra-indent-by-region:
	$(call run-ert-parallel,^f90-ts-mode-test-extra--indent-by-region--)
