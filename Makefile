EMACS ?= emacs
EMACSFLAGS = -batch -Q -L .
LOAD = \
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

define run-ert
	@$(EMACS) $(EMACSFLAGS) $(LOAD) $(ERTFLAGS) \
		--eval '(ert-run-tests-batch-and-exit "$(1)")'
endef

INDENT_BY_LINE_TESTS := $(shell \
	$(EMACS) $(EMACSFLAGS) $(LOAD) \
	--eval '(with-temp-file "/tmp/ert-tests.txt" \
	          (mapc (lambda (s) (insert (format "%s\n" (ert-test-name s)))) \
	                (ert-select-tests "^f90-ts-mode-test-extra--indent-by-line--" t)))' \
	2>/dev/null && cat /tmp/ert-tests.txt)

INDENT_BY_LINE_TARGETS := $(foreach t,$(INDENT_BY_LINE_TESTS),\
	test-ert-p-ibl-$(subst --,-,$(subst f90-ts-mode-test-extra--indent-by-line--,,$(t))))

define make-indent-by-line-target
.PHONY: test-ert-p-ibl-$(subst --,-,$(subst f90-ts-mode-test-extra--indent-by-line--,,$(1)))
test-ert-p-ibl-$(subst --,-,$(subst f90-ts-mode-test-extra--indent-by-line--,,$(1))):
	$$(call run-ert,^$(1)$$)
endef

$(foreach t,$(INDENT_BY_LINE_TESTS),$(eval $(call make-indent-by-line-target,$(t))))

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
			        (princ (concat \"checkdoc: $$file passed without warnings or errors\\n\") \
			          'external-debugging-output)))))" || exit 1; \
	done

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
		echo "byte-compile: all files passed without errors or warnings"; \
	fi ; exit $$rc


.PHONY: check
check: checkdoc byte-compile

.PHONY: test-ert-std
test-ert-std:
	$(call run-ert,^f90-ts-mode-test-std--)

.PHONY: test-ert-extra
test-ert-extra:
	$(call run-ert,^f90-ts-mode-test-extra--)

.PHONY: test-ert-all
test-ert-all:
	$(call run-ert,^f90-ts-mode-test)

.PHONY: test-ert-parallel
test-ert-parallel: test-ert-p-main \
               test-ert-p-extra-font-lock \
               test-ert-p-extra-indent-by-region \
               $(INDENT_BY_LINE_TARGETS)

.PHONY: test-ert-p-main
test-ert-p-main:
	$(call run-ert,^f90-ts-mode-test-std)

.PHONY: test-ert-p-extra-font-lock
test-ert-p-extra-font-lock:
	$(call run-ert,^f90-ts-mode-test-extra--font-lock--)

.PHONY: test-ert-p-extra-indent-by-region
test-ert-p-extra-indent-by-region:
	$(call run-ert,^f90-ts-mode-test-extra--indent-by-region--)
