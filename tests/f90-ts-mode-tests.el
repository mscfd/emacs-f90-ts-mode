(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'f90-ts-mode)

(defcustom f90-ts-mode-tests-diff-command "kompare"
  "External diff tool to use for test comparisons.
Can be 'kompare', 'meld', 'kdiff3', 'diffuse', etc."
  :type 'string
  :group 'f90-ts-mode)


;;------------------------------------------------------------------------------
;; Auxiliary stuff

(defun f90-ts-mode-tests--indent-buffer ()
  "Indent current buffer, including the final line. This is relevant as
we also want to test the final line, which is relevant during typing
at the end of file. Intended for incomplete files or those without
final newline."
  (indent-region (point-min) (point-max))
  (save-excursion
    (goto-char (point-max))
    (indent-for-tab-command)))

;;------------------------------------------------------------------------------
;; custom variable handling for testing

(defconst f90-ts-mode-tests-managed-custom
  '(f90-ts-indent-toplevel
    f90-ts-indent-contain
    f90-ts-indent-block
    f90-ts-indent-continued
    f90-ts-indent-lists-region
    f90-ts-indent-list-always-include-default
    f90-ts-special-var-regexp
    f90-ts-separator-comment-regexp
    require-final-newline)
  "Custom variables whose values can be temporarily overridden.")


(defvar f90-ts-mode-tests-saved-custom-values
  '()
  "Alist of saved custom variable values for temporary overrides.")


(defun f90-ts-mode-tests-save-custom ()
  "Save current custom values of `f90-ts-mode-tests-managed-custom`."
  (setq f90-ts-mode-tests-saved-custom-values
        (mapcar (lambda (var)
                  (cons var (symbol-value var)))
                f90-ts-mode-tests-managed-custom)))


(defconst f90-ts-mode-tests-custom-testing
  '((f90-ts-indent-toplevel . 1)
    (f90-ts-indent-contain . 3)
    (f90-ts-indent-block . 5)
    (f90-ts-indent-continued . 7)
    (f90-ts-indent-lists-region . keep-or-first)
    (f90-ts-indent-list-always-include-default . nil)
    (f90-ts-special-var-regexp . "\\_<\\(self\\|this\\)\\_>")
    (f90-ts-separator-comment-regexp . "! \\(result\\|=\\{10\\}\\|arguments\\|local\\)$")
    (require-final-newline . nil)
    )
  "Alist of custom variable values for testing purposes.")


(defun f90-ts-mode-tests-set-custom-testing ()
  "Save current values and apply temporary ones for testing purposes."
  (f90-ts-mode-tests-save-custom)
  (dolist (entry f90-ts-mode-tests-custom-testing)
    (set (car entry) (cdr entry))))


(defun f90-ts-mode-tests-restore-custom ()
  "Restore previously saved custom variable values."
  (unless f90-ts-mode-tests-saved-custom-values
    (error "f90-ts-mode-tests: No saved custom variable state to restore"))
  (dolist (entry f90-ts-mode-tests-saved-custom-values)
    (set (car entry) (cdr entry)))
  (setq f90-ts-mode-tests-saved-custom-values nil))


(defmacro f90-ts-mode-tests-with-custom-testing (&rest body)
  `(let ((f90-ts-mode-tests-saved-custom-values nil))
     (unwind-protect
         (progn
           (f90-ts-mode-tests-set-custom-testing)
           ,@body)
       (f90-ts-mode-tests-restore-custom))))


(defun f90-ts-mode-tests--run-with-testing (file body-fn)
  "Run BODY-FN on FILE with custom values for testing. For example, use
different indentation values for each indentation type, so that
selection of indentation rules is tested properly."
  (f90-ts-mode-tests-with-custom-testing
   (let ((f90-ts-log-categories '()))
     (with-temp-buffer
       (insert-file-contents file)
       (f90-ts-mode)
       (setq-local indent-tabs-mode nil)
       (treesit-parser-create 'fortran)
       (font-lock-ensure)
       (funcall body-fn)))))


;;------------------------------------------------------------------------------
;; ERT: basic stuff

(ert-deftest f90-ts-mode-test/activates ()
  "Check whether f90-ts-mode properly starts."
  (skip-unless (treesit-ready-p 'fortran))
  (with-temp-buffer
    (insert "program activation\nend program activation\n")
    (f90-ts-mode)
    (should (derived-mode-p 'f90-ts-mode))))


;;------------------------------------------------------------------------------
;; ERTS: auxiliary

(defvar f90-ts-mode-tests-erts-diff nil
  "When nil, just fail in erts tests as usual.
If non-nil it should contain a diff command as string, which is used to
show the difference between actual and expected result.
This variable is used to pass the diff option into the erts fail
handling.")


(defvar f90-ts-mode-test-prepare-fun nil
  "A function for preparing buffers, like remove all indent add
 additional indent etc.")


(defun f90-ts-mode-tests--show-diff (actual expected diff)
  "Show diff between ACTUAL and EXPECTED external tool DIFF."
  (let* ((actual-file (make-temp-file "f90-ts-mode-actual-"))
         (expected-file (make-temp-file "f90-ts-mode-expected-")))
    (unwind-protect
        (progn
          (with-temp-file actual-file
            (insert actual))
          (with-temp-file expected-file
            (insert expected))
          (call-process diff nil 0 nil actual-file expected-file)))
    ))


(defmacro f90-ts-mode-tests-erts-with-diff (&rest body)
  "Execute BODY, containing some ert test. If an ERTS test fails and
`f90-ts-mode-tests-erts-diff` is contains a diff command as string,
launch the diff to compare actual and expected results."
  `(if (not f90-ts-mode-tests-erts-diff)
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (ert-test-failed
        (let* ((details (cadr err))
               (actual (nth 1 details))
               (expected (nth 2 details)))
          (f90-ts-mode-tests--show-diff actual
                                        expected
                                        f90-ts-mode-tests-erts-diff)
          ;; Re-signal the error so ERT still records the failure
          (signal (car err) (cdr err)))))))


(defun f90-ts-mode-test-remove-indent ()
  "Remove any indentation from buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (delete-horizontal-space)
    (forward-line 1)))


(defun f90-ts-mode-test-add-indent ()
  "Add additional indent to see whether reducing existing indentation
causes any problems."
  (goto-char (point-min))
  (while (not (eobp))
    (unless (looking-at-p "^[ \t]*$")
      ;; do not add indentation to empty lines, these are not touched
      ;; by indent-region
      (indent-to (+ (current-indentation) 1)))
    (forward-line 1)))


(defun f90-ts-mode-test-shorten-to-end ()
  "Shorten end statements to just 'end', intended for testing smart end
completion."
  (goto-char (point-min))
  (while (not (eobp))
    (beginning-of-line)
    ;; preserve trailing whitespace characters possibly
    ;; followed by a comment
    ;; (note: looking-at case insensitive)
    (when (looking-at "\\(\\s-*end\\)[^!\n]*?\\(\\s-*[!\n]\\)")
      (replace-match "\\1\\2"))
    (forward-line 1)))


;;------------------------------------------------------------------------------
;; ERTS: indentation

(defun f90-ts-mode-tests-update-erts-after (&optional update-fn indent-variant)
  "If point is in a piece of code representing the after part in an
erts file, then apply an update function to the code. The code must be
delimited by markers =-= and =-=-= to be recognised as the after part.
If UPDATE-FN is non-nil, then apply the update function.
If UPDATE-FN is nil, apply indent-region itself and use INDENT-VARIANT
as variant for how to indent continued lines.

The prepare and then indent steps are done together in a test run,
but here they are executed separately, so that intermediate results
can be observed and checked."
  (interactive
   (let* ((update-fn-choices '(("indent-region" . nil)
                               ("remove indentation" . f90-ts-mode-test-remove-indent)
                               ("add some indentation" . f90-ts-mode-test-add-indent)
                               ("shorten end statements to 'end'" . f90-ts-mode-test-shorten-to-end)))
          (chosen-update-fn (cdr (assoc
                                  (completing-read "preparation function to apply: "
                                                   update-fn-choices nil t)
                                  update-fn-choices)))
          (indent-variant-choices (mapcar (lambda (x) (cons (symbol-name (cdr x)) (cdr x)))
                                          f90-ts-indent-lists-options))
          (chosen-indent-variant
           (unless chosen-update-fn  ; chosen-update-fn=nil is option indent-region
             (cdr (assoc
                   (completing-read "indent option: "
                                    indent-variant-choices nil t)
                   indent-variant-choices)))))
     (list chosen-update-fn chosen-indent-variant)))

  (save-excursion
    (condition-case err
        (let ((beg (progn
                     (unless (re-search-backward "^=-=\\(-=\\)?$" nil t)
                       (user-error "start marker =-= not found"))
                     (if (match-string 1)
                         (user-error "wrong start marker: outside of 'after' code block")
                       (forward-line 1)
                       (point))))
              (end (progn
                     (unless (re-search-forward "^=-=\\(-=\\)?$" nil t)
                       (user-error "end marker =-=-= not found"))
                     (if (not (match-string 1))
                         (user-error "wrong end marker: inside of 'before' code block")
                       (beginning-of-line)
                       (point)))))

          (when (>= beg end)
            (user-error "end marker must come after start marker"))

          (let* ((code (buffer-substring-no-properties beg
                                                       end))
                 (updated-code
                  (with-temp-buffer
                    (insert code)
                    (f90-ts-mode)
                    (f90-ts-mode-tests-with-custom-testing
                     (if update-fn
                         (funcall update-fn)
                       (let ((f90-ts-indent-lists-region indent-variant))
                         (message "variants: %s %s" f90-ts-indent-lists-region indent-variant)
                         (indent-region (point-min) (point-max)))))
                    (buffer-substring-no-properties (point-min)
                                                    (point-max)))))
            (if (string= code updated-code)
                (when (called-interactively-p 'interactive)
                  (message "'after' part of code block not changed by update"))
              (delete-region beg end)
              (goto-char beg)
              (insert updated-code)
              (when (called-interactively-p 'interactive)
                (message "'after' part of code block updated"))
              )
            )
          )

      (error
       (message "error: %s" (error-message-string err))))))


(defun f90-ts-mode-tests-indent-register (files prep-fns)
  "Dynamically generate tests for all FILES assumed to be in erts
format, one test per file.
For each test, run the specified prep-fns functions."
  (cl-loop
   for file in files
   for test-name = (intern (format "f90-ts-mode-test/%s"
                                   (file-name-sans-extension file)))
   do (eval
     `(ert-deftest ,test-name ()
        (skip-unless (treesit-ready-p 'fortran))
        (f90-ts-mode-tests-with-custom-testing
         (cl-loop
          for prep-fn in ',prep-fns
          do (ert-info ((if prep-fn
                            (format "test with prep-fn: %s" prep-fn)
                          "test without modifications"))
               (message "test %s with prepare function <%s>" ,file prep-fn)
               (let ((f90-ts-mode-test-prepare-fun prep-fn))
                 (f90-ts-mode-tests-erts-with-diff
                  (ert-test-erts-file (ert-resource-file ,file))))))
         )))
   ))


;; register tests, general indentation
;; (with three different prep functions to vary initial indentation)
(f90-ts-mode-tests-indent-register
 '("indent_region_basic.erts"
   "indent_region_comments.erts"
   "indent_region_constructs.erts"
   "indent_region_nonewline.erts"
   "indent_region_preproc.erts"
   "indent_integration_collatz.erts")
 '(nil ; no modification
   f90-ts-mode-test-remove-indent
   f90-ts-mode-test-add-indent
   f90-ts-mode-test-shorten-to-end)
 )

(f90-ts-mode-tests-indent-register
 '("indent_region_smart_end.erts")
 '(nil ; no modification
   f90-ts-mode-test-shorten-to-end)
 )

;; alignment tests, leave as is, the alignment variant to apply
;; should be specified for each test header (default: keep-or-first)
(f90-ts-mode-tests-indent-register
 '("indent_region_align.erts")
 '(nil ; no preparation
   ))


;;------------------------------------------------------------------------------
;; ERT: font locking

(defun f90-ts-mode-tests--next-boundary (beg end)
  "Find next face or blank/non-blank boundary from BEG to END."
  (let ((next-face-change (next-single-property-change beg 'face nil end))
        (face (get-text-property beg 'face)))
    (save-excursion
      (goto-char beg)
      (let ((is-blank (looking-at "[[:blank:]]")))
        ;; if we have a face (in particular a comment), do not split on blanks
        (cond
         (face
          next-face-change)

         ;; search for blank/non-blank boundary between beg and next-face-change
         ((re-search-forward (if is-blank "[^[:blank:]]" "[[:blank:]]")
                             next-face-change t)
          (match-beginning 0))

         (t
          ;; no face and no blank/non-blank boundary before next
          ;; face starts or line ends
          next-face-change))))))


(defun f90-ts-mode-tests--annotate-faces ()
  "Generate font-lock tests for the current line and insert them."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (let* ((start (point))
           (end (line-end-position))
           (span-faces
            (cl-loop while (< (point) end)
                     for face = (get-text-property (point) 'face)
                     for next = (f90-ts-mode-tests--next-boundary (point) end)
                     if (or face (not (looking-at "[[:blank:]]")))
                       collect (list (max 1 (- (point) start))
                                     (- next start)
                                     face)
                     do (goto-char next)))
           )
      (forward-line 1)
      (cl-loop for (beg end face) in span-faces
               do (when (< beg end)
                    (insert "!"
                            (make-string (1- beg) ?\s)
                            (make-string (- end beg) ?^)
                            (format " %s" face)
                            "\n")))
      )))


(defun f90-ts-mode-tests-update-face-annotations ()
  "Update face annotations in buffer.

Start with indenting the whole buffer, then add a blank on any line
which is not a font lock assertion line and which is not empty.
Then process from last line to first. Remove existing annotation lines
(those starting with ! followed by optional spaces and carets).
For other lines, generate annotations using
`f90-ts-mode-tests--annotate-faces'."
  (interactive)
  (f90-ts-mode-tests-with-custom-testing
   (let* ((pos (point))
          (pos-min (point-min))
          (pos-max (point-max))
          (was-modified (buffer-modified-p))
          (content-before (buffer-substring-no-properties pos-min
                                                          pos-max)))
     (save-excursion
       ;; always end buffer with a newline, the loop below starts by
       ;; moving back one line
       (goto-char (point-max))
       (unless (bolp)
         (insert "\n"))
       (indent-region (point-min) (point-max))

       ;; start at end of file, so that we can freely insert
       (goto-char (point-max))

       ;; if already at beginning of buffer (bobp), then forward-line
       ;; returns number of lines left to move, which is 1, otherwise
       ;; it always returns zero
       (while (= 0 (forward-line -1))
         (beginning-of-line)
         (cond
          ;; if this is an annotation line, delete it
          ((looking-at "\\s-*!\\s-*\\^+")
           (let ((start (point)))
             (forward-line 1)
             (delete-region start (point))))

          ((not (looking-at "^\\s-*$"))
           ;; regular but not empty line, annotate it;
           ;; indent-region has removed any leading blanks,
           ;; insert a blank to allow exact caret assertions
           (insert " ")
           (backward-char 1)
           (f90-ts-mode-tests--annotate-faces))))
       )
     (if (and (= pos-min (point-min))
              (= pos-max (point-max))
              (string= content-before
                       (buffer-substring-no-properties (point-min) (point-max))))
         (progn
           ;; do not reset modified status on an unsaved buffer!
           (unless was-modified
             (set-buffer-modified-p nil))
           ;; content is the same, jump to the old position
           (goto-char pos))
       (goto-char (point-max))))))


(defun f90-ts-mode-tests-font-lock-register (files)
  "Dynamically generate ERT tests for all font_lock_*.f90 files
in the resource folder."
  (cl-loop
   for file in files
   for test-name = (intern (format "f90-ts-mode-test/%s"
                                   (file-name-sans-extension file)))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-tests-with-custom-testing
           (ert-font-lock-test-file
            (ert-resource-file ,file)
            'f90-ts-mode))))
   ))


;; register font lock tests
(f90-ts-mode-tests-font-lock-register
 '("font_lock_basic.f90"
   "font_lock_builtin.f90"
   "font_lock_comments.f90"
   "font_lock_openmp.f90"
   "font_lock_special_var.f90"
   "font_lock_integration_collatz.f90"))


;;------------------------------------------------------------------------------

(defun f90-ts-mode-tests-run (&optional diff-tool)
  "Run all f90-ts-mode tests. If diff-tool is specified, use it in case
of failure to show the difference."
  (interactive
   (list (completing-read "diff tool (empty for none): "
                         (list "" f90-ts-mode-tests-diff-command)
                         nil nil "")))
  (let ((f90-ts-mode-tests-erts-diff
         (and diff-tool
              (not (string-empty-p diff-tool))
              diff-tool)))
    (ert "^f90-ts-mode-")))



(provide 'f90-ts-mode-tests)
