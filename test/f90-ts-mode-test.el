(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'f90-ts-mode)

(defcustom f90-ts-mode-test-diff-command "kompare"
  "External diff tool to use for test comparisons.
Can be 'kompare', 'meld', 'kdiff3', 'diffuse', etc."
  :type 'string
  :group 'f90-ts-mode)


;;------------------------------------------------------------------------------
;; Indentation functions for erts files

(defun f90-ts-mode-test--indent-by-region ()
  "Indent whole current buffer with f90-ts-indent-and-complete-region."
  (f90-ts-indent-and-complete-region (point-min) (point-max)))


(defun f90-ts-mode-test--indent-by-line ()
  "Indent whole current buffer line by line. This is intended for
testing f90-ts-indent-and-complete-line."
  (cl-loop initially (goto-char (point-min))
         do (unless (looking-at-p "^[[:space:]]*$")
              (f90-ts-indent-and-complete-line))
         while (zerop (forward-line 1))))


(defun f90-ts-mode-test--indent-buffer-last-tab ()
  "Indent current buffer, including the final line. This is relevant as
we also want to test the final line, which is relevant during typing
at the end of file. Intended for incomplete files or those without
final newline."
  (goto-char (point-max))
  (beginning-of-line)
  (f90-ts-indent-and-complete-region (point-min) (point))
  (goto-char (point-max))
  (f90-ts-indent-and-complete-line))


;;------------------------------------------------------------------------------
;; custom variable handling for testing

(defconst f90-ts-mode-test-custom-settings
  '((f90-ts-indent-toplevel . 1)
    (f90-ts-indent-contain . 3)
    (f90-ts-indent-block . 5)
    (f90-ts-indent-continued . 7)
    (f90-ts-indent-list-region . keep-or-primary)
    (f90-ts-indent-list-line . keep-or-primary)
    (f90-ts-indent-list-always-include-default . nil)
    (f90-ts-beginning-ampersand . nil)
    (f90-ts-comment-prefix-regexp . "!\\S-*\\s-+")
    (f90-ts-openmp-prefix-regexp . "!\\$\\(?:omp\\)?\\s-+")
    (f90-ts-special-var-regexp . "\\_<\\(self\\|this\\)\\_>")
    (f90-ts-separator-comment-regexp . "! \\(result\\|=\\{10\\}\\|arguments\\|local\\)$")
    (f90-ts-comment-region-prefix . "!!$")
    (f90-ts-extra-comment-prefixes . '("!%%!" "!>"))
    (treesit-font-lock-level . 4)       ; buffer-local variable, changes to this variable
                                        ; also needs treesit-font-lock-recompute-features
    (indent-tabs-mode . nil)
    (require-final-newline . nil)
    )
  "Alist of custom variable values for testing purposes.")


(defvar f90-ts-mode-test-custom-saved
  nil
  "Alist of saved custom variable values for temporary overrides.
It has the same structure and the same set of keys as
`f90-ts-mode-test-custom-settings'")


(defun f90-ts-mode-test-save-custom ()
  "Save current custom values of managed variables listed as keys in
`f90-ts-mode-test-custom-settings'."
  (setq f90-ts-mode-test-custom-saved
        (cl-loop for (var . _) in f90-ts-mode-test-custom-settings
                 collect (cons var (default-value var)))))


;;;###autoload
(defun f90-ts-mode-test-set-custom-testing ()
  "Save current values and apply temporary ones for testing purposes."
  (f90-ts-mode-test-save-custom)
  (cl-loop for (var . val) in f90-ts-mode-test-custom-settings
           do (progn
                (when (local-variable-p var)
                  ;; if current buffer has a local copy, set it as well
                  (setq-local var val))
                (set-default var val)))
  ;; treesit-font-lock-level requires a recompute
  (treesit-font-lock-recompute-features nil nil 'fortran))


;;;###autoload
(defun f90-ts-mode-test-restore-custom ()
  "Restore previously saved custom variable values."
  (unless f90-ts-mode-test-custom-saved
    (error "f90-ts-mode-test: No saved custom variable state to restore"))
  (cl-loop for (var . val) in f90-ts-mode-test-custom-saved
           do (progn
                (when (local-variable-p var)
                  ;; if current buffer has a local copy, set it as well
                  (setq-local var val))
                (set-default var val)))
  ;; treesit-font-lock-level requires a recompute
  (treesit-font-lock-recompute-features nil nil 'fortran)
  (setq f90-ts-mode-test-custom-saved nil))


;;;###autoload
(defmacro f90-ts-mode-test-with-custom-testing (&rest body)
  "Bind test settings dynamically using cl-progv, then call BODY-FN."
  `(let ((vars (mapcar #'car f90-ts-mode-test-custom-settings))
         (vals (mapcar #'cdr f90-ts-mode-test-custom-settings))
         ;; save current buffer-local values for variables that have one
         (saved-locals (cl-loop
                        for (var . _ ) in f90-ts-mode-test-custom-settings
                        when (local-variable-p var)
                        collect (cons var
                                      (buffer-local-value var (current-buffer)))
                        )))
     (cl-progv vars vals
       ;; also set buffer-local values where needed
       (cl-loop for (var . val) in f90-ts-mode-test-custom-settings
                when (local-variable-p var)
                do (setq-local var val))
       (unwind-protect
           (progn
             ;; treesit-font-lock-level requires a recompute
             (treesit-font-lock-recompute-features nil nil 'fortran)
             (progn ,@body))
         ;; restore buffer-local values on exit
         (cl-loop for (var . val) in saved-locals
                  do (setq-local var val))
         ;; treesit-font-lock-level requires a recompute
         (treesit-font-lock-recompute-features nil nil 'fortran)))))


;;;###autoload
(defun f90-ts-mode-test--run-with-testing (file body-fn)
  "Run BODY-FN on FILE with custom values for testing. For example, use
different indentation values for each indentation type, so that
selection of indentation rules is tested properly."
  (f90-ts-mode-test-with-custom-testing
   (with-temp-buffer
     (insert-file-contents file)
     (f90-ts-mode)
     (treesit-parser-create 'fortran)
     (font-lock-ensure)
     (funcall body-fn))))


;;------------------------------------------------------------------------------
;; ERT: basic stuff

(ert-deftest f90-ts-mode/basic/activates ()
  "Check whether f90-ts-mode properly starts."
  (skip-unless (treesit-ready-p 'fortran))
  (with-temp-buffer
    (insert "program activation\nend program activation\n")
    (f90-ts-mode)
    (should (derived-mode-p 'f90-ts-mode))))


;;------------------------------------------------------------------------------
;; ERTS: auxiliary

(defvar f90-ts-mode-test-erts-diff nil
  "When nil, just fail in erts tests as usual.
If non-nil it should contain a diff command as string, which is used to
show the difference between actual and expected result.
This variable is used to pass the diff option into the erts fail
handling.")


(defvar f90-ts-mode-test--prepare-fn nil
  "A function for preparing a buffer, like remove all indent add
 additional indent etc.")


(defvar f90-ts-mode-test--action-fn nil
  "A function for performing some indent action in a buffer,
like indent whole buffer as a whole by region, line-by-line
or just a single line etc.")


(defun f90-ts-mode-test--show-diff (actual expected diff)
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


(defmacro f90-ts-mode-test-erts-with-diff (&rest body)
  "Execute BODY, containing some ert test. If an ERTS test fails and
`f90-ts-mode-test-erts-diff' is contains a diff command as string,
launch the diff to compare actual and expected results."
  `(if (not f90-ts-mode-test-erts-diff)
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (ert-test-failed
        (let* ((details (cadr err))
               (msg (nth 0 details))
               (actual (nth 1 details))
               (expected (nth 2 details)))
          ;; The other known error is "Point wrong in ...",
          ;; which has nothing to diff
          (when (string-prefix-p "Mismatch in" msg)
            (f90-ts-mode-test--show-diff
             actual
             expected
             f90-ts-mode-test-erts-diff))
          ;; Re-signal the error so ERT still records the failure
          (signal (car err) (cdr err)))))))


(defun f90-ts-mode-test--remove-indent ()
  "Remove any indentation from buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (delete-horizontal-space)
    (forward-line 1)))


(defun f90-ts-mode-test--add-indent ()
  "Add additional indent to see whether reducing existing indentation
causes any problems."
  (goto-char (point-min))
  (while (not (eobp))
    (unless (looking-at-p "^[ \t]*$")
      ;; do not add indentation to empty lines, these are not touched
      ;; by f90-ts-indent-and-complete-region
      (indent-to (+ (current-indentation) 1)))
    (forward-line 1)))


(defun f90-ts-mode-test--shorten-to-end ()
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

;;;###autoload
(defun f90-ts-mode-test-update-erts-after (&optional update-fn indent-variant)
  "If point is in a piece of code representing the after part in an
erts file, then apply an update function to the code. The code must be
delimited by markers =-= and =-=-= to be recognised as the after part.
If UPDATE-FN is non-nil, then apply the update function.
If UPDATE-FN is nil, apply indent-region itself and use INDENT-VARIANT
as variant for how to indent continued lines.
(Note that this actually uses f90-ts-indent-and-complete-region, which
is called by indent-region usually.)

The prepare and then indent steps are done together in a test run,
but here they are executed separately, so that intermediate results
can be observed and checked."
  (interactive
   (let* ((update-fn-choices '(("indent-region" . nil)
                               ("remove indentation" . f90-ts-mode-test--remove-indent)
                               ("add some indentation" . f90-ts-mode-test--add-indent)
                               ("shorten end statements to 'end'" . f90-ts-mode-test--shorten-to-end)))
          (chosen-update-fn (cdr (assoc
                                  (completing-read "preparation function to apply: "
                                                   update-fn-choices nil t)
                                  update-fn-choices)))
          (indent-variant-choices (mapcar (lambda (x) (cons (symbol-name (cdr x)) (cdr x)))
                                          f90-ts-indent-list-options))
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
                    (f90-ts-mode-test-with-custom-testing
                     (if update-fn
                         (funcall update-fn)
                       (let ((f90-ts-indent-list-region indent-variant))
                         (message "variants: %s %s" f90-ts-indent-list-region indent-variant)
                         (f90-ts-indent-and-complete-region (point-min) (point-max)))))
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


(defun f90-ts-mode-test-register (prefix files)
  "Dynamically generate tests for all FILES assumed to be in erts
format, one test per file. Necessary preparation and action to test
must be given in the Code preamble within the erts test.
PREFIX is the test name prefix, usual f90-ts-mode or f90-ts-mode-extra"
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-"
                    (string-remove-suffix
                     "/"
                     (replace-regexp-in-string
                      "^\\(indent_region\\|\\(indent\\|break\\|join\\)_line\\)_?"
                      "\\1/"
                      (file-name-sans-extension file))))
   for test-name = (intern
                    (format "%s/custom/%s"
                            prefix
                            name-base))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-test-with-custom-testing
           (f90-ts-mode-test-erts-with-diff
            (ert-test-erts-file (ert-resource-file ,file))))))
   ))


(defun f90-ts-mode-test-prep-act-register (prefix files prep-fns action-fns)
  "Dynamically generate tests for all FILES assumed to be in erts
format, one test per file and per prep-fn/action-fn combination.
PREFIX is the test name prefix, usual f90-ts-mode or f90-ts-mode-extra"
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-" (string-remove-prefix
                             "indent_"
                             (file-name-sans-extension file)))
   do (cl-loop
       ;; prep-fn is optional, nil means no preparation
       for prep-fn in prep-fns
       for prep-name = (replace-regexp-in-string
                        "^f90-ts\\(?:-mode-test--\\|-\\)" ""
                        (if prep-fn (symbol-name prep-fn) "prep-none"))
       do (cl-loop
           for action-fn in action-fns
           for action-name = (replace-regexp-in-string
                              "^f90-ts\\(?:-mode-test--\\|-\\)" ""
                              (symbol-name action-fn))
           for test-name = (intern
                            (format "%s/%s/%s/%s"
                                    prefix
                                    action-name
                                    prep-name
                                    name-base))
           for info-text = (format "prep-fn: %s, action-fn: %s"
                                   prep-name action-name)
           do (eval
               `(ert-deftest ,test-name ()
                  (skip-unless (treesit-ready-p 'fortran))
                  (ert-info
                   (,info-text)
                   (f90-ts-mode-test-with-custom-testing
                    (let ((f90-ts-mode-test--prepare-fn ',prep-fn)
                          (f90-ts-mode-test--action-fn ',action-fn))
                      (f90-ts-mode-test-erts-with-diff
                       (ert-test-erts-file (ert-resource-file ,file))))))))
               ))))


;;------------------------------------------------------------------------------
;; ERT: font locking

(defun f90-ts-mode-test--next-boundary (beg end)
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
          ;; no face and no blank/non-blank boundary before next face
          ;; starts or line ends
          next-face-change))))))


(defun f90-ts-mode-test--annotate-faces ()
  "Generate font-lock tests for the current line and insert them."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (let* ((start (point))
           (end (line-end-position))
           (span-faces
            (cl-loop while (< (point) end)
                     for face = (get-text-property (point) 'face)
                     for next = (f90-ts-mode-test--next-boundary (point) end)
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


(defun f90-ts-mode-test--update-face-line (annotate)
  "Update a single line in face annotation update process.
If ANNOTATE is nil, just remove annotations, otherwise
re-add annotations."
  (beginning-of-line)
  (cond
   ;; if this is an annotation line, delete it
   ((looking-at "\\s-*!\\s-*\\^+")
    (let ((start (point)))
      (forward-line 1)
      (delete-region start (point))))

   ((and annotate
         (not (looking-at "^\\s-*$")))
    ;; regular but not empty line, annotate it;
    ;; f90-ts-indent-and-complete-region has removed any leading blanks,
    ;; insert a blank to allow exact caret assertions
    (insert " ")
    (backward-char 1)
    (f90-ts-mode-test--annotate-faces))))


;;;###autoload
(defun f90-ts-mode-test-update-face-annotations (annotate)
  "Update face annotations in buffer.
If ANNOTATE is nil, just remove annotations, otherwise
re-add annotations.

Start with indenting the whole buffer, then add a blank on any line
which is not a font lock assertion line and which is not empty.
Then process from last line to first. Remove existing annotation lines
(those starting with ! followed by optional spaces and carets).
For other lines, generate annotations."
  (interactive
   (list (y-or-n-p "With adding face annotations? (n = only remove) ")))
  (f90-ts-mode-test-with-custom-testing
   (let* ((pos (point))
          (pos-min (point-min))
          (pos-max (point-max))
          (was-modified (buffer-modified-p))
          (content-before (buffer-substring-no-properties pos-min
                                                          pos-max)))
     (font-lock-flush pos-min pos-max)
     (font-lock-ensure pos-min pos-max)

     (save-excursion
       ;; always end buffer with a newline, the loop below starts by
       ;; moving back one line
       (goto-char (point-max))
       (unless (bolp)
         (insert "\n"))
       (f90-ts-indent-and-complete-region (point-min) (point-max))

       ;; start at end of file, so that we can freely insert
       (goto-char (point-max))

       ;; if already at beginning of buffer (bobp), then forward-line
       ;; returns number of lines left to move, which is 1, otherwise
       ;; it always returns zero
       (while (zerop (forward-line -1))
         (f90-ts-mode-test--update-face-line annotate))))

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
       (goto-char (point-max)))))


(defun f90-ts-mode-test-font-lock-register (prefix files)
  "Dynamically generate ERT tests for all font_lock_*.f90 files
in the resource folder.
PREFIX is the prefix of the test file name, either f90-ts-mode
or f90-ts-mode-extra."
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-" (string-remove-prefix
                             "font_lock_"
                             (file-name-sans-extension file)))
   for test-name = (intern
                    (format "%s/font-lock/%s"
                            prefix
                            name-base))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-test-with-custom-testing
           (ert-font-lock-test-file
            (ert-resource-file ,file)
            'f90-ts-mode))))
   ))


;;------------------------------------------------------------------------------
;; register tests


;; general indentation with indent-region
;; (with three different prep functions to vary initial indentation)
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode"
 '("indent_region_basic.erts"
   "indent_region_comments.erts"
   "indent_region_constructs.erts"
   "indent_region_preproc.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent)
 '(f90-ts-mode-test--indent-by-region)
 )


;; smart end with specific shorten-to-end preparation
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode"
 '("indent_region_smart_end.erts")
 '(nil ; no modification
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-region
   )
 )


;; incomplete code with ERROR nodes
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode"
 '("indent_line_incomplete.erts")
 '(nil ; no modification
   )
 '(f90-ts-indent-and-complete-line)
 )


;; alignment tests, leave as is, the alignment variant to apply
;; should be specified for each test header (default: keep-or-primary)
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode"
 '("indent_region_align.erts")
 '(nil ; no preparation
   )
 '(f90-ts-mode-test--indent-by-region)
 )


;; indentation tests with custom erts Code block
(f90-ts-mode-test-register
 "f90-ts-mode"
 '("indent_region_partial.erts"
   "break_line.erts"
   "join_line.erts"
   "comment_region.erts")
 )


;; expensive tests
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode-extra"
 '("indent_integration_collatz.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-region
   f90-ts-mode-test--indent-by-line)
 )

;; expensive test variant of tests already registered
;; with indent-by-region action;
;; note that indent-by-line requires reparsing of the treesitter AST
;; after each line, which is very expensive
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode-extra"
 '("indent_region_basic.erts"
   "indent_region_comments.erts"
   "indent_region_constructs.erts"
   "indent_region_preproc.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent)
 '(f90-ts-mode-test--indent-by-line)
 )


(f90-ts-mode-test-prep-act-register
 "f90-ts-mode-extra"
 '("indent_region_smart_end.erts")
 '(nil ; no modification
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-line
   )
 )


;; register font lock tests
(f90-ts-mode-test-font-lock-register
 "f90-ts-mode"
 '("font_lock_basic.f90"
   "font_lock_builtin.f90"
   "font_lock_comments.f90"
   "font_lock_openmp.f90"
   "font_lock_special_var.f90"))


;; register extra font lock tests
(f90-ts-mode-test-font-lock-register
 "f90-ts-mode-extra"
 '("font_lock_integration_collatz.f90"))


;;------------------------------------------------------------------------------

;;;###autoload
(defun f90-ts-mode-test-run (&optional regexp-test diff-tool)
  "Run all f90-ts-mode tests matching REGEXP-TEST. If DIFF-TOOL is
specified, use it in case of failure to show the difference.
If REGEXP-TEST is nil, then use \"^f90-ts-mode/\" to execute all
standard tests, extra tests are excluded."
  (interactive
   (let* (;; map from ert tests to ert test name to string
          (regexp-choice
           (completing-read
            "Additional regexp appended to \"^f90-ts-mode\": "
            nil nil nil nil nil nil))

          ;; diff tool selection
          (diff-tool-choice
           (completing-read "diff tool (empty for none): "
                         (list "" f90-ts-mode-test-diff-command)
                         nil nil "")))
     (list (concat "^f90-ts-mode" regexp-choice)
           diff-tool-choice)))

  (let (;; set defvar to the selected test, we cannot pass diff-tool directly
        (f90-ts-mode-test-erts-diff
         (and diff-tool
              (not (string-empty-p diff-tool))
              diff-tool)))
    (if regexp-test
        (ert regexp-test)
      (ert "^f90-ts-mode/"))))


(provide 'f90-ts-mode-test)
