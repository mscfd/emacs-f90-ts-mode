;;; f90-ts-mode-test.el --- Test code for f90-ts-mode-el -*- lexical-binding: t; -*-
;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein
;; URL: https://github.com/mscfd/emacs-f90-ts-mode
;; Keywords: languages, treesitter, fortran
;; Version: 0.1.2pre
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides functions to run the tests in test/resources.

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'xref)
(require 'f90-ts-mode)

;;; Code:

(defcustom f90-ts-mode-test-diff-command "kompare"
  "External diff tool to use for test comparisons.
Can be \"kompare\", \"meld\", \"kdiff3\", \"diffuse\", etc."
  :type  'string
  :group 'f90-ts-mode)


;;------------------------------------------------------------------------------
;; Indentation functions for erts files

(defun f90-ts-mode-test--indent-by-region ()
  "Indent whole current buffer with `f90-ts-indent-and-complete-region'."
  (f90-ts-indent-and-complete-region (point-min) (point-max)))


(defun f90-ts-mode-test--indent-by-line ()
  "Indent whole current buffer line by line.
This is intended for testing `f90-ts-indent-and-complete-line'."
  (cl-loop initially (goto-char (point-min))
         do (unless (looking-at-p "^[[:space:]]*$")
              (f90-ts-indent-and-complete-line))
         while (zerop (forward-line 1))))


(defun f90-ts-mode-test--indent-buffer-last-tab ()
  "Indent current buffer, including the final line.
This is relevant as we also want to test the final line, which is relevant
during typing at the end of file.  Intended for incomplete files or those
without final newline."
  (goto-char (point-max))
  (beginning-of-line)
  (f90-ts-indent-and-complete-region (point-min) (point))
  (goto-char (point-max))
  (f90-ts-indent-and-complete-line))


;;------------------------------------------------------------------------------
;; custom variable handling for testing

(defconst f90-ts-mode-test--special-comment-rules
  '((:name "openmp simd rule"
     :match "^!\\$omp simd\\b"
     :indent indented
     :face f90-ts-font-lock-openmp-face)
    (:name "general openmp rule"
     :match "^!\\$\\(?:omp\\)?\\b"
     :indent column-0
     :face f90-ts-font-lock-openmp-face)
    (:name "separator comment rule"
     :match "^! \\(result\\|=\\{10\\}\\|arguments\\|local\\)$"
     :indent context
     :face f90-ts-font-lock-separator-comment-face)
    (:name "ford documentation"
     :match "^![<>]"
     :indent indented
     :face font-lock-doc-face)
    (:name "comment region prefixes: column-0"
     :match "^!!\\$"
     :indent column-0
     :face font-lock-comment-face)
    (:name "comment region prefixes: context"
     :match "^!!!"
     :indent context
     :face font-lock-comment-face)
    (:name "assertion for xref testing"
     :match "^!@\\(def\\|ref\\)"
     :indent column-0
     :face font-lock-doc-face))
  "Test rules for special comment node indentation.")


;; these cannot by pasted into erts-Code blocks, as the erts parsers
;; seems to mangle the plist data, it becomes corrupted
(defconst f90-ts-mode-test--special-comment-rules-omp-context
  '((:name "openmp simd rule"
     :match "^!\\$omp declare\\b"
     :indent indented
     :face f90-ts-font-lock-openmp-face)
   (:name "openmp context rule"
     :match "^!\\$\\(?:omp\\)?\\b"
     :indent context
     :face f90-ts-font-lock-openmp-face)))


(defconst f90-ts-mode-test--special-comment-rules-omp-indented
  '((:name "general indented rule"
     :match "^!\\$\\(?:omp\\)?\\b"
     :indent indented
     :face f90-ts-font-lock-openmp-face)))


;; values are chosen for testing purposes (like different indentation levels
;; to distinguish different rules and discover if the wrong rule is applied)
(defconst f90-ts-mode-test-custom-settings
  `((f90-ts-indent-toplevel . 1)
    (f90-ts-indent-contain . 3)
    (f90-ts-indent-block . 5)
    (f90-ts-indent-continued . 7)
    (f90-ts-indent-list-region . keep-or-primary)
    (f90-ts-indent-list-line . keep-or-primary)
    (f90-ts-indent-paren-default . 1)
    (f90-ts-indent-paren-close . 3)
    (f90-ts-indent-expr-assign-default . 2)
    (f90-ts-indent-expr-assign-assoc-op . 1)
    (f90-ts-indent-declaration . 3)
    (f90-ts-beginning-ampersand . nil)
    (f90-ts-special-comment-rules . ,f90-ts-mode-test--special-comment-rules)
    (f90-ts-comment-prefix-regexp . "!\\S-*\\s-+")
    (f90-ts-openmp-prefix-regexp . "!\\$\\(?:omp\\)?\\s-+")
    (f90-ts-special-var-regexp . "\\_<\\(self\\|this\\)\\_>")
    (f90-ts-comment-keyword-regexp . "\\<\\(TODO\\|FIXME\\|Remarks?\\)\\>")
    (f90-ts-comment-region-prefix . "!!$ ")
    (f90-ts-extra-comment-prefixes . ("!!!" "! " "!> "))
    (f90-ts-comment-prefix-keep-indent . t)
    (f90-ts-mark-region-reversed . nil)
    (treesit-font-lock-level . 4)       ; buffer-local variable, changes to this variable
                                        ; also needs treesit-font-lock-recompute-features
    (indent-tabs-mode . nil)
    (require-final-newline . nil)
    (transient-mark-mode . t))
  "Alist of custom variable values for testing purposes.")


(defvar f90-ts-mode-test-custom-saved
  nil
  "Alist of saved custom variable values for temporary overrides.
It has the same structure and the same set of keys as
`f90-ts-mode-test-custom-settings'")


(defun f90-ts-mode-test-save-custom ()
  "Save current custom values of managed variables.
Relevant variables are listed as keys in `f90-ts-mode-test-custom-settings'."
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
                  (set var val))
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
                  (set var val))
                (set-default var val)))
  (treesit-font-lock-recompute-features nil nil 'fortran)
  (setq f90-ts-mode-test-custom-saved nil))


;;;###autoload
(defmacro f90-ts-mode-test-with-custom-testing (&rest body)
  "Bind test settings dynamically using `cl-progv', then call BODY."
  `(let ((vars (mapcar #'car f90-ts-mode-test-custom-settings))
         (vals (mapcar #'cdr f90-ts-mode-test-custom-settings))
         ;; save current buffer-local values for variables that have one
         (saved-locals (cl-loop
                        for (var . _ ) in f90-ts-mode-test-custom-settings
                        when (local-variable-p var)
                        collect (cons var
                                      (buffer-local-value var (current-buffer))))))
     (cl-progv vars vals
       ;; also set buffer-local values where needed
       (cl-loop for (var . val) in f90-ts-mode-test-custom-settings
                when (local-variable-p var)
                do (set var val))
       (unwind-protect
           (progn
             ;; treesit-font-lock-level requires a recompute
             (treesit-font-lock-recompute-features nil nil 'fortran)
             (progn ,@body))
         ;; restore buffer-local values on exit
         (cl-loop for (var . val) in saved-locals
                  do (set var val))
         ;; treesit-font-lock-level requires a recompute
         (treesit-font-lock-recompute-features nil nil 'fortran)))))


(defun f90-ts-mode-test--run-with-testing (file body-fn)
  "Run BODY-FN on FILE with custom values for testing.
For example, use different indentation values for each indentation type,
so that selection of indentation rules is tested properly."
  (f90-ts-mode-test-with-custom-testing
   (with-temp-buffer
     (insert-file-contents file)
     (f90-ts-mode)
     (treesit-parser-create 'fortran)
     (font-lock-ensure)
     (funcall body-fn))))


;;------------------------------------------------------------------------------
;; ERT: basic stuff

(ert-deftest f90-ts-mode-test-std--activates ()
  "Check whether `f90-ts-mode' properly starts."
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
  "If non-nil use provided function for preparing a buffer.
For example, preparation function are intended to like remove all indentation,
add additional indentation, etc.")


(defvar f90-ts-mode-test--action-fn nil
  "Stores a function for performing some action in a buffer.
This can be indentation of a whole buffer by `indent-region', line-by-line
or just a single line.  But other actions like break or join line operations,
mark region functions etc. are possible as well.")


(defun f90-ts-mode-test--show-diff (actual expected diff)
  "Show diff between ACTUAL and EXPECTED external tool DIFF."
  (let* ((actual-file (make-temp-file "f90-ts-mode-actual-"))
         (expected-file (make-temp-file "f90-ts-mode-expected-")))

    (with-temp-file actual-file
      (insert actual))
    (with-temp-file expected-file
      (insert expected))
    (let ((proc (start-process "f90-ts-mode-test-diff" nil diff actual-file expected-file)))
      (set-process-sentinel
       proc
       (lambda (_p _e)
         (delete-file actual-file)
         (delete-file expected-file))))))


(defmacro f90-ts-mode-test-erts-with-diff (&rest body)
  "Execute BODY, containing some ert test.
If an ERTS test fails and `f90-ts-mode-test-erts-diff' contains a diff command
as string, launch the diff to compare actual and expected results."
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
  "Remove any indentation from current buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (delete-horizontal-space)
    (forward-line 1)))


(defun f90-ts-mode-test--add-indent ()
  "Add additional indentation to each line in current buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (unless (looking-at-p "^[ \t]*$")
      ;; do not add indentation to empty lines, these are not touched
      ;; by f90-ts-indent-and-complete-region
      (indent-to (+ (current-indentation) 1)))
    (forward-line 1)))


(defun f90-ts-mode-test--shorten-to-end ()
  "Shorten end statements to just \"end\".
This is intended for testing smart end completion."
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
  "Apply UPDATE-FN to the code at point.
This is only done if point is in a piece of code representing the after part
in an erts file.  The code must be delimited by markers =-= and =-=-= to be
recognised as the after part.
If UPDATE-FN is non-nil, then apply the update function.
If UPDATE-FN is nil, apply `indent-region' itself and use INDENT-VARIANT
as variant for how to indent continued lines.

The prepare and indent steps are done together in a test run,
but here they are executed separately, so that intermediate results
can be observed and checked."
  (interactive
   (let* ((update-fn-choices '(("indent-region" . nil)
                               ("remove indentation" . f90-ts-mode-test--remove-indent)
                               ("add some indentation" . f90-ts-mode-test--add-indent)
                               ("shorten end statements to \"end\"" . f90-ts-mode-test--shorten-to-end)))
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
                       (user-error "Start marker =-= not found"))
                     (if (match-string 1)
                         (user-error "Wrong start marker: outside of \"after\" code block")
                       (forward-line 1)
                       (point))))
              (end (progn
                     (unless (re-search-forward "^=-=\\(-=\\)?$" nil t)
                       (user-error "End marker =-=-= not found"))
                     (if (not (match-string 1))
                         (user-error "Wrong end marker: inside of \"before\" code block")
                       (beginning-of-line)
                       (point)))))

          (when (>= beg end)
            (user-error "End marker must come after start marker"))

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
                (message "'after' part of code block updated")))))

      (error
       (message "error: %s" (error-message-string err))))))


(defun f90-ts-mode-test--erts-simple-register (prefix files)
  "Dynamically generate ert-tests for FILES.
All FILES are assumed to be in erts format.  One ert-test per file is created.
Preparations and actions to test must be given in the Code preamble
within the erts test.
PREFIX is the test name prefix, usual \"f90-ts-mode\" or \"f90-ts-mode-extra\"."
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-"
                    (string-remove-suffix
                     "--"
                     (replace-regexp-in-string
                      "^\\(indent_region\\|mark_region\\|\\(indent\\|break\\|join\\)_line\\)_?"
                      "\\1--"
                      (file-name-sans-extension file))))
   for test-name = (intern
                    (format "%s--custom--%s"
                            prefix
                            name-base))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-test-with-custom-testing
           (f90-ts-mode-test-erts-with-diff
            (ert-test-erts-file (ert-resource-file ,file))))))))


(defun f90-ts-mode-test--prep-act-register (prefix files prep-fns action-fns)
  "Dynamically generate ert-tests for all FILES.
FILES are assumed to be in erts format.  One test per file and per
prep-fn/action-fn combination is created, with prep-fn and action-fn taken
from PREP_FNS and ACTION_FNS, respectively.
PREFIX is the test name prefix, usual \"f90-ts-mode\" or \"f90-ts-mode-extra\"."
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
                            (format "%s--%s--%s--%s"
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
                       (ert-test-erts-file (ert-resource-file ,file))))))))))))


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
  "Insert font-lock assertions for ert-tests for the current line."
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
                     do (goto-char next))))
      (forward-line 1)
      (cl-loop for (beg end face) in span-faces
               do (when (< beg end)
                    (insert "!"
                            (make-string (1- beg) ?\s)
                            (make-string (- end beg) ?^)
                            (format " %s" face)
                            "\n"))))))


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
If ANNOTATE is nil, just remove annotations, otherwise re-add annotations.

Start with indenting the whole buffer, then add a blank on any line
which is not a font lock assertion line and which is not empty.
Then process from last line to first.  Remove existing annotation lines (those
starting with ! followed by optional spaces and carets).
For other lines, generate annotations."
  (interactive
   (list (y-or-n-p "With adding face annotations? (n = only remove)?")))
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
         (f90-ts-mode-test--update-face-line annotate)))

     (if (and (= pos-min (point-min))
              (= pos-max (point-max))
              (string= content-before
                       (buffer-substring-no-properties pos-min pos-max)))
         (progn
           ;; do not reset modified status on an unsaved buffer!
           (unless was-modified
             (set-buffer-modified-p nil))
           ;; content is the same, jump to the old position
           (goto-char pos))
       (goto-char (point-max))))))


(defun f90-ts-mode-test--font-lock-register (prefix files)
  "Dynamically generate ert-tests for all FILES.
FILES are assumed to contain fortran code face assertions as special comments.
PREFIX is the prefix of the test file name, either \"f90-ts-mode\"
or \"f90-ts-mode-extra\"."
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-" (string-remove-prefix
                             "font_lock_"
                             (file-name-sans-extension file)))
   for test-name = (intern
                    (format "%s--font-lock--%s"
                            prefix
                            name-base))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-test-with-custom-testing
           (ert-font-lock-test-file
            (ert-resource-file ,file)
            'f90-ts-mode))))))


;; -----------------------------
;; xref

(defun f90-ts-mode-test--xref-parse-assertions ()
  "Parse !@def and !@ref blocks located BELOW the target code.
Return a list of tuples (type sym lines target-pos).

Type is symbol def or ref.
Sym is the name of the symbol passed to xref commands for checking.
Lines is a list of lines (for def a single line, for ref a list of lines),
which is compared with results from executing corresponding xref command.
Target-pos is position of the line (the directly previous non-assertion line),
on which to execute the xref command."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "!@\\(def\\|ref\\) \\(.+\\)" nil t)
             for type = (if (string= (match-string 1) "def") 'def 'ref)
             for raw-line = (match-string 2)
             for target-pos = (save-excursion
                                (goto-char (line-beginning-position))
                                (while (progn (forward-line -1)
                                              (looking-at-p "[[:space:]]*!@")))
                                (line-beginning-position))
             nconc (cl-loop for entry in (split-string raw-line "[[:space:]]+" t)
                            for parts = (split-string entry "#")
                            for sym = (car parts)
                            for line-assert = (line-number-at-pos)
                            ;; Parse comma-separated line numbers into a list of ints
                            for line-parts = (and (cadr parts) (split-string (cadr parts) "," t))
                            for expected-lines = (mapcar #'string-to-number line-parts)
                            collect (list type sym expected-lines target-pos line-assert)))))


(defun f90-ts-mode-test--xref-extract-line-numbers (items)
  "Map a list of xref ITEMS to their line numbers."
  (mapcar (lambda (item)
            (let ((loc (xref-item-location item)))
              (cond
               ((markerp loc) (line-number-at-pos loc))
               ((and (fboundp 'xref-buffer-location-p) (xref-buffer-location-p loc))
                (with-current-buffer (xref-buffer-location-buffer loc)
                  (line-number-at-pos (xref-buffer-location-position loc))))
               (t (or (xref-location-line loc) 0)))))
          items))


(defun f90-ts-mode-test--xref-verify-def (assertion)
  "Verify parsed assertion of type def.
ASSERTION is a 5-tuple (TYPE SYM EXPECTED-LINES TARGET-POS LINE-ASSERT)
Verify that SYM is defined exactly at the single line in EXPECTED-LINES.

LINE-ASSERT is the line number of the assertion."
  (cl-destructuring-bind (_ sym expected-lines _ line-assert) assertion
    (let* ((backend (xref-find-backend))
           (items (xref-backend-definitions backend sym))
           (actual-lines (f90-ts-mode-test--xref-extract-line-numbers items)))
      (unless (= (length expected-lines) 1)
        (ert-fail (format "!@def for '%s' at line %d must specify exactly one line (e.g. sym#3)" sym line-assert)))
      (unless (member (car expected-lines) actual-lines)
        (ert-fail (format "Definition mismatch for '%s' at line %d: Expected line %d, but found %S"
                          sym line-assert (car expected-lines) actual-lines))))))


(defun f90-ts-mode-test--xref-verify-ref (assertion)
  "Check parsed ASSERTION of type ref.
ASSERTION is a 5-tuple (TYPE SYM EXPECTED-LINES TARGET-POS LINE-ASSERT)
Verify that SYM references match EXPECTED-LINES exactly.

LINE-ASSERT is the line number of the assertion."
  (cl-destructuring-bind (_ sym expected-lines _ line-assert) assertion
    (let* ((backend (xref-find-backend))
           (items (xref-backend-references backend sym))
           (actual-lines (sort (f90-ts-mode-test--xref-extract-line-numbers items) #'<))
           (sorted-expected (sort expected-lines #'<)))
      (unless (equal actual-lines sorted-expected)
        (ert-fail (format "References mismatch for '%s' at line %d: Expected %S, but found %S"
                          sym line-assert sorted-expected actual-lines))))))


(defun f90-ts-mode-test--xref-check-assertion (assertion)
  "Check parsed ASSERTION.
ASSERTION is a 5-tuple (TYPE SYM EXPECTED-LINES TARGET-POS LINE-ASSERT)
derived from !@def and !@ref statements."
  (cl-destructuring-bind (type sym _ target-pos line-assert) assertion
    (save-excursion
      (goto-char target-pos)
      (let ((regexp (cond
                     ;; take .someop. symbols into account
                     ((string-match-p "\\." sym) (regexp-quote sym))
                     ;; take assignment into account, only looking for assignment(=)
                     ((string= "=" sym) "(\\s-*=\\s-*)")
                     ;; take overloaded operator into account, only looking for operator(+) and similar
                     ((string-match-p "^\\Sw" sym) (concat "(\\s-*" (regexp-quote sym) "\\s-*)"))
                     ;; standard identifier starting with a letter
                     (t (concat "\\_<" (regexp-quote sym) "\\_>")))))
        (if (not (re-search-forward regexp (line-end-position) t))
            (ert-fail (format "Symbol '%s' not found on code line %d from assertion at line %d"
                              sym (line-number-at-pos) line-assert))
          (backward-char 1)
          (pcase type
            ('def (f90-ts-mode-test--xref-verify-def assertion))
            ('ref (f90-ts-mode-test--xref-verify-ref assertion))))))))


(defun f90-ts-mode-test--xref-run-assertions ()
  "Run all parsed xref assertions."
  (cl-loop for assertion in (f90-ts-mode-test--xref-parse-assertions)
           do (f90-ts-mode-test--xref-check-assertion assertion)))


(defun f90-ts-mode-test--xref-run-file (file)
  "Load FILE from resources and run xref assertions."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file file))
    (f90-ts-mode)
    (font-lock-ensure)
    (when (treesit-ready-p 'fortran)
      (treesit-parser-create 'fortran))
    (f90-ts-mode-test--xref-run-assertions)))


(defun f90-ts-mode-test--xref-register (prefix files)
  "Dynamically generate ert-tests for xref in FILES.
PREFIX is the test name prefix, usually \"f90-ts-mode-test-std\"."
  (cl-loop
   for file in files
   for name-base = (string-replace
                    "_" "-"
                    (replace-regexp-in-string
                     "^xref_" ""
                     (file-name-sans-extension file)))
   for test-name = (intern (format "%s--xref--%s" prefix name-base))
   do (eval
       `(ert-deftest ,test-name ()
          (skip-unless (treesit-ready-p 'fortran))
          (f90-ts-mode-test-with-custom-testing
           (f90-ts-mode-test--xref-run-file ,file))))))


;;------------------------------------------------------------------------------
;; mark region helpers

(defun f90-ts-mode-test--mark-region (command)
  "Test function for mark region with no prior region selected.
Execute COMMAND, which marks some region and insert markers @..| for
result region."
  (funcall command)
  (f90-ts-mode-test--insert-region-markers))


(defun f90-ts-mode-test--modify-region (command)
  "Setup region and apply COMMAND for modifying region.
Set up region from @ to | markers, then call COMMAND and reinsert @..| markers
for modified region."
  (let ((pos (point)))
    (goto-char (point-min))
    (search-forward "@")
    (delete-char -1)
    (push-mark (point) t t)
    (if (< (point) pos)
        (goto-char (1- pos))
      (goto-char pos))
    (funcall command))
  (f90-ts-mode-test--insert-region-markers))


(defun f90-ts-mode-test--insert-region-markers ()
  "Insert @ at `region-beginning' and leave point at `region-end'.
Note that inserting @ at beginning of region requires to place
point at 1+end of region."
  (let ((beg (region-beginning))
        (end (region-end)))
    (if (= (point) beg)
        (progn
          (goto-char end)
          (insert "@")
          (goto-char beg))
      (goto-char beg)
      (insert "@")
      (goto-char (1+ end)))))


;;------------------------------------------------------------------------------
;; register tests


;; general indentation with indent-region
;; (with three different prep functions to vary initial indentation)
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-std"
 '("indent_region_progmod.erts"
   "indent_region_comments.erts"
   "indent_region_interface.erts"
   "indent_region_type.erts"
   "indent_region_constructs.erts"
   "indent_region_select.erts"
   "indent_region_preproc.erts"
   "indent_region_openmp.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent)
 '(f90-ts-mode-test--indent-by-region))


;; smart end with specific shorten-to-end preparation
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-std"
 '("indent_region_smart_end.erts")
 '(nil ; no modification
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-region))


;; incomplete code with ERROR nodes
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-std"
 '("indent_line_incomplete.erts"
   "indent_line_empty.erts")
 '(nil ; no modification
   )
 '(f90-ts-indent-and-complete-line))


;; alignment tests, leave as is, the alignment variant to apply
;; should be specified for each test header (default: keep-or-primary)
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-std"
 '("indent_region_align_misc.erts"
   "indent_region_align_expr.erts")
 '(nil ; no preparation
   )
 '(f90-ts-mode-test--indent-by-region))


;; indentation tests with custom erts Code block
(f90-ts-mode-test--erts-simple-register
 "f90-ts-mode-test-std"
 '("indent_region_partial.erts"
   "indent_line_align.erts"
   "break_line.erts"
   "join_line.erts"
   "mark_region.erts"
   "comment_region.erts"))


;; expensive tests
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-extra"
 '("indent_integration_collatz.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-region
   f90-ts-mode-test--indent-by-line))

;; expensive test variant of tests already registered
;; with indent-by-region action;
;; note that indent-by-line requires reparsing of the treesitter AST
;; after each line, which is very expensive
(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-extra"
 '("indent_region_progmod.erts"
   "indent_region_comments.erts"
   "indent_region_interface.erts"
   "indent_region_constructs.erts"
   "indent_region_select.erts"
   "indent_region_preproc.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent)
 '(f90-ts-mode-test--indent-by-line))


(f90-ts-mode-test--prep-act-register
 "f90-ts-mode-test-extra"
 '("indent_region_smart_end.erts")
 '(nil ; no modification
   f90-ts-mode-test--shorten-to-end)
 '(f90-ts-mode-test--indent-by-line))


;; register font lock tests
(f90-ts-mode-test--font-lock-register
 "f90-ts-mode-test-std"
 '("font_lock_progmod.f90"
   "font_lock_comment.f90"
   "font_lock_builtin.f90"
   "font_lock_interface.f90"
   "font_lock_type.f90"
   "font_lock_enum.f90"
   "font_lock_select.f90"
   "font_lock_do_concurrent.f90"
   "font_lock_where.f90"
   "font_lock_forall.f90"
   "font_lock_openmp.f90"
   "font_lock_operator.f90"
   "font_lock_special_var.f90"))


;; xref tests
(f90-ts-mode-test--xref-register
 "f90-ts-mode-test-std"
 '("xref_misc.f90"
   "xref_interface.f90"
   "xref_type.f90"))


;; register extra font lock tests
(f90-ts-mode-test--font-lock-register
 "f90-ts-mode-test-extra"
 '("font_lock_integration_collatz.f90"))


;;------------------------------------------------------------------------------

;;;###autoload
(defun f90-ts-mode-test-run (&optional regexp-test diff-tool)
  "Run all ert-tests matching REGEXP-TEST.
If DIFF-TOOL is specified, use it in case of failure to show the difference.
If REGEXP-TEST is nil, then use \"^f90-ts-mode-test-std--\" to execute all
standard tests.  Extra tests can be selected by \"^f90-ts-mode-test-extra--\"."
  (interactive
   (let* (;; map from ert tests to ert test name to string
          (regexp-choice
           (completing-read
            "Additional regexp appended to \"^f90-ts-mode-test-\": "
            nil nil nil nil nil nil))

          ;; diff tool selection
          (diff-tool-choice
           (completing-read "diff tool (empty for none): "
                         (list "" f90-ts-mode-test-diff-command)
                         nil nil "")))
     (list (concat "^f90-ts-mode-test-" regexp-choice)
           diff-tool-choice)))

  (let (;; set defvar to the selected test, we cannot pass diff-tool directly
        (f90-ts-mode-test-erts-diff
         (and diff-tool
              (not (string-empty-p diff-tool))
              diff-tool)))
    (if regexp-test
        (ert regexp-test)
      (ert "^f90-ts-mode-test-std--"))))


(provide 'f90-ts-mode-test)

;;; f90-ts-mode-test.el ends here
