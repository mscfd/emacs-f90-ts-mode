;;; toc-tests.el --- Test collection for the toc buffer of f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Resource file with subtests for `f90-ts-toc', not meant to be loaded
;; directly.  It is registered in f90-ts-mode-test.el through
;; `f90-ts-mode-test--collection-register' and runs as a single ert test, with
;; one subtest per `f90-ts-mode-test-defsub' form below.
;;
;; These subtests check the toc buffer (rendering, point sync, switching,
;; refresh, toggles, mouse commands, node operations) and the timer life cycle.
;; Most of them use hard coded sparse trees as produced by
;; `f90-ts--toc-tree-build'.  Subtests using `f90-ts-mode-test--toc-with-real'
;; and those in the last section parse real source.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'f90-ts-mode)
(require 'f90-ts-mode-test)
(require 'f90-ts-toc)


;;;-----------------------------------------------------------------------------
;; auxiliary stuff

(defvar f90-ts-mode-test--toc-text
  "module m
  integer :: a, b
contains
  subroutine s
    real :: x
    x = 1
  end subroutine
  function f()
  end function
end module
"
  "Source text matching the mock sparse tree.
Tests may bind this and `f90-ts-mode-test--toc-tree-function'.")


(defvar f90-ts-mode-test--toc-tree-function #'f90-ts-mode-test--toc-mock-tree
  "Function returning the mock sparse tree for the current buffer.
There is a test which checks that the parser tree and the mock tree are equal.")


(defconst f90-ts-mode-test--toc-short-labels
  '(("program"                 . "prog")
    ("module"                  . "mod")
    ("submodule"               . "submod")
    ("subroutine"              . "subr")
    ("function"                . "fun")
    ("module_procedure"        . "mproc")
    ("derived_type_definition" . "type")
    ("interface"               . "ifc")
    ("procedure_statement"     . "proc")
    ("variable_declaration"    . "var"))
  "Short labels used by the tests, independent of user customization.")


(defvar f90-ts-mode-test--toc-build-count 0
  "Number of mock tree builds, to detect needless refreshes.")


(defun f90-ts-mode-test--toc-pos (regexp &optional group)
  "Start of the first match of REGEXP (or of its GROUP) in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp)
    (match-beginning (or group 0))))


(defconst f90-ts-mode-test--toc-text-tree
  '(nil ((("module" "m" 8 28 (1 . 133) nil))
         ((("variable_declaration" "a" 23 nil (12 . 27) nil)
           ("variable_declaration" "b" 26 nil (12 . 27) nil)))
         ((("subroutine" "s" 50 nil (39 . 92) nil))
          ((("variable_declaration" "x" 64 nil (56 . 65) nil))))
         ((("function" "f" 104 nil (95 . 122) nil)))))
  "Sparse toc tree of `f90-ts-mode-test--toc-text', built by the real grammar.
A sparse node is (ENTRIES . CHILDREN), the root has no entries.  An entry is
\(KEY NAME POS CONTAINS-POS SPAN DOC-BEG), see `f90-ts--toc-tree-node-table'.
Keys are strings, as for the real queries.  The test
`f90-ts-mode-test--toc--real-tree-text' checks that this is the real tree.")


(defun f90-ts-mode-test--toc-mock-tree ()
  "Return the sparse tree for `f90-ts-mode-test--toc-text'.
This is a copy of `f90-ts-mode-test--toc-text-tree'.  Count the build in
`f90-ts-mode-test--toc-build-count'."
  (cl-incf f90-ts-mode-test--toc-build-count)
  (copy-tree f90-ts-mode-test--toc-text-tree))


(defun f90-ts-mode-test--toc-tree-shift (tree offset)
  "Return a copy of the sparse toc TREE with all positions moved by OFFSET.
For tests which change the source text before the tree is built."
  (cl-labels ((shift (pos)
                (and pos (+ pos offset)))
              (entry (e)
                (list (nth 0 e) (nth 1 e) (shift (nth 2 e)) (shift (nth 3 e))
                      (and (nth 4 e) (cons (shift (car (nth 4 e)))
                                           (shift (cdr (nth 4 e)))))
                      (shift (nth 5 e))))
              (node (n)
                (cons (mapcar #'entry (car n)) (mapcar #'node (cdr n)))))
    (node tree)))


(defun f90-ts-mode-test--toc-source (name)
  "Return a new source buffer called NAME."
  (with-current-buffer (generate-new-buffer name)
    (f90-ts-mode)
    (insert f90-ts-mode-test--toc-text)
    (goto-char (point-min))
    (current-buffer)))


(defun f90-ts-mode-test--toc-cleanup (buffers)
  "Kill the toc buffer and BUFFERS, stop the timer, reset windows."
  ;; quitting first, so that no timer or panel refers to a killed buffer
  (f90-ts-toc-buffer-quit)
  (cl-loop
   for b in buffers
   do (when (buffer-live-p b)
        (kill-buffer b)))
  (delete-other-windows)
  (f90-ts--toc-stop))


(defmacro f90-ts-mode-test--toc-with-sources (bindings &rest body)
  "Run BODY with BINDINGS ((VAR NAME) ...) bound to fresh source buffers.
The first source is shown in the selected window.  Use the mock tree and
default display options, and clean up afterwards."
  (declare (indent 1))
  ;; Bind all display options to fixed values, so the tests do not depend
  ;; on the user's customization, then create the source buffers.
  `(let* ((f90-ts-toc-show-variables      t)
          (f90-ts-toc-show-interface-procedures t)
          (f90-ts-toc-include-doc-comments nil)
          (f90-ts-toc-buffer-side         'left)
          (f90-ts-toc-name-shortening     'truncate)
          (f90-ts-toc-short-labels        f90-ts-mode-test--toc-short-labels)
          (f90-ts-toc-ellipsis            "…")
          (f90-ts-toc-abbreviate-labels   t)
          (f90-ts-toc-buffer-auto-sync    t)
          (f90-ts-mode-test--toc-build-count   0)
          ,@(mapcar (lambda (b) `(,(car b) (f90-ts-mode-test--toc-source ,(cadr b))))
                    bindings))
     ;; replace the tree builder by the function of the current test
     (cl-letf (((symbol-function 'f90-ts--toc-tree-build)
                (lambda () (funcall f90-ts-mode-test--toc-tree-function))))
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer ,(car (car bindings)))
             ,@body)
         (f90-ts-mode-test--toc-cleanup (list ,@(mapcar #'car bindings)))))))


(defun f90-ts-mode-test--toc-lines ()
  "Return the lines of the toc buffer as plain strings."
  (with-current-buffer f90-ts--toc-buffer-name
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))


(defun f90-ts-mode-test--toc-current-line ()
  "Return the trimmed toc line shown at the point in the toc window."
  (let ((win (get-buffer-window f90-ts--toc-buffer-name 'visible)))
    (with-current-buffer f90-ts--toc-buffer-name
      (save-excursion
        ;; the window point is where the sync put the cursor, which is not
        ;; necessarily the buffer point
        (goto-char (window-point win))
        (string-trim (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))))


(defun f90-ts-mode-test--toc-sync-at (src pos)
  "Put point of SRC at POS, run the timer function, return the toc line."
  (with-current-buffer src (goto-char pos))
  ;; the timer function does the sync, so call it directly
  (f90-ts--toc-tick)
  (f90-ts-mode-test--toc-current-line))


(defun f90-ts-mode-test--toc-goto-line (n)
  "Move point in the toc buffer to line N (1-based)."
  (with-current-buffer f90-ts--toc-buffer-name
    (goto-char (point-min))
    (forward-line (1- n))))


;;;-----------------------------------------------------------------------------
;; rendering and labels

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--render-abbreviated ()
  "Abbreviated labels render the whole tree with the contains separator."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (equal (f90-ts-mode-test--toc-lines)
                   '("mod: m" "  var: a" "  var: b" "  ---contains---"
                     "  subr: s" "    var: x" "  fun: f")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--render-long-labels ()
  "With abbreviation switched off, entries use the long labels."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let ((f90-ts-toc-abbreviate-labels nil))
      (with-current-buffer src (f90-ts-toc-buffer-open))
      (should (equal (car (f90-ts-mode-test--toc-lines))
                     (f90-ts--toc-entry-label 'module "m"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--short-label-fallback ()
  "Kinds without short label use the long label."
  (let ((f90-ts-toc-abbreviate-labels t)
        (f90-ts-toc-short-labels '(("module" . "mod"))))
    (should (equal (f90-ts--toc-entry-short-label 'module "m") "mod: m"))
    (should (equal (f90-ts--toc-entry-short-label 'function "f")
                   (f90-ts--toc-entry-label 'function "f")))
    (should (equal (f90-ts--toc-entry-short-label 'module "") "mod"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--face-lookup-key-types ()
  "Face lookup works for symbol and string keys."
  (should (eq (f90-ts--toc-entry-face 'subroutine) 'f90-ts-toc-procedure-face))
  (should (eq (f90-ts--toc-entry-face "subroutine") 'f90-ts-toc-procedure-face))
  (should (eq (f90-ts--toc-entry-face 'nonexistent) 'f90-ts-toc-variable-face)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--text-properties ()
  "Each line carries marker, kind, mouse face and tooltip."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (with-current-buffer f90-ts--toc-buffer-name
      (goto-char (point-min))
      (forward-line 4)
      (let ((pos (line-beginning-position)))
        (should (markerp (get-text-property pos 'f90-ts-toc-marker)))
        (should (equal (get-text-property pos 'f90-ts-toc-kind) "subroutine"))
        (should (eq (get-text-property pos 'mouse-face) 'highlight))
        (should (equal (get-text-property pos 'help-echo)
                       (f90-ts--toc-entry-label 'subroutine "s")))))))


;;;-----------------------------------------------------------------------------
;; point sync

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--sync-positions ()
  "Point in the source selects the matching toc line.
Covers names, declarations, the separator, body lines and a position
inside a keyword."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "module m")))
                   "mod: m"))
    ;; first variable on the line also captures point before it
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "integer")))
                   "var: a"))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "b$")))
                   "var: b"))
    ;; lines without own entry map to the closest preceding non-variable entry
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "contains")))
                   "---contains---"))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "subroutine s")))
                   "subr: s"))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "real")))
                   "var: x"))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "x = 1")))
                   "subr: s"))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src
                          (+ 3 (f90-ts-mode-test--toc-pos "function f"))))
                   "fun: f"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--sync-before-first-entry ()
  "Point before the first entry selects the first entry."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let* ((header "! header comment\n")
           ;; the header is inserted before the tree is built, so all
           ;; positions of the tree are moved by its length
           (f90-ts-mode-test--toc-tree-function
            (lambda ()
              (cl-incf f90-ts-mode-test--toc-build-count)
              (f90-ts-mode-test--toc-tree-shift
               f90-ts-mode-test--toc-text-tree (length header)))))
      (with-current-buffer src
        (goto-char (point-min))
        (insert header)
        (f90-ts-toc-buffer-open))
      (should (equal (f90-ts-mode-test--toc-sync-at src (point-min))
                     "mod: m")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--auto-sync-off ()
  "Without `f90-ts-toc-buffer-auto-sync' the toc point does not follow."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (let ((f90-ts-toc-buffer-auto-sync nil))
      (f90-ts-mode-test--toc-sync-at
       src (with-current-buffer src (f90-ts-mode-test--toc-pos "function f")))
      (should-not (equal (f90-ts-mode-test--toc-current-line) "fun: f")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--tick-idempotent ()
  "Nothing is rebuilt if neither buffer nor point changed."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (f90-ts--toc-tick)
    (let ((n f90-ts-mode-test--toc-build-count))
      (f90-ts--toc-tick)
      (f90-ts--toc-tick)
      (should (= n f90-ts-mode-test--toc-build-count)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--refresh-after-edit ()
  "A change of the source makes the next tick rebuild the toc."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (let ((n f90-ts-mode-test--toc-build-count))
      (with-current-buffer src (goto-char (point-max)) (insert "! c\n"))
      (f90-ts--toc-tick)
      (should (> f90-ts-mode-test--toc-build-count n)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--markers-follow-edits ()
  "Entries keep pointing at their name after text is inserted before them."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (with-current-buffer src (goto-char (point-min)) (insert "! new\n"))
    (f90-ts-mode-test--toc-goto-line 5)
    (with-current-buffer f90-ts--toc-buffer-name
      (let ((marker (f90-ts--toc-buffer-marker-at-point)))
        (with-current-buffer src
          (goto-char marker)
          (should (looking-at "s$")))))))


;;;-----------------------------------------------------------------------------
;; contains separator

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-separator-line ()
  "The separator sits between the entries before and after `contains'."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (equal (nth 3 (f90-ts-mode-test--toc-lines)) "  ---contains---"))
    (with-current-buffer f90-ts--toc-buffer-name
      (goto-char (point-min))
      (forward-line 3)
      (let ((pos (line-beginning-position)))
        (should (equal (get-text-property pos 'f90-ts-toc-kind) "contains"))
        (should (eq (get-text-property (+ pos 3) 'face)
                    'f90-ts-toc-separator-face))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-jump ()
  "Jumping to the separator goes to the `contains' keyword."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 4)
    (f90-ts-toc-buffer-jump)
    (should (eq (window-buffer (selected-window)) src))
    (should (= (point) (f90-ts-mode-test--toc-pos "contains")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-label-nil ()
  "Without `f90-ts-toc-contains-label' no separator line is shown."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let ((f90-ts-toc-contains-label nil))
      (with-current-buffer src (f90-ts-toc-buffer-open))
      (should (equal (f90-ts-mode-test--toc-lines)
                     '("mod: m" "  var: a" "  var: b"
                       "  subr: s" "    var: x" "  fun: f"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-custom-label ()
  "A custom `f90-ts-toc-contains-label' is used for the separator."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let ((f90-ts-toc-contains-label "-- contains --"))
      (with-current-buffer src (f90-ts-toc-buffer-open))
      (should (equal (nth 3 (f90-ts-mode-test--toc-lines)) "  -- contains --")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-needs-child-after ()
  "No separator without entries after `contains', or without `contains'."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src
      (let* ((a (f90-ts--toc-buffer-node-from-entry
                 (list 'variable_declaration "a" (f90-ts-mode-test--toc-pos "a, b"))))
             (nodes (list a))
             (after-all (point-max)))
        ;; contains behind every child
        (should (eq nodes (f90-ts--toc-buffer-add-contains
                           (list 'module "m" 1 after-all) nodes)))
        ;; no contains at all
        (should (eq nodes (f90-ts--toc-buffer-add-contains
                           (list 'module "m" 1) nodes)))
        ;; contains before the child: separator in front of it
        (let ((result (f90-ts--toc-buffer-add-contains
                       (list 'module "m" 1 2) nodes)))
          (should (= (length result) 2))
          (should (equal (f90-ts-toc-buffer-node-kind (car result)) "contains"))
          (should (eq (cadr result) a)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--contains-in-middle ()
  "Entries before and after `contains' are kept in order."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src
      (let* ((a (f90-ts--toc-buffer-node-from-entry
                 (list 'variable_declaration "a" (f90-ts-mode-test--toc-pos "a, b"))))
             (f (f90-ts--toc-buffer-node-from-entry
                 (list 'function "f" (f90-ts-mode-test--toc-pos "f()"))))
             (result (f90-ts--toc-buffer-add-contains
                      (list 'module "m" 1 (f90-ts-mode-test--toc-pos "contains"))
                      (list a f))))
        (should (equal (mapcar #'f90-ts-toc-buffer-node-kind result)
                       (list 'variable_declaration "contains" 'function)))))))


;;;-----------------------------------------------------------------------------
;; interfaces: unnamed interface lines and procedure statements

(defconst f90-ts-mode-test--toc-iface-text
  "module m
  interface gen
     module procedure sub1
     module procedure sub2, sub3
  end interface gen
  interface
     module subroutine fs()
     end subroutine
  end interface
end module
"
  "Source text for the interface tests.")


(defconst f90-ts-mode-test--toc-iface-text-tree
  '(nil ((("module" "m" 8 nil (1 . 192) nil))
         ((("interface" "gen" 22 nil (12 . 105) nil))
          ((("procedure_statement" "sub1" 48 nil (31 . 52) nil)))
          ((("procedure_statement" "sub2" 75 nil (58 . 85) nil)
            ("procedure_statement" "sub3" 81 nil (58 . 85) nil))))
         ((("interface" nil 108 nil (108 . 181) nil))
          ((("subroutine" "fs" 141 nil (123 . 165) nil))))))
  "Sparse toc tree of `f90-ts-mode-test--toc-iface-text', from the real grammar.
See `f90-ts-mode-test--toc-text-tree'.  The unnamed interface has no name and
its position is the one of the statement.")


(defun f90-ts-mode-test--toc-iface-tree ()
  "Return the sparse tree for `f90-ts-mode-test--toc-iface-text'.
This is a copy of `f90-ts-mode-test--toc-iface-text-tree'.  Count the build in
`f90-ts-mode-test--toc-build-count'."
  (cl-incf f90-ts-mode-test--toc-build-count)
  (copy-tree f90-ts-mode-test--toc-iface-text-tree))


(defmacro f90-ts-mode-test--toc-with-iface (var &rest body)
  "Run BODY with VAR bound to a source buffer holding the interface text."
  (declare (indent 1))
  `(let ((f90-ts-mode-test--toc-text f90-ts-mode-test--toc-iface-text)
         (f90-ts-mode-test--toc-tree-function #'f90-ts-mode-test--toc-iface-tree))
     (f90-ts-mode-test--toc-with-sources ((,var "i.f90"))
       (with-current-buffer ,var (f90-ts-toc-buffer-open))
       ,@body)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-render ()
  "Unnamed interfaces get a line, their contents are indented below it."
  (f90-ts-mode-test--toc-with-iface src
    (should (equal (f90-ts-mode-test--toc-lines)
                   '("mod: m"
                     "  ifc: gen"
                     "    proc: sub1" "    proc: sub2" "    proc: sub3"
                     "  ifc"
                     "    subr: fs")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-long-labels ()
  "With abbreviations off, interface lines use the long labels."
  (f90-ts-mode-test--toc-with-iface src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-abbreviations))
    (should (equal (nth 1 (f90-ts-mode-test--toc-lines))
                   (concat "  " (f90-ts--toc-entry-label "interface" "gen"))))
    (should (equal (nth 2 (f90-ts-mode-test--toc-lines))
                   (concat "    " (f90-ts--toc-entry-label
                                   "procedure_statement" "sub1"))))
    (should (equal (nth 5 (f90-ts-mode-test--toc-lines))
                   (concat "  " (f90-ts--toc-entry-label "interface" nil))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-toggle ()
  "Toggling interface procedures hides and shows all interface lines."
  (f90-ts-mode-test--toc-with-iface src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-interface-procedures))
    (should-not f90-ts-toc-show-interface-procedures)
    ;; no interface line at all and none of the contents
    (should (equal (f90-ts-mode-test--toc-lines) '("mod: m")))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-interface-procedures))
    (should (= (length (f90-ts-mode-test--toc-lines)) 7))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-toggle-independent-of-variables ()
  "Hiding variables keeps interface procedures and vice versa."
  (f90-ts-mode-test--toc-with-iface src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-variables))
    (should (= (length (f90-ts-mode-test--toc-lines)) 7))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-variables)
      (f90-ts-toc-buffer-toggle-interface-procedures))
    (should (= (length (f90-ts-mode-test--toc-lines)) 1))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-sync ()
  "Point in an interface block selects the matching entry.
Covers named and unnamed interfaces, procedure statements and the
subroutine declared inside an interface."
  (f90-ts-mode-test--toc-with-iface src
    (cl-flet ((sync-at (regexp)
                (f90-ts-mode-test--toc-sync-at
                 src (with-current-buffer src
                       (f90-ts-mode-test--toc-pos regexp)))))
      (should (equal (sync-at "interface gen") "ifc: gen"))
      (should (equal (sync-at "module procedure sub1") "proc: sub1"))
      (should (equal (sync-at "sub3") "proc: sub3"))
      ;; the first procedure on the line captures point before it
      (should (equal (sync-at "module procedure sub2") "proc: sub2"))
      ;; the line after the procedures belongs to the interface again
      (should (equal (sync-at "end interface gen") "ifc: gen"))
      (should (equal (sync-at "interface$") "ifc"))
      ;; the name of the subroutine is on that line
      (should (equal (sync-at "module subroutine") "subr: fs"))
      (should (equal (sync-at "fs()") "subr: fs")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-sync-hidden ()
  "With interfaces hidden, their lines belong to the enclosing entry."
  (f90-ts-mode-test--toc-with-iface src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-interface-procedures))
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "sub3")))
                   "mod: m"))
    ;; also the subroutine declared in the (hidden) interface block
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "fs()")))
                   "mod: m"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-jump-unnamed ()
  "Jumping to an unnamed interface goes to its statement."
  (f90-ts-mode-test--toc-with-iface src
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 6)
    (f90-ts-toc-buffer-jump)
    (should (eq (window-buffer (selected-window)) src))
    (should (= (point) (f90-ts-mode-test--toc-pos "interface$")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--iface-key-binding ()
  "The key i is bound to the interface procedures toggle."
  (should (eq (lookup-key f90-ts-toc-mode-map "i")
              'f90-ts-toc-buffer-toggle-interface-procedures)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--kind-predicates ()
  "The kind predicates follow the display toggles.
`f90-ts--toc-kind-hidden-p' reports kinds hidden by the toggles,
`f90-ts--toc-line-kind-p' kinds which are shown as a single line."
  (let ((f90-ts-toc-show-variables nil)
        (f90-ts-toc-show-interface-procedures t))
    (should (f90-ts--toc-kind-hidden-p "variable_declaration"))
    (should-not (f90-ts--toc-kind-hidden-p "procedure_statement"))
    (should-not (f90-ts--toc-kind-hidden-p "subroutine"))
    (should-not (f90-ts--toc-kind-hidden-p nil))
    (should-not (f90-ts--toc-kind-hidden-p "interface")))
  (let ((f90-ts-toc-show-variables t)
        (f90-ts-toc-show-interface-procedures nil))
    (should (f90-ts--toc-kind-hidden-p "procedure_statement"))
    (should-not (f90-ts--toc-kind-hidden-p "variable_declaration"))
    ;; interface blocks are hidden with their contents
    (should (f90-ts--toc-kind-hidden-p "interface"))
    (should (f90-ts--toc-kind-hidden-p 'interface))
    (should-not (f90-ts--toc-kind-hidden-p "subroutine")))
  (should (f90-ts--toc-line-kind-p "variable_declaration"))
  (should (f90-ts--toc-line-kind-p "procedure_statement"))
  (should-not (f90-ts--toc-line-kind-p "interface"))
  (should-not (f90-ts--toc-line-kind-p nil)))


;;;-----------------------------------------------------------------------------
;; side of the panel

(defun f90-ts-mode-test--toc-panel-side ()
  "Return the side (left or right) of the visible toc panel window."
  (window-parameter (get-buffer-window f90-ts--toc-buffer-name 'visible)
                    'window-side))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--side-default-left ()
  "The panel opens on the left by default."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (eq (f90-ts-mode-test--toc-panel-side) 'left))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--side-right-option ()
  "With side set to right the panel opens right of the source window."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let ((f90-ts-toc-buffer-side 'right))
      (with-current-buffer src (f90-ts-toc-buffer-open))
      (should (eq (f90-ts-mode-test--toc-panel-side) 'right))
      ;; the panel is right of the source window
      (should (> (window-left-column (get-buffer-window f90-ts--toc-buffer-name))
                 (window-left-column (get-buffer-window src)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--side-toggle-moves-panel ()
  "Toggling the side moves the panel and keeps its selected entry.
Only one panel window may exist after the move."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (f90-ts-mode-test--toc-sync-at
     src (with-current-buffer src (f90-ts-mode-test--toc-pos "function f")))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-side))
    (should (eq f90-ts-toc-buffer-side 'right))
    (should (eq (f90-ts-mode-test--toc-panel-side) 'right))
    (should (equal (f90-ts-mode-test--toc-current-line) "fun: f"))
    (should (= 1 (cl-count-if (lambda (w)
                                (eq (window-buffer w)
                                    (get-buffer f90-ts--toc-buffer-name)))
                              (window-list))))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-side))
    (should (eq f90-ts-toc-buffer-side 'left))
    (should (eq (f90-ts-mode-test--toc-panel-side) 'left))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--side-toggle-keeps-focus ()
  "A selected panel stays selected, a source window stays selected."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    ;; toggle from the source buffer: focus stays there
    (f90-ts-toc-buffer-toggle-side)
    (should (eq (window-buffer (selected-window)) src))
    ;; toggle from the panel: focus stays in the panel
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-toc-buffer-toggle-side)
    (should (eq (window-buffer (selected-window))
                (get-buffer f90-ts--toc-buffer-name)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--side-toggle-without-panel ()
  "Without a panel only the option changes, no window is created."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-toggle-side))
    (should (eq f90-ts-toc-buffer-side 'right))
    (should-not (get-buffer-window f90-ts--toc-buffer-name 'visible))))


;;;-----------------------------------------------------------------------------
;; shortening of long names

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shorten-name ()
  "`f90-ts--toc-shorten-name' cuts names left or right, or leaves them.
Names are kept if they fit, if the width is too small for the marker,
or if shortening is `truncate'."
  (let ((f90-ts-toc-ellipsis "…"))
    (let ((f90-ts-toc-name-shortening 'left))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 8) "…ng_name"))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 14) "very_long_name"))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 99) "very_long_name"))
      ;; too narrow for anything besides the marker
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 1) "very_long_name"))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 2) "…e")))
    (let ((f90-ts-toc-name-shortening 'right))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 8) "very_lo…"))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 14) "very_long_name"))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 1) "very_long_name")))
    (let ((f90-ts-toc-name-shortening 'truncate))
      (should (equal (f90-ts--toc-shorten-name "very_long_name" 8) "very_long_name")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shorten-name-ellipsis-choices ()
  "Every predefined ellipsis choice works, and the default is among them."
  (let* ((choices (cl-loop for item in (cdr (get 'f90-ts-toc-ellipsis 'custom-type))
                           when (eq (car item) 'const)
                           collect (car (last item))))
         (f90-ts-toc-name-shortening 'left))
    (should (>= (length choices) 8))
    (should (member "…" choices))
    (should (member "*" choices))
    (should (member "+" choices))
    (cl-loop
     for ellipsis in choices
     do (progn
          (should (= (length ellipsis) 1))
          (let* ((f90-ts-toc-ellipsis ellipsis)
                 (result (f90-ts--toc-shorten-name "very_long_name" 8)))
            (should (<= (string-width result) 8))
            (should (string-prefix-p ellipsis result))
            (should (string-suffix-p "name" result)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--fit-label ()
  "`f90-ts--toc-fit-label' shortens only the name part of a label.
The prefix is kept, and labels which fit or do not end with the name
stay unchanged."
  (let ((f90-ts-toc-ellipsis "…")
        (f90-ts-toc-name-shortening 'left))
    (should (equal (f90-ts--toc-fit-label "subr: very_long_name" "very_long_name" 12)
                   "subr: …_name"))
    ;; prefix is kept
    (should (string-prefix-p "subr: "
                             (f90-ts--toc-fit-label "subr: very_long_name"
                                                    "very_long_name" 10)))
    ;; unchanged: fits, no width, no name, label does not end with name
    (should (equal (f90-ts--toc-fit-label "subr: x" "x" 12) "subr: x"))
    (should (equal (f90-ts--toc-fit-label "subr: very_long_name" "very_long_name" nil)
                   "subr: very_long_name"))
    (should (equal (f90-ts--toc-fit-label "---contains---" nil 5) "---contains---"))
    (should (equal (f90-ts--toc-fit-label "subr" "very_long_name" 3) "subr"))))


(defconst f90-ts-mode-test--toc-long-text
  "module a_rather_long_module_name
contains
  subroutine very_long_subroutine_name_which_does_not_fit
  end subroutine
end module
"
  "Source text with long names.")


(defconst f90-ts-mode-test--toc-long-text-tree
  '(nil ((("module" "a_rather_long_module_name" 8 34 (1 . 128) nil))
         ((("subroutine" "very_long_subroutine_name_which_does_not_fit" 56 nil (45 . 117) nil)))))
  "Sparse toc tree of `f90-ts-mode-test--toc-long-text' from the real grammar.
See `f90-ts-mode-test--toc-text-tree'.")


(defun f90-ts-mode-test--toc-long-tree ()
  "Return the sparse tree for `f90-ts-mode-test--toc-long-text'.
This is a copy of `f90-ts-mode-test--toc-long-text-tree'.  Count the build in
`f90-ts-mode-test--toc-build-count'."
  (cl-incf f90-ts-mode-test--toc-build-count)
  (copy-tree f90-ts-mode-test--toc-long-text-tree))


(defmacro f90-ts-mode-test--toc-with-long (var &rest body)
  "Run BODY with VAR bound to a source buffer with long names.
The panel is opened with a width of 24 columns."
  (declare (indent 1))
  `(let ((f90-ts-mode-test--toc-text f90-ts-mode-test--toc-long-text)
         (f90-ts-mode-test--toc-tree-function #'f90-ts-mode-test--toc-long-tree))
     (f90-ts-mode-test--toc-with-sources ((,var "long.f90"))
       (let ((f90-ts-toc-buffer-width 24))
         (with-current-buffer ,var (f90-ts-toc-buffer-open)))
       ,@body)))


(defun f90-ts-mode-test--toc-panel-width ()
  "Return the body width of the toc panel window."
  (window-body-width (get-buffer-window f90-ts--toc-buffer-name 'visible)))


(defun f90-ts-mode-test--toc-all-lines-fit-p ()
  "Return non-nil if no toc line is wider than the panel."
  (let ((width (f90-ts-mode-test--toc-panel-width)))
    (cl-every (lambda (line) (<= (string-width line) width))
              (f90-ts-mode-test--toc-lines))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-truncate-keeps-names ()
  "With `truncate' names stay whole, so long lines may exceed the panel."
  (f90-ts-mode-test--toc-with-long src
    (should (equal (car (f90-ts-mode-test--toc-lines)) "mod: a_rather_long_module_name"))
    (should-not (f90-ts-mode-test--toc-all-lines-fit-p))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-left ()
  "Left shortening cuts the start of long names so all lines fit."
  (f90-ts-mode-test--toc-with-long src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-set-shortening 'left))
    (should (f90-ts-mode-test--toc-all-lines-fit-p))
    (let ((lines (f90-ts-mode-test--toc-lines)))
      (should (string-prefix-p "mod: …" (car lines)))
      (should (string-suffix-p "module_name" (car lines)))
      ;; the second line is the contains separator
      (should (string-prefix-p "  subr: …" (nth 2 lines)))
      (should (string-suffix-p "oes_not_fit" (nth 2 lines))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-right ()
  "Right shortening cuts the end of long names so all lines fit."
  (f90-ts-mode-test--toc-with-long src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-set-shortening 'right))
    (should (f90-ts-mode-test--toc-all-lines-fit-p))
    (let ((lines (f90-ts-mode-test--toc-lines)))
      (should (string-prefix-p "mod: a_rather" (car lines)))
      (should (string-suffix-p "…" (car lines)))
      ;; the second line is the contains separator
      (should (string-prefix-p "  subr: very_long" (nth 2 lines)))
      (should (string-suffix-p "…" (nth 2 lines))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-ellipsis-option ()
  "`f90-ts-toc-ellipsis' replaces the default ellipsis character."
  (f90-ts-mode-test--toc-with-long src
    (let ((f90-ts-toc-ellipsis "*"))
      (with-current-buffer f90-ts--toc-buffer-name
        (f90-ts-toc-buffer-set-shortening 'left))
      (should (string-prefix-p "mod: *" (car (f90-ts-mode-test--toc-lines))))
      (should-not (string-match-p "…" (car (f90-ts-mode-test--toc-lines)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-cycle ()
  "Cycling the shortening mode goes truncate, left, right, truncate."
  (f90-ts-mode-test--toc-with-long src
    (should (eq f90-ts-toc-name-shortening 'truncate))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-cycle-shortening))
    (should (eq f90-ts-toc-name-shortening 'left))
    (should (string-prefix-p "mod: …" (car (f90-ts-mode-test--toc-lines))))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-cycle-shortening))
    (should (eq f90-ts-toc-name-shortening 'right))
    (should (string-suffix-p "…" (car (f90-ts-mode-test--toc-lines))))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-cycle-shortening))
    (should (eq f90-ts-toc-name-shortening 'truncate))
    (should (equal (car (f90-ts-mode-test--toc-lines)) "mod: a_rather_long_module_name"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-tooltip-and-jump ()
  "The full name stays available and the entry still points at the source."
  (f90-ts-mode-test--toc-with-long src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-set-shortening 'left))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 3)          ; line 2 is the separator
    (should (string-match-p
             "very_long_subroutine_name_which_does_not_fit"
             (get-text-property (line-beginning-position) 'help-echo)))
    (f90-ts-toc-buffer-jump)
    (should (eq (window-buffer (selected-window)) src))
    (should (looking-at "very_long_subroutine_name_which_does_not_fit"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-sync ()
  "Point sync still finds an entry whose name is shortened."
  (f90-ts-mode-test--toc-with-long src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-set-shortening 'left))
    (should (string-prefix-p
             "subr: "
             (f90-ts-mode-test--toc-sync-at
              src (with-current-buffer src
                    (f90-ts-mode-test--toc-pos "very_long_subroutine")))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-follows-resize ()
  "Shortened names are fitted again after the panel was resized."
  (f90-ts-mode-test--toc-with-long src
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-set-shortening 'left))
    (let ((old-width (f90-ts-mode-test--toc-panel-width))
          (win (get-buffer-window f90-ts--toc-buffer-name 'visible)))
      (window-resize win 10 t)
      (should (> (f90-ts-mode-test--toc-panel-width) old-width))
      (f90-ts--toc-tick)
      (should (f90-ts-mode-test--toc-all-lines-fit-p))
      ;; the cut moved: more of the name is visible now
      (should (> (string-width (car (f90-ts-mode-test--toc-lines)))
                 old-width)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-open-fits-immediately ()
  "Opening with shortening already active fits names without a tick."
  (let ((f90-ts-mode-test--toc-text f90-ts-mode-test--toc-long-text)
        (f90-ts-mode-test--toc-tree-function #'f90-ts-mode-test--toc-long-tree))
    (f90-ts-mode-test--toc-with-sources ((src "long.f90"))
      (let ((f90-ts-toc-buffer-width 24)
            (f90-ts-toc-name-shortening 'right))
        (with-current-buffer src (f90-ts-toc-buffer-open)))
      (should (f90-ts-mode-test--toc-all-lines-fit-p)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-invalid-mode ()
  "An unknown shortening mode is a user error."
  (should-error (f90-ts-toc-buffer-set-shortening 'middle) :type 'user-error))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--shortening-key ()
  "The key t is bound to cycling the name shortening."
  (should (eq (lookup-key f90-ts-toc-mode-map "t")
              'f90-ts-toc-buffer-cycle-shortening)))


;;;-----------------------------------------------------------------------------
;; Node operations: guards (no grammar needed)

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--node-ops-need-span ()
  "Lines without span (here the contains separator) signal a user-error.
Variables have a span, but narrowing uses the structure they belong to, see
`f90-ts-mode-test--toc--narrow-node-on-variable'."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 4)          ; the contains separator
    (should-error (f90-ts-toc-buffer-mark-node) :type 'user-error)
    (should-error (f90-ts-toc-buffer-mark-node-with-doc) :type 'user-error)
    (should-error (f90-ts-toc-buffer-comment-node) :type 'user-error)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-node-on-variable ()
  "On a variable, narrowing uses the structure the entry belongs to."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 2)          ; "var: a", belongs to module m
    (f90-ts-toc-buffer-narrow-node)
    (with-current-buffer src
      ;; the module span is the whole buffer without the final newline
      (should (buffer-narrowed-p))
      (should (= (point-min) 1))
      (should (= (point-max) (buffer-size))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--node-ops-refuse-modified-source ()
  "Spans are positions, so they are only used while the source is unchanged."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 1)          ; module, has a span
    (should (consp (f90-ts--toc-buffer-entry-span)))
    (with-current-buffer src (goto-char (point-min)) (insert "! edit\n"))
    (let ((err (should-error (f90-ts-toc-buffer-mark-node) :type 'user-error)))
      (should (string-match-p "modified" (cadr err))))
    (should-error (f90-ts-toc-buffer-narrow-node) :type 'user-error)
    (should-error (f90-ts-toc-buffer-comment-node) :type 'user-error)
    ;; after the refresh the span is valid again
    (with-current-buffer f90-ts--toc-buffer-name (f90-ts-toc-buffer-refresh))
    (f90-ts-mode-test--toc-goto-line 1)
    (should (consp (f90-ts--toc-buffer-entry-span)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--node-ops-keys-and-menu ()
  "The node operations and toggles have their keys in the toc keymap."
  (cl-loop
   for binding in '(("m" . f90-ts-toc-buffer-mark-node)
                    ("M" . f90-ts-toc-buffer-mark-node-with-doc)
                    ("c" . f90-ts-toc-buffer-comment-node)
                    ("w" . f90-ts-toc-buffer-narrow-node)
                    ("d" . f90-ts-toc-buffer-toggle-doc-comments)
                    ("i" . f90-ts-toc-buffer-toggle-interface-procedures))
   do (should (eq (lookup-key f90-ts-toc-mode-map (car binding)) (cdr binding))))
  (cl-loop
   for command in '(f90-ts-toc-buffer-mark-node
                    f90-ts-toc-buffer-mark-node-with-doc
                    f90-ts-toc-buffer-comment-node
                    f90-ts-toc-buffer-narrow-node
                    f90-ts-toc-buffer-toggle-doc-comments)
   do (should (where-is-internal command f90-ts-toc-mode-map))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--toggle-doc-comments ()
  "Toggling doc comments flips `f90-ts-toc-include-doc-comments'."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should-not f90-ts-toc-include-doc-comments)
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-doc-comments))
    (should f90-ts-toc-include-doc-comments)
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-doc-comments))
    (should-not f90-ts-toc-include-doc-comments)))


;;;-----------------------------------------------------------------------------
;; Doc comments, spans and node operations (real grammar)

(defconst f90-ts-mode-test--toc-doc-text
  "! file header comment

! doc of module m
module m
  ! doc of type t
  type t
    integer :: i  ! trailing comment of i
  end type

  ! doc line 1 of gen
  ! doc line 2 of gen
  interface gen
     module procedure sub1
  end interface gen

contains
  ! first doc of s (first after contains)

  ! second doc of s
  subroutine s()
  end subroutine

  ! not adjacent

  ! adjacent a
  ! adjacent b
  function f()
  end function
end module
"
  "Source text with doc comments for the real-grammar tests.")


(defconst f90-ts-mode-test--toc-doc-text-tree
  '(nil ((("module" "m" 49 240 (42 . 435) 24))
         ((("derived_type_definition" "t" 76 nil (71 . 130) 53))
          ((("variable_declaration" "i" 93 nil (82 . 94) nil))))
         ((("interface" "gen" 188 nil (178 . 238) 134))
          ((("procedure_statement" "sub1" 214 nil (197 . 218) nil))))
         ((("subroutine" "s" 325 nil (314 . 345) 294)))
         ((("function" "f" 406 nil (397 . 424) 367)))))
  "Sparse toc tree of `f90-ts-mode-test--toc-doc-text' from the real grammar.
See `f90-ts-mode-test--toc-text-tree'.  The entries of structures have the
start of their doc comments in DOC-BEG.")


(defmacro f90-ts-mode-test--toc-with-real (var text &rest body)
  "Run BODY with VAR a real f90-ts-mode buffer holding TEXT and the panel open.
Skip the test if the Fortran grammar is not available."
  (declare (indent 2))
  `(progn
     (let* ((f90-ts-toc-show-variables             t)
            (f90-ts-toc-show-interface-procedures  t)
            (f90-ts-toc-include-doc-comments       nil)
            (f90-ts-toc-abbreviate-labels          t)
            (f90-ts-toc-name-shortening            'truncate)
            (f90-ts-toc-buffer-auto-sync           t)
            (f90-ts-toc-short-labels               f90-ts-mode-test--toc-short-labels)
            (transient-mark-mode                   t)
            (,var (generate-new-buffer "real.f90")))
       (with-current-buffer ,var
         (insert ,text)
         (f90-ts-mode)
         (goto-char (point-min)))
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer ,var)
             (with-current-buffer ,var (f90-ts-toc-buffer-open))
             ,@body)
         (f90-ts-mode-test--toc-cleanup (list ,var))))))


(defun f90-ts-mode-test--toc-line-of (pos)
  "Return the line number of POS in the current buffer, or nil."
  (and pos (line-number-at-pos pos)))


(defun f90-ts-mode-test--toc-entry-info (src)
  "Return a list (NAME DOC-LINE BEG-LINE END-LINE) per entry for SRC."
  (with-current-buffer src
    (let (result)
      ;; entry layout: (KIND NAME POS CONTAINS-POS SPAN DOC-POS), where SPAN
      ;; is (BEG . END); positions are turned into line numbers
      (maphash
       (lambda (_node entries)
         (cl-loop
          for entry in entries
          do (let ((span (nth 4 entry)))
               (push (list (nth 1 entry)
                           (f90-ts-mode-test--toc-line-of (nth 5 entry))
                           (f90-ts-mode-test--toc-line-of (car span))
                           (f90-ts-mode-test--toc-line-of (cdr span)))
                     result))))
       (f90-ts--toc-tree-node-table (treesit-buffer-root-node)))
      result)))


(defun f90-ts-mode-test--toc-span-of (src begin-regexp end-regexp)
  "Return (BEG . END) in SRC from BEGIN-REGEXP to the end of END-REGEXP."
  (with-current-buffer src
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward begin-regexp)
        (let ((beg (match-beginning 0)))
          (re-search-forward end-regexp)
          (cons beg (match-end 0)))))))


(defun f90-ts-mode-test--toc-select-entry (text)
  "Select the toc window and move point to the line showing TEXT."
  (select-window (get-buffer-window f90-ts--toc-buffer-name 'visible))
  (with-current-buffer f90-ts--toc-buffer-name
    (goto-char (point-min))
    (re-search-forward (concat "^ *" (regexp-quote text) "$"))
    (beginning-of-line)))


(defun f90-ts-mode-test--toc-region-of (src)
  "Return the active region of SRC as (BEG . END), or nil."
  (with-current-buffer src
    (and (use-region-p)
         (cons (region-beginning) (region-end)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--doc-start-detection ()
  "Doc comments: previous comment siblings, adjacent ones, empty lines."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((info (f90-ts-mode-test--toc-entry-info src)))
      (cl-flet ((doc (name)
                  (nth 0 (cdr (assoc name info))))
                (span (name)
                  (nthcdr 2 (assoc name info))))
        ;; the nearest comment is a doc comment, even after an empty line
        (should (= (doc "m") 3))
        (should (= (doc "t") 5))
        ;; two adjacent comment lines
        (should (= (doc "gen") 10))
        ;; first comment of a wrapper node (after `contains')
        ;; only the comment block next to the structure counts
        (should (= (doc "s") 19))
        (should (= (doc "f") 25))
        ;; no doc comments for variables and procedure statements
        (should-not (doc "i"))
        (should-not (doc "sub1"))
        ;; spans are trimmed: from the statement to the end statement
        (should (equal (span "m") '(4 29)))
        (should (equal (span "t") '(6 8)))
        (should (equal (span "gen") '(12 14)))
        (should (equal (span "s") '(20 21)))
        (should (equal (span "f") '(27 28)))
        (should (equal (span "i") '(7 7)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--doc-start-trailing-comment ()
  "A trailing comment of a statement is not a doc comment."
  (f90-ts-mode-test--toc-with-real src
      "module m
contains
  subroutine a()
  end subroutine  ! trailing comment of a
  subroutine b()
  end subroutine
end module
"
    (let ((info (f90-ts-mode-test--toc-entry-info src)))
      (should-not (nth 1 (assoc "b" info)))
      (should-not (nth 1 (assoc "a" info))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--sync-doc-comments ()
  "A doc comment belongs to its entry only if enabled."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((pos (with-current-buffer src
                 (f90-ts-mode-test--toc-pos "doc line 2 of gen"))))
      ;; without doc comments the previous entry is selected
      (should (equal (f90-ts-mode-test--toc-sync-at src pos) "type: t"))
      (let ((f90-ts-toc-include-doc-comments t))
        (with-current-buffer f90-ts--toc-buffer-name
          (setq f90-ts--toc-buffer-last-sync-pos nil))
        (should (equal (f90-ts-mode-test--toc-sync-at src pos) "ifc: gen"))
        ;; first and second doc line of s: only the adjacent block counts
        (cl-flet ((sync-at (regexp)
                    (f90-ts-mode-test--toc-sync-at
                     src (with-current-buffer src (f90-ts-mode-test--toc-pos regexp)))))
          (should (equal (sync-at "second doc of s") "subr: s"))
          ;; separated from the doc block of s by an empty line, so it is no
          ;; doc comment of s
          (should (equal (sync-at "first doc of s") "---contains---"))
          ;; comment which is not adjacent to the doc block of f
          (should (equal (sync-at "not adjacent") "subr: s"))
          (should (equal (sync-at "adjacent a") "fun: f"))
          (should (equal (sync-at "adjacent b") "fun: f"))
          (should (equal (sync-at "doc of module m") "mod: m"))
          ;; the file header is not a doc comment
          (should (equal (sync-at "file header") "mod: m")))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mark-node ()
  "Mark selects the trimmed node, a second mark unmarks it."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((span (f90-ts-mode-test--toc-span-of src "subroutine s()" "end subroutine")))
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-mark-node)
      ;; the region is only visible in the source window
      (should (eq (window-buffer (selected-window)) src))
      (should (equal (f90-ts-mode-test--toc-region-of src) span))
      ;; second time: deactivate
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-mark-node)
      (should-not (f90-ts-mode-test--toc-region-of src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mark-node-with-doc ()
  "The variant with doc comments starts at the first doc comment."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-mark-node-with-doc)
    (should (equal (f90-ts-mode-test--toc-region-of src)
                   (f90-ts-mode-test--toc-span-of src "! adjacent a" "end function")))
    ;; a node without doc comments gets the plain span
    (f90-ts-mode-test--toc-select-entry "var: i")
    (f90-ts-toc-buffer-mark-node-with-doc)
    (should (equal (f90-ts-mode-test--toc-region-of src)
                   (f90-ts-mode-test--toc-span-of src "integer :: i" "")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mark-node-ignores-doc-option ()
  "Marking does not depend on `f90-ts-toc-include-doc-comments'."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((f90-ts-toc-include-doc-comments t))
      (f90-ts-mode-test--toc-select-entry "fun: f")
      (f90-ts-toc-buffer-mark-node)
      (should (equal (f90-ts-mode-test--toc-region-of src)
                     (f90-ts-mode-test--toc-span-of src "function f()" "end function"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mark-node-switches-marked-node ()
  "Marking another node replaces the region instead of unmarking."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-mark-node)
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-mark-node)
    (should (equal (f90-ts-mode-test--toc-region-of src)
                   (f90-ts-mode-test--toc-span-of src "function f()" "end function")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-node ()
  "Narrow to the node, narrowing again widens."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((span (f90-ts-mode-test--toc-span-of src "subroutine s()" "end subroutine")))
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-narrow-node)
      (with-current-buffer src
        (should (buffer-narrowed-p))
        (should (= (point-min) (car span)))
        (should (= (point-max) (cdr span)))
        (should (= (point) (car span))))
      ;; like preview, narrowing leaves the focus in the panel
      (should (eq (window-buffer (selected-window))
                  (get-buffer f90-ts--toc-buffer-name)))
      ;; same node again: widen
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-narrow-node)
      (with-current-buffer src
        (should-not (buffer-narrowed-p))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-node-redisplay ()
  "Widening from the panel redisplays the source window."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((updated nil))
      (cl-letf (((symbol-function 'force-window-update)
                 (lambda (&optional object) (push object updated))))
        (f90-ts-mode-test--toc-select-entry "subr: s")
        (f90-ts-toc-buffer-narrow-node)
        (should (memq src updated))
        (setq updated nil)
        ;; same node again: widen
        (f90-ts-mode-test--toc-select-entry "subr: s")
        (f90-ts-toc-buffer-narrow-node)
        (with-current-buffer src
          (should-not (buffer-narrowed-p)))
        (should (memq src updated))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-node-doc-option ()
  "Narrowing includes doc comments if the option is set."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((f90-ts-toc-include-doc-comments t))
      (f90-ts-mode-test--toc-select-entry "fun: f")
      (f90-ts-toc-buffer-narrow-node)
      (with-current-buffer src
        (should (= (point-min)
                   (car (f90-ts-mode-test--toc-span-of src "! adjacent a" "end function"))))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-node-other-node ()
  "Narrowing to another node replaces the restriction."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-narrow-node)
    (with-current-buffer src
      (should (buffer-narrowed-p))
      (should (looking-at "function f()")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrowed-source-panel-update ()
  "Rebuilding the panel works while the source is narrowed.
The panel keeps listing the whole buffer."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((all-lines (f90-ts-mode-test--toc-lines)))
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-narrow-node)
      ;; edit within the restriction, the timer must rebuild without error;
      ;; it follows the selected window
      (select-window (get-buffer-window src))
      (with-current-buffer src
        (goto-char (point-min))
        (end-of-line)
        (insert " ! edit"))
      (f90-ts--toc-tick)
      (should (timerp f90-ts--toc-timer))
      (should (equal (f90-ts-mode-test--toc-lines) all-lines))
      ;; also the manual refresh and the menu tree
      (with-current-buffer f90-ts--toc-buffer-name (f90-ts-toc-buffer-refresh))
      (should (equal (f90-ts-mode-test--toc-lines) all-lines))
      (with-current-buffer src
        (should (buffer-narrowed-p))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--comment-node ()
  "Comment the node, the panel keeps the focus and source point stays."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((point-before (with-current-buffer src (point)))
          (prefix f90-ts-comment-region-prefix))
      (f90-ts-mode-test--toc-select-entry "fun: f")
      (f90-ts-toc-buffer-comment-node)
      (should (eq (window-buffer (selected-window))
                  (get-buffer f90-ts--toc-buffer-name)))
      (with-current-buffer src
        (should (= (point) point-before))
        (save-excursion
          (goto-char (point-min))
          ;; the two lines of the function, nothing else
          (should (= 2
                     (cl-loop
                      while (re-search-forward (regexp-quote prefix) nil t)
                      count (point))))
          (goto-char (point-min))
          (should-not (re-search-forward "^[ \t]*function f()" nil t))
          ;; doc comments of f are untouched
          (goto-char (point-min))
          (should (re-search-forward "^  ! adjacent a$" nil t))))
      ;; the source changed: the old span must not be used again
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (should-error (f90-ts-toc-buffer-comment-node) :type 'user-error)
      ;; after the refresh the function entry is gone
      (f90-ts--toc-tick)
      (select-window (get-buffer-window src))
      (f90-ts--toc-tick)
      (should-not (cl-some (lambda (l) (string-match-p "fun: f" l))
                           (f90-ts-mode-test--toc-lines))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--comment-node-read-only ()
  "Commenting a node in a read-only source signals and changes nothing."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (with-current-buffer src (setq buffer-read-only t))
    (let ((text (with-current-buffer src (buffer-string))))
      (f90-ts-mode-test--toc-select-entry "fun: f")
      (should-error (f90-ts-toc-buffer-comment-node) :type 'buffer-read-only)
      (should (equal text (with-current-buffer src (buffer-string)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--node-ops-outside-restriction ()
  "Nodes outside the narrowed region are refused.
Operations like selection of an entry are not allowed outside narrowed region."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (let ((text (with-current-buffer src
                  (save-restriction (widen) (buffer-string))))
          (restriction (with-current-buffer src (cons (point-min) (point-max)))))
      (cl-loop
       for command in '(f90-ts-toc-buffer-mark-node
                        f90-ts-toc-buffer-mark-node-with-doc
                        f90-ts-toc-buffer-comment-node)
       do (progn
            (f90-ts-mode-test--toc-select-entry "fun: f")
            (should-error (funcall command) :type 'user-error)
            (should (equal text (with-current-buffer src
                                  (save-restriction (widen) (buffer-string)))))
            (should-not (f90-ts-mode-test--toc-region-of src))
            (should (equal restriction
                           (with-current-buffer src (cons (point-min) (point-max)))))))
      ;; the node the source is narrowed to is accessible
      (f90-ts-mode-test--toc-select-entry "subr: s")
      (f90-ts-toc-buffer-mark-node)
      (should (f90-ts-mode-test--toc-region-of src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mark-with-doc-needs-doc-in-restriction ()
  "Narrowed to the plain node, the doc comments are outside the restriction."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (should-error (f90-ts-toc-buffer-mark-node-with-doc) :type 'user-error)
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-mark-node)
    (should (f90-ts-mode-test--toc-region-of src))))


;;;-----------------------------------------------------------------------------
;; Dimming and walking while the source is narrowed (real grammar)

(defconst f90-ts-mode-test--toc-nested-text
  "module m
  integer :: mv
contains
  subroutine s
    integer :: a, b
  contains
    subroutine inner
    end subroutine
  end subroutine
  subroutine t
  end subroutine
end module
"
  "Source text with nested procedures for the confinement tests.")


(defconst f90-ts-mode-test--toc-nested-text-tree
  '(nil ((("module" "m" 8 26 (1 . 180) nil))
         ((("variable_declaration" "mv" 23 nil (12 . 25) nil)))
         ((("subroutine" "s" 48 72 (37 . 137) nil))
          ((("variable_declaration" "a" 65 nil (54 . 69) nil)
            ("variable_declaration" "b" 68 nil (54 . 69) nil)))
          ((("subroutine" "inner" 96 nil (85 . 120) nil))))
         ((("subroutine" "t" 151 nil (140 . 169) nil)))))
  "Sparse toc tree of `f90-ts-mode-test--toc-nested-text' from the real grammar.
See `f90-ts-mode-test--toc-text-tree'.")


(defconst f90-ts-mode-test--toc-nested-lines
  '("mod: m" "  var: mv" "  ---contains---"
    "  subr: s" "    var: a" "    var: b" "    ---contains---" "    subr: inner"
    "  subr: t")
  "TOC lines for `f90-ts-mode-test--toc-nested-text'.")


(defun f90-ts-mode-test--toc-confined-lines ()
  "Return the lines of the toc buffer point is confined to, or nil."
  (with-current-buffer f90-ts--toc-buffer-name
    (and f90-ts--toc-buffer-confine
         (split-string
          (buffer-substring-no-properties (car f90-ts--toc-buffer-confine)
                                          (cdr f90-ts--toc-buffer-confine))
          "\n" t))))


(defun f90-ts-mode-test--toc-line-number (text)
  "Return the number of the toc line showing TEXT after trimming."
  (with-current-buffer f90-ts--toc-buffer-name
    (1+ (cl-position text (split-string (buffer-substring-no-properties
                                         (point-min) (point-max))
                                        "\n" t)
                     :test (lambda (a b) (equal a (string-trim b)))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-narrow-node ()
  "Narrowed to a node, all lines stay, and point is on one of its lines."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (should (equal (f90-ts-mode-test--toc-lines) f90-ts-mode-test--toc-nested-lines))
    (should-not (f90-ts-mode-test--toc-confined-lines))
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    ;; nothing is hidden, the toc buffer is not narrowed
    (should (equal (f90-ts-mode-test--toc-lines) f90-ts-mode-test--toc-nested-lines))
    (with-current-buffer f90-ts--toc-buffer-name
      (should-not (buffer-narrowed-p)))
    ;; the subroutine, its variables, its contains and the inner procedure
    (should (equal (f90-ts-mode-test--toc-confined-lines)
                   '("  subr: s" "    var: a" "    var: b"
                     "    ---contains---" "    subr: inner")))
    ;; and point is on one of them
    (should (member (f90-ts-mode-test--toc-current-line)
                    '("subr: s" "var: a" "var: b" "---contains---"
                      "subr: inner")))))


(defun f90-ts-mode-test--toc-dimmed-lines ()
  "Return the trimmed toc lines which are displayed with the dimmed face."
  (with-current-buffer f90-ts--toc-buffer-name
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       for face = (get-char-property (line-beginning-position) 'face)
       while (not (eobp))
       when (or (eq face 'f90-ts-toc-dimmed-face)
                (and (listp face)
                     (memq 'f90-ts-toc-dimmed-face face)))
       collect (string-trim
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
       do (forward-line 1)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--dim-outside-narrowing ()
  "Lines outside the narrowed region are dimmed, widening removes the dimming."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (should-not (f90-ts-mode-test--toc-dimmed-lines))
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: t")))
    ;; narrowing to another node moves the dimming
    (f90-ts-mode-test--toc-select-entry "subr: t")
    (f90-ts-toc-buffer-narrow-node)
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: s" "var: a"
                     "var: b" "---contains---" "subr: inner")))
    ;; same node again: widen
    (f90-ts-mode-test--toc-select-entry "subr: t")
    (f90-ts-toc-buffer-narrow-node)
    (should-not (f90-ts-mode-test--toc-dimmed-lines))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-variable-parent ()
  "Narrowing on a variable narrows to the structure declaring it."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "var: a")
    (f90-ts-toc-buffer-narrow-node)
    (should (string-prefix-p "subroutine s" (f90-ts-mode-test--toc-narrowed-text src)))
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: t")))
    ;; again on a variable of the same structure: widen
    (f90-ts-mode-test--toc-select-entry "var: b")
    (f90-ts-toc-buffer-narrow-node)
    (with-current-buffer src
      (should-not (buffer-narrowed-p)))
    ;; a variable of the module
    (f90-ts-mode-test--toc-select-entry "var: mv")
    (f90-ts-toc-buffer-narrow-node)
    (should (string-prefix-p "module m" (f90-ts-mode-test--toc-narrowed-text src)))))


(defun f90-ts-mode-test--toc-narrowed-text (src)
  "Return the trimmed text SRC is narrowed to."
  (with-current-buffer src
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))


(defun f90-ts-mode-test--toc-walk ()
  "Run the walking hook in the toc buffer, as after a command there."
  ;; `f90-ts--toc-buffer-walk' normally runs from a hook after toc commands
  (with-current-buffer f90-ts--toc-buffer-name
    (f90-ts--toc-buffer-walk)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--walk-to-structure ()
  "Point on a dimmed structure widens and narrows to this structure."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (should (memq #'f90-ts--toc-buffer-walk
                  (buffer-local-value 'post-command-hook
                                      (get-buffer f90-ts--toc-buffer-name))))
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    ;; inside of the narrowed region nothing happens
    (f90-ts-mode-test--toc-select-entry "var: b")
    (f90-ts-mode-test--toc-walk)
    (should (string-prefix-p "subroutine s" (f90-ts-mode-test--toc-narrowed-text src)))
    ;; a dimmed structure
    (f90-ts-mode-test--toc-select-entry "subr: t")
    (f90-ts-mode-test--toc-walk)
    (should (string-prefix-p "subroutine t" (f90-ts-mode-test--toc-narrowed-text src)))
    ;; point stays on its line, the dimming follows
    (should (equal (f90-ts-mode-test--toc-current-line) "subr: t"))
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: s" "var: a"
                     "var: b" "---contains---" "subr: inner")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--walk-to-variable ()
  "Point on a dimmed variable narrows to the structure declaring it."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: t")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "var: a")
    (f90-ts-mode-test--toc-walk)
    (should (string-prefix-p "subroutine s" (f90-ts-mode-test--toc-narrowed-text src)))
    (should (equal (f90-ts-mode-test--toc-current-line) "var: a"))
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: t")))
    ;; a variable of the module
    (f90-ts-mode-test--toc-select-entry "var: mv")
    (f90-ts-mode-test--toc-walk)
    (should (string-prefix-p "module m" (f90-ts-mode-test--toc-narrowed-text src)))
    (should (equal (f90-ts-mode-test--toc-current-line) "var: mv"))
    (should-not (f90-ts-mode-test--toc-dimmed-lines))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--narrow-further ()
  "Inside of a narrowed module nothing is dimmed, narrowing again narrows more."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "mod: m")
    (f90-ts-toc-buffer-narrow-node)
    (should (string-prefix-p "module m" (f90-ts-mode-test--toc-narrowed-text src)))
    (should-not (f90-ts-mode-test--toc-dimmed-lines))
    ;; walking to a substructure does not change the narrowing
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-mode-test--toc-walk)
    (should (string-prefix-p "module m" (f90-ts-mode-test--toc-narrowed-text src)))
    ;; but narrowing does
    (f90-ts-toc-buffer-narrow-node)
    (should (string-prefix-p "subroutine s" (f90-ts-mode-test--toc-narrowed-text src)))
    (should (equal (f90-ts-mode-test--toc-dimmed-lines)
                   '("mod: m" "var: mv" "---contains---" "subr: t")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--walk-movement-commands ()
  "Moving with the keys walks through the structures."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (cl-flet ((move (command)
                (call-interactively command)
                (run-hooks 'post-command-hook))
              (narrowed-to (text)
                (string-prefix-p text (f90-ts-mode-test--toc-narrowed-text src))))
      ;; down to the last line of the subroutine
      (dotimes (_ 4)
        (move #'next-line))
      (should (equal (f90-ts-mode-test--toc-current-line) "subr: inner"))
      (should (narrowed-to "subroutine s"))
      ;; one more leaves it
      (move #'next-line)
      (should (equal (f90-ts-mode-test--toc-current-line) "subr: t"))
      (should (narrowed-to "subroutine t"))
      ;; and back
      (move #'previous-line)
      (should (equal (f90-ts-mode-test--toc-current-line) "subr: inner"))
      (should (narrowed-to "subroutine inner"))
      (move #'previous-line)
      (should (equal (f90-ts-mode-test--toc-current-line) "---contains---"))
      (should (narrowed-to "subroutine s"))
      (move #'previous-line)
      (should (equal (f90-ts-mode-test--toc-current-line) "var: b"))
      (should (narrowed-to "subroutine s")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--walk-mouse ()
  "A click on a dimmed line narrows to its structure, then acts on the entry."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (let ((toc (get-buffer f90-ts--toc-buffer-name)))
      (select-window (get-buffer-window toc))
      (f90-ts-toc-buffer-mouse-preview
       (f90-ts-mode-test--toc-click-event (f90-ts-mode-test--toc-line-number "subr: t")))
      (should (string-prefix-p "subroutine t"
                               (f90-ts-mode-test--toc-narrowed-text src)))
      ;; previewed the name of the subroutine, the panel keeps the focus
      (should (= (with-current-buffer src (point))
                 (+ 11 (with-current-buffer src (point-min)))))
      (should (eq (window-buffer (selected-window)) toc))
      ;; a variable of the module narrows to the module
      (f90-ts-toc-buffer-mouse-jump
       (f90-ts-mode-test--toc-click-event (f90-ts-mode-test--toc-line-number "var: mv")))
      (should (string-prefix-p "module m" (f90-ts-mode-test--toc-narrowed-text src)))
      (should (eq (window-buffer (selected-window)) src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-widen ()
  "Widening the source removes the dimming."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (should (f90-ts-mode-test--toc-confined-lines))
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (with-current-buffer src (should-not (buffer-narrowed-p)))
    (should-not (f90-ts-mode-test--toc-confined-lines))
    (with-current-buffer f90-ts--toc-buffer-name
      (should-not f90-ts--toc-buffer-restriction))
    (should-not (f90-ts-mode-test--toc-dimmed-lines))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-external-narrowing ()
  "Narrowing and widening in the source by other means is noticed."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (let ((span (f90-ts-mode-test--toc-span-of src "subroutine t" "end subroutine")))
      (select-window (get-buffer-window src))
      (with-current-buffer src
        (narrow-to-region (car span) (cdr span))
        (goto-char (car span)))
      (f90-ts--toc-tick)
      (should (equal (f90-ts-mode-test--toc-confined-lines) '("  subr: t")))
      (should (equal (f90-ts-mode-test--toc-current-line) "subr: t"))
      (with-current-buffer src (widen))
      (f90-ts--toc-tick)
      (should-not (f90-ts-mode-test--toc-confined-lines)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-sync-inside ()
  "Following point in the source works inside the confined lines."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (select-window (get-buffer-window src))
    (cl-flet ((sync-at (regexp)
                (f90-ts-mode-test--toc-sync-at
                 src (with-current-buffer src (f90-ts-mode-test--toc-pos regexp)))))
      (should (equal (sync-at "subroutine inner") "subr: inner"))
      (should (equal (sync-at "integer :: a") "var: a"))
      (should (equal (sync-at "  contains") "---contains---"))
      (should (equal (sync-at "subroutine s") "subr: s")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-sync-clamped ()
  "A sync target outside of the confined lines is moved inside."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    ;; the restriction starts with the doc comments of f, which belong to
    ;; the previous entry as long as doc comments are not part of entries
    (let ((span (f90-ts-mode-test--toc-span-of src "! adjacent a" "end function")))
      (select-window (get-buffer-window src))
      (with-current-buffer src
        (narrow-to-region (car span) (cdr span))
        (goto-char (car span)))
      (f90-ts--toc-tick)
      (should (equal (f90-ts-mode-test--toc-confined-lines) '("  fun: f")))
      (should (equal (f90-ts-mode-test--toc-current-line) "fun: f")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-region-without-entries ()
  "Nothing is confined if the narrowed region contains no entry."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (let ((span (f90-ts-mode-test--toc-span-of src "! not adjacent" "")))
      (select-window (get-buffer-window src))
      (with-current-buffer src
        (narrow-to-region (car span) (cdr span))
        (goto-char (car span)))
      (f90-ts--toc-tick)
      (should-not (f90-ts-mode-test--toc-confined-lines))
      (with-current-buffer f90-ts--toc-buffer-name
        (should (equal f90-ts--toc-buffer-restriction span))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-follows-display-options ()
  "Hiding entries recomputes the confined lines."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-toggle-variables)
    (should (equal (f90-ts-mode-test--toc-confined-lines)
                   '("  subr: s" "    ---contains---" "    subr: inner")))
    (f90-ts-toc-buffer-toggle-variables)
    (should (= 5 (length (f90-ts-mode-test--toc-confined-lines))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-survives-edit-and-refresh ()
  "The confinement is recomputed after the panel was rebuilt."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (select-window (get-buffer-window src))
    ;; a new variable inside the narrowed region, in front of a and b
    (with-current-buffer src
      (goto-char (point-min))
      (end-of-line)
      (insert "\n    integer :: c"))
    (f90-ts--toc-tick)
    (should (equal (f90-ts-mode-test--toc-confined-lines)
                   '("  subr: s" "    var: c" "    var: a" "    var: b"
                     "    ---contains---" "    subr: inner")))
    (with-current-buffer f90-ts--toc-buffer-name (f90-ts-toc-buffer-refresh))
    (should (= 6 (length (f90-ts-mode-test--toc-confined-lines))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-source-killed ()
  "The confinement is dropped with the source."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (let ((other (generate-new-buffer "other")))
      (unwind-protect
          (progn
            (select-window (get-buffer-window src))
            (switch-to-buffer other)
            (kill-buffer src)
            (f90-ts--toc-tick)
            (should-not (f90-ts-mode-test--toc-confined-lines))
            (with-current-buffer f90-ts--toc-buffer-name
              (should-not f90-ts--toc-buffer-restriction)))
        (kill-buffer other)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--confine-operations-on-confined-lines ()
  "Mark and comment work on the lines point can be on."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-nested-text
    (f90-ts-mode-test--toc-select-entry "subr: s")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "subr: inner")
    (f90-ts-toc-buffer-mark-node)
    (should (equal (f90-ts-mode-test--toc-region-of src)
                   (f90-ts-mode-test--toc-span-of src "subroutine inner"
                                                  "end subroutine")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--comment-node-with-narrowing ()
  "Commenting works while the source is narrowed to the node."
  (f90-ts-mode-test--toc-with-real src f90-ts-mode-test--toc-doc-text
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-narrow-node)
    (f90-ts-mode-test--toc-select-entry "fun: f")
    (f90-ts-toc-buffer-comment-node)
    (with-current-buffer src
      (should (buffer-narrowed-p))
      (should (string-match-p (regexp-quote f90-ts-comment-region-prefix)
                              (buffer-string))))))


;;;-----------------------------------------------------------------------------
;; Switching between source buffers and non-source buffers

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--switch-source ()
  "Opening another f90-ts buffer switches the panel (and syncs)."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90") (src2 "b.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (switch-to-buffer src2)
    (with-current-buffer src2
      (goto-char (f90-ts-mode-test--toc-pos "subroutine s")))
    (f90-ts--toc-tick)
    (should (eq (buffer-local-value 'f90-ts--toc-buffer-source
                                    (get-buffer f90-ts--toc-buffer-name))
                src2))
    (should (equal (f90-ts-mode-test--toc-current-line) "subr: s"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--other-buffer-leaves-panel ()
  "Selecting an unrelated buffer neither rebuilds nor changes the panel."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (f90-ts--toc-tick)
    (let ((n f90-ts-mode-test--toc-build-count)
          (other (generate-new-buffer "other")))
      (unwind-protect
          (progn
            (switch-to-buffer other)
            (f90-ts--toc-tick)
            (should (= n f90-ts-mode-test--toc-build-count))
            (should (eq (buffer-local-value 'f90-ts--toc-buffer-source
                                            (get-buffer f90-ts--toc-buffer-name))
                        src)))
        (kill-buffer other)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--source-killed-clears-panel ()
  "Killing the source buffer empties the panel at the next tick."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90") (src2 "b.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (switch-to-buffer src2)
    (f90-ts--toc-tick)                  ; panel now shows src2
    (switch-to-buffer src)
    (f90-ts--toc-tick)                  ; and src again
    (let ((other (generate-new-buffer "other")))
      (unwind-protect
          (progn
            (switch-to-buffer other)
            (kill-buffer src)
            (f90-ts--toc-tick)
            (should (null (f90-ts-mode-test--toc-lines))))
        (kill-buffer other)))))


;;;-----------------------------------------------------------------------------
;; Display options

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--toggle-variables ()
  "Toggling variables hides and shows the variable lines.
While hidden, a position on a variable maps to the enclosing entry."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-variables))
    (should (equal (f90-ts-mode-test--toc-lines)
                   '("mod: m" "  ---contains---" "  subr: s" "  fun: f")))
    ;; a position on a hidden variable maps to the enclosing entry
    (should (equal (f90-ts-mode-test--toc-sync-at
                    src (with-current-buffer src (f90-ts-mode-test--toc-pos "real")))
                   "subr: s"))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-variables))
    (should (= (length (f90-ts-mode-test--toc-lines)) 7))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--toggle-keeps-entry ()
  "Toggling variables keeps the cursor on its entry (here the function)."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 7)
    (f90-ts-toc-buffer-toggle-variables)
    (should (equal (f90-ts-mode-test--toc-current-line) "fun: f"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--toggle-abbreviations ()
  "Toggling abbreviations switches between short and long labels."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-abbreviations))
    (should (equal (car (f90-ts-mode-test--toc-lines))
                   (f90-ts--toc-entry-label 'module "m")))
    (with-current-buffer f90-ts--toc-buffer-name
      (f90-ts-toc-buffer-toggle-abbreviations))
    (should (equal (car (f90-ts-mode-test--toc-lines)) "mod: m"))))


;;;-----------------------------------------------------------------------------
;; Jump, preview, mouse

(defun f90-ts-mode-test--toc-click-event (line)
  "Return a fake click event on LINE (1-based) of the toc window."
  ;; the list has the shape of a `mouse-1' event: (POSN-WINDOW POS
  ;; (X . Y) TIMESTAMP ...); only window and buffer position are used
  (let* ((win (get-buffer-window f90-ts--toc-buffer-name 'visible))
         (pos (with-current-buffer f90-ts--toc-buffer-name
                (goto-char (point-min))
                (forward-line (1- line))
                (point))))
    (list 'mouse-1
          (list win pos '(0 . 0) 0 nil pos '(0 . 0) nil '(0 . 0) '(1 . 1)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--preview-keeps-toc-selected ()
  "Preview moves point in the source but keeps the toc window selected.
The timer must not move point in the selected panel."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 7)
    (f90-ts-toc-buffer-preview)
    (should (eq (window-buffer (selected-window))
                (get-buffer f90-ts--toc-buffer-name)))
    (should (= (with-current-buffer src (point))
               (with-current-buffer src (f90-ts-mode-test--toc-pos "f()"))))
    ;; the timer must not yank point in the panel while it is selected
    (f90-ts--toc-tick)
    (should (equal (f90-ts-mode-test--toc-current-line) "fun: f"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--jump ()
  "Jump selects the source window and moves point to the entry name."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 5)
    (f90-ts-toc-buffer-jump)
    (should (eq (window-buffer (selected-window)) src))
    (should (= (point) (f90-ts-mode-test--toc-pos "subroutine \\(s\\)" 1)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--jump-displays-hidden-source ()
  "Jump and preview work if the source buffer has no window."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (let ((other (generate-new-buffer "other")))
      (unwind-protect
          (progn
            (set-window-buffer (get-buffer-window src) other)
            (select-window (get-buffer-window f90-ts--toc-buffer-name))
            (f90-ts-mode-test--toc-goto-line 5)
            (f90-ts-toc-buffer-preview)
            (should (get-buffer-window src 'visible)))
        (kill-buffer other)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--dead-source-is-user-error ()
  "Jumping after the source buffer was killed is a user error."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90") (src2 "b.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (f90-ts-mode-test--toc-goto-line 1)
    (kill-buffer src)
    (should-error (f90-ts-toc-buffer-jump) :type 'user-error)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--mouse ()
  "Mouse bindings: click previews, double click and mouse-2 jump.
Uses a fake click event on the toc window."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (eq (lookup-key f90-ts-toc-mode-map [mouse-1])
                'f90-ts-toc-buffer-mouse-preview))
    (should (eq (lookup-key f90-ts-toc-mode-map [double-mouse-1])
                'f90-ts-toc-buffer-mouse-jump))
    (should (eq (lookup-key f90-ts-toc-mode-map [mouse-2])
                'f90-ts-toc-buffer-mouse-jump))
    (should (eq (lookup-key f90-ts-toc-mode-map [mouse-3])
                'f90-ts-toc-buffer-context-menu))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (let ((event (f90-ts-mode-test--toc-click-event 2)))      ; "var: a"
      (f90-ts-toc-buffer-mouse-preview event)
      (should (eq (window-buffer (selected-window))
                  (get-buffer f90-ts--toc-buffer-name)))
      (should (= (with-current-buffer src (point))
                 (with-current-buffer src (f90-ts-mode-test--toc-pos "a, b"))))
      (f90-ts-toc-buffer-mouse-jump event)
      (should (eq (window-buffer (selected-window)) src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--click-on-empty-area ()
  "A click below the last entry does nothing and does not signal."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (select-window (get-buffer-window f90-ts--toc-buffer-name))
    (let ((event (f90-ts-mode-test--toc-click-event 99))
          (point-before (with-current-buffer src (point))))
      (f90-ts-toc-buffer-mouse-preview event)
      (should (= point-before (with-current-buffer src (point)))))))


;;;-----------------------------------------------------------------------------
;; life cycle: no pop-up, no stale timers

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--open-state ()
  "Opening shows the panel and starts the timer, focus stays in the source."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (get-buffer-window f90-ts--toc-buffer-name 'visible))
    (should (timerp f90-ts--toc-timer))
    ;; focus stays in the source buffer
    (should (eq (window-buffer (selected-window)) src))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--no-popup-after-window-closed ()
  "Edits must not re-open a panel whose window was closed."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (delete-window (get-buffer-window f90-ts--toc-buffer-name))
    (with-current-buffer src (insert "! edit\n"))
    (f90-ts--toc-tick)
    (should-not (get-buffer-window f90-ts--toc-buffer-name 'visible))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--focus-reopens ()
  "The focus command re-opens a closed panel and selects it."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (delete-window (get-buffer-window f90-ts--toc-buffer-name))
    (with-current-buffer src (f90-ts-toc-buffer-focus))
    (should (eq (window-buffer (selected-window))
                (get-buffer f90-ts--toc-buffer-name)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--quit-stops-everything ()
  "Quit kills the panel and the timer, and later ticks do not reopen it."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (with-current-buffer f90-ts--toc-buffer-name (f90-ts-toc-buffer-quit))
    (should-not (get-buffer f90-ts--toc-buffer-name))
    (should-not f90-ts--toc-timer)
    (with-current-buffer src (insert "! edit\n"))
    (f90-ts--toc-tick)
    (should-not (get-buffer-window f90-ts--toc-buffer-name 'visible))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--kill-buffer-cancels-timer ()
  "Killing the toc buffer cancels the timer."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (should (timerp f90-ts--toc-timer))
    (kill-buffer f90-ts--toc-buffer-name)
    (should-not f90-ts--toc-timer)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--single-timer ()
  "Opening repeatedly never leaves more than one timer behind."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (let ((count-before (cl-count-if
                         (lambda (tm) (eq (timer--function tm) #'f90-ts--toc-tick))
                         timer-idle-list)))
      (with-current-buffer src
        (f90-ts-toc-buffer-open)
        (f90-ts-toc-buffer-open)
        (f90-ts-toc-buffer-open))
      (should (= (1+ count-before)
                 (cl-count-if
                  (lambda (tm) (eq (timer--function tm) #'f90-ts--toc-tick))
                  timer-idle-list))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--tick-error-stops-timer ()
  "An error in the background update cancels the timer instead of repeating."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (let ((debug-on-error nil))
      (cl-letf (((symbol-function 'f90-ts--toc-update-visible)
                 (lambda (_) (error "Boom")))
                ((symbol-function 'message) #'ignore))
        (f90-ts--toc-tick)))
    (should-not f90-ts--toc-timer)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--open-in-wrong-mode ()
  "Opening the panel from a buffer not derived from `f90-ts-mode' fails."
  (with-temp-buffer
    (should-error (f90-ts-toc-buffer-open) :type 'user-error)))


;;;-----------------------------------------------------------------------------
;; Source window choice

(f90-ts-mode-test-defsub f90-ts-mode-test--toc--source-window-most-recent ()
  "With the source in several windows, the one selected last is used."
  (f90-ts-mode-test--toc-with-sources ((src "a.f90"))
    (with-current-buffer src (f90-ts-toc-buffer-open))
    (let* ((left  (selected-window))
           (mid   (split-window left nil 'right))
           (panel (get-buffer-window f90-ts--toc-buffer-name 'visible)))
      (set-window-buffer mid src)
      ;; select the right one last, then go to the panel
      (select-window left)
      (select-window mid)
      (select-window panel)
      (should (eq (f90-ts--toc-buffer-source-window src) mid))
      ;; the other way round
      (select-window mid)
      (select-window left)
      (select-window panel)
      (should (eq (f90-ts--toc-buffer-source-window src) left)))))


;;;-----------------------------------------------------------------------------
;; Real grammar

(defun f90-ts-mode-test--toc-real-tree (text)
  "Return the sparse toc tree which the real grammar builds for TEXT.
TEXT is parsed in a temporary `f90-ts-mode' buffer, the tree is the one of
`f90-ts--toc-tree-build'."
  (with-temp-buffer
    (insert text)
    (f90-ts-mode)
    (f90-ts--toc-tree-build)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-tree-text ()
  "Mock tree for the basic text is the tree of the real grammar."
  (should (equal (f90-ts-mode-test--toc-real-tree f90-ts-mode-test--toc-text)
                 f90-ts-mode-test--toc-text-tree)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-tree-iface-text ()
  "Mock tree for the interface text is the tree of the real grammar."
  (should (equal (f90-ts-mode-test--toc-real-tree f90-ts-mode-test--toc-iface-text)
                 f90-ts-mode-test--toc-iface-text-tree)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-tree-long-text ()
  "Mock tree for the text with long names is the tree of the real grammar."
  (should (equal (f90-ts-mode-test--toc-real-tree f90-ts-mode-test--toc-long-text)
                 f90-ts-mode-test--toc-long-text-tree)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-tree-doc-text ()
  "Tree for the text with doc comments is the tree of the real grammar."
  (should (equal (f90-ts-mode-test--toc-real-tree f90-ts-mode-test--toc-doc-text)
                 f90-ts-mode-test--toc-doc-text-tree)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-tree-nested-text ()
  "Tree for the text with nested procedures is the tree of the real grammar."
  (should (equal (f90-ts-mode-test--toc-real-tree f90-ts-mode-test--toc-nested-text)
                 f90-ts-mode-test--toc-nested-text-tree)))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-grammar ()
  "Build the toc buffer from a tree-sitter parsed source."
  (let ((src (generate-new-buffer "real.f90"))
        (f90-ts-toc-short-labels f90-ts-mode-test--toc-short-labels)
        (f90-ts-toc-show-variables t)
        (f90-ts-toc-abbreviate-labels t))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "module m\n  integer :: a\ncontains\n"
                    "  subroutine s\n    real :: x\n  end subroutine\n"
                    "end module\n")
            (f90-ts-mode)
            (f90-ts--toc-buffer-update src))
          (let ((lines (f90-ts-mode-test--toc-lines)))
            (should (cl-some (lambda (l) (string-match-p "contains" l)) lines))
            (should (cl-some (lambda (l) (string-match-p "\\`mod.*m\\'" l)) lines))
            (should (cl-some (lambda (l) (string-match-p "\\`\\s-*subr.*s\\'" l)) lines))))
      (f90-ts-mode-test--toc-cleanup (list src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-grammar-contains-nesting ()
  "Each `contains' separates only the entries of its own structure."
  (let ((src (generate-new-buffer "real2.f90"))
        (f90-ts-toc-short-labels f90-ts-mode-test--toc-short-labels)
        (f90-ts-toc-show-variables t)
        (f90-ts-toc-contains-label "---contains---"))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "module m\n  type t\n    integer :: i\n  contains\n"
                    "    procedure :: p\n  end type\n"
                    "contains\n  subroutine s\n  contains\n"
                    "    subroutine inner\n    end subroutine\n"
                    "  end subroutine\nend module\n")
            (f90-ts-mode)
            (f90-ts--toc-buffer-update src))
          ;; module: before s;  s: before inner;  the type has no entry
          ;; after its contains and must not add one
          (should (= 2 (cl-count-if
                        (lambda (l) (string-match-p "contains" l))
                        (f90-ts-mode-test--toc-lines)))))
      (f90-ts-mode-test--toc-cleanup (list src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-grammar-interfaces ()
  "Interface blocks with real parse trees."
  (let ((src (generate-new-buffer "real3.f90"))
        (f90-ts-toc-short-labels f90-ts-mode-test--toc-short-labels)
        (f90-ts-toc-show-variables t)
        (f90-ts-toc-show-interface-procedures t)
        (f90-ts-toc-abbreviate-labels t))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "module m\n"
                    "  type t\n    integer :: i\n  contains\n"
                    "    procedure :: p1\n  end type\n"
                    "  interface gen\n"
                    "    module procedure sub1\n    module procedure sub2, sub3\n"
                    "  end interface gen\n"
                    "  interface\n"
                    "    module subroutine fs()\n    end subroutine\n"
                    "  end interface\n"
                    "  abstract interface\n"
                    "    subroutine ab()\n    end subroutine\n"
                    "  end interface\n"
                    "  interface operator(+)\n    module procedure add\n  end interface\n"
                    "end module\n")
            (f90-ts-mode)
            (f90-ts--toc-buffer-update src))
          (should (equal (f90-ts-mode-test--toc-lines)
                         '("mod: m"
                           "  type: t" "    var: i"
                           "  ifc: gen"
                           "    proc: sub1" "    proc: sub2" "    proc: sub3"
                           "  ifc"
                           "    subr: fs"
                           "  ifc: abstract"
                           "    subr: ab"
                           "  ifc: operator(+)"
                           "    proc: add")))
          ;; type-bound procedure p1 is not listed
          (should-not (cl-some (lambda (l) (string-match-p "p1" l))
                               (f90-ts-mode-test--toc-lines)))
          ;; toggle: no interface lines and no procedure statements
          (let ((f90-ts-toc-show-interface-procedures nil))
            (f90-ts--toc-buffer-update src)
            (should (equal (f90-ts-mode-test--toc-lines)
                           '("mod: m" "  type: t" "    var: i")))))
      (f90-ts-mode-test--toc-cleanup (list src)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--toc--real-grammar-interfaces-hidden ()
  "Generic, unnamed, abstract and nested interfaces all follow the toggle."
  (let ((src (generate-new-buffer "real4.f90"))
        (f90-ts-toc-short-labels f90-ts-mode-test--toc-short-labels)
        (f90-ts-toc-show-variables t)
        (f90-ts-toc-abbreviate-labels t))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "module m\n"
                    "  interface gen\n    module procedure sub1\n  end interface gen\n"
                    "  interface\n    module subroutine fs()\n    end subroutine\n"
                    "  end interface\n"
                    "  abstract interface\n    subroutine ab()\n    end subroutine\n"
                    "  end interface\n"
                    "  interface operator(+)\n    module procedure add\n  end interface\n"
                    "contains\n"
                    "  subroutine s\n"
                    "    interface\n      subroutine ext()\n      end subroutine\n"
                    "    end interface\n"
                    "  end subroutine\n"
                    "end module\n")
            (f90-ts-mode))
          (let ((f90-ts-toc-show-interface-procedures t))
            (f90-ts--toc-buffer-update src)
            (should (equal (f90-ts-mode-test--toc-lines)
                           '("mod: m"
                             "  ifc: gen" "    proc: sub1"
                             "  ifc" "    subr: fs"
                             "  ifc: abstract" "    subr: ab"
                             "  ifc: operator(+)" "    proc: add"
                             "  ---contains---"
                             "  subr: s"
                             "    ifc" "      subr: ext"))))
          (let ((f90-ts-toc-show-interface-procedures nil))
            (f90-ts--toc-buffer-update src)
            ;; everything inside interface blocks is hidden, also in a procedure
            (should (equal (f90-ts-mode-test--toc-lines)
                           '("mod: m"
                             "  ---contains---"
                             "  subr: s")))))
      (f90-ts-mode-test--toc-cleanup (list src)))))


(provide 'toc-tests)

;;; toc-tests.el ends here
