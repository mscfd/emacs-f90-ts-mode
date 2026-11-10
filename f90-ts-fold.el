;;; f90-ts-fold.el --- Hideshow and outline support for f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein <mscfd@gmx.net>

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

;; The module provides support for `hs-minor-mode' (Emacs 29+) and for
;; `outline-minor-mode' Emacs 30+ in `f90-ts-mode'.
;;
;; A foldable node hides everything from the end of its header line to the end
;; of the last line before the closing statement, so that the "end ..."
;; line stays visible.
;;
;; There is a minor difference in how hideshow is setup:
;;   Emacs 29/30:  entry in `hs-special-modes-alist'
;;   Emacs 31+:    buffer-local `hs-*' variables (the alist is obsolete)
;; (Note that f90-ts-mode itself requires Emacs 29+).
;;
;; Enable with (add-hook 'f90-ts-mode-hook #'hs-minor-mode).
;; `f90-ts-mode' must call `f90-ts-hideshow-setup' at the end of its body.
;;
;; Support for outline is requires setting `treesit-outline-predicate'.
;; This should be done before `treesit-major-mode-setup' is invoked.
;;
;; Enable with (add-hook 'f90-ts-mode-hook #'outline-minor-mode).
;; `f90-ts-mode' must set `treesit-outline-predicate' before calling
;; `treesit-major-mode-setup', and call `f90-ts-outline-setup' after it.

;;

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'hideshow)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)

;; provides f90-ts--thing-defun-regexp-pred
(require 'f90-ts-thing)


;;;-----------------------------------------------------------------------------
;; hideshow

(defconst f90-ts--hs-block-specs
  '(("program"                    named)
    ("module"                     named)
    ("submodule"                  named)
    ("subroutine"                 named)
    ("function"                   named)
    ("module_procedure"           named)
    ("derived_type_definition"    named)
    ("enum"                       named)
    ("enumeration_type"           named)
    ("interface"                  named)
    ("preproc_if"                 named)
    ("preproc_ifdef"              named)
    ("do_loop"                    types "do_statement")
    ("block_construct"            types "block")
    ("associate_statement"        types "associate" "association_list")
    ("select_case_statement"      types "select" "case" "selectcase" "selector")
    ("select_type_statement"      types "select" "type" "selecttype" "selector")
    ("select_rank_statement"      types "select" "rank" "selectrank" "selector")
    ("coarray_critical_statement" types "critical" "argument_list")
    ("coarray_team_statement"     types "change" "team" "argument_list")
    ("if_statement"               sentinel "then" "end_if_statement")
    ("where_statement"            sentinel "parenthesized_expression" "end_where_statement")
    ("forall_statement"           sentinel ")" "end_forall_statement"))
  "Foldable node types and how to determine the end of their header.

It maps node type to how to find the end of the header line (the fortran
grammar has no named node for this line in most cases unfortunately):
  (TYPE named)              header ends with first named child
  (TYPE types T1 T2 ...)    header consists of leading children of these
                            types (an optional block label is skipped)
  (TYPE sentinel S CLOSE)   header ends with first child of type S;
                            only a block form if last child has type CLOSE
                            (IF/WHERE/FORALL also have an inline form)

Note that header lines might be spread across several lines, like in complex
logical expressions in if or while conditions.")


(defun f90-ts--hs-header-end-types (node types)
  "Return end position of the header of NODE made of children of TYPES.
An optional leading `block_label_start_expression' is skipped."
  (let* ((child0 (treesit-node-child node 0))
         (child-start (if (equal (treesit-node-type child0) "block_label_start_expression")
                          (treesit-node-next-sibling child0)
                        child0)))
    (cl-loop
     for child = child-start then (treesit-node-next-sibling child)
     while (and child
                (member (treesit-node-type child) types))
     ;; save end position of child if it satisfies condition and before
     ;; going next child, which might fail it
     for end = (treesit-node-end child)
     finally return end)))


(defun f90-ts--hs-header-end-sentinel (node sentinel)
  "Return end position of the first child of NODE with type SENTINEL."
  (when-let* ((children (treesit-node-children node))
              (child-first (seq-find
                            (lambda (n) (f90-ts--node-type-p n sentinel))
                            children)))
    (treesit-node-end child-first)))


(defun f90-ts--hs-header-end (node)
  "Return end position of the header of NODE, or nil if NODE is not a block."
  (when-let* ((spec (assoc (treesit-node-type node)
                           f90-ts--hs-block-specs)))
    (pcase (cdr spec)
      (`(named)
       (when-let* ((header (treesit-node-child node 0 t)))
         (treesit-node-end header)))
      (`(types . ,types)
       (f90-ts--hs-header-end-types node types))
      (`(sentinel ,sentinel ,close)
       (let ((last (treesit-node-child node -1)))
         (when (equal (treesit-node-type last) close)
           (f90-ts--hs-header-end-sentinel node sentinel)))))))


(defun f90-ts--hs-region (node)
  "Return the region (BEG . END) of NODE that hideshow should hide, or nil.
BEG is the end of the header line, END the end of the last line before the
closing statement."
  (when-let* ((header-end (f90-ts--hs-header-end node))
              (close (treesit-node-child node -1))
              (beg (save-excursion
                     (goto-char header-end)
                     (skip-chars-backward " \t\n\r")
                     (pos-eol)))
              (end (save-excursion
                     (goto-char (treesit-node-start close))
                     (forward-line -1)
                     (pos-eol))))
    (and (< beg end) (cons beg end))))


(defun f90-ts--hs-block-node-starting-at (pos)
  "Return the outermost block node at POS that has a fold region."
  (let ((node-at (treesit-node-at pos)))
    (cl-loop
     with result = nil
     for node = node-at then (treesit-node-parent node)
     while (and node
                (= (treesit-node-start node) pos))
     when (f90-ts--hs-region node)
     do (setq result node)
     finally return result)))


(defun f90-ts--hs-block-node-enclosing (pos)
  "Return the innermost block node enclosing POS.
The node must start at or before POS and end after POS."
  (let ((node-at (treesit-node-at pos)))
    (treesit-parent-until
     node-at
     (lambda (n)
       (and (<= (treesit-node-start n) pos)
            (> (treesit-node-end n) pos)
            (f90-ts--hs-region n)))
     t)))


(defun f90-ts--hs-find-node-descend (node pos bound pred)
  "Find node witin NODE starting in [POS, BOUND) and satisfying PRED.
Return the first node found traversing the tree depth-first and
left-to-right."
  (if (and (>= (treesit-node-start node) pos)
           (funcall pred node))
      node
    (cl-loop
     for child in (treesit-node-children node)
     while (< (treesit-node-start child) bound)
     when (> (treesit-node-end child) pos)
     thereis (f90-ts--hs-find-node-descend child pos bound pred))))


(defun f90-ts--hs-find-node (pos bound pred)
  "Return first node starting in [POS, BOUND) that satisfies PRED.
Nodes are visited in pre-order (parents first).  The search starts at the
smallest node spanning [POS, BOUND], extended upwards over ancestors that
also start at POS, instead of at the root."
  (when (< pos bound)
    (let* ((node (treesit-node-descendant-for-range
                  (treesit-buffer-root-node) pos bound))
           (top (or (treesit-parent-while
                     node
                     (lambda (n) (= (treesit-node-start n) pos)))
                    node)))
      (f90-ts--hs-find-node-descend top pos bound pred))))


(defun f90-ts--hs-next-block-or-comment (pos bound comments)
  "Return first block node in [POS, BOUND].
If COMMENTS is non-nil, then also consider comment nodes."
  (when-let* ((node (f90-ts--hs-find-node
                     pos bound
                     (lambda (n)
                       (or (f90-ts--hs-region n)
                           (and comments
                                (f90-ts--node-type-p n "comment")))))))
    (and (<= (treesit-node-start node)
             bound)
         node)))


;; block start:
;; - a "block start" is a position where the predicate returns non-nil
;;   and sets the match data (group 0 and 1 = start ... start+1)
;; - `hs-forward-sexp-function' is called at the block start and must move
;;   to the END of the region to hide (not the end of the node!)
;; - the adjust function receives the match end and returns a position on
;;   the last header line; hideshow hides from the end of that line

(defun f90-ts--hs-set-match (beg)
  "Set match data to mark a block start at BEG."
  (let ((end (min (1+ beg) (point-max))))
    (set-match-data (list beg end beg end))))


(defun f90-ts--hs-looking-at-block-start-p ()
  "Return non-nil if point is at the start of a foldable block."
  (when (f90-ts--hs-block-node-starting-at (point))
    (f90-ts--hs-set-match (point))
    t))


(defun f90-ts--hs-find-block-beginning ()
  "Move to the start of the innermost foldable block enclosing point.
Return non-nil and set match data if there is one."
  (when-let* ((node (f90-ts--hs-block-node-enclosing (point))))
    ;; The innermost node may not start at the position of the outermost node
    ;; starting there; normalize so that the predicate agrees.
    (let ((start (treesit-node-start node)))
      (goto-char start)
      (f90-ts--hs-set-match start)
      t)))


(defun f90-ts--hs-find-next-block (_regexp bound comments)
  "Move to the start of the next block or, if COMMENTS, comment before BOUND.
The match data is set as documented for `hs-find-next-block-function': group 1
is a block start, group 2 a comment start.  Point is left after the first
character of the found node."
  (unless comments
    (forward-comment (point-max)))
  (when-let* ((node (f90-ts--hs-next-block-or-comment (point) bound comments)))
    (let* ((beg (treesit-node-start node))
           (end (min (1+ beg) (point-max)))
           (comment-p (equal (treesit-node-type node) "comment")))
      (when (< beg bound)
        (goto-char end)
        (set-match-data (if comment-p
                            (list beg end nil nil beg end)
                          (list beg end beg end)))
        t))))


(defun f90-ts--hs-forward-sexp (&optional _arg)
  "Move from a block start to the end of the region hideshow hides."
  (let* ((node (or (f90-ts--hs-block-node-starting-at (point))
                   (signal 'scan-error (list "Not at a block start" (point) (point)))))
         (region (f90-ts--hs-region node)))
    (goto-char (cdr region))))


(defun f90-ts--hs-adjust-block-beginning (pos)
  "Return a position on the last header line of the block whose match ends at POS."
  (let ((node (f90-ts--hs-block-node-starting-at (1- pos))))
    (if-let* ((region (and node (f90-ts--hs-region node))))
        (car region)
      pos)))


;;;-----------------------------------------------------------------------------
;; setup

(defconst f90-ts--hs-block-start-regexp "\\_<\\w"
  "Block start regexp for hideshow.
Blocks are found through the parse tree, so this only has to be a valid regexp
that matches at the start of a block; hideshow concatenates it with the comment
start regexp in some places.")


;; emacs 29/30: variable declared by hideshow
(defvar hs-special-modes-alist)


(defun f90-ts--hs-register-alist-prior-31 ()
  "Register `f90-ts-mode' in `hs-special-modes-alist'.
This is for Emacs versions prior to Emacs 31."
  (with-suppressed-warnings ((obsolete hs-special-modes-alist))
    (setf (alist-get 'f90-ts-mode hs-special-modes-alist)
        `((,f90-ts--hs-block-start-regexp 0)
          ,regexp-unmatchable
          "!"
          f90-ts--hs-forward-sexp
          f90-ts--hs-adjust-block-beginning
          f90-ts--hs-find-block-beginning
          f90-ts--hs-find-next-block
          f90-ts--hs-looking-at-block-start-p))))


(defun f90-ts--hs-set-locals-31-plus ()
  "Set hideshow buffer-local variables.
This is for Emacs 31+."
  (cl-loop
   for (variable . value) in
   `((hs-block-start-regexp . ,f90-ts--hs-block-start-regexp)
     (hs-block-start-mdata-select . 0)
     (hs-block-end-regexp . ,regexp-unmatchable)
     (hs-c-start-regexp . "!")
     (hs-forward-sexp-function . f90-ts--hs-forward-sexp)
     (hs-adjust-block-beginning-function . f90-ts--hs-adjust-block-beginning)
     (hs-find-block-beginning-function . f90-ts--hs-find-block-beginning)
     (hs-find-next-block-function . f90-ts--hs-find-next-block)
     (hs-looking-at-block-start-predicate . f90-ts--hs-looking-at-block-start-p))
   do (set (make-local-variable variable) value)))


;; executed once at load time to populate `hs-special-modes-alist'
(when (< emacs-major-version 31)
  (require 'hideshow)
  (f90-ts--hs-register-alist-prior-31))


(defun f90-ts-hideshow-setup ()
  "Set up `hs-minor-mode' for the current `f90-ts-mode' buffer."
  (setq-local comment-start (or comment-start "! ")
              comment-end   (or comment-end ""))

  (when (>= emacs-major-version 31)
    (f90-ts--hs-set-locals-31-plus)))


;;;-----------------------------------------------------------------------------

(defvar f90-ts--outline-predicate f90-ts--thing-defun-regexp-pred
  "Predicate matching outline headings in `f90-ts-mode'.
Either a regexp matched against the node type or a cons (REGEXP . FUNCTION).
Thing symbols and `or'/`not' forms are avoided so that the fallbacks
for Emacs 29 can handle it.  See module `f90-ts-workaround'.")


(defun f90-ts--outline-level ()
  "Return the depth of the current outline heading.

This replaces the generic `treesit-outline-level', which determines the
heading node with `treesit-node-at' at `pos-bol'.  In this grammar, a
header-only statement (e.g. a bare \"interface\" line, or a \"program NAME\"
or \"module NAME\" line) absorbs its trailing newline into its own node
span, so `pos-bol' of the following line coincides exactly with that
statement's end position.  `treesit-node-at' then returns a leaf ending at
that boundary, rather than anything on the current line, which wrongly counts
the nesting depth by one for any heading immediately following such a
header-only line (like a subroutine declared as the first line inside an
interface block).

Instead, this finds the heading with `treesit-thing-at' at `pos-eol', the
same lookup `treesit-outline-search' itself uses to test `looking-at', which
is unambiguous because `pos-eol' sits inside the heading's own node."
  (when-let* ((node (or (f90-ts--thing-at (pos-eol) f90-ts--outline-predicate)
                  (f90-ts--thing-at (pos-bol) f90-ts--outline-predicate))))
    (cl-loop for n = node then (treesit-node-parent n)
             while n
             count (f90-ts--node-match-p n f90-ts--outline-predicate))))


(defun f90-ts--outline-heading-bol (node)
  "Return the line start of NODE if NODE is the first thing on its line.
Otherwise return nil."
  (save-excursion
    (goto-char (treesit-node-start node))
    (when (f90-ts--bol-to-point-blank-p)
      (line-beginning-position))))


(defun f90-ts--outline-heading-at-line-p ()
  "Return non-nil if the current line start with an outline heading.
Append an \"s\" to \"start\" in the first line, which is forbidden to me
by checkdoc (at least in Emacs 29)."
  (save-excursion
    (back-to-indentation)
    (when-let* ((node (f90-ts--thing-at (point) f90-ts--outline-predicate)))
      (= (treesit-node-start node) (point)))))


(defun f90-ts--outline-search (&optional bound move backward looking-at)
  "Search for the next outline heading in the syntax tree.
BOUND, MOVE, BACKWARD and LOOKING-AT are as described in
`outline-search-function'.  On success point is at the beginning of the
heading line and the match data span that line."
  (if looking-at
      (when (f90-ts--outline-heading-at-line-p)
        (set-match-data (list (line-beginning-position)
                              (line-end-position)))
        t)
    (let* ((origin (point))
           (lo (if backward (or bound (point-min)) origin))
           (hi (cond
                (backward (pos-eol))
                (bound (save-excursion
                         (goto-char bound)
                         (pos-eol)))
                (t (point-max))))
           (candidates
            (when (< lo hi)
              (cl-loop
               for node in (treesit-query-capture
                            (treesit-buffer-root-node 'fortran)
                            '((_) @n) lo hi t)
               for bol = (and (f90-ts--node-match-p node f90-ts--outline-predicate)
                              (f90-ts--outline-heading-bol node))
               when (and bol
                         (if backward
                             (and (< bol origin) (>= bol lo))
                           (and (>= bol origin)
                                (<= bol (or bound (point-max))))))
               collect bol)))
           (pos (when candidates
                  (if backward
                      (apply #'max candidates)
                    (apply #'min candidates)))))
      (cond
       (pos
        (goto-char pos)
        (set-match-data (list pos
                              (line-end-position)))
        t)
       (move
        (goto-char (if backward
                       (or bound (point-min))
                     (or bound (point-max))))
        nil)
       (t
        nil)))))


(defun f90-ts-outline-setup ()
  "Set up `outline-minor-mode' for the current `f90-ts-mode' buffer.
Note that `treesit-outline-predicate' might not exist on \"older\"
Emacs versions. Hence `f90-ts--outline-predicate' is provided."
  (setq-local outline-level #'f90-ts--outline-level)
  (setq-local outline-search-function #'f90-ts--outline-search)
  (when (boundp 'treesit-outline-predicate)
    (setq-local treesit-outline-predicate f90-ts--outline-predicate))
  (when (< emacs-major-version 30)
    (setq-local outline-search-function #'f90-ts--outline-search)))


;;;-----------------------------------------------------------------------------

(defun f90-ts-fold-setup ()
  "Set up outline and hideshow folding for the current `f90-ts-mode' buffer.
Must be called after `treesit-major-mode-setup'."
  (f90-ts-outline-setup)
  (f90-ts-hideshow-setup))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-fold)

;;; f90-ts-fold.el ends here
