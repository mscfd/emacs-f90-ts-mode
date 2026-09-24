;;; f90-ts-workaround.el --- Tree-sitter based Fortran 90 mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein <mscfd@gmx.net>
;; Maintainer: Martin Stein <mscfd@gmx.net>
;; URL: https://github.com/mscfd/emacs-f90-ts-mode
;; Keywords: languages, treesitter, fortran
;; Version: 0.4.0-snapshot
;; Package-Requires: ((emacs "29.1"))

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

;; Provides missing functions or patches related to tree-sitter
;; for `f90-ts-mode'.  Rather than checking tree-sitter and Emacs verions
;; the package relies on the usual bundling and only uses Emacs major version
;; number, currently.  Emacs 29 commonly ships with tree-sitter 0.20.x,
;; Emacs 30 usually links against tree-sitter 0.25 and Emacs 31 understands
;; tree-sitter 0.26.  Most of the bugs below are specific to 0.20.x and no
;; longer present on 30+, so most of this file dispatches to native
;; functions on Emacs 30+ and to workaround implementations on
;; Emacs 29.x.  There is just one bug in Emacs 30+ (or rather
;; tree-sitter 0.25+) in `treesit-node-parent', which needs patching.
;;
;; Three kinds of bugs are addressed:
;;
;; - The optional leading ampersand in continued lines is represented as
;;   a virtual (zero-width) token.  Around such virtual tokens, parent and
;;   sibling queries can fail, and `treesit-node-on' fails to return
;;   virtual nodes at all.  On Emacs 29.x this is worked around with
;;   advice on `treesit-node-on', `treesit-node-parent',
;;   `treesit-node-prev-sibling' and `treesit-node-next-sibling', falling
;;   back to an exhaustive top-down search of the syntax tree when the
;;   native function comes up empty or can otherwise fail.  On Emacs 30+,
;;   `treesit-node-parent' still needs a simpler (and much faster) patch
;;   for the same virtual nodes.
;;
;; - Field queries fail if a node has several children with the same
;;   field name behind an anonymous node (this happens for variable
;;   declarations and "declarator" fields after the anonymous node "::").
;;   This is worked around by dispatching, at load time, to a
;;   `treesit-query-capture'-based implementation instead of the native
;;   field-name accessors.
;;
;; - `treesit-navigate-thing', `treesit-beginning-of-thing' and
;;   `treesit-end-of-thing' do not exist yet on Emacs 29 or have a different
;;   interface.  This is resolved by `f90-ts--navigate-thing-29',
;;   `f90-ts--beginning-of-thing-29' and `f90-ts--end-of-thing-29'.
;;   Implementations do not mirror those of Emacs 30+, but are simpler (with
;;   respect to argument TACTIC).  Thus navigation in 29 is somewhat different.
;;
;; The public dispatch between native and fallback/patched
;; implementations (`f90-ts--field-name-p', `f90-ts--children-by-fields',
;; `f90-ts--beginning-of-thing', the `treesit-node-parent' advice, etc.)
;; is not defined here but in f90-ts-mode.el.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; The following are defined in f90-ts-mode.el, which requires this file.
;; Declared here so that byte-compiling this file standalone does not
;; warn about free variables/functions; at actual load and call time
;; f90-ts-mode.el has already defined them.
(defvar f90-ts--thing-defun-regexp-pred)
(defvar f90-ts--thing-procedure-regexp-pred)
(defvar f90-ts--thing-interface-regexp-pred)
(defvar f90-ts--thing-type-regexp-pred)

(declare-function f90-ts--node-type-match-p "f90-ts-mode")

;;;-----------------------------------------------------------------------------
;; auxiliary function for work-around and advices

(defun f90-ts--fortran-node-p (node)
  "Return non-nil if NODE belongs to the Fortran grammar."
  (and node
       (eq (treesit-parser-language
            (treesit-node-parser node))
           'fortran)))


(defun f90-ts--fortran-lang-p (parser-or-lang)
  "Return non-nil if PARSER-OR-LANG resolved to the Fortran grammar.
If PARSER-OR-LANG is nil, use the first parser of the current buffer."
  (let* ((parser (or parser-or-lang
                     (car (treesit-parser-list))))
         (lang (if (treesit-parser-p parser)
                   (treesit-parser-language parser)
                 parser)))
    (eq lang 'fortran)))


(defun f90-ts--node-virtual-p (node)
  "Return non-nil if NODE is virtual (has zero width)."
  (and node
       (= (treesit-node-start node)
          (treesit-node-end node))))

(defun f90-ts--proper-cover-of-region (beg end)
  "Return some node which properly covers BEG..END.
Start and end of node should be before BEG and after END, respectively.

This is used to restrict `treesit-induce-sparse-tree' for relatives of
a node or nodes at some region to a proper ascendant, which is as small
as possible."
  (treesit-node-on (max (point-min) (1- beg))
                   (min (point-max) (1+ end))))


(defun f90-ts--node-virtual-at (pos)
  "Query whether there is a virtual node at POS and return it if there is one.

This uses an expensive top-down tree search."
  ;; TODO: use a direct search, but avoid any sibling operations
  (let ((root (or (f90-ts--proper-cover-of-region pos pos)
                  (treesit-buffer-root-node 'fortran))))
  (caadr
   (treesit-induce-sparse-tree
    root
    (lambda (node)
      (and (= (treesit-node-start node) pos)
           (= (treesit-node-end node) pos)))))))


(defun f90-ts--node-child-index (node parent)
  "Return the index of NODE as a child of PARENT.
If NODE is not a child of PARENT, then return nil."
  (cl-loop
   for i below (treesit-node-child-count parent)
   when (treesit-node-eq
         (treesit-node-child parent i)
         node)
   return i))


(defun f90-ts--node-parent-by-search (node)
  "Find the parent of NODE by top-down searching the Fortran syntax tree.

This might look like a bad idea, but the core parent function of
tree-sitter 0.20.x fails to determine the parent in if NODE is in the
vicinity of a virtual node."
  ;; TODO: use a direct search, but avoid any sibling operations
  (when node
    (let ((root (or (f90-ts--proper-cover-of-region (treesit-node-start node)
                                                    (treesit-node-end node))
                    (treesit-buffer-root-node 'fortran))))
      (caadr
       (treesit-induce-sparse-tree
        root
        (lambda (n)
          (cl-loop
           for i below (treesit-node-child-count n)
           thereis
           (treesit-node-eq
            (treesit-node-child n i)
            node))))))))


(defun f90-ts--node-sibling-by-search (node direction &optional named)
  "Return NODE's sibling in DIRECTION.

DIRECTION must be either `prev' or `next'.  If NAMED is non-nil,
only return a named sibling.

Intended for older tree-sitter versions, where sibling and parent operations
fail in the vicinity of virtual nodes."
  (let ((parent (f90-ts--node-parent-by-search node)))
    (when parent
      (let ((count (treesit-node-child-count parent))
            (index
             (cl-loop
              for i below (treesit-node-child-count parent)
              when (treesit-node-eq
                    (treesit-node-child parent i)
                    node)
              return i)))
        (when index
          (let ((step (if (eq direction 'prev) -1 1))
                (i (if (eq direction 'prev)
                       (1- index)
                     (1+ index))))
            (cl-loop
             while (and (>= i 0) (< i count))
             for sibling = (treesit-node-child parent i)
             when (or (not named)
                      (treesit-node-check sibling 'named))
             return sibling
             do (setq i (+ i step)))))))))


(defun f90-ts--field-name-p-native (node field-name)
  "Return non-nil if the field name of NODE is FIELD-NAME.
Use the field name of NODE as a child of its parent."
  (string= (treesit-node-field-name node)
           field-name))


(defun f90-ts--field-name-p-query (node field-name)
  "Return non-nil if the field name of NODE is FIELD-NAME.
Use the field name of NODE as a child of its parent.

Work-around for older tree-sitter versions with bugs in the core."
  (cl-some
   (lambda (n) (treesit-node-eq n node))
   (treesit-query-capture
    (treesit-node-parent node)
    (format "(_ %s: _ @cap)" field-name)
    nil nil t)))


(defun f90-ts--field-names-any-p-native (node field-names)
  "Return non-nil if the field name of NODE is in FIELD-NAMES.
Use the field name of NODE as a child of its parent."
  (member (treesit-node-field-name node)
          field-names))


(defun f90-ts--field-names-any-p-query (node field-names)
  "Return non-nil if the field name of NODE is in FIELD-NAMES.
Use the field name of NODE as a child of its parent.

Work-around for older tree-sitter versions with bugs in the core."
  (let ((query
         (format
          "(_ %s: _ @cap)"
          (mapconcat #'identity field-names ": _ @cap) (_ "))))
    (cl-some
     (lambda (n) (treesit-node-eq n node))
     (treesit-query-capture
      (treesit-node-parent node)
      query
      nil nil t))))


(defun f90-ts--node-child-by-field-name-native (node field-name)
  "Return the first child of NODE with FIELD-NAME, or nil."
  (treesit-node-child-by-field-name node field-name))


(defun f90-ts--node-child-by-field-name-query (node field-name)
  "Return the first child of NODE with FIELD-NAME, or nil.

Work-around for older tree-sitter versions with bugs in the core."
  (car
   (last
    (treesit-query-capture
     node
     (format "(_ %s: _ @cap)" field-name)
     nil nil t))))


(defun f90-ts--children-by-fields-native (parent field-names)
  "Return direct children of PARENT whose field is in FIELD-NAMES."
  (cl-loop
   for i below (treesit-node-child-count parent)
   when (member (treesit-node-field-name-for-child parent i) field-names)
   collect (treesit-node-child parent i)))


(defun f90-ts--children-by-fields-query (parent field-names)
  "Return direct children of PARENT whose field is in FIELD-NAMES.

Work-around for older tree-sitter versions with bugs in the core."
  (treesit-query-capture
   parent
   (format "(%s %s)"
           (treesit-node-type parent)
           (mapconcat (lambda (f) (format "%s: (_) @cap" f)) field-names " "))
   nil nil t))


;; ---------------------------------------------------------------------------
;; Node queries, parents and siblings: workarounds for Emacs 29

(defun f90-ts--node-on-workaround-29 (orig beg end &optional parser-or-lang named)
  "Evaluate `treesit-node-on' using function ORIG.
Arguments BEG, END, PARSER-OR-LANG and NAMED are the same as
for `treesit-node-on'.
If BEG and END are equal and there is a virtual node at this point, older
tree-sitter versions fail to return this node.  The function makes an
exhaustive search for these nodes.
In general or if there are zero-width nodes, it just calls ORIG."
  (or (and (= beg end)
           (f90-ts--fortran-lang-p parser-or-lang)
           (f90-ts--node-virtual-at beg))
      (funcall orig beg end parser-or-lang named)))


(defun f90-ts--node-parent-workaround-29 (orig node)
  "Evaluate `treesit-node-parent' for NODE using function ORIG.
In the vicinity of virtual nodes, `treesit-node-parent' might fail
by returning nil instead of the parent of NODE.
If this is the case, make an exhaustive top-down search to find the parent."
  (let ((parent (funcall orig node)))
    (cond
     ((null node)
      nil)

     ((not (f90-ts--fortran-node-p node))
      parent)

     ((or parent
          (not (f90-ts--node-virtual-p node)))
      parent)

     (t
      (f90-ts--node-parent-by-search node)))))


(defun f90-ts--node-prev-sibling-workaround-29 (orig node &optional named)
  "Evaluate `treesit-node-prev-sibling' for NODE and NAMED using function ORIG.

If necessary, use an exhaustive top-down tree search to work around a
bug in older tree-sitter version."
  (if (f90-ts--fortran-node-p node)
      (f90-ts--node-sibling-by-search node 'prev named)
    (funcall orig node named)))


(defun f90-ts--node-next-sibling-workaround-29 (orig node &optional named)
  "Evaluate `treesit-node-next-sibling' for NODE and NAMED using function ORIG.

If necessary, use an exhaustive top-down tree search to work around a
bug in older tree-sitter version."
  (if (f90-ts--fortran-node-p node)
      (f90-ts--node-sibling-by-search node 'next named)
    (funcall orig node named)))


;;;-----------------------------------------------------------------------------
;;; Defun and thing: Emacs 29 fallbacks

(defun f90-ts--navigate-thing-29 (pos direction thing)
  "Navigate to next or previous THING from POS depending on DIRECTION.

Implementation for Emacs 29, where `treesit-navigate-thing' is not yet
available.  This function does not implemented the default tactic `nested'
from Emacs 30+, so it does not behave like that.  The tactic can be described
as `flat' (forget the tree hierarchy).  The `nested' tactic could probably
be implemented using `treesit-induce-sparse-tree' to navigate to siblings
and parents."
  (let* ((pattern
          (pcase thing
            ('defun     (car f90-ts--thing-defun-regexp-pred))
            ('procedure (car f90-ts--thing-procedure-regexp-pred))
            ('interface (car f90-ts--thing-interface-regexp-pred))
            ('type      (car f90-ts--thing-type-regexp-pred))))
         (captures
          (treesit-query-capture (treesit-buffer-root-node) '((_) @thing)))
         (matches
          (seq-filter
           (lambda (cap)
             (let ((node (cdr cap)))
               (and (f90-ts--node-type-match-p node pattern)
                    (if (> direction 0)
                        (> (treesit-node-start node) pos)
                      (< (treesit-node-start node) pos)))))
           captures))
         (node-thing
          (if (> direction 0)
              (cdar matches)
            (cdar (last matches)))))
    (when node-thing
      (treesit-node-start node-thing))))


(defun f90-ts--beginning-of-thing-29 (thing &optional arg _tactic)
  "Execute `treesit-beginning-of-thing' with arguments THING and ARG.
THING is actually mapped to a pattern as required by Emacs 29."
  (let ((pattern (pcase thing
                   ('defun     (car f90-ts--thing-defun-regexp-pred))
                   ('procedure (car f90-ts--thing-procedure-regexp-pred))
                   ('interface (car f90-ts--thing-interface-regexp-pred))
                   ('type      (car f90-ts--thing-type-regexp-pred)))))
    (treesit-beginning-of-thing pattern arg)))


(defun f90-ts--end-of-thing-29 (thing &optional arg _tactic)
  "Execute `treesit-end-of-thing' with arguments THING and ARG.
THING is actually mapped to a pattern as required by Emacs 29."
  (let ((pattern (pcase thing
                   ('defun     (car f90-ts--thing-defun-regexp-pred))
                   ('procedure (car f90-ts--thing-procedure-regexp-pred))
                   ('interface (car f90-ts--thing-interface-regexp-pred))
                   ('type      (car f90-ts--thing-type-regexp-pred)))))
    (treesit-end-of-thing pattern arg)))


;;;-----------------------------------------------------------------------------
;; work-around for a parent query bug in emacs 30+ in conjunction with
;; virtual nodes (probably a bug in tree-sitter core)

;; For virtual zero-length nodes, `treesit-node-parent' fails. For
;;
;;    call something(arg1, &
;;                   arg2)
;;
;; the grammar injects a second ampersand of length zero right before "a".
;; `treesit-node-at' at a point before arg2, returns the virtual ampersand node.
;; But his node has no parent, but it has a previous and next sibling. A couple of
;; treesit functions (like treesit-thing-at, used for which-function-mode etc.)
;; using `treesit-node-at' followed by `treesit-parent-until' fail as a consequence.
;; To circumvent this, treesit-node-parent is patched by using previous or next
;; sibling and check, whether those have a proper parent. Which seems always the case.

(defun f90-ts--node-parent-workaround-30 (orig node)
  "If NODE is zero-width with no parent, walk siblings to find a real parent.
This is required for virtual ampersand continuation line nodes, for which
there is no parent.  Those nodes always have direct proper previous and next
siblings with the correct parent.  So walking is just one step in general.

ORIG is a reference to the original `treesit-node-parent' function used by
default."
  (let ((parent (funcall orig node)))
    (cond
     ((null node)
      ;; nothing to do if node is nil (original function returns nil in that case)
      ;; (do not try to query the parser with a nil node)
      nil)
     ((not (f90-ts--fortran-node-p node))
        ;; always use default function for non-fortran languages
        parent)
     ((or parent
          (< (treesit-node-start node) (treesit-node-end node)))
      ;; just use original function for nodes with a non-zero span
      parent)
     (t
      ;; try previous siblings first, then next siblings as fallback,
      ;; but for the intended case, one step to the prev-sibling resolves the issue
      (or (cl-loop for n = (treesit-node-prev-sibling node)
                   then (treesit-node-prev-sibling n)
                   while n
                   thereis (funcall orig n))
          (cl-loop for n = (treesit-node-next-sibling node)
                   then (treesit-node-next-sibling n)
                   while n
                   thereis (funcall orig n)))))))


(provide 'f90-ts-workaround)

;;; f90-ts-workaround.el ends here
