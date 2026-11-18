;;; f90-ts-workaround.el --- Workarounds for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provides missing functions or patches related to tree-sitter
;; for `f90-ts-mode'.  The bugs below are bugs of the tree-sitter core
;; library linked into Emacs, not of the Emacs tree-sitter Lisp layer:
;; Emacs 29 commonly ships with tree-sitter 0.20.x, Emacs 30 usually
;; links against tree-sitter 0.24/0.25 and Emacs 31 understands
;; tree-sitter 0.26, but not always.  Instead of using Emacs version,
;; the dispatch between native and workaround implementations probes at
;; load time using a small Fortran snippet (see `f90-ts--ts-core-probe').
;; If the grammar is not installed yet, the dispatch falls back to the
;; original Emacs-version based assumptions documented with each probe.
;;
;; Most of the bugs below are specific to 0.20.x and no longer present
;; on 0.22.x, so on those versions most of this file dispatches to the
;; native functions.  One bug is specific to 0.22.x
;; (`treesit-node-next-sibling' skips the node directly following a
;; virtual one), and one bug exists in 0.25.x and 0.26.x in
;; `treesit-node-parent', which needs a patch there.
;;
;; It also provides code to discover and manage different grammar variants.
;;
;; Four kinds of issues are addressed:
;;
;; - The optional leading ampersand in continued lines is represented as
;;   a virtual (zero-width) token.  Around such virtual tokens, parent and
;;   sibling queries can fail, and `treesit-node-on' fails to return
;;   virtual nodes at all (0.20.x and 0.22.x) while 0.25.x and 0.26.x returns
;;   them but fails on their parent query.  In addition, on 0.22.x
;;   `treesit-node-next-sibling' of a virtual node skips the node directly
;;   following it (for example failing on the construct keyword behind the
;;   virtual ampersand of a continuation between block label name and construct
;;   keyword), which breaks indentation of construct bodies.  Depending on
;;   what the probes find, `treesit-node-on', `treesit-node-parent',
;;   `treesit-node-prev-sibling' and `treesit-node-next-sibling' get advice
;;   falling back to an exhaustive top-down search of the syntax tree (0.20.x),
;;   only `treesit-node-parent' gets the simpler (and much faster) sibling-walk
;;   patch (0.25.x+), only `treesit-node-next-sibling' gets a patch based
;;   on the parent's child list (0.22.x), or none of them is patched
;;   (0.24.x).
;;
;; - Field queries fail on 0.20.x if a node has several children with the
;;   same field name behind an anonymous node (this happens for variable
;;   declarations and "declarator" fields after the anonymous node "::").
;;   This is worked around by dispatching, at load time, to a
;;   `treesit-query-capture'-based implementation instead of the native
;;   field-name accessors, but only if the probe shows the bug.
;;
;; - `treesit-navigate-thing', `treesit-beginning-of-thing' and
;;   `treesit-end-of-thing' do not exist yet on Emacs 29 or have a different
;;   interface.  This is resolved by `f90-ts--navigate-thing-29',
;;   `f90-ts--beginning-of-thing-29' and `f90-ts--end-of-thing-29'.
;;   Implementations do not mirror those of Emacs 30+, but are simpler (with
;;   respect to argument TACTIC).  Thus navigation in 29 is somewhat different.
;;   This is a Lisp API change, not a core library bug, so dispatch is done
;;   via `fboundp' on the Emacs 30 thing API.
;;
;; - Grammar versions requiring a switch.  Currently this is required for
;;   the `string_literal' variant and a grammar fix concerning `type_name'.
;;   There are also some keywords, which have been added, and might not yet
;;   be present in the installed grammar.
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


;;;-----------------------------------------------------------------------------
;;; Feature detection for the tree-sitter core library

;; The bugs worked around below live in the tree-sitter core library
;; linked into Emacs, not in the Emacs Lisp layer.  The dispatch is based on
;; probing the actual behavior on a small Fortran snippet at load time instead
;; of the Emacs version.  The observed behavior matrix:
;;
;; core lib  parent-of-virtual  sibling-of-virtual  node-on-virtual  next-sibling-of-virtual  field-name
;; 0.20.x    broken             broken              skipped          broken                   broken
;; 0.22.x    ok                 ok                  skipped          skips next node          ok
;; 0.24.x    ok                 ok                  not tested       ok                       ok
;; 0.25.x+   broken             ok                  returned         ok                       ok
;;
;; If the Fortran grammar is not installed at load time, probing is not
;; possible and the dispatch falls back to the original Emacs version
;; based dispatch (0.20.x on Emacs 29, 0.25.x on Emacs 30+).

(defvar f90-ts--ts-core-cache nil
  "Alist of probed tree-sitter core library behaviors.
An entry (KEY . RESULT) caches the probe for KEY.  RESULT is t or nil,
An `unavailable' result (grammar not installed) is never stored, so that
the probe is repeated once the grammar is available.")


(defconst f90-ts--ts-core-code-virtual
  "subroutine s()\n  call something(arg1, &\n                 arg2)\n\
end subroutine s\n"
  "Fortran snippet containing a virtual ampersand in a continued call.
The parser injects a zero-width ampersand right before the argument
after the line break, which the probes below operate on.")


(defconst f90-ts--ts-core-code-fields
  "module m\n  integer :: x1, x2, x3\nend module m\n"
  "Fortran snippet with several declarators behind the anonymous node \"::\".")


(defun f90-ts--ts-core-probe (key code probe)
  "Run PROBE on CODE and cache the result under KEY.
PROBE is called with no arguments in a temporary buffer holding CODE
with a Fortran parser.  It runs with the current buffer being the probe
buffer.  Return the cached result: t or nil, or `unavailable' if the
Fortran grammar is not installed or the probe signaled an error."
  (let ((entry (assq key f90-ts--ts-core-cache)))
    (if entry
        (cdr entry)
      (let ((result
             (condition-case nil
                 (with-temp-buffer
                   (insert code)
                   (treesit-parser-create 'fortran)
                   (funcall probe))
               (error 'unavailable))))
        (unless (eq result 'unavailable)
          (push (cons key result) f90-ts--ts-core-cache))
        result))))


(defun f90-ts--ts-core-virtual-node (node)
  "Return the first zero-width node below NODE, including NODE itself.
Search by child indices only, which works on all tree-sitter versions,
unlike sibling or parent operations in the vicinity of virtual nodes.
Return nil if the parse tree has no virtual node."
  (when node
    (if (f90-ts--node-virtual-p node)
        node
      (cl-loop
       for i below (treesit-node-child-count node)
       thereis (f90-ts--ts-core-virtual-node
                (treesit-node-child node i))))))


(defun f90-ts--ts-core-probe-virtual-node ()
  "Return the virtual node of the current probe buffer.
Signal an error if there is none (for example if a grammar change makes
the probe snippet parse without a virtual ampersand), so that the probe
reports `unavailable' and the fallback dispatch applies, instead of
silently reporting that the core library is fine."
  (or (f90-ts--ts-core-virtual-node (treesit-buffer-root-node))
      (error "Probe snippet contains no virtual node")))


(defun f90-ts--ts-core-parent-of-virtual-broken-p ()
  "Return non-nil if `treesit-node-parent' gives nil for a virtual node.
Broken in core 0.25.x (and presumably 0.26.x); 0.20.x also fails near
virtual nodes.  Surprisingly works on 0.22.6 and who knows which treesitter
versions."
  (f90-ts--ts-core-probe
   'parent-of-virtual
   f90-ts--ts-core-code-virtual
   (lambda ()
     (null (treesit-node-parent (f90-ts--ts-core-probe-virtual-node))))))


(defun f90-ts--ts-core-sibling-of-virtual-broken-p ()
  "Return non-nil if sibling queries fail in the vicinity of virtual nodes.
Broken in core 0.20.x, resolved in 0.22.x and later."
  (f90-ts--ts-core-probe
   'sibling-of-virtual
   f90-ts--ts-core-code-virtual
   (lambda ()
     (let ((virtual (f90-ts--ts-core-probe-virtual-node)))
       (or (null (treesit-node-prev-sibling virtual))
           (null (treesit-node-next-sibling virtual)))))))


(defun f90-ts--node-eq-p (a b)
  "Return non-nil if A is a node and identical to B."
  (and (treesit-node-p a)
       (treesit-node-eq a b)))


(defun f90-ts--ts-core-sibling-of-virtual-skips-p ()
  "Return non-nil if sibling queries skip nodes around a virtual one.
Tree-sitter core 0.22.x misbehaves on sibling queries in the presence
of virtual (zero-width) nodes in two ways: `treesit-node-next-sibling'
of a virtual node skips the node directly following it (typically the
construct keyword behind the virtual ampersand of a continued construct
opener), and sibling queries that should return a virtual node skip it:
the continuation machinery then fails to detect the ampersand and
indents continuation lines as ordinary statements.  Resolved in 0.24.x.
Core 0.20.x fails even harder (returns nil, see
`f90-ts--ts-core-sibling-of-virtual-broken-p'), which this probe also
reports as non-nil."
  (f90-ts--ts-core-probe
   'sibling-of-virtual-skips
   f90-ts--ts-core-code-virtual
   (lambda ()
     (let* ((virtual (f90-ts--ts-core-probe-virtual-node))
            (parent (treesit-node-parent virtual)))
       (and parent
            (cl-loop
             for i from 1 below (1- (treesit-node-child-count parent))
             for child = (treesit-node-child parent i)
             thereis (not (and (f90-ts--node-eq-p
                                (treesit-node-next-sibling child)
                                (treesit-node-child parent (1+ i)))
                               (f90-ts--node-eq-p
                                (treesit-node-prev-sibling child)
                                (treesit-node-child parent (1- i)))))))))))


(defun f90-ts--ts-core-node-on-skips-virtual-p ()
  "Return non-nil if `treesit-node-on' at POS does not return a virtual node.
0.20.x and 0.22.x skip virtual nodes here, while 0.25.x returns them."
  (f90-ts--ts-core-probe
   'node-on-skips-virtual
   f90-ts--ts-core-code-virtual
   (lambda ()
     (let* ((virtual (f90-ts--ts-core-probe-virtual-node))
            (pos (treesit-node-start virtual)))
       (not (f90-ts--node-eq-p
             (treesit-node-on pos pos 'fortran)
             virtual))))))


(defun f90-ts--ts-core-field-name-broken-p ()
  "Return non-nil if native field name accessors fail behind anonymous nodes.
Broken in core 0.20.x: `treesit-node-field-name' returns nil or a wrong
value for nodes whose field name is shadowed by several same-field
children behind an anonymous node (declarator fields after \"::\").
Resolved in 0.22.x and later."
  (f90-ts--ts-core-probe
   'field-name
   f90-ts--ts-core-code-fields
   (lambda ()
     ;; ground truth: every node captured by the query as a declarator
     ;; must also report "declarator" via the native accessor
     (let ((declarators (treesit-query-capture
                         (treesit-buffer-root-node)
                         "(_ declarator: (_) @cap)"
                         nil nil t)))
       (unless declarators
         (error "Probe snippet contains no declarator"))
       (cl-loop
        for node in declarators
        thereis (not (equal (treesit-node-field-name node)
                            "declarator")))))))


(defun f90-ts--ts-core-resolved (probed fallback)
  "Resolve a probe result PROBED with FALLBACK for `unavailable'.
Return PROBED unless it is `unavailable', in which case return FALLBACK."
  (if (eq probed 'unavailable)
      fallback
    probed))


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


(defun f90-ts--first-child-ending-at-or-after (node pos)
  "Return the index of the first child of NODE whose end is at or after POS.
Return the child count if there is none.  Children are ordered by
position, so the search is a binary search, and it relies on indices only,
not on (treesitter) cursors or siblings.  Note that indexed child access is
cheap."
  (cl-loop
   for (lo . hi) = (cons 0 (treesit-node-child-count node))
   then (let ((mid (/ (+ lo hi) 2)))
          (if (< (treesit-node-end (treesit-node-child node mid)) pos)
              (cons (1+ mid) hi)
            (cons lo mid)))
   while (< lo hi)
   finally return lo))


(defun f90-ts--virtual-node-at-pos (node pos)
  "Return the first zero-width node starting and ending at POS below NODE.
Search by child indices only, which works on all tree-sitter versions,
unlike cursor based walks in the vicinity of virtual nodes.  Only
children covering POS are visited, found by binary search."
  (if (and (= (treesit-node-start node) pos)
           (= (treesit-node-end node) pos))
      node
    (cl-loop
     for i from (f90-ts--first-child-ending-at-or-after node pos)
           below (treesit-node-child-count node)
     for child = (treesit-node-child node i)
     while (<= (treesit-node-start child) pos)
     thereis (f90-ts--virtual-node-at-pos child pos))))


(defun f90-ts--node-virtual-at (pos)
  "Query whether there is a virtual node at POS and return it if there is one.

On cores where parent queries are reliable (0.22.x and later), this
uses a direct search by child indices, because the cursor based walk
of `treesit-induce-sparse-tree' misreports nodes in the vicinity of
virtual nodes on 0.22.x (the search can return wrong phantom nodes
there).  On 0.20.x, where parent queries are broken but the cursor
walk works, the original exhaustive top-down tree search is used."
  (if (not (f90-ts--ts-core-resolved
            (f90-ts--ts-core-parent-of-virtual-broken-p)
            t))
      (when-let* ((root (treesit-buffer-root-node 'fortran)))
        (f90-ts--virtual-node-at-pos root pos))
    (let ((root (or (f90-ts--proper-cover-of-region pos pos)
                    (treesit-buffer-root-node 'fortran))))
      (caadr
       (treesit-induce-sparse-tree
        root
        (lambda (node)
          (and (= pos (treesit-node-start node))
               (= pos (treesit-node-end node)))))))))


(defun f90-ts--node-child-index (node parent)
  "Return the index of NODE as a child of PARENT.
If NODE is not a child of PARENT, then return nil."
  (cl-loop
   for i below (treesit-node-child-count parent)
   when (treesit-node-eq
         (treesit-node-child parent i)
         node)
   return i))


(defun f90-ts--sibling-in-parent (node parent direction named)
  "Return sibling of NODE in DIRECTION (`prev' or `next') among PARENT's children."
  (when-let* ((index (f90-ts--node-child-index node parent)))
    (let ((count (treesit-node-child-count parent))
          (step (if (eq direction 'prev) -1 1)))
      (cl-loop
       for i = (+ index step) then (+ i step)
       for sibling = (and (< -1 i count)
                          (treesit-node-child parent i))
       while sibling
       when (or (not named)
                (treesit-node-check sibling 'named))
       return sibling))))


(defun f90-ts--node-sibling-by-parent (node direction &optional named)
  "Return NODE's sibling in DIRECTION.

DIRECTION must be either `prev' or `next'.  If NAMED is non-nil,
only return a named sibling.

Intended for older tree-sitter versions, where sibling and parent operations
fail in the vicinity of virtual nodes."
  (when-let* ((parent (treesit-node-parent node)))
    (f90-ts--sibling-in-parent node parent direction named)))


(defun f90-ts--node-sibling-by-search (node direction &optional named)
  "Return NODE's sibling in DIRECTION.

DIRECTION must be either `prev' or `next'.  If NAMED is non-nil,
only return a named sibling.

Intended for older tree-sitter versions, where sibling and parent operations
fail in the vicinity of virtual nodes."
  (when-let* ((parent (f90-ts--node-parent-by-search node)))
    (f90-ts--sibling-in-parent node parent direction named)))


(defun f90-ts--parent-by-descent (from node beg end)
  "Return the parent of NODE (spanning BEG..END) by descending from FROM.
Only children covering BEG..END are visited, found by binary search,
so the cost is about tree depth times the logarithm of the width."
  (cl-loop
   for i from (f90-ts--first-child-ending-at-or-after from end)
         below (treesit-node-child-count from)
   for child = (treesit-node-child from i)
   while (<= (treesit-node-start child) beg)
   thereis (if (treesit-node-eq child node)
               from
             (f90-ts--parent-by-descent child node beg end))))


(defun f90-ts--node-parent-by-search (node)
  "Find the parent of NODE by searching the Fortran syntax tree top-down.

This might look like a bad idea, but the core parent function of
tree-sitter 0.20.x fails to determine the parent if NODE is in the
vicinity of a virtual node.  The search uses child indices only and
prunes by position, so it is cheap and independent of the buggy
parent and sibling operations."
  (when node
    (when-let* ((root (treesit-buffer-root-node 'fortran)))
      (f90-ts--parent-by-descent root node
                                 (treesit-node-start node)
                                 (treesit-node-end node)))))


(defun f90-ts--field-name-p-native (node field-name)
  "Return non-nil if the field name of NODE is FIELD-NAME.
Use the field name of NODE as a child of its parent."
  (string= (treesit-node-field-name node)
           field-name))


(defun f90-ts--field-captures (parent field-names &optional node)
  "Return nodes below PARENT captured as a value of one of FIELD-NAMES.
The captures are made by query, which is immune to the field name bug of
older cores, but they cover the whole subtree of PARENT, not only its
direct children.  If NODE is a non-empty node below PARENT, restrict the
query range to NODE, which makes the query cheap for large parents."
  (let ((query (mapconcat (lambda (f) (format "(_ %s: _ @cap)" f))
                          field-names " "))
        (beg (and node (treesit-node-start node)))
        (end (and node (treesit-node-end node))))
    (if (and beg end (< beg end))
        (treesit-query-capture parent query beg end t)
      (treesit-query-capture parent query nil nil t))))


(defun f90-ts--field-name-p-query (node field-name)
  "Return non-nil if the field name of NODE is FIELD-NAME.
Use the field name of NODE as a child of its parent.

Work-around for older tree-sitter versions with bugs in the core."
  (cl-some
   (lambda (n) (treesit-node-eq n node))
   (f90-ts--field-captures (treesit-node-parent node)
                           (list field-name)
                           node)))


(defun f90-ts--field-names-any-p-native (node field-names)
  "Return non-nil if the field name of NODE is in FIELD-NAMES.
Use the field name of NODE as a child of its parent."
  (member (treesit-node-field-name node)
          field-names))


(defun f90-ts--field-names-any-p-query (node field-names)
  "Return non-nil if the field name of NODE is in FIELD-NAMES.
Use the field name of NODE as a child of its parent.

Work-around for older tree-sitter versions with bugs in the core."
  (cl-some
   (lambda (n)
     (treesit-node-eq n node))
   (f90-ts--field-captures (treesit-node-parent node)
                           field-names
                           node)))


(defun f90-ts--node-child-by-field-name-native (node field-name)
  "Return the first child of NODE with FIELD-NAME, or nil."
  (treesit-node-child-by-field-name node field-name))


(defun f90-ts--node-child-by-field-name-query (node field-name)
  "Return the first child of NODE with FIELD-NAME, or nil.

Work-around for older tree-sitter versions with bugs in the core."
  (car (f90-ts--children-by-fields-query node
                                         (list field-name))))


(defun f90-ts--children-by-fields-native (parent field-names)
  "Return direct children of PARENT whose field is in FIELD-NAMES."
  (cl-loop
   for i below (treesit-node-child-count parent)
   when (member (treesit-node-field-name-for-child parent i) field-names)
   collect (treesit-node-child parent i)))


(defun f90-ts--children-by-fields-query (parent field-names)
  "Return direct children of PARENT whose field is in FIELD-NAMES.

Work-around for older tree-sitter versions with bugs in the core.  The
query captures nodes of the whole subtree, so keep only direct children
of PARENT.  Identity is checked against the child list, because parent
queries are unreliable on those cores."
  (let ((children (cl-loop
                   for i below (treesit-node-child-count parent)
                   collect (treesit-node-child parent i))))
    (cl-remove-if-not
     (lambda (n)
       (cl-member n children :test #'treesit-node-eq))
     (f90-ts--field-captures parent field-names))))


;; ---------------------------------------------------------------------------
;; Node queries, parents and siblings: workarounds for older tree-sitter
;; cores (0.20.x; installed only when the load-time probe shows the bug)

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


(defun f90-ts--node-next-sibling-workaround-22 (orig node &optional named)
  "Evaluate `treesit-node-next-sibling' for NODE and NAMED using function ORIG.

Tree-sitter core 0.22.x misbehaves on sibling queries in the presence
of virtual (zero-width) nodes: `treesit-node-next-sibling' of a virtual
node skips the node directly following it (typically the construct
keyword behind the virtual ampersand of a continued construct opener),
and sibling queries that should return a virtual node skip it (the
continuation machinery then fails to detect the trailing ampersand of
the previous line).  The parent query is reliable on this core, so
determine the sibling from the parent's child list instead, which
works on all tree-sitter versions.  In all other cases just call ORIG."
  (let ((sibling
         (and (f90-ts--fortran-node-p node)
              (treesit-node-parent node)
              (f90-ts--node-sibling-by-parent node 'next named))))
    (or sibling (funcall orig node named))))


(defun f90-ts--node-prev-sibling-workaround-22 (orig node &optional named)
  "Evaluate `treesit-node-prev-sibling' for NODE and NAMED using function ORIG.

Counterpart of `f90-ts--node-next-sibling-workaround-22' for the
previous sibling."
  (let ((sibling
         (and (f90-ts--fortran-node-p node)
              (treesit-node-parent node)
              (f90-ts--node-sibling-by-parent node 'prev named))))
    (or sibling (funcall orig node named))))


(defun f90-ts--indent-full-reparse ()
  "Delete and recreate the Fortran parser, forcing a full reparse.
Works around the incremental reparse divergence of tree-sitter core
0.22.x (missing keyword tokens injected by incremental reparses
after the blanking of leading continuation ampersands in the region indent
operation).  A full parse of the same buffer text does not contain the injected
tokens of the incremental parse.

The deleted parser may be the buffer's `treesit-primary-parser' (Emacs 30+),
on which `treesit-major-mode-setup' also registered
`treesit--font-lock-mark-ranges-to-fontify' as a notifier.  Deleting the
parser leaves that variable pointing at a deleted parser, and
`treesit--pre-redisplay' then signals \"This parser is deleted and cannot
be used\" at every subsequent buffer change, while the lost notifier stops
font-lock from catching up at all.  On Emacs 29 the notifiers
`treesit--font-lock-notifier' and `treesit--syntax-propertize-notifier'
are likewise registered on the parser object itself and are lost as well.
Therefore restore the primary parser (Emacs 30+) and the notifiers (Emacs 29
and 30+) onto the new parser."
  ;; cost: O(buffer size) per call, so region indent is quadratic on
  ;; affected cores, is there any better way to avoid this bug?
  ;; select the Fortran parser by hand, `treesit-parser-list' has no LANGUAGE
  ;; argument on Emacs 29
  (let ((old (cl-find
              'fortran (treesit-parser-list)
              :key #'treesit-parser-language)))
    (when old
      (treesit-parser-delete old)
      (let ((new (treesit-parser-create 'fortran)))
        ;; restore the primary parser wiring (Emacs 30+ only, there the
        ;; variable does not exist on Emacs 29)
        (when (and (boundp 'treesit-primary-parser)
                   (eq treesit-primary-parser old))
          (setq treesit-primary-parser new))
        ;; restore the notifiers that `treesit-major-mode-setup' had
        ;; registered on the deleted parser object, they are lost with
        ;; it (`treesit-parser-add-notifier' deduplicates).  Emacs 30+
        ;; registers `treesit--font-lock-mark-ranges-to-fontify' on the
        ;; primary parser; Emacs 29 registers `treesit--font-lock-notifier'
        ;; and `treesit--syntax-propertize-notifier' on every parser.
        (cl-loop
         for notifier in (if (boundp 'treesit-primary-parser)
                             '(treesit--font-lock-mark-ranges-to-fontify)
                           '(treesit--font-lock-notifier
                             treesit--syntax-propertize-notifier))
         when (fboundp notifier)
         do (treesit-parser-add-notifier new notifier))
        new))))


(defvar-local f90-ts--reparse-tick nil
  "Value of `buffer-chars-modified-tick' at the last forced full reparse.
While the buffer text is unchanged, the parse tree is the one of a full
parse already, so another forced reparse is useless.")


(defun f90-ts--indent-reparse-workaround-22 (&rest _args)
  "Force a full reparse before indenting a line (advice :before).
Tree-sitter core 0.22.x diverges on incremental reparses after the
bulk blanking of continuation ampersands in `f90-ts--indent-region-aux':
missing keyword tokens (observed: a virtual \"size\") are injected and
displace the ampersand of a continued line, so the region indent works
on a wrong tree.  `treesit--indent-1' is the common per-line entry of
`treesit-indent' and `treesit-indent-region', and fetches its nodes
after being called, so a full reparse here is always effective.
Only acts in `f90-ts-mode' buffers."
  (when (and (derived-mode-p 'f90-ts-mode)
             (not (eql f90-ts--reparse-tick (buffer-chars-modified-tick)))
             (f90-ts--fortran-lang-p nil))
    (f90-ts--indent-full-reparse)
    (setq f90-ts--reparse-tick (buffer-chars-modified-tick))))


(defvar f90-ts--indent-reparse-workaround-installed nil
  "Non-nil once the region reparse workaround has been dispatched.")


(defvar f90-ts--workarounds-22-active nil
  "Non-nil if the tree-sitter 0.22.x workarounds are active.
Resolved once by probe at the first `f90-ts-mode' activation.")


(defun f90-ts--node-leaf-by-children (node pos)
  "Return the leaf below NODE whose span strictly covers POS.
Descend by child indices into the child with start <= POS < end,
mirroring the well-behaved leaf resolution of `treesit-node-at' on
recent cores, which also skips virtual (zero-width) nodes.  Return nil
if no such leaf exists."
  (if (zerop (treesit-node-child-count node))
      (and (<= (treesit-node-start node) pos)
           (< pos (treesit-node-end node))
           node)
    (let* ((count (treesit-node-child-count node))
           (child
            (cl-loop for i below count
                     for c = (treesit-node-child node i)
                     when (and (<= (treesit-node-start c) pos)
                               (< pos (treesit-node-end c)))
                     return c)))
      (when child
        (f90-ts--node-leaf-by-children child pos)))))


(defun f90-ts--largest-node-starting-at (leaf bol root)
  "Ascend from LEAF to the largest node starting at BOL.
Stop before ROOT, mirroring the `treesit-parent-while' step of
`treesit--indent-1'.  BOL is the beginning-of-line position and ROOT
the root node of the parse tree."
  (car (last (cl-loop
              for node = leaf then (treesit-node-parent node)
              while (and node
                         (= bol (treesit-node-start node))
                         (not (treesit-node-eq node root)))
              collect node))))


(defun f90-ts--node-readout-same-p (a b)
  "Return non-nil if A and B have the same type and range.
`treesit-node-eq' cannot be used here: on tree-sitter 0.22.x, nodes
obtained via cursor based queries can carry corrupted type or range
readouts of the very same node, so identity holds while the readouts
differ.  The indent rules match on the readouts, so they have to be
compared here."
  (and (treesit-node-p a) (treesit-node-p b)
       (= (treesit-node-start a) (treesit-node-start b))
       (= (treesit-node-end a) (treesit-node-end b))
       (string= (treesit-node-type a) (treesit-node-type b))))


(defun f90-ts--indent-function-workaround-22 (node parent bol)
  "Workaround wrapper for `treesit-indent-function'.
Tree-sitter core 0.22.x misresolves `treesit-node-at' near virtual
zero-width nodes: the leaf found for a line start can be a wrong
node (observed: a \"size\" node covering a comment instead of the
comment), so `treesit--indent-1' passes a wrong NODE to the indent
rules and, e.g., the comment rules never fire.  Re-derive NODE (the
largest node starting at BOL) and PARENT by child indices, which work
on all versions, and evaluate the rules on the corrected nodes.  Fall
back to the passed values whenever the re-derivation is not possible
or does not differ."
  (when-let* (((and node
                    bol
                    (f90-ts--fortran-node-p node)))
              (root (treesit-buffer-root-node 'fortran))
              (leaf (f90-ts--node-leaf-by-children root bol))
              (fixed (f90-ts--largest-node-starting-at leaf bol root))
              ((not (f90-ts--node-readout-same-p fixed node))))
    (setq node fixed
          parent (treesit-node-parent fixed)))
  (treesit-simple-indent node parent bol))


(defun f90-ts--install-indent-reparse-workaround ()
  "Install the region reparse workaround depending on the probed behavior.
Called from `f90-ts-mode' rather than dispatched at load time, so that
the probe is repeated once the grammar is installed, if it was not yet
available at load time (`unavailable' probe results are not cached)."
  (unless f90-ts--indent-reparse-workaround-installed
    (setq f90-ts--indent-reparse-workaround-installed t
          f90-ts--workarounds-22-active
          ;; Same gate as for the sibling advice in the load time
          ;; dispatch: the sibling skip probe also reports non-nil for
          ;; 0.20.x (where sibling queries fail entirely), but the
          ;; reparse and indent wrapper workarounds below rely on a
          ;; reliable parent query and well-behaved node readouts, which
          ;; only hold on 0.22.x.  So require the broken probe to be nil.
          (and (f90-ts--ts-core-resolved
                (f90-ts--ts-core-sibling-of-virtual-skips-p)
                nil)
               (not (f90-ts--ts-core-resolved
                     (f90-ts--ts-core-sibling-of-virtual-broken-p)
                     (< emacs-major-version 30)))))
    ;; The sibling skip probe identifies tree-sitter 0.22.x (its result
    ;; is already cached from the load time dispatch).  On those cores
    ;; the incremental reparse after the bulk blanking of continuation
    ;; ampersands in `f90-ts--indent-region-aux' can inject missing
    ;; keyword tokens, which the region indent then operates on.
    (when f90-ts--workarounds-22-active
      ;; tree-sitter 0.22.x: force a full reparse before each line
      ;; indentation.  On Emacs 30 `treesit--indent-1' is the common
      ;; per-line entry of `treesit-indent' and `treesit-indent-region';
      ;; on Emacs 29 there is no `treesit--indent-1', patch the line
      ;; and region entries instead.
      (if (fboundp 'treesit--indent-1)
          (advice-add #'treesit--indent-1 :before
                      #'f90-ts--indent-reparse-workaround-22)
        (advice-add #'treesit-indent :before
                    #'f90-ts--indent-reparse-workaround-22)
        (advice-add #'treesit-indent-region :before
                    #'f90-ts--indent-reparse-workaround-22))))
  ;; per buffer: route indent rule evaluation through the wrapper that
  ;; re-derives the rule node when `treesit-node-at' misresolves it
  ;; near virtual nodes (tree-sitter 0.22.x)
  (when f90-ts--workarounds-22-active
    (setq-local treesit-indent-function
                #'f90-ts--indent-function-workaround-22)))


;;;-----------------------------------------------------------------------------
;;; Defun and thing: Emacs 29 fallbacks

(defun f90-ts--navigate-thing-window (pattern pos forward beg end)
  "Return the nearest node matching PATTERN from POS within BEG..END.
FORWARD selects the first node (in tree order) starting after POS,
otherwise the last node starting before POS.  Only nodes intersecting
BEG..END are queried.  In the backward case, return nil if the best node
starts before BEG, because a nearer one might have been missed outside
the window (unless BEG is the start of the buffer)."
  (let ((matches
         (cl-loop
          for cap in (treesit-query-capture
                      (treesit-buffer-root-node) '((_) @thing) beg end)
          for node = (cdr cap)
          when (and (f90-ts--node-type-match-p node pattern)
                    (if forward
                        (> (treesit-node-start node) pos)
                      (< (treesit-node-start node) pos)))
          collect node)))
    (if forward
        (car matches)
      (when-let* ((node (car (last matches))))
        (and (or (<= beg (point-min))
                 (>= (treesit-node-start node) beg))
             node)))))


(defun f90-ts--navigate-thing-29 (pos direction thing)
  "Navigate to next or previous THING from POS depending on DIRECTION.

Implementation for Emacs 29, where `treesit-navigate-thing' is not yet
available.  This function does not implemented the default tactic `nested'
from Emacs 30+, so it does not behave like that.  The tactic can be described
as `flat' (forget the tree hierarchy).  The `nested' tactic could probably
be implemented using `treesit-induce-sparse-tree' to navigate to siblings
and parents.

The query is restricted to a window around POS, which is doubled until a
match is found or the window covers the buffer, so the cost depends on
the distance to the result and not on the buffer size."
  (let ((pattern
         (pcase thing
           ('defun     (car f90-ts--thing-defun-regexp-pred))
           ('procedure (car f90-ts--thing-procedure-regexp-pred))
           ('interface (car f90-ts--thing-interface-regexp-pred))
           ('type      (car f90-ts--thing-type-regexp-pred))))
        (forward (> direction 0)))
    (cl-loop
     for width = 4096 then (* 2 width)
     for beg = (if forward
                   (min (1+ pos) (point-max))
                 (max (point-min) (- pos width)))
     for end = (if forward
                   (min (point-max) (+ pos width))
                 (min pos (point-max)))
     for node = (f90-ts--navigate-thing-window pattern pos forward beg end)
     when node return (treesit-node-start node)
     until (if forward (>= end (point-max)) (<= beg (point-min))))))


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
;;; Thing predicates:
;;  `treesit-node-match-p': not available before Emacs 30
;;  `treesit-thing-at': not available before Emacs 31

(defun f90-ts--node-match-p-fallback (node predicate &optional _ignore-missing)
  "Return non-nil if NODE matches PREDICATE.
Fallback for `treesit-node-match-p'.  Supports the predicate forms
usable on Emacs 29: a regexp matched against the node type, a function
called with NODE, and a cons cell (REGEXP . FUNCTION) requiring both.
Thing symbols are not supported."
  (and node
       (cond
        ((stringp predicate)
         (string-match-p predicate (treesit-node-type node)))
        ((and (consp predicate)
              (stringp (car predicate)))
         (and (string-match-p (car predicate) (treesit-node-type node))
              (or (null (cdr predicate))
                  (funcall (cdr predicate) node))))
        ((functionp predicate)
         (funcall predicate node))
        (t
         (error "Unsupported tree-sitter predicate: %S" predicate)))))


(defun f90-ts--thing-at-fallback (pos predicate &optional _strict)
  "Return the smallest node at POS matching PREDICATE, or nil.
Fallback for `treesit-thing-at': take the leaf node at POS and walk up
through its ancestors until one matches.
The STRICT argument is not implemented and ignored."
  (cl-loop
   for n = (treesit-node-at pos)
   then (treesit-node-parent n)
   while n
   when (f90-ts--node-match-p n predicate)
   return n))


(defalias 'f90-ts--node-match-p
  (if (fboundp 'treesit-node-match-p)
      #'treesit-node-match-p
    #'f90-ts--node-match-p-fallback)
  "Return non-nil if NODE matches PREDICATE.
Native `treesit-node-match-p' if available, else a regexp/function-only
fallback.")


(defalias 'f90-ts--thing-at
  (if (fboundp 'treesit-thing-at)
      #'treesit-thing-at
    #'f90-ts--thing-at-fallback)
  "Return the smallest node at POS matching a predicate or thing.
Native `treesit-thing-at' if available, else a fallback built on
`treesit-node-at' and `treesit-node-parent'.")


;;;-----------------------------------------------------------------------------
;; work-around for a parent query bug of tree-sitter core 0.25.x in
;; conjunction with virtual nodes

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
      (or (cl-loop
           for n = (treesit-node-prev-sibling node)
           then (treesit-node-prev-sibling n)
           while n
           thereis (funcall orig n))
          (cl-loop
           for n = (treesit-node-next-sibling node)
           then (treesit-node-next-sibling n)
           while n
           thereis (funcall orig n)))))))


;;;-----------------------------------------------------------------------------
;; defalias and advices

(defun f90-ts--ts-core-field-name-query-p ()
  "Return non-nil if field names should be determined by query.
Probes the core library; if probing is not possible, resolve by Emacs version
number (assume it is broken on on Emacs 29)."
  (f90-ts--ts-core-resolved
   (f90-ts--ts-core-field-name-broken-p)
   (< emacs-major-version 30)))


(defalias 'f90-ts--field-name-p
  (if (f90-ts--ts-core-field-name-query-p)
      #'f90-ts--field-name-p-query
    #'f90-ts--field-name-p-native)
  "Return non-nil if the field name of NODE is FIELD-NAME.
Use the field name of NODE as a child of its parent.

Uses the native `treesit-node-field-name' unless the core library
probe shows the field name bug, in which case a `treesit-query-capture'
based implementation is used (virtual nodes trigger a bug and resolve
to nil there).  See `f90-ts--ts-core-field-name-broken-p'.")


(defalias 'f90-ts--field-names-any-p
  (if (f90-ts--ts-core-field-name-query-p)
      #'f90-ts--field-names-any-p-query
    #'f90-ts--field-names-any-p-native)
  "Return non-nil if the field name of NODE is in FIELD-NAMES.
Use the field name of NODE as a child of its parent.

Uses the native `treesit-node-field-name' unless the core library
probe shows the field name bug, in which case a `treesit-query-capture'
based implementation is used (virtual nodes trigger a bug and resolve
to nil there).  See `f90-ts--ts-core-field-name-broken-p'.")


;; currently not used, but keep it as failure of node-child-by-field-name
;; are possible, but not for the types of nodes where it is currently used
(defalias 'f90-ts--node-child-by-field-name
  (if (f90-ts--ts-core-field-name-query-p)
      #'f90-ts--node-child-by-field-name-query
    #'f90-ts--node-child-by-field-name-native)
  "Return the first child of NODE with FIELD-NAME, or nil

Uses the native `treesit-node-child-by-field-name' unless the core
library probe shows the field name bug, in which case a
`treesit-query-capture' based implementation is used.  See
`f90-ts--ts-core-field-name-broken-p'.")


(defalias 'f90-ts--children-by-fields
  (if (f90-ts--ts-core-field-name-query-p)
      #'f90-ts--children-by-fields-query
    #'f90-ts--children-by-fields-native)
  "Return direct children of PARENT whose field name is in FIELD-NAMES.
See `f90-ts--field-name-p' for the reason of this dispatch.")


(defun f90-ts--navigate-thing (pos direction thing)
  "Navigate to next or previous THING from POS depending on DIRECTION.
Uses the native `treesit-navigate-thing' if the Emacs 30 thing API is
available, and `f90-ts--navigate-thing-29' as a fallback otherwise."
  (if (fboundp 'treesit-navigate-thing)
      (treesit-navigate-thing pos direction 'beg thing)
    (f90-ts--navigate-thing-29 pos direction thing)))


;; `treesit-beginning-of-thing' and `treesit-end-of-thing' exist on Emacs
;; 29 as well, but they take a regexp instead of a thing symbol on the older
;; Emacs version.  The Emacs 30 thing API (thing symbols, TACTIC argument)
;; changes were introduced together with `treesit-navigate-thing', so its
;; availability is the feature test.
(defalias 'f90-ts--beginning-of-thing
  (if (fboundp 'treesit-navigate-thing)
      #'treesit-beginning-of-thing
    #'f90-ts--beginning-of-thing-29)
  "Invoke `f90-ts--beginning-of-thing-29' without the Emacs 30 thing API.")


(defalias 'f90-ts--end-of-thing
  (if (fboundp 'treesit-navigate-thing)
      #'treesit-end-of-thing
    #'f90-ts--end-of-thing-29)
  "Invoke `f90-ts--end-of-thing-29' without the Emacs 30 thing API.")


;; Advices, dispatched on probed core library behavior.  See the behavior
;; matrix in the comment introducing the feature detection above.
;;
;; The Emacs-version fallbacks mirror the historical dispatch (before this
;; file probed the core library): 0.20.x-style bugs on Emacs 29.x, and the
;; 0.25.x parent bug on Emacs 30+.

(let ((parent-broken
       (f90-ts--ts-core-resolved
        (f90-ts--ts-core-parent-of-virtual-broken-p)
        t))
      (sibling-broken
       (f90-ts--ts-core-resolved
        (f90-ts--ts-core-sibling-of-virtual-broken-p)
        (< emacs-major-version 30)))
      (node-on-skips
       (f90-ts--ts-core-resolved
        (f90-ts--ts-core-node-on-skips-virtual-p)
        (< emacs-major-version 30)))
      (sibling-skips
       (f90-ts--ts-core-resolved
        (f90-ts--ts-core-sibling-of-virtual-skips-p)
        nil)))
  (when node-on-skips
    (advice-add #'treesit-node-on
                :around #'f90-ts--node-on-workaround-29))
  (when sibling-broken
    (advice-add #'treesit-node-prev-sibling
                :around #'f90-ts--node-prev-sibling-workaround-29)
    (advice-add #'treesit-node-next-sibling
                :around #'f90-ts--node-next-sibling-workaround-29))
  (when (and sibling-skips
             (not sibling-broken))
    ;; tree-sitter 0.22.x: sibling queries skip virtual nodes, both as
    ;; arguments and as expected results.  The parent query is reliable
    ;; on this core, so patch by walking the parent's child list.  On
    ;; 0.20.x the sibling queries fail entirely (handled by the -29
    ;; workarounds above) and the parent query is broken as well, so the
    ;; child list variant cannot be used there.
    (advice-add #'treesit-node-next-sibling
                :around #'f90-ts--node-next-sibling-workaround-22)
    (advice-add #'treesit-node-prev-sibling
                :around #'f90-ts--node-prev-sibling-workaround-22))
  ;; The region reparse workaround for 0.22.x is dispatched lazily from
  ;; `f90-ts-mode' via `f90-ts--install-indent-reparse-workaround',
  ;; which retries the probe if the grammar was missing at load time.
  (when parent-broken
    (if sibling-broken
        ;; siblings are unreliable on this core: find the parent by an
        ;; exhaustive top-down search (tree-sitter 0.20.x)
        (advice-add #'treesit-node-parent
                    :around #'f90-ts--node-parent-workaround-29)
      ;; siblings are reliable: walk to a proper sibling and take its
      ;; parent, much faster (tree-sitter 0.25.x style)
      (advice-add #'treesit-node-parent
                  :around #'f90-ts--node-parent-workaround-30))))


;;;-----------------------------------------------------------------------------
;; switches for grammar variant:
;; * leaf nodes (string_literal) or decomposed string literals with
;;   children for its parts
;; * bug fix and fixes related to type_name, like in allocate(my_t :: x)

(defvar f90-ts--grammar-feature-cache nil
  "Alist mapping grammar feature symbols to t or nil.
An entry (FEATURE . t) signals that a query associated with FEATURE is
accepted.
If FEATURE is not listed in the cache, it has not yet been probed.")


(defun f90-ts--grammar-feature-p (feature query)
  "Return non-nil if Fortran grammar accepts QUERY associated with FEATURE.
FEATURE is the cache key.  A `treesit-query-error' means the grammar
does not conform to QUERY (like lacks the node or field structure) and is cached
as nil.
The result of `treesit-query-error', used to probe whether query is valid,
is cached so that the detection runs at most once for the session."
  (let ((entry (assq feature f90-ts--grammar-feature-cache)))
    (if entry
        (cdr entry)
      (let ((accepted
             (condition-case nil
                 (progn
                   ;; eagerly compile a query, if it fails, this signals
                   ;; a `treesit-query-error', otherwise a `treesit-compiled-query'
                   ;; object is returned
                   (treesit-query-compile 'fortran query t)
                   (push (cons feature t) f90-ts--grammar-feature-cache)
                   t)
               (treesit-query-error
                (push (cons feature nil) f90-ts--grammar-feature-cache)
                nil)
               (error nil))))
        ;;(message "f90-ts-mode: probed feature %s = %s" feature accepted)
        accepted))))


(defun f90-ts--grammar-keyword-p (token)
  "Return non-nil if the Fortran grammar has anonymous node TOKEN.
TOKEN is the keyword string as emitted by the grammar (like \"non_recursive\").
Results are cached in `f90-ts--grammar-feature-cache', using TOKEN as key."
  (let ((entry (assoc token f90-ts--grammar-feature-cache)))
    (if entry
        (cdr entry)
      (let ((accepted
             (condition-case nil
                 (progn
                   (treesit-query-compile 'fortran `((,(vector token) @x)) t)
                   (push (cons token t) f90-ts--grammar-feature-cache)
                   t)
               (treesit-query-error
                (push (cons token nil) f90-ts--grammar-feature-cache)
                nil)
               (error nil))))
        ;;(message "f90-ts-mode: probed token \"%s\" = %s" token accepted)
        accepted))))


(defun f90-ts--grammar-string-literal-decomposed-p ()
  "Return non-nil if string_literal is decomposed by the Fortran grammar.
The original grammar provided `string_literal' as a named leaf node.
Newer grammars decompose it into its parts (with `string_content',
quotation, comment etc. as children of `string_literal').
Result is detected once and cached for the session."
  (f90-ts--grammar-feature-p
   'string_content
   '((string_content) @x)))


(defun f90-ts--grammar-derived-type-name-p ()
  "Return non-nil if grammar wraps type names in `derived_type'/`type_name'.
Newer grammar version fixes a bug in statements like \"allocate(my_t :: x)\",
which changes the tree structure slightly.
The tree structure for \"type is\" and \"class is\" in \"select type\" clauses
was also changed in the same grammar commit."
  (f90-ts--grammar-feature-p
   'derived-type-name
   '((allocate_statement
      type: (derived_type name: (type_name) @x)))))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-workaround)

;;; f90-ts-workaround.el ends here
