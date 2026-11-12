;;; f90-ts-indent.el --- Indentation and smart end completion for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide tree-sitter based indentation for `f90-ts-mode'.  The core part
;; is the set of indentation rules `f90-ts-indent-rules' used by
;; `treesit-simple-indent'.  Besides simple indentation rules, it also deals
;; with continued lines, leading ampersands and statement labels, smart end
;; completion (usually as part of indentation), and the various line, statement
;; and region commands for indentation.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)

;;;-----------------------------------------------------------------------------

;; defined in simple.el, used for showing start of block during smart end
;; completion (see `f90-ts-smart-end')
(defvar blink-matching-delay)


;;;-----------------------------------------------------------------------------
;;; Indentation

(defvar-local f90-ts--align-continued-variant-tab nil
  "Current variant for indentation.
If nil use region variant, otherwise use stored tab variant.
This is also used for deciding whether `treesit-indent-region' is at work,
in which case computed (anchor offset) pairs need to be cached for continued
lines, see `f90-ts--continued-line-cache'.")


(defvar-local f90-ts--indent-cache nil
  "Current indent cache vector:
[node parent child0 psibp pstmt-1 pstmt-k].
Slots 2+ are lazily filled.
Value `unset' means not yet computed.
Value nil means that this node does not exist.  For example, on empty
lines, the node itself is nil.")


(defconst f90-ts--indent-cache-size 10
  "Fixed size for `f90-ts--indent-cache'.")


;; slot indices of indent cache
;; (node,parent) and various related nodes
(defconst f90-ts--indent-slot-node               0)
(defconst f90-ts--indent-slot-parent             1)
(defconst f90-ts--indent-slot-parent-nopp        2)
(defconst f90-ts--indent-slot-gparent-nopp       3)
(defconst f90-ts--indent-slot-child0             4)
(defconst f90-ts--indent-slot-prev-sib-by-parent 5)
(defconst f90-ts--indent-slot-prev-stmt-first    6)
(defconst f90-ts--indent-slot-prev-stmt-keyword  7)
;; anchor and/or offset data
;; note: matcher or anchor can sometimes determine anchor/offset
;; in more complex rules, like for continued lines, and it is not
;; possible to pass any results from matcher to anchor to offset
;; in simple indent rules machinery
(defconst f90-ts--indent-slot-anchor             8)
(defconst f90-ts--indent-slot-offset             9)


(defmacro f90-ts--indent-with-cache (slot query)
  "Return cache value at SLOT if value is present.
If SLOT is not populated, compute value using QUERY, store the result and
return it."
  `(let ((val (aref f90-ts--indent-cache ,slot)))
     (if (eq val 'unset)
         (let ((result ,query))
           (aset f90-ts--indent-cache ,slot result)
           result)
       val)))


(defun f90-ts--indent-cached-node ()
  "Return cached node at index 0."
  (and f90-ts--indent-cache
       (aref f90-ts--indent-cache f90-ts--indent-slot-node)))


(defun f90-ts--indent-cached-parent ()
  "Return cached parent at index 0."
  (and f90-ts--indent-cache
       (aref f90-ts--indent-cache f90-ts--indent-slot-parent)))


(defun f90-ts--indent-create-cache (node parent)
  "Create cache for NODE and PARENT for indentation rules."
  (setq f90-ts--indent-cache (make-vector f90-ts--indent-cache-size 'unset))
  (aset f90-ts--indent-cache f90-ts--indent-slot-node   node)
  (aset f90-ts--indent-cache f90-ts--indent-slot-parent parent)
  f90-ts--indent-cache)


(defun f90-ts--indent-parent-nopp ()
  "Return no-preproc parent of node.
Use cached value or compute using cached node.
The no-preproc parent is parent if it is a normal non-preproc node,
otherwise it is the first such ancestor of parent."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-parent-nopp
   (let ((parent (f90-ts--indent-cached-parent)))
     (and parent (f90-ts--parent-no-preproc parent)))))


(defun f90-ts--indent-grandparent-nopp ()
  "Return no-preproc grandparent of node.
Use cached value or compute using cached node.
It is the no-preproc parent of the no-preproc parent of the (node,parent) pair."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-gparent-nopp
   (let ((parent (f90-ts--indent-cached-parent)))
     (and parent (f90-ts--grandparent-no-preproc parent)))))


(defun f90-ts--indent-child0 ()
  "Return first named child of node.
Use cached value or compute using cached node."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-child0
   (let ((node (f90-ts--indent-cached-node)))
     (and node (treesit-node-child node 0 t)))))


(defun f90-ts--indent-prev-sib-by-parent ()
  "Return result of `f90-ts--prev-sib-by-parent' for cached node and parent.
Use cached value or compute using cached parent."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-prev-sib-by-parent
   (let ((parent (aref f90-ts--indent-cache f90-ts--indent-slot-parent)))
     (and parent
          (f90-ts--prev-sib-by-parent parent)))))


(defun f90-ts--indent-prev-stmt-first ()
  "Return result of `f90-ts--previous-stmt-first' for cached node and parent.
Use cached value or compute using cached node and parent."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-prev-stmt-first
   (let ((node   (f90-ts--indent-cached-node))
         (parent (f90-ts--indent-cached-parent)))
     (f90-ts--previous-stmt-first node parent))))


(defun f90-ts--indent-prev-stmt-keyword ()
  "Return result of `f90-ts--previous-stmt-keyword-by-first'.
Use cached value or compute using cached node and parent."
  (f90-ts--indent-with-cache
   f90-ts--indent-slot-prev-stmt-keyword
   (let ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
     (f90-ts--previous-stmt-keyword-by-first pstmt-1))))


(defun f90-ts--indent-anchor-cache (anchor)
  "Store ANCHOR in cache."
  (aset f90-ts--indent-cache
        f90-ts--indent-slot-anchor
        anchor))


(defun f90-ts--indent-offset-cache (offset)
  "Store OFFSET in cache."
  (aset f90-ts--indent-cache
        f90-ts--indent-slot-offset
        offset))


(defun f90-ts--indent-cached-anchor ()
  "Return cached anchor."
  (and f90-ts--indent-cache
       (aref f90-ts--indent-cache
             f90-ts--indent-slot-anchor)))


(defun f90-ts--indent-cached-offset ()
  "Return cached offset."
  (and f90-ts--indent-cache
       (aref f90-ts--indent-cache
             f90-ts--indent-slot-offset)))


;;++++++++++++++

(defvar-local f90-ts--continued-line-cache nil
  "Cache for continued-line anchor indentation.
The cache is an alist with entries (LINE . (COL-CACHED DELTA))
for already indented lines of a continued statement.

For alignment operations, we need node column numbers of nodes on
previously (already indented) lines to select the proper one.
The cache is required for `indent-region' operations, which work in a
batch mode.  If a previous line has not already been flushed, which is
the default case, we need to compute the column number ourselves after
not-yet-applied indentation.
To this end, we store the computed delta in the cache.  Except for the first
line, we can compute the indentation delta and cache it.  For the first line,
the delta is not known and initially stored as DELTA=0.  Once we detect a
flush, we can compute the delta and add it to all cached lines.  This is
necessary to have consistent indentation across all previous lines

The cache also stores COL-CACHED to detect, whether internal indentation
buffer of `treesit-indent-region' has already been flushed for a line.

As mentioned above, the cache is an alist (LINE . (COL-CACHED DELTA)),
mapping buffer LINE numbers to COL-CACHED and DELTA, where:
  COL-CACHED: indentation column at cache time, used as flush detector:
              if current indentation at line == COL-CACHED, line is not yet
              flushed
  DELTA:   delta of original to new indentation, if line is not
           flushed, then the column after applying indentation is
           column number of node + DELTA,
           and if the line has been flushed, it is just current node
           column

The first line of the statement initially is stored with DELTA=0 and
current COL-CACHED.  For each new line, the current indentation of the
first line is checked and if a buffer flush is detected, the applied
delta is computed and added to the delta of all subsequent cached
lines.

The state of `f90-ts--align-continued-variant-tab' is used to decide
whether `indent-region' or a line variant is in use.  The cache is
required only if `indent-region' with buffering is done.")


(defun f90-ts--continued-line-cache-lookup (pos)
  "Return entry associated with line at POS.
If there is no entry, then return current indentation with DELTA=0.
This is equivalent to no indentation at this line.  This is necessary for lines,
which are part of a continued line but not part of the marked region currently
processed.  For such lines, the returned value makes sense."
  (if-let* ((line (line-number-at-pos pos))
            (entry (assq line f90-ts--continued-line-cache)))
      (cdr entry)
    (list (f90-ts--indentation-at-pos pos) 0)))


(defun f90-ts--continued-line-cache-reset ()
  "Reset the continued-line indentation cache."
  (setq f90-ts--continued-line-cache nil))


(defun f90-ts--continued-line-cache-put-first (pos)
  "Store a cache entry for the first line of a continued statement.
POS is a position within that line.  Always store DELTA=0.

If later on a flush is detected (actual column at indentation is different
from COL-CACHED), the DELTA is computed and added to all other cached line.
This is done in `f90-ts--continued-line-cache-update'."
  (f90-ts--continued-line-cache-reset)
  (unless f90-ts--align-continued-variant-tab
    (let* ((col-cached (f90-ts--indentation-at-pos pos))
           (line (line-number-at-pos pos)))
      (setq f90-ts--continued-line-cache
            (list (cons line (list col-cached 0)))))))


(defun f90-ts--continued-line-cache-put-subsequent (pos anchor offset)
  "Compute and store the indent delta for a subsequent line at POS.
ANCHOR is the anchor position returned by the anchor function.
OFFSET is the offset from that anchor.
The delta is computed as:
  delta = delta-at-anchor-line + OFFSET,
where delta-at-anchor-line is taken from the cache entry at the anchor line."
  (unless f90-ts--align-continued-variant-tab
    (let* ((anchor-col (f90-ts--column-number-at-pos anchor))
           (anchor-entry (f90-ts--continued-line-cache-lookup anchor))
           (anchor-delta (cadr anchor-entry))
           (col-current  (f90-ts--indentation-at-pos pos))
           (col-new (+ anchor-col anchor-delta offset))
           (delta (- col-new col-current))
           (line (line-number-at-pos pos)))
      (push (cons line (list col-current delta))
            f90-ts--continued-line-cache))))


(defun f90-ts--continued-line-cache-get-first (first-pos)
  "Find entry for line at FIRST-POS in the cache.
This is the first line of the continued statement.
The cache is reverese ordered, so this is the last entry, if present.
If not present, a fake entry with current indentation at first line
and delta=0 is returned.  This happens if the region does not contain
the first line."
  ;; cache is constructed by push, the last entry is the first line
  (let ((first-line (line-number-at-pos first-pos))
        (entry (car (last f90-ts--continued-line-cache))))
    (if (and entry
             (= (car entry) first-line))
        ;; first line has been processed and is in the cache
        entry
      ;; construct a fake entry signalling that first line of continued
      ;; statement keeps it current indentation
      (cons first-line
            (list (f90-ts--indentation-at-pos first-pos) 0)))))


(defun f90-ts--continued-line-cache-update (first-pos)
  "Check whether first line at FIRST-POS was flushed.
This is the case if current column and cached column are different.
If it was, update the entry and apply delta to all other cached lines.
Argument FIRST-POS is used to jump to this line efficiently (jumping
to a line by line number is far more expensive)."
  (unless f90-ts--align-continued-variant-tab
    (let* ((first (f90-ts--continued-line-cache-get-first first-pos))
           (first-line (car first))
           (first-col-cached (cadr first))
           (first-col-current (f90-ts--indentation-at-pos first-pos))
           (first-delta (- first-col-current first-col-cached)))
      ;; indentation of first line has been flushed, add delta to all
      ;; other lines
      (when (/= first-col-cached first-col-current)
        (setq f90-ts--continued-line-cache
              (seq-map (lambda (entry)
                         (let ((line (car entry))
                               (col-cached (cadr entry))
                               (delta (caddr entry)))
                           (cons line
                                 (if (= line first-line)
                                     (list first-col-current delta)
                                   (list col-cached (+ delta first-delta))))))
                       f90-ts--continued-line-cache))))))


(defun f90-ts--continued-line-cache-get-col (anoff)
  "Return the final column of ANOFF in a continued statement.
ANOFF is a cons (ANCHOR.OFFSET), where ANCHOR is a node or buffer position on
some previous line, for which indentation has already been computed.
OFFSET is an integer value to provide a relative offset to the anchor.
It is allowed to be negative.  If the resulting column becomes negative,
then it is bounded by zero.

Lookup the cache for the line anchor is placed on.  If current and cached
indentation are equal, then apply cached delta to current column,
otherwise return column as is.  The bound by zero is applied only after the
cached delta has been added, as the delta of the anchor line may be positive
and compensate a negative offset."
  (let* ((anchor (car anoff))
         (offset (cdr anoff))
         (pos (if (treesit-node-p anchor)
                  (treesit-node-start anchor)
                anchor))
         (col (+ (f90-ts--column-number-at-pos pos) offset)))
    (if f90-ts--align-continued-variant-tab
        ;; line based indentation, no caching
        (max 0 col)
      (let* ((entry (f90-ts--continued-line-cache-lookup pos))
             (delta (cadr entry))
             (col-cached (car entry))
             (col-current (f90-ts--indentation-at-pos pos)))
        ;;(f90-ts-log-msg :cachecol "line, entry, delta = %s, %s, %s" line entry delta)
        ;;(f90-ts-log-msg :cachecol "col: cached, current = %s, %s" col-cached col-current)
        (cl-assert entry
                   nil
                   "no entry for line in cache, line=%s, cache=%s"
                   (line-number-at-pos pos) f90-ts--continued-line-cache)
        (max 0 (if (= col-cached col-current)
                   ;; not yet flushed
                   (+ col delta)
                 col))))))


(defun f90-ts--continued-line-cache-get-orig (col pos)
  "Return the original column of COL at position POS in a continued statement.

The function looks up the cache at the line given by POS.  If current and
cached indentation are not equal, and hence indentation changes have been
flushed by the treesit indent engine, it subtracts cached delta to revert
column to the original indentation."
  (if f90-ts--align-continued-variant-tab
      ;; line based indentation, no caching
      col
    (let* ((entry (f90-ts--continued-line-cache-lookup pos))
           (delta (cadr entry))
           (col-cached (car entry))
           (col-current (f90-ts--indentation-at-pos pos)))
        (cl-assert entry
                   nil
                   "no entry for line in cache, line=%s, cache=%s"
                   (line-number-at-pos pos) f90-ts--continued-line-cache)
        (if (= col-cached col-current)
            ;; not yet flushed
            (- col delta)
          col))))


;;++++++++++++++
;; matchers

(defun f90-ts--populate-cache (node parent _bol &rest _)
  "Always fail, but prepare the indent cache.
This dummy matcher should be the very first rule to be executed to reset the
cache for a new indentation run.
The cache stores NODE and PARENT and prepares further internal values."
  (f90-ts--indent-create-cache node parent)
  nil)


(defun f90-ts--continued-line-cache-start (node _parent bol &rest _)
  "Always fail, but prepare the continued line cache.
This dummy matcher is used for continued lines and required in `indent-region'
operations to take internal buffering in `treesit-indent-region' into account.
It uses NODE and BOL to determine whether it is on the first line of a
continued statement."
  (when (and node
             (f90-ts--first-line-of-continued-stmt-p bol))
    (f90-ts--continued-line-cache-put-first bol))
  ;; always fail
  nil)


(defun f90-ts--continued-subsequent-line-is (node parent bol &rest _)
  "Succeed if on some continued statement line.
NODE, PARENT and BOL are used to determine, whether position is at a continued
line.  The first statement line itself is not matched."
  (cond
   (node
    ;; if node is not nil, then there are nodes on the line
    (f90-ts--subsequent-line-of-continued-stmt-p bol))

   (parent
    ;; node=nil but parent is a proper node, then we are probably on an empty line
    (when-let* ((psibp (f90-ts--indent-prev-sib-by-parent)))
      ;; prev-sib-by-parent already excludes comment node, hence we can directly
      ;; check whether there is an ampersand
      (f90-ts--node-type-p psibp "&")))))


(defun f90-ts--openmp-comment-is (node _parent _bol &rest _)
  "Succeed if NODE is an OpenMP comment."
  (and (f90-ts--node-type-p node "comment")
       (f90-ts--node-openmp-p node)))


(defun f90-ts--preproc-node-is (node _parent _bol &rest _)
  "Succeed if NODE a fortran preprocessor statement."
  (and node (f90-ts--node-preproc-p node)))


(defun f90-ts--preproc-at-toplevel-is (_node parent _bol &rest _)
  "Succeed if PARENT is a toplevel preprocessor node.
It is a toplevel node if its first non-preprocessor ancestor is a toplevel
node (like program, module, etc.)."
  ;; Content inside preprocessor:
  ;; - Search up the tree for the first "Ancestor" that is NOT a
  ;;   preprocessor node.
  ;; - If that ancestor is a module or program -> toplevel indent
  (and parent
       (f90-ts--node-preproc-p parent)
       (let ((ancestor (treesit-parent-until
                        parent
                        (lambda (n)
                          (not (f90-ts--node-preproc-p n))))))
         (and ancestor
              (string-match-p "module\\|program"
                              (treesit-node-type ancestor))))))


(defun f90-ts--cache-anchor-offset (node parent bol anchor offset)
  "Cache ANCHOR and OFFSET.
Always cache these vaues in the indent cache.
If the line is also within a continued statement, then cache the anchor-offset
pair in the continued line cache as well.  Use NODE PARENT and BOL to
determine, whether this is a continued line."
  (f90-ts--indent-anchor-cache anchor)
  (f90-ts--indent-offset-cache offset)
  ;; when within continued line statement, then cache anchor offset;
  ;; apply a possible flush delta of the first line before (as is done in
  ;; `f90-ts--continued-line-anchor'), otherwise anchor line deltas are stale,
  ;; note that comment regions within continued lines are matched before the
  ;; general continued line machinery kicks in (which also updates the cache)
  (when (f90-ts--continued-subsequent-line-is node parent bol)
    (when-let* ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
      (f90-ts--continued-line-cache-update (treesit-node-start pstmt-1)))
    (f90-ts--continued-line-cache-put-subsequent
     bol anchor offset)))


(defun f90-ts--special-comment-is (node parent bol &rest _)
  "Matcher for special comment indentation.
If NODE matches a rule in `f90-ts-special-comment-rules' with a
non-indented indent style, cache the anchor and offset and return
non-nil to signal a match.
If indent style is column-0, use BOL to determine column-0 anchor.
If indent style is context, use PARENT as anchor.
If indent styoe is indented, indent like code.  Do not match here,
but instead let other rules handle it."

  ;; Note: if this matcher signals match and is within a continued
  ;; line, then indentation (anchor offset) needs to be cached in the
  ;; continued line cache as well, using:
  ;;
  ;;(when (f90-ts--continued-subsequent-line-is node parent bol)
  ;;  (f90-ts--continued-line-cache-put-subsequent
  ;;   bol anchor 0))

  (when (f90-ts--node-type-p node "comment")
    (when-let* ((rule (f90-ts--comment-matching-rule node))
                (indent (plist-get rule :indent)))
      ;; indented variant is handled like normal code
      (pcase indent
        ('column-0
         (let ((anchor (save-excursion
                         (goto-char bol)
                         (line-beginning-position))))
           (f90-ts--cache-anchor-offset node parent bol anchor 0)
           ;; signal match
           t))

        ('context
         ;; special comment indentation within continued lines does not seem
         ;; to make much sense except for column-0
         (unless (f90-ts--continued-subsequent-line-is node parent bol)
           (let ((anchor (treesit-node-start parent)))
             (f90-ts--indent-anchor-cache anchor)
             (f90-ts--indent-offset-cache 0))
           ;; signal match
           t))

        (_
         ;; signal no match
         nil)))))


(defun f90-ts--comment-region-is (node parent bol &rest _)
  "Succeed if NODE is a comment following another comment of the same kind.
Comments match if they are matched by the same special rule in
`f90-ts-special-comment-rules' or, if there is no rule, if they share the same
comment prefix, see `f90-ts--comments-match-p'.  The operation requires PARENT
and BOL for possible caching of anchor and offset values."
  (when (f90-ts--node-type-p node "comment")
    (when-let* ((prev-sib (treesit-node-prev-sibling node))
                (prev-line (f90-ts--first-node-on-line
                            (treesit-node-start prev-sib))))
      (when (and (f90-ts--node-type-p prev-line "comment")
                 (f90-ts--comments-match-p node prev-line))
        (let ((anchor (treesit-node-start prev-line)))
          (f90-ts--cache-anchor-offset node parent bol anchor 0)
          ;; signal match
          t)))))


(defun f90-ts--trailing-comment-prev (node)
  "Return previous sibling of NODE if that is a trailing comment, otherwise nil.
NODE must be a comment.  A trailing comment is a comment which does not start
its line.  The previous sibling is then the last comment on the previous
nonblank line."
  (when (f90-ts--node-type-p node "comment")
    (when-let* ((prev-sib (treesit-node-prev-sibling node))
                ((f90-ts--node-type-p prev-sib "comment"))
                (first (f90-ts--first-node-on-line
                        (treesit-node-start prev-sib)))
                ((not (treesit-node-eq first prev-sib))))
      prev-sib)))


(defun f90-ts--trailing-comment-primary (node)
  "Return the primary anchor-offset for comment NODE on a continued line.
If the previous sibling of NODE is a trailing comment and both comments belong
to the same comment region, see `f90-ts--comments-match-p', return the
pair (PREV-SIB . 0) which aligns NODE with that comment, otherwise nil.
This is used by the continued line anchor, which also provides the secondary
candidates."
  (when-let* (((f90-ts--node-type-p node "comment"))
              (prev-sib (f90-ts--trailing-comment-prev node))
              ((f90-ts--comments-match-p node prev-sib)))
    (cons prev-sib 0)))


(defun f90-ts--trailing-comment-region-is (node parent bol &rest _)
  "Succeed for a comment on its own line following a trailing comment.
Only lines which are not part of a continued statement are handled here, see
`f90-ts--trailing-comment-primary' for the other case.  The previous sibling of
NODE must be a trailing comment.  Succeed only if both comments match the same
special rule in `f90-ts-special-comment-rules'.  Then NODE is aligned with the
trailing comment.  Otherwise fail, such that the remaining rules determine the
default indentation.

Comments following a comment which starts its own line are handled by
`f90-ts--comment-region-is'.  The operation requires PARENT and BOL for
caching."
  (when-let* (((f90-ts--node-type-p node "comment"))
              ((not (f90-ts--continued-subsequent-line-is node parent bol)))
              (prev-sib (f90-ts--trailing-comment-prev node))
              ((f90-ts--comments-same-rule-p node prev-sib)))
    (f90-ts--cache-anchor-offset node parent bol
                                 (treesit-node-start prev-sib) 0)
    ;; signal match
    t))


(defun f90-ts--nopp-parent-is (type-p)
  "Provide patched matcher `parent-is' as used by `treesit-simple-indent'.
TYPE-P is expected to be a regular expressions.  It is matched against type
of the parent of node, in general.
However, if parent is a preprocessor node, then ascend the tree until
a normal node is encountered."
  (lambda (_node parent _bol &rest _)
    (let ((parent-nopp (f90-ts--parent-no-preproc parent)))
      (f90-ts--node-type-match-p parent-nopp type-p))))


(defun f90-ts--nopp-n-p-gp (type-n type-p type-gp)
  "Provide patched matcher `n-p-gp' as used by `treesit-simple-indent'.
TYPE-N, TYPE-P and TYPE-GP are expected to be a regular expressions.
They are matched against type of node, its parent and grandparent, in general.
However, if parent or grand-parent are preprocessor node, then ascend the
tree until normal nodes are encountered."
  (lambda (node parent _bol &rest _)
    (let* ((parent-nopp (f90-ts--parent-no-preproc parent))
           (grandparent-nopp (f90-ts--grandparent-no-preproc parent)))
      (and (f90-ts--node-type-match-p node type-n)
           (f90-ts--node-type-match-p parent-nopp type-p)
           (f90-ts--node-type-match-p grandparent-nopp type-gp)))))


(defun f90-ts--nopp-n-p-pstmtk (type-n type-p type-pstmtk)
  "Succeed if types of nodes match provided types.
TYPE-N, TYPE-P and TYPE-PSTMTK are expected to be regular expressions
or nil.  If nil, everything is matched, hence the type of the corresponding
node is ignored.
TYPE-N is matched against type of node,
TYPE-P is matched against type of no-preproc parent and
TYPE-PSTMTK is matched against type of (cached) pstmtk, which is
the previous statement keyword node."
  (lambda (node parent _bol &rest _)
    (let ((pstmt-k (f90-ts--indent-prev-stmt-keyword))
          (parent-nopp (f90-ts--parent-no-preproc parent)))
      (and (f90-ts--node-type-match-p node type-n)
           (f90-ts--node-type-match-p parent-nopp type-p)
           (f90-ts--node-type-match-p pstmt-k type-pstmtk)))))


(defun f90-ts--nopp-n-p-gp-pstmtk (type-n type-p type-gp type-pstmtk)
  "Succeed if types of nodes match provided types.
TYPE-N, TYPE-P, TYPE-GP and TYPE-PSTMTK are expected to be regular expressions
or nil.  If nil, everything is matched, hence the type of the corresponding
node is ignored.
TYPE-N is matched against type of node,
TYPE-P is matched against type of no-preproc parent and
TYPE-GP is matched against type of no-preproc grandparent and
TYPE-PSTMTK is matched against type of (cached) pstmtk, which is
the previous statement keyword node."
  (lambda (node parent _bol &rest _)
    (let ((pstmt-k (f90-ts--indent-prev-stmt-keyword))
          (parent-nopp (f90-ts--parent-no-preproc parent))
          (grandparent-nopp (f90-ts--grandparent-no-preproc parent)))
      (and (f90-ts--node-type-match-p node type-n)
           (f90-ts--node-type-match-p parent-nopp type-p)
           (f90-ts--node-type-match-p grandparent-nopp type-gp)
           (f90-ts--node-type-match-p pstmt-k type-pstmtk)))))


(defun f90-ts--n-p-ch-psibp (type-n type-p type-ch type-psibp)
  "Succeed if types of nodes match provided types.
TYPE-N, TYPE-P TYPE-CH and TYPE-PSIBP are expected to be regular expressions
or nil.  If nil, everything is matched, hence the type of the corresponding
node is ignored.
TYPE-N is matched against type of node,
TYPE-P is matched against type of parent,
TYPE-CH is matched against type of (cached) first child of node and
TYPE-PSIBP is matched against type of (cached) previous sibling by parent."
  (lambda (node parent _bol &rest _)
    (let ((child0 (f90-ts--indent-child0))
          (psibp (f90-ts--indent-prev-sib-by-parent)))
      (and (f90-ts--node-type-match-p node type-n)
           (f90-ts--node-type-match-p parent type-p)
           (f90-ts--node-type-match-p child0 type-ch)
           (f90-ts--node-type-match-p psibp type-psibp)))))


;;++++++++++++++
;; anchors

(defun f90-ts--nopp-parent (_node _parent bol &rest _)
  "Return anchor position determine by start of no-preproc PARENT.
If PARENT is not available, return BOL."
  (if-let* ((nopp-parent (f90-ts--indent-parent-nopp)))
      (treesit-node-start nopp-parent)
    bol))


(defun f90-ts--nopp-grandparent (_node _parent bol &rest _)
  "Return anchor position determine by start of no-preproc grandparaent.
The grandparent is determined by PARENT.  If PARENT is not available,
return BOL."
  (if-let* ((nopp-gparent (f90-ts--indent-grandparent-nopp)))
      (treesit-node-start nopp-gparent)
    bol))


(defun f90-ts--previous-stmt-anchor (_node _parent bol &rest _rest)
  "Return anchor at previous statements indentation or BOL.
If no previous statement can be found (for example at start of buffer),
then BOL is returned as anchor position."
  (if-let* ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
      (f90-ts--indent-pos-at-node pstmt-1)
    bol))


(defun f90-ts--catch-all-anchor (node parent bol &rest _rest)
  "Provide a fallback anchor for the catch-all case.
Use NODE and PARENT to select a relevant node to align.  Use BOL if no relevant
node was found.
This applies if some rule is missing, but also if we just want to indent
with the previous relevant line."
  ;; node is nil if on an empty line, thus we should use parent to
  ;; find a meaningful previous "sibling" of node
  (let* ((psibp (f90-ts--indent-prev-sib-by-parent))
         (pstmt-1 (f90-ts--indent-prev-stmt-first))
         (node-sel (or pstmt-1
                       psibp
                       parent
                       node)))
    (if node-sel
        (f90-ts--indent-pos-at-node node-sel)
      bol)))


(defun f90-ts--cached-anchor (_node _parent _bol &rest _rest)
  "Return previously determined cached anchor.
This function requires that the succeeding matcher has computed and stored the
anchor (and possibly offset)."
  (f90-ts--indent-cached-anchor))


;;++++++++++++++
;; offset functions: general

;; We cannot directly write (defun f90-ts--minus-offset (offset) ...)
;; and use it in indent rules with the desired offset value like
;; f90-ts-indent-block, as this bakes the current value into the rule,
;; and makes it immune to later changes.
(defun f90-ts--minus-block-offset (_node _parent _bol &rest _rest)
  "Return the offset value minus `f90-ts-indent-block' unconditionally."
  (- f90-ts-indent-block))


(defun f90-ts--toplevel-offset (_node _parent _bol &rest _rest)
  "Return offset for stuff right below top level nodes.
If inside module, submodule or program then return `f90-ts-indent-toplevel',
otherwise return `f90-ts-indent-contain'.
Deciding which of the two cases is present requires to access grandparent as
well as grand-grandparent, which are obtained via PARENT."
  ;; skip preprocessor nodes in ancestor line
  (let* ((grandparent (f90-ts--indent-grandparent-nopp))
         (ggparent    (and grandparent
                           (f90-ts--grandparent-no-preproc grandparent))))
    ;; before 'contains' statement, grandparent is translation_unit,
    ;; after 'contains' it is module or program
    (if (or (f90-ts--node-type-p grandparent '("module" "submodule" "program" "translation_unit"))
            (and (f90-ts--node-type-p grandparent "ERROR")
                 (f90-ts--node-type-p ggparent "translation_unit")))
        f90-ts-indent-toplevel
      f90-ts-indent-contain)))


(defun f90-ts--cached-offset (_node _parent _bol &rest _rest)
  "Return previously computed cached offset.
This function requires that the offset was computed and stored by the
succeeding matcher or the corresponding anchor."
  (f90-ts--indent-cached-offset))


;;++++++++++++++
;; lists on continued lines: auxiliary functions

(defun f90-ts--node-leftmost-unary (node)
  "Return leftmost descendant of NODE if it is an unary_expression.
This is only done, if NODE is a binary expression of type math_expression,
logical_expression or relational_expression.  It checks for field :left
and descend it until there is no such field or the node is of
type unary_expression."
  (cond
   ((f90-ts--node-type-p node "unary_expression")
    node)

   ((f90-ts--node-type-p node '("math_expression"
                                "logical_expression"
                                "relational_expression"))
    (cl-loop
     for n = node then (treesit-node-child-by-field-name n "left")
     while (and n (treesit-node-child-by-field-name n "left"))
     ;; note that a logical_expression without a left field is an unary expression
     ;; with operator .not., but it is not recorded as an unary_expression node
     finally return (when (and n
                               (f90-ts--node-type-p n '("unary_expression"
                                                        "logical_expression")))
                      n)))))


(defun f90-ts--align-node-symbol (node)
  "Return a symbol for NODE to group and select nodes for alignment selection.
Nodes for alignment are mostly selected among nodes with the same symbol.
If NODE is nil return nil."
  (when node
    (let ((unary-leftm (f90-ts--node-leftmost-unary node)))
      (cond
       ((f90-ts--node-type-p node "ERROR")
        'error)

       ((f90-ts--node-type-p node "comment")
        'comment)

       (unary-leftm
        (if (f90-ts--node-type-p (treesit-node-child-by-field-name unary-leftm
                                                                   "operator")
                                 '("-" "+"))
            'operator-unary-minusplus
          ;; either user defined operator or logical .not.
          'operator-unary-other))

       ((f90-ts--field-name-p node "operator")
        ;; applies if node is a binary operator like "+" or ".and."
        (let* ((parent (treesit-node-parent node))
               (type (and parent (treesit-node-type parent))))
          (pcase type
            ("logical_expression"        'operator-logical)
            ("math_expression"           'operator-math)
            ("relational_expression"     'operator-relation)
            ;; map concatenation to math_expression, as user defined
            ;; operators are also handled by that way
            ("concatenation_expression"  'operator-math)
            (_                           'operator) ; anything still missing?
            )))

       ((f90-ts--node-type-p node "&")
        'ampersand)

       ((f90-ts--node-type-p node "\\")
        ;; escape quote in string (double quote)
        'escape)

       (t
        (let ((text (treesit-node-text node)))
          (pcase text
            ((or "(" ")" "[" "]" "(/" "/)")
             'parenthesis)
            (","  'comma)
            ("=>" 'associate)
            ("="  'assignment)
            ((or "\"" "'")
             'quote)
            ;; default: argument for anything else (this is probably a named node
            ;; of type identifier, number_literal, call_expression, parenthesized_expression etc.
            (_    'named))))))))


(defun f90-ts--align-nsym-compatible-p (nsym1 nsym2)
  "Return non-nil if NSYM1 and NSYM2 are considered compatible for alignment.
Two node symbols are equal if `eq', or if both NSYM1 and NSYM2 are
members of `(named operator-unary-minusplus operator-unary-other)'
We consider operator-unary of type named, but with the hint to adjust
offset to align the body and ignore the leading minus."
  (or (eq nsym1 nsym2)
      (and (memq nsym1 '(named operator-unary-minusplus operator-unary-other))
           (memq nsym2 '(named operator-unary-minusplus operator-unary-other)))))


(defun f90-ts--align-location (node)
  "Determine position related values for node.
These are position (start of node or point), column at position, line number
and symbol type at point (for deciding where to align).  If NODE is nil,
then no relevant node is at point and point position is used instead.
This is important for handling empty lines.
The data is returned as an alist with keys `node', `pos', `col', `line'
and `nsym'."
  (if node
      (list (cons 'node node)
            (cons 'pos  (treesit-node-start node))
            (cons 'col  (f90-ts--node-column node))
            (cons 'line (f90-ts--node-line node))
            ;; decide which kind of (named or anonymous) nodes to filter
            ;; (argument, parentheses, comma, ampersend, etc)
            (cons 'nsym (f90-ts--align-node-symbol node)))
    (list (cons 'node nil)
          (cons 'pos  (point))
          (cons 'col  (f90-ts--column-number-at-pos (point)))
          (cons 'line (line-number-at-pos))
          (cons 'nsym nil))))


(defun f90-ts--node-op-expr-chain-root (node)
  "Return the topmost ancestor of NODE in a chain of operator expression nodes.
Relevant type are listed in `f90-ts--node-op-expr-types'.

Intended for associative expressions represented as binarized trees,
where repeated nesting of the same node type encodes a chain.  For
example, for a logical/math/relational etc. expression chain like
   (binary_expression
    left:
     (unary_expression
     argument:
      (binary_expression
       left:
        (binary_expression
         left: ...
         right: ...
        )
       right: ...
      ))
    right: ...)
the function returns the outermost expression node."
  (treesit-parent-while
   node
   #'f90-ts--node-op-expr-p))


(defun f90-ts--align-aux-context-p (context)
  "Return t if the node CONTEXT represents an auxiliary context node.
In some cases, a list context is part of an assignment, parenthesized
expression or a comma separated list, which are called auxiliary context."
  (f90-ts--node-type-p context '("=" "(" ",")))


;;++++++++++++++
;; list context: association_list

(defun f90-ts--align-expand-assoc (nodes)
  "Replace association nodes in list NODES by their first two children.
These are field \"name\" and \"=>\".  There is no handling of the selector
part currently.  Skip continuation symbols, which might be between
\"name\" and arrow \"=>\"."
  (seq-mapcat
   (lambda (node)
     (if (f90-ts--node-type-p node "association")
         (let* ((children (treesit-node-children node))
                (first (car children))
                (arrow (seq-find (lambda (n) (f90-ts--node-type-p n "=>"))
                                 children)))
           (list first arrow))
       (list node)))
   nodes))


(defun f90-ts--align-items-assocation (context _loc)
  "Determine relevant childrens of \"association_list\" context.
The list context is provided as car of CONTEXT."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context "association_list")
               nil "expected list context: association_list, got '%s'" list-context)
    ;; expand it, as it contains a list of nodes, whose children are required,
    ;; drop the first node, which is the opening parenthesis
    (when-let* ((children (treesit-node-children list-context))
               (assocs (seq-drop children 1)))
      (f90-ts--align-expand-assoc assocs))))


;;++++++++++++++
;; list context: operator expressions

(defun f90-ts--align-expand-op-expr (node)
  "Recursively compute relevant children of a expression tree NODE.
An expression like (A and B and C) or (x + y) is represented as a binary tree
like logical_expression(logical_expression(A and B) and C).
The routine returns the five children A, and, B, and, C.
Similar for other expression types like math_expression.
It does not descend into parenthesized_expressions and unary_expression whose
operator is a minus."
  (if (and (f90-ts--node-op-expr-p node)
           (not (and (f90-ts--node-type-p node "unary_expression")
                     (f90-ts--node-type-p (treesit-node-child-by-field-name node
                                                                            "operator")
                                          "-"))))
      (mapcan #'f90-ts--align-expand-op-expr
              (treesit-node-children node))
    (list node)))


(defun f90-ts--align-items-op-expr (context _loc)
  "Determine relevant children of CONTEXT being some operator expression.
Check and include any assignment or parentheses directly prior to list-context.
The expression node is provided as car of CONTEXT, and the assignment or
parenthesis node if present as cdr of CONTEXT."
  (let ((list-context (car context))
        (aux-context (cdr context)))
    (cl-assert (f90-ts--node-op-expr-p list-context)
               nil
               "expected list context: some expression node, got list-context=%s"
               list-context)
    ;; assert that we are already at the root of an op-expr chain
    (cl-assert (eq list-context
                   (f90-ts--node-op-expr-chain-root list-context))
               nil
               "list context not root of chain of op-expr, list-context=%s"
               list-context)

    ;; assert that auxiliary context is nil or assignment or opening parenthesis
    (cl-assert (or (null aux-context)
                   (f90-ts--align-aux-context-p aux-context))
               "invalid aux context, list-context=%s, aux-context=%s"
               list-context aux-context)

    (let* ((items-expr (f90-ts--align-expand-op-expr list-context))
           (items-aux (and aux-context
                           (f90-ts--siblings-between aux-context list-context))))
      (append items-aux items-expr))))


;;++++++++++++++
;; list context: parenthesized expressions, arguments, array, parameters

(defun f90-ts--align-items-children-no-paren (context _loc)
  "Return all children of CONTEXT except for parenthesis.
This is used for a context where all direct children are relevant,
which are \"parenthesized_expression\", \"argument_list\",
\"array_literal\", \"parameters\", and the parenthesis free item lists
\"output_item_list\" and \"input_item_list\" of print, write and read
statements."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context
                                    '("parenthesized_expression"
                                      "argument_list"
                                      "array_literal"
                                      "parameters"
                                      "output_item_list"
                                      "input_item_list"))
               nil "align-list-items-children: wrong context, got '%s'" list-context)
    ;; drop parenthesis, as these are dealt with directly
    (seq-filter
     (lambda (n)
       (not (eq (f90-ts--align-node-symbol n) 'parenthesis)))
     (treesit-node-children list-context))))


;;++++++++++++++
;; list context: variable_declarations

(defun f90-ts--align-items-var-decl (context loc)
  "Determine relevant children of CONTEXT = \"variable_declaration\".
Use LOC to determine whether attribute or declarator items are relevant (those
before and after \"::\")."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context "variable_declaration")
               nil "expected list context: variable_declaration, got '%s'" list-context)
    (when (< 0 (treesit-node-child-count list-context))
      (let* ((pos (alist-get 'pos loc))
             (attr-children (f90-ts--children-by-fields list-context '("attribute")))
             (decl-children (f90-ts--children-by-fields list-context '("declarator")))

             ;; decl-children should not be empty, but might be in incomplete code
             ;; depending on how error recovery works out; just use first decl child,
             ;; as the children should be sorted by buffer position
             (first-decl (car decl-children)))
        (if (or (null first-decl)
                (< pos (treesit-node-start first-decl)))
            attr-children
          ;; for init declarator children, add assignment nodes as well:
          ;; "=" for init_declarator, "=>" for pointer_init_declarator
          (cl-mapcan
           (lambda (n)
             (cons n (when (f90-ts--node-type-p n '("init_declarator"
                                                    "pointer_init_declarator"))
                       (cl-remove-if-not
                        (lambda (c) (f90-ts--node-type-p c '("=" "=>")))
                        (treesit-node-children n)))))
           decl-children))))))


;;++++++++++++++
;; list context: string_literal

(defun f90-ts--align-items-string (context _loc)
  "Return almost all children of CONTEXT of type string_literal.
There is one small exception:
Always drop the second quote in an escape double quote sequence,
which is represented as a string_content."
  (cl-assert (f90-ts--grammar-string-literal-decomposed-p)
             nil "wrong grammar, string_literal not decomposed by parser")
  ;; example code (where we want to primarily try to align the text):
  ;; s = """start with quoted quote&
  ;;        and continue without leading ampersand"
  ;; exclude the escape node (second \" of the three quotes) from the items list
  (let* ((list-context (car context))
        (children (treesit-node-children list-context)))
    (cl-assert (f90-ts--node-type-p list-context
                                    '("string_literal"))
               nil "expected list context: string_literal, got '%s'" list-context)
    (seq-filter
     (lambda (n)
       (let* ((psib (treesit-node-prev-sibling n)))
         (not
           (and (f90-ts--node-type-p psib "\\")
                (f90-ts--node-type-p n "string_content")))))
     children)))


;;++++++++++++++
;; list context: import statement

(defun f90-ts--align-items-children1 (context _loc)
  "Determine relevant children of CONTEXT by dropping first child.
This is used for nodes of type \"import_statement\"."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context "import_statement")
               nil "expected list context: import_statement, got '%s'" list-context)
    (when-let* ((children (treesit-node-children list-context)))
      ;; drop the "import" keyword
      (seq-drop children 1))))


;;++++++++++++++
;; list context: binding_list, final_statement

(defun f90-ts--align-items-children2 (context _loc)
  "Determine relevant children of CONTEXT by dropping first two children.
This is used for nodes of type \"binding_list\" or \"final_statement\".
Both occur in the contains part of a derived type definition."
  (let ((list-context (car context)))
    (cl-assert (or (f90-ts--node-type-p list-context "binding_list")
                   (f90-ts--node-type-p list-context "final_statement"))
               nil "expected list context: binding_list or final_statement, got '%s'" list-context)
    (when-let* ((children (treesit-node-children list-context)))
      ;; drop the (binding_name ...) part, and the => binding symbol
      (seq-drop children 2))))


;;++++++++++++++
;; list context: init_declarator, pointer_init_declarator

(defun f90-ts--align-items-init-decl (context _loc)
  "Determine relevant children of CONTEXT by dropping the declared variable.
This is used for nodes of type \"init_declarator\" and
\"pointer_init_declarator\", like \"a = 1\" or \"p => null()\".  The remaining
children are the \"=\" or \"=>\" symbol and the initialisation expression."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context '("init_declarator"
                                                   "pointer_init_declarator"))
               nil
               "expected list context: init_declarator, got '%s'" list-context)
    (when-let* ((children (treesit-node-children list-context)))
      ;; drop the declared variable (the left field)
      (seq-drop children 1))))


;;++++++++++++++
;; list context: case_statement

(defun f90-ts--align-items-case-paren-children (case-stmt)
  "Return the pair (OPEN . CLOSE) of parentheses of the case statement CASE-STMT.
Both are direct children of the statement.  Return nil if either is missing,
as for \"case default\"."
  (let* ((children (treesit-node-children case-stmt))
         (open  (seq-find (lambda (n) (f90-ts--node-type-p n "(")) children))
         (close (seq-find (lambda (n) (f90-ts--node-type-p n ")")) children)))
    (when (and open close)
      (cons open close))))


(defun f90-ts--align-items-case (context _loc)
  "Return the items of the case statement CONTEXT between its parentheses.
These are the children of the \"case_value_range_list\" (values, commas,
ampersands and comments inside the list) and all further nodes between the
parentheses, like comments before the list or after the last item."
  (let* ((case-stmt (car context))
         (parens (f90-ts--align-items-case-paren-children case-stmt))
         (beg (and parens (treesit-node-start (car parens))))
         (end (and parens (treesit-node-start (cdr parens)))))
    (cl-assert (f90-ts--node-type-p case-stmt "case_statement")
               nil "align-items-case: wrong context, got '%s'" case-stmt)
    (when parens
      (cl-loop
       for n in (treesit-node-children case-stmt)
       when (and (< beg (treesit-node-start n))
                 (< (treesit-node-start n) end))
       append (if (f90-ts--node-type-p n "case_value_range_list")
                  (treesit-node-children n)
                (list n))))))


;;++++++++++++++
;; additional anchor for aligned lists:

;; The :get-other-fn functions of a list context are called as
;;
;;   (FN CONTEXT LOC ITEMS-PLIST)
;;
;; and return nil or a plist with the keys
;;
;;   :primary ANOFF      context specific default for the primary position
;;   :force   BOOL       non-nil: :primary (or the standard continued line
;;                       position if :primary is nil) is the primary position,
;;                       even if there are earlier items compatible with the
;;                       current node (see `f90-ts--align-other-var-decl'
;;                       for an example)
;;   :others  ANOFFS     further candidate positions
;;   :no-items-sym BOOL  non-nil: the earlier items compatible with the current
;;                       node are not candidate positions (see
;;                       `f90-ts--align-other-array-commas' for an example)
;;
;; An ANOFF is a cons (ANCHOR . OFFSET) with ANCHOR a node or a buffer
;; position.  :primary is the primary candidate position, it does not
;; need to be repeated in :others.  The standard continued line position
;; (pstmt-1 plus `f90-ts-indent-continued') is always a candidate, too.
;;
;; The final primary position is determined in one place, see
;; `f90-ts--align-anchor-offset'.

(defun f90-ts--align-pstmt1-anoff ()
  "Return the standard continued line indentation.
This is start position of pstmt-1 as anchor and `f90-ts-indent-continued'
as offset."
  (let ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
    (cons (treesit-node-start pstmt-1)
          f90-ts-indent-continued)))


(defun f90-ts--align-smallest-anoff (items)
  "Return the item with the smallest column number in a list of ITEMS.
ITEMS is not asssumed to be sorted by their column.
Currently an item must be a cons (node . offset).  It is assumed that anchor
is a node (in general it is a node or a buffer position) and offset is 0."
  (car (seq-sort-by #'f90-ts--continued-line-cache-get-col
                    #'<
                    items)))


(defun f90-ts--align-smallest-column (items)
  "Return the smallest column number found in a list of ITEMS.
ITEMS is not asssumed to be sorted.
Currently an item must be a cons (node . offset).  It is assumed that anchor
is a node (in general it is a node or a buffer position) and offset is 0."
  ;;(f90-ts-log-msg :smallcol "cache = %s" f90-ts--continued-line-cache)
  ;;(cl-loop
  ;; for node-off in items
  ;; do (f90-ts-log-msg :smallcol "node-off col = %s, %s"
  ;;     node-off (f90-ts--continued-line-cache-get-col node-off)))
  (cl-loop
   for node-off in items
   minimize (f90-ts--continued-line-cache-get-col node-off)))


(defun f90-ts--align-other-paren-expr-core
    (node-sym node-lead start-lead offset-lead indent-at-lead items-plist)
  "Return the other plist shared by paren and op-expr contexts.
The lead node is the token the continuation lines are positioned relative
to: the opening parenthesis for paren contexts, and the preceding \"=\", \"(\"
or \",\" (aux context) for op-expr contexts.  Examples for operator expressions:

    x = aa + bb + &       ! lead is \"=\"
        cc
    call foo(aa + bb + &  ! lead is \"(\"
             cc)
    call foo(aa, bb + &   ! lead is \",\" (for expression aa+bb)
                 cc)

Two positions are candidates:

Aligned: START-LEAD plus OFFSET-LEAD.  OFFSET-LEAD is a customizable value
like 1 for parenthesis and 2 for a comma.

Hanging: `f90-ts-indent-continued' relative to the indentation of the line
holding the lead (INDENT-AT-LEAD).  The column depends only on that
indentation, not on the position of the lead within the line:

    call foo( &
       aa, bb, &   ! hanging
       cc)

Hanging positions do not move if the text before the lead changes (e.g.
renaming foo), and they are the most sensible choice if nothing follows
the lead on its line.

Which one is primary depends on the style already used by earlier items:
aligned if there are items on the head line or no later item is left of the
lead column (COL-LEAD), hanging otherwise.  Both positions are always
candidates (for rotation), also for a closing parenthesis.  For the closing
parenthesis the hanging position is primary if an item is left of COL-LEAD,
otherwise the aligned position.

NODE-SYM is the nsym of the current node, NODE-LEAD the lead node (for its
column), ITEMS-PLIST the items lists from `f90-ts--align-anoff-items'."
  (let* ((items-head (plist-get items-plist :head))
         (items-tail (plist-get items-plist :tail))
         (col-min    (f90-ts--align-smallest-column items-tail))
         (col-lead   (f90-ts--continued-line-cache-get-col (cons node-lead 0)))
         (anoff-lead (cons start-lead offset-lead))
         (anoff-hang (cons indent-at-lead f90-ts-indent-continued)))
    (if (eq node-sym 'parenthesis)
        ;; closing parenthesis: align relative to indentation if any prior
        ;; item is to the left of the context column, otherwise use anchor,
        ;; the other one is secondary
        (if (and col-min (< col-min col-lead))
            (list :primary anoff-hang
                  :others  (list anoff-lead))
          (list :primary anoff-lead
                :others  (list anoff-hang)))
      (list
       :primary
       (cond
        ;; if there are items on the head line of context or all items on tail
        ;; lines have column at least as large as column of context node, use
        ;; position relative to lead node as primary
        ((or items-head
             (and col-min
                  (>= col-min col-lead)))
         anoff-lead)

        ;; both positions are potentially relevant, pick the smaller column
        ;; as primary
        (t
         (cl-assert
          (= (line-number-at-pos start-lead)
             (line-number-at-pos indent-at-lead))
          nil
          "start-lead and indent-at-lead must be on the same line, got lines %s and %s"
          (line-number-at-pos start-lead)
          (line-number-at-pos indent-at-lead))
         (if (< (+ indent-at-lead f90-ts-indent-continued)
                (+ start-lead offset-lead))
             anoff-hang
           anoff-lead)))
       ;; ensure both positions are always candidates
       :others (list anoff-lead anoff-hang)))))


(defun f90-ts--align-other-paren-offset (nsym)
  "Return the offset relative to an opening parenthesis for node symbol NSYM.
This is `f90-ts-indent-paren-close' for a closing parenthesis, and
`f90-ts-indent-paren-default' for everything else.
Note that if nsym is `parenthesis', then it must be a closing parenthesis.
The opening parenthesis would not be possible as an op-expr list context."
  (if (eq nsym 'parenthesis)
      f90-ts-indent-paren-close
    f90-ts-indent-paren-default))


(defun f90-ts--align-other-op-expr-offset-aux (psib-type nsym)
  "Return the offset of NSYM relative to the aux context of an op-expr.
PSIB-TYPE is the node type of the previous sibling of the operator expression,
or nil if there is no previous sibling.  If there is an auxiliary context, one
of the following cases is true:
\"(\": parenthesis offsets, see `f90-ts--align-other-paren-offset';
\"=\" or \",\": `f90-ts-indent-expr-assign-assoc-op' if NSYM is a math operator,
                `f90-ts-indent-expr-assign-default' otherwise;
anything else: 0."
  (pcase psib-type
    ("(" (f90-ts--align-other-paren-offset nsym))
    ((or "=" ",")
     (if (memq nsym '(operator-logical operator-math))
         f90-ts-indent-expr-assign-assoc-op
       f90-ts-indent-expr-assign-default))
    (_ 0)))


(defun f90-ts--align-other-paren (context loc items-plist)
  "Return the other plist for parenthesized list contexts.
See `f90-ts--align-other-paren-expr-core' for the shared logic.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (cl-assert (f90-ts--node-type-p (car context)
                                  '("parenthesized_expression"
                                    "argument_list"
                                    "array_literal"
                                    "parameters"))
             nil
             "align-list-other-paren: wrong context, got '%s'"
             (car context))
  (let* ((list-context    (car context))
         (node-sym        (alist-get 'nsym loc))
         (open-paren      (treesit-node-at (treesit-node-start list-context)))
         (indent-at-paren (f90-ts--indent-pos-at-node open-paren))
         (start-paren     (treesit-node-start open-paren))
         (offset-paren    (+ (1- (length (treesit-node-text open-paren)))
                             (f90-ts--align-other-paren-offset node-sym))))
    (f90-ts--align-other-paren-expr-core
     node-sym list-context start-paren offset-paren indent-at-paren items-plist)))


(defun f90-ts--align-other-op-expr (context loc items-plist)
  "Return the other plist for operator-expression contexts.
There is only something to add if there is an auxiliary context.
See `f90-ts--align-other-paren-expr-core' for the shared logic.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (cl-assert (f90-ts--node-op-expr-p (car context))
             nil "wrong context, expected some op-expr, got '%s'" (car context))
  (when-let* ((aux-context (cdr context)))
    (cl-assert (f90-ts--align-aux-context-p aux-context)
               nil
               "invalid aux context, list-context=%s, aux-context=%s"
               (car context) aux-context)
    (let* ((list-context  (car context))
           (node-sym      (alist-get 'nsym loc))
           (psib-context  (f90-ts--prev-sibling-proper list-context))
           (psib-type     (and psib-context (treesit-node-type psib-context)))
           (start-aux     (treesit-node-start aux-context))
           (indent-at-aux (f90-ts--indent-pos-at-node aux-context))
           (offset-aux    (f90-ts--align-other-op-expr-offset-aux psib-type
                                                                  node-sym)))
      (f90-ts--align-other-paren-expr-core
       node-sym aux-context start-aux offset-aux indent-at-aux items-plist))))


(defun f90-ts--align-other-case-head-comment (open-paren)
  "Return the last comment on the line of OPEN-PAREN of a case statement.
Only nodes following OPEN-PAREN are considered.  The comment is either a
sibling of the parenthesis, or a child of the list, if the list starts on that
line.  Return nil if there is no such comment."
  (let ((pos (treesit-node-start open-paren)))
    (cl-flet
        ((same-line-comment-p (c)
           (and (f90-ts--node-type-p c "comment")
                (f90-ts--same-line-p pos (treesit-node-start c)))))
      (cl-loop
       for n = (treesit-node-next-sibling open-paren)
       then (treesit-node-next-sibling n)
       while (and n (f90-ts--same-line-p pos (treesit-node-start n)))
       if (f90-ts--node-type-p n "comment")
       collect n into comments
       else if (f90-ts--node-type-p n "case_value_range_list")
       append (cl-remove-if-not #'same-line-comment-p
                                (treesit-node-children n))
       into comments
       finally return (car (last comments))))))


(defun f90-ts--align-other-case (context loc items-plist)
  "Return the other plist for the list context \"case_statement\".
The list is the one in \"case (a, b, c)\", restricted to the header by
`f90-ts--align-list-context-case-aux'.  Unlike for argument lists, the
context node starts with the keyword \"case\".  The lead node is the opening
parenthesis, which is the auxiliary node of CONTEXT.
A closing parenthesis on a continued line is offset by
`f90-ts-indent-paren-close', as for expressions.
A comment can additionally be aligned with a trailing comment on the line of the
opening parenthesis, which is relevant if the first item is on a later line.
See `f90-ts--align-other-paren-expr-core' for the shared logic.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context "case_statement")
               nil "align-list-other-case: wrong context, got '%s'" list-context)
    (when-let* ((open-paren (cdr context))
                ((f90-ts--node-type-p open-paren "(")))
      (let* ((node            (alist-get 'node loc))
             (node-sym        (alist-get 'nsym loc))
             (indent-at-paren (f90-ts--indent-pos-at-node open-paren))
             (start-paren     (treesit-node-start open-paren))
             (offset-paren    (+ (1- (length (treesit-node-text open-paren)))
                                 (f90-ts--align-other-paren-offset node-sym)))
             (other (f90-ts--align-other-paren-expr-core
                     node-sym open-paren start-paren offset-paren
                     indent-at-paren items-plist))
             (head-comment (and (eq node-sym 'comment)
                                (f90-ts--align-other-case-head-comment open-paren))))
        ;; comment: further candidate is the trailing comment on the head line
        ;; (unless already provided as primary of the continued line anchor)
        (if (and head-comment
                 (f90-ts--comments-match-p node head-comment)
                 (not (let ((prev (f90-ts--trailing-comment-prev node)))
                        (and prev (treesit-node-eq prev head-comment)))))
            (plist-put other :others
                       (append (plist-get other :others)
                               (list (cons head-comment 0))))
          other)))))


(defun f90-ts--align-other-association (context loc _items-plist)
  "Return the other plist for association nodes.
For association lists (associate(x => y, ...)), the primary position is to
align with the initial opening parenthesis plus some offset.
However, if node is the =>, then apply indent with `f90-ts-indent-continued'
relative to opening parenthesis.

CONTEXT and LOC are as described in the calling convention of the
align-list-other family."
  ;; for argument_list, there should always be the opening parenthesis
  (let* ((list-context (car context))
         (nsym (alist-get 'nsym loc))
         (child-paren (treesit-node-child list-context 0 nil)))
    (cl-assert (f90-ts--node-type-p list-context "association_list")
               nil "wrong context, got '%s'" list-context)
    (cl-assert (f90-ts--node-type-p child-paren "(")
               nil
               "expected opening parenthesis as child-0, got '%s'"
               child-paren)
    (list :primary
          (cons (treesit-node-start child-paren)
                (pcase nsym
                  ('associate   f90-ts-indent-continued)     ; indents "=>"
                  ('parenthesis f90-ts-indent-paren-close)   ; indents ")"
                  (_            f90-ts-indent-paren-default) ; all other kind of nodes
                  )))))


(defun f90-ts--align-init-decl-prev-symbols (init nsym line)
  "Return anoffs of earlier \"=\" or \"=>\" symbols of init declarators.
INIT is an init declarator (or pointer init declarator) node.  Collect the
assignment symbol of NSYM (`assignment': \"=\", `associate': \"=>\") of all init
declarators in the same variable_declaration which precede INIT, and which are
on a line before LINE.  Return a list of pairs (node . 0), or nil for other
values of NSYM."
  (let ((decl (treesit-node-parent init))
        (sym-type (pcase nsym
                    ('assignment "=")
                    ('associate  "=>"))))
    (when (and sym-type
               (f90-ts--node-type-p decl "variable_declaration"))
      (cl-loop
       for n in (f90-ts--children-by-fields decl '("declarator"))
       when (and (< (treesit-node-start n) (treesit-node-start init))
                 (f90-ts--node-type-p n '("init_declarator"
                                          "pointer_init_declarator")))
       append (cl-loop
               for c in (treesit-node-children n)
               when (and (f90-ts--node-type-p c sym-type)
                         (f90-ts--node-before-line-p c line))
               collect (cons c 0))))))


(defun f90-ts--align-other-init-decl (context loc _items-plist)
  "Return the other plist for init declarator nodes.
An init declarator is \"left = value\" or \"left => value\" in a variable
declaration, spread over several lines.  Examples:

  integer, dimension(1:6) :: &
       lebedev_N = &          ! start of the init declarator
            [6, 14, 26, &     ! value: hanging relative to the declarator
             38, 50, 74]
  integer, dimension(1:2), parameter :: &
       lebedev_N &
            = [6, 14]         ! \"=\": hanging as well

There is no alignment with earlier rhs items, as there is only one value on the
right hand side.  However, `f90-ts-indent-continued' relative to the indentation
of the line holding the declared variable and the declared variable itself are
further candidates.

If the current node is the symbol \"=\" or \"=>\", the primary position is the
standard continued line position, unless an earlier init declarator in the
variable declaration has the same symbol on an earlier line and this symbol is
itself preceded by an ampersand, like the current one.  Then align with the one
with the smallest column:

    real, target :: x = 1, &
                    y &
                      = 1, &    ! aligned und previous \"=\", but not primary
                    z &
                      = 2       ! aligned with \"=\" in \"y = 1\", primary

The :primary position has :forced t.  Positions relative to the whole variable
declaration, like the one after \"::\", are not appropriate here.

CONTEXT and LOC are as described in the calling convention of the
align-list-other family."
  (let* ((list-context (car context))
         (nsym (alist-get 'nsym loc))
         (symbol-p (memq nsym '(assignment associate)))
         (left (or (treesit-node-child-by-field-name list-context "left")
                   (treesit-node-child list-context 0 nil)))
         (anoff-hang (cons (f90-ts--indent-pos-at-node left)
                           f90-ts-indent-continued))
         (anoffs-sym (f90-ts--align-init-decl-prev-symbols
                      list-context nsym (alist-get 'line loc)))
         ;; only symbols which start a continued line are primary candidates
         (anoffs-amp (seq-filter
                      (lambda (anoff)
                        (let ((n (car anoff)))
                          (f90-ts--node-type-p (treesit-node-prev-sibling n)
                                               "&")))
                      anoffs-sym))
         (anoff-amp  (and anoffs-amp
                          (f90-ts--align-smallest-anoff anoffs-amp))))
    (cl-assert (f90-ts--node-type-p list-context '("init_declarator"
                                                   "pointer_init_declarator"))
               nil "wrong context, got '%s'" list-context)
    (list :primary (cond
                    (anoff-amp anoff-amp)
                    ;; nil: forced primary becomes the continued line position
                    (symbol-p nil)
                    (t anoff-hang))
          :force   t
          :others  (append (and symbol-p (list anoff-hang))
                           anoffs-sym
                           (list (cons (treesit-node-start left) 0))))))


(defun f90-ts--align-other-string (context loc items-plist)
  "Return the other plist for string nodes.
A string_literal consists of opening and closing quoting symbol,
string_content, ampersand, quote escape character and comments.
Note that a leading ampersand cannot be removed, as following blank characters
must be preserved.  Thus we need to deal with the ampersand.  It should be
aligned to the opening quote as primary position.

If the node is a string_content (symbol `named') the escape nodes are
candidates, and vice versa.  If there is nothing else, the first of these is
the primary position.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (cl-assert (f90-ts--grammar-string-literal-decomposed-p)
             nil "wrong grammar, string_literal not decomposed by parser")
  (let* ((list-context (car context))
         (nsym (alist-get 'nsym loc))
         (open-quote (treesit-node-child list-context 0 nil))
         (items-prev (plist-get items-plist :prev))
         (anoff-quote (and (memq nsym '(ampersand comment quote))
                           (cons open-quote 0)))
         (opp-sym (pcase nsym
                    ('named  'escape)
                    ('escape 'named)))
         (anoffs-opp (and opp-sym
                          (cl-loop
                           for (n . s) in items-prev
                           when (eq s opp-sym)
                           collect (cons n 0)))))
    (cl-assert (f90-ts--node-type-p list-context "string_literal")
               nil "wrong context, got '%s'" list-context)
    (list :primary (or anoff-quote (car anoffs-opp))
          :others anoffs-opp)))


(defun f90-ts--align-other-var-decl-anoff (decl cur-line)
  "Return the alignment candidate anoff for DECL on line CUR-LINE, or nil.
DECL is a variable_declaration.  The candidate is the position of \"::\" plus
`f90-ts-indent-declaration'.  If there is no \"::\", it is the end of the last
type or attribute node minus the length of \"::\" plus
`f90-ts-indent-declaration'.  In both cases the node must be on a line before
CUR-LINE, otherwise there is no candidate."
  (if-let* ((node-colons (treesit-search-subtree decl
                                                 "::"
                                                 nil ; search forward
                                                 t   ; include anonymous nodes
                                                 1   ; maximal depth of 1, search only children
                                                 )))
      (when (< (f90-ts--node-line node-colons) cur-line)
        ;; start of "::" plus f90-ts-indent-declaration
        (cons (treesit-node-start node-colons)
              f90-ts-indent-declaration))
    ;; no colons, take last node (type or attribute) and add to end position
    ;; of that node f90-ts-indent-declaration minus the length of "::"
    (when-let* ((node-last (treesit-search-subtree
                            decl
                            (lambda (n) (f90-ts--field-names-any-p
                                         n '("type" "attribute")))
                            t   ; search backward to find last attribute node
                            t   ; only named nodes
                            1   ; maximal depth of 1, search only children
                            ))
                ((< (f90-ts--node-line node-last) cur-line)))
      (cons (treesit-node-end node-last)
            (- f90-ts-indent-declaration (length "::"))))))


(defun f90-ts--align-other-var-decl (context loc items-plist)
  "Return the other plist for variable declarations.
Car of CONTEXT is a variable_declaration.

The position of \"::\" plus `f90-ts-indent-declaration' (or the end of the
last type or attribute node if there is no \"::\") is a candidate, if it is on
a previous line.  It is the primary position only if there are items on the
head line and no earlier items compatible with the current node.

For a continuation line starting with \"=\" (nsym `assignment') the standard
continued line position (pstmt-1 plus `f90-ts-indent-continued') is the
primary position, even if there are compatible earlier items.  This is what
:force is used for.  Without it, the \"=\" of the earlier initialisation would
be the smallest compatible earlier item and would be used:

    integer :: a = 1, b &
                 = 2        ! without force: under the \"=\" of \"a = 1\"

    integer :: a = 1, b &
       = 2                  ! with force: standard continued position

The column of the earlier \"=\" is still a candidate for rotation and
keep-or-* variants.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (let* ((items-head   (plist-get items-plist :head))
         (assignment-p (eq (alist-get 'nsym loc) 'assignment))
         (anoff-decl   (f90-ts--align-other-var-decl-anoff (car context)
                                                           (alist-get 'line loc))))
    (list :primary (and items-head (not assignment-p) anoff-decl)
          :force   assignment-p
          :others  (and anoff-decl (list anoff-decl)))))



;;++++++++++++++
;; list context: output_item_list, input_item_list

(defun f90-ts--align-other-item-list (context loc _items-plist)
  "Return the other plist for the item lists of print, write and read.
These lists have no parentheses, but the statement head provides an anchor for
a comma starting a continued line.  These are closing parenthesis of the
control list of write and read, or the comma after the format of print:

    write(*,*) a1 &
             , a2         ! comma below the closing parenthesis
    print fmt, x1 &
             , x2         ! comma below the comma after fmt

This anchor is the primary position for a comma, but only if there is no earlier
comma in the list on a previous line, otherwise the usual selection among
earlier items takes over (the smallest column).  It is a candidate for
rotation anyway.  There is nothing to add for other node symbols.

CONTEXT and LOC are as described in the calling convention of the
align-list-other family."
  (let ((list-context (car context)))
    (cl-assert (f90-ts--node-type-p list-context '("output_item_list"
                                                   "input_item_list"))
               nil "wrong context, got '%s'" list-context)
    (when (eq (alist-get 'nsym loc) 'comma)
      ;; skips ampersands and comments between statement head and list
      (when-let* ((psib (f90-ts--prev-sibling-proper list-context))
                  ((f90-ts--node-type-p psib '(")" ","))))
        (list :primary (cons psib 0))))))


;;++++++++++++++
;; list context: array literals, alignment at commata

(defun f90-ts--align-comma-delta (node)
  "Return the distance in columns from the start of NODE to its comma, or nil.
If the next sibling of NODE is a comma on the same line, it is its comma.
If NODE is not followed by a comma on its line, but by the closing bracket
\"]\" or \"/)\" of the array literal, an ampersand or a comment, a virtual
comma right after NODE is assumed.  Return nil for other siblings or if NODE
spans several lines."
  (when-let* ((next (treesit-node-next-sibling node))
              ((f90-ts--node-type-p next '("," "]" "/)" "&" "comment")))
              ;; if node spans more than one line, right alignment makes no sense
              ((f90-ts--same-line-p (treesit-node-start node)
                                    (treesit-node-end node)))
              (pos-comma (if (and (f90-ts--node-type-p next ",")
                                  (f90-ts--same-line-p (treesit-node-start node)
                                                       (treesit-node-start next)))
                             (treesit-node-start next)
                           (treesit-node-end node))))
    (- (f90-ts--column-number-at-pos pos-comma)
       (f90-ts--node-column node))))


(defun f90-ts--align-comma-anoff-less-p (anoff1 anoff2)
  "Return non-nil if ANOFF1 is before ANOFF2 columnwise.
Compare the final columns for the two anoffs, see
`f90-ts--continued-line-cache-get-col'.  For equal columns compare the start
positions of the anchors, which must be nodes."
  (let ((col1 (f90-ts--continued-line-cache-get-col anoff1))
        (col2 (f90-ts--continued-line-cache-get-col anoff2)))
    (or (< col1 col2)
        (and (= col1 col2)
             (> (treesit-node-start (car anoff1))
                (treesit-node-start (car anoff2)))))))


(defun f90-ts--align-other-array-commas (context loc items-plist other)
  "Return the other plist for alignment at commata, or nil if not applicable.
This is used for array literals, if `f90-ts-indent-array-align' is `right',
`both-left-primary' or `both-right-primary', the node at LOC is a direct child
of the array literal in CONTEXT, and it is followed by a comma or a closing
bracket on its line.

Every comma on a previous line of the array literal yields a candidate
represented as \(COMMA . -DELTA), with DELTA the distance of the current node
to its (virtual) comma, see `f90-ts--align-comma-delta'.  This right-aligns
the current node at that comma.  Candidates placing the node left of the
position of the first element after the opening bracket are dropped.  Among the
candidates, the one with the smallest column is the right-aligned primary
position (if several have the same column, the one with the smaller anchor
position).

The value of `f90-ts-indent-array-align' selects:
`right'               the right-aligned primary is forced, earlier items are
                      no candidates (:no-items-sym),
`both-right-primary'  the right-aligned primary is forced, earlier items
                      remain candidates,
`both-left-primary'   the primary of OTHER and the usual selection among
                      earlier items stay in charge, the commata are further
                      candidates.
For `left' there is nothing to do and nil is returned.

OTHER is the plist returned by `f90-ts--align-other-paren'; its :others (the
positions relative to the opening bracket) are always kept as candidates.
CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (when (memq f90-ts-indent-array-align '(right
                                          both-left-primary
                                          both-right-primary))
    (when-let* ((list-context (car context))
                (node         (alist-get 'node loc))
                ((f90-ts--align-nsym-compatible-p (alist-get 'nsym loc) 'named))
                ((treesit-node-eq (treesit-node-parent node) list-context))
                (delta        (f90-ts--align-comma-delta node))
                (open         (treesit-node-child list-context 0))
                ((f90-ts--node-type-p open '("[" "(/")))
                (col-min      (f90-ts--continued-line-cache-get-col
                               (cons open
                                     (+ (1- (length (treesit-node-text open)))
                                        f90-ts-indent-paren-default))))
                (anoffs       (cl-loop
                               for (n . s) in (plist-get items-plist :prev)
                               for anoff = (cons n (- delta))
                               when (and (eq s 'comma)
                                         (>= (f90-ts--continued-line-cache-get-col anoff)
                                             col-min))
                               collect anoff))
                (sorted       (seq-sort #'f90-ts--align-comma-anoff-less-p
                                        anoffs)))
      (let ((others (append sorted (plist-get other :others))))
        (pcase f90-ts-indent-array-align
          ;; right-aligned only: forced primary, no left-aligned candidates
          ('right
           (list :primary (car sorted)
                 :force t
                 :no-items-sym t
                 :others others))
          ;; both: right-aligned is primary
          ('both-right-primary
           (list :primary (car sorted)
                 :force t
                 :others others))
          ;; both: left-aligned is primary, the commata are further candidates
          (_
           (list :primary (plist-get other :primary)
                 :force   (plist-get other :force)
                 :others  others)))))))


(defun f90-ts--align-other-array (context loc items-plist)
  "Return the other plist for array literals.
Use alignment at commata, if enabled and applicable, see
`f90-ts--align-other-array-commas'.  Otherwise this is the same as for other
parenthesized lists, see `f90-ts--align-other-paren'.

CONTEXT, LOC and ITEMS-PLIST are as described in the calling convention of
the align-list-other family."
  (let ((other (f90-ts--align-other-paren context loc items-plist)))
    (or (f90-ts--align-other-array-commas context loc items-plist other)
        other)))


;;++++++++++++++
;; list context: items and columns

(defun f90-ts--align-filter-prev (items cur-line)
  "Filter ITEMS by line number and remove most ampersands.
For alignment purposes, only items on previous lines are relevant.

The filter removes any items not before line CUR-LINE.  It also filters out
trailing and virtual ampersand nodes, which are not relevant for alignment.
Leading ampersands are removed during indentation, except for ampersands
within strings, which we want to keep."
  (seq-filter
   ;; filter by line number, use only items on some previous line,
   ;; also drop continuation symbols, as these are not relevant for alignment
   (lambda (n) (and (< (f90-ts--node-line n)
                       cur-line)
                    (or (not (f90-ts--node-type-p n "&"))
                        ;; accept a real leading ampersand
                        ;; (which appears within a string_literal)
                        (f90-ts--node-leading-real-ampersand-p n))))
   items))


(defun f90-ts--align-attach-nsym (items)
  "Attach node symbol information to a list of ITEMS.
Map each NODE in ITEMS, to a cons cell (NODE . NSYM)."
  (mapcar
   (lambda (n) (cons n (f90-ts--align-node-symbol n)))
   items))


(defun f90-ts--align-items-head-p (node-nsym context-line)
  "Predicate to determine relevant items on first context line (head line).
NODE-NSYM is a cons (NODE . NSYM) providing NODE and the symbol for NODE.
The predicate compares line number of NODE with CONTEXT-LINE.
It also drops comments and auxiliary context nodes (assignment, parenthesis,
comma) on the head line, as items on the head line are used to  verify whether
proper items are present on this line."
  (let ((n (car node-nsym)))
    (and (= context-line (f90-ts--node-line n))
         (not (f90-ts--node-type-p n "comment"))
         (not (f90-ts--align-aux-context-p n)))))


(defun f90-ts--align-items-tail-p (node-nsym context-line)
  "Predicate to determine relevant items in the tail after context line.
NODE-NSYM is a cons (NODE . NSYM) providing NODE and the symbol for NODE.
The predicate compares line number of NODE with CONTEXT-LINE.
It also drops comments, like `f90-ts--align-items-head-p' does for the head
line.  The column of a comment is a consequence of alignment (with items or
with a trailing comment of the head line), it does not reflect the style of the
list items, and must not influence the decision which position is primary."
  (let ((n (car node-nsym)))
    (and (< context-line (f90-ts--node-line n))
         (not (f90-ts--node-type-p n "comment")))))


(defun f90-ts--align-unary-arg-delta (node)
  "Delta from NODE start to its unary argument start, nil if on another line."
  (let ((arg (treesit-node-child-by-field-name
              (f90-ts--node-leftmost-unary node) "argument")))
    (when (and arg
               (= (f90-ts--node-line node)
                  (f90-ts--node-line arg)))
      (- (treesit-node-start arg)
         (treesit-node-start node)))))


(defun f90-ts--align-nsym-offset (loc item-nsym)
  "Compute alignment offset for ITEM-NSYM depending on LOC.
LOC is the location data at point, and provides a reference symbol and the
node at point.  ITEM-NSYM is a cons (NODE . NSYM) for a filtered item of the
list context.

NSYM at LOC and item-nsym determine the offset:
`named' is paired with `operator-unary-minusplus' gives offset -1;
`operator-unary-minusplus' paired with an item `named' gives offset 1;
otherwise offset is 0.
Note that `operator-unary-minusplus' are unary_expression nodes with
operator \"-\" and no other unary expression.
Furthermore, offset is in fact the difference of \"-\" and argument node of
unary_expression, to handle blanks between the minus and the value content."
  (let ((nsym (cdr item-nsym))
        (nsym-loc (alist-get 'nsym loc)))
    (cond
     ((and (memq nsym-loc '(named operator-unary-other))
           (eq nsym 'operator-unary-minusplus))
      (or (f90-ts--align-unary-arg-delta (car item-nsym))
          1))

     ((and (eq nsym-loc 'operator-unary-minusplus)
           (memq nsym '(named operator-unary-other)))
      (if-let* ((delta (f90-ts--align-unary-arg-delta (alist-get 'node loc))))
          (- delta)
        ;; no usable argument on this line: just move the minus one
        ;; character to the left of the `named'/`operator-unary-other' anchor
        -1))

     (t 0))))


(defun f90-ts--align-nsym-filter-offset (items-prev-sym pred loc)
  "Filter ITEMS-PREV-SYM by PRED, then map to (NODE . OFFSET) cons cells.
ITEMS-PREV-SYM is a list of (NODE . NSYM).  PRED is called for
each (NODE . NSYM) cons cell in ITEMS-PREV-SYM.  LOC is passed to
`f90-ts--align-nsym-offset' to compute the offset for each item,
which satisfies PRED."
  (mapcar
   (lambda (nsym)
     (cons (car nsym)
           (f90-ts--align-nsym-offset loc nsym)))
   (seq-filter pred items-prev-sym)))


(defun f90-ts--align-map-col-pos-off (anoff)
  "Map an ANOFF = (ANCHOR.OFFSET) to a triple (COLUMN POSITION OFFSET).
ANCHOR is either a node or an otherwise obtained position.
For a node, the start position of node is used.

The returned COLUMN in the triple is the final column, taking (possibly)
cached indentation on the line at anchor into account.

OFFSET is allowed to be negative and restricted if necessary to avoid negative
column numbers."
  (cl-assert
   (and (or (treesit-node-p (car anoff))
            (integerp (car anoff)))
        (integerp (cdr anoff)))
   nil
   "anoff is not a cons (node . offset) or (pos . offset), got %s" anoff)

  (let* ((anchor (car anoff))
         (offset (cdr anoff))
         (pos (if (treesit-node-p anchor)
                  (treesit-node-start anchor)
                anchor))
         ;; we need to bound offset here if negative
         (col (f90-ts--continued-line-cache-get-col (cons pos 0)))
         (offset-bounded (max (- col) offset)))
    ;; if offset is negative, we need to be careful, to avoid getting a
    ;; negative column (corresponding to a buffer position on the
    ;; previous line), the used position must always be valid in the
    ;; current buffer before indentation takes place
    (list
     (+ col offset-bounded)
     pos
     offset-bounded)))


(defun f90-ts--align-cpo-max-pos (cpo-list)
  "Return the entry in CPO-LIST with the largest position.
CPO-LIST is a non-empty list of (column position offset) triples.
In general, the cpo item with the largest position is the most relevant,
as it is closest to the current line."
  (cl-reduce
   (lambda (cpo1 cpo2)
     ;; extract and compare positions
     (if (> (cadr cpo1) (cadr cpo2)) cpo1 cpo2))
   cpo-list))


(defun f90-ts--align-cpo-sort (cpo-list)
  "Make CPO-LIST of (COLUMN POSITION OFFSET) triples unique by column position.
For several entries with same column number, take the element with the largest
buffer position."
  (let ((cpo-list-unique
         (seq-map (lambda (group)
                    ;; elements produced by seq-group-by are:
                    ;; group = (col (col pos1 offset1) (col pos2 offset2) ...)
                    ;; ((cdr group) gets rid of initial col)
                    (f90-ts--align-cpo-max-pos (cdr group)))
                  (seq-group-by #'car cpo-list))))
    (seq-sort (lambda (a b) (< (car a) (car b))) cpo-list-unique)))


(defun f90-ts--align-aligned-p (cpo col-cur)
  "Check whether item provided by tuple CPO was aligned at COL-CUR.
Current column COL-CUR is the column number at line to be indented.
CPO = (COLUMN POSITION OFFSET) represents a potential anchor with offset on a
previous line.  POSITION is serves as anchor and column is the resulting
column, after applying OFFSET.
In order to preserve existing alignment, the current column COL-CUR must be
compared with the original column of the item represented by CPO.  This depends
on the flush state of the internal buffer of the treesit indent engine."
  (let* ((col-item (car cpo))
         (pos (cadr cpo))
         (col-orig (f90-ts--continued-line-cache-get-orig col-item pos)))
    (= col-cur col-orig)))


(defun f90-ts--align-find-aligned (col-pos-off col-cur)
  "Return the entry in COL-POS-OFF that was originally aligned at COL-CUR.
COL-POS-OFF is an unsorted list of (COLUMN POSITION OFFSET) triples, see
`f90-ts--align-aligned-p'.  If several entries were aligned at COL-CUR,
return the one with the largest position.
If there is no such cpo item, return nil."
  (let ((aligned (seq-filter (lambda (cpo)
                                (f90-ts--align-aligned-p cpo col-cur))
                              col-pos-off)))
    (when aligned
      (f90-ts--align-cpo-max-pos aligned))))


(defun f90-ts--align-orig-col (cpo)
  "Return the original column of the candidate CPO = (COLUMN POSITION OFFSET).
This is the column the candidate had before indentation of earlier lines of the
continued statement, see `f90-ts--continued-line-cache-get-orig'."
  (f90-ts--continued-line-cache-get-orig (car cpo) (cadr cpo)))


(defun f90-ts--align-next-orig (cpo-list col-cur)
  "Return the entry of CPO-LIST with the smallest original column above COL-CUR.
Among several entries with that original column take the one with the largest
position.  Return nil if there is no entry with original column above COL-CUR."
  (when-let* ((greater (seq-filter (lambda (cpo)
                                     (< col-cur (f90-ts--align-orig-col cpo)))
                                   cpo-list))
              (col-min (apply #'min (mapcar #'f90-ts--align-orig-col greater))))
    (f90-ts--align-cpo-max-pos
     (seq-filter (lambda (cpo) (= col-min (f90-ts--align-orig-col cpo)))
                 greater))))



;; General note on columns in `f90-ts--align-select': the column COL-CUR of the
;; line to be indented is the original column, but the columns of the
;; candidates are final columns, which include the shift of the (already
;; indented) lines the candidates are on.  Comparing COL-CUR with them directly
;; is wrong in `indent-region' for input whose indentation differs from the
;; final one.  Thus all comparisons with COL-CUR use the original columns of the
;; candidates, see `f90-ts--align-orig-col'.

(defun f90-ts--align-select (variant col-cur primary items)
  "Select (anchor offset) from PRIMARY and ITEMS.
Depending on VARIANT and current column COL-CUR, select the relevant
anchor and offset.

PRIMARY is a default/fallback anchor (position offset).  Depending on
VARIANT (like keep-or-primary) and current alignment, PRIMARY is
selected."
  ;; note that entries in col-pos are triples
  ;; cpo = (column position offset),
  ;; where any position in the list must be the start of a previous node
  ;; to ensure that treesitter buffering in indent-region works as expected,
  (let* ((col-pos-off-unsorted (seq-map #'f90-ts--align-map-col-pos-off items))
         ;; alignment check needs the unsorted list, as sorting also removes double
         ;; entries resulting in the same column AFTER indentation, but aligned-at
         ;; checks the original column
         (aligned-at (f90-ts--align-find-aligned col-pos-off-unsorted
                                                 col-cur))
         ;; sort and uniquify the list, keep only one entry per resulting column
         (col-pos-off (f90-ts--align-cpo-sort col-pos-off-unsorted))
         (primary-col-pos-off (f90-ts--align-map-col-pos-off primary)))
    ;; :get-other-fn should always return some fallback position
    ;; (like pstmt-1+default indent for continued lines), and thus
    ;; there must always be some anchors in col-pos-off
    (cl-assert col-pos-off nil "no relevant columns found")

    ;; the selection process selects a triple, drops the column and returns
    ;; the two remaining elements (buffer position, offset)
    (cond
     ;; special case (e.g. after inserting newline by <return> or f90-line-break)
     ;; check whether we are before first entry in col-pos-off,
     ;; if this is the case we go to primary, not to first entry in col-pos-off;
     ;; compare original columns, see note above
     ((< col-cur (apply #'min (mapcar #'f90-ts--align-orig-col
                                      col-pos-off-unsorted)))
      (cdr primary-col-pos-off))

     ;; cases: (aligned, rotate), (not-aligned, rotate),
     ;;        (not-aligned,keep-or-rotate)
     ((or (eq variant 'rotate)
          (and (not aligned-at)
               (eq variant 'keep-or-rotate)))
      ;; go to next column (in original columns, see note above) or wrap
      ;; around, recall that col-pos-off is sorted by final columns
      (let ((aligned-next (f90-ts--align-next-orig col-pos-off-unsorted
                                                   col-cur)))
        ;; next if there is a next, otherwise first entry (not necessarily primary)
        (or (cdr aligned-next)
            (cdar col-pos-off))))

     ;; cases: (not-aligned, keep-or-primary),
     ;;        (not-aligned, keep-or-continued-line),
     ;;        (aligned, primary), (not-aligned, primary)
     ((or (not aligned-at)
          (eq variant 'primary))
      ;; not that for keep-or-continued-line, primary is forced to continued line offset
      (cdr primary-col-pos-off))

     ;; cases: (aligned, keep-or-primary)
     ;;        (aligned, keep-or-rotate)
     ;;        (aligned, keep-or-continued-line)
     ((and aligned-at
           (member variant '(keep-or-primary
                             keep-or-rotate
                             keep-or-continued-line)))
      ;; aligned, keep current column, but use proper element from col-pos-off
      ;; as anchor, otherwise indent-region does not take indentation of anchor
      ;; position into account
      (cdr aligned-at))

     (t
      ;; all ten cases plus node before minimal column are covered above
      (cl-assert col-pos-off nil "cond logic not complete")
      (cdr primary-col-pos-off)))))


(defun f90-ts--align-expand-items-op-expr (items)
  "Return ITEMS with each operator expression replaced by its operands.
See `f90-ts--align-expand-op-expr'.  Other items are kept."
  (seq-mapcat (lambda (n)
                (if (f90-ts--node-op-expr-p n)
                    (f90-ts--align-expand-op-expr n)
                  (list n)))
              items 'list))


(defun f90-ts--align-prev-nsym (items cur-line)
  "Return ITEMS located before CUR-LINE, each attached to its node symbol."
  (f90-ts--align-attach-nsym
   (f90-ts--align-filter-prev items cur-line)))


(defun f90-ts--align-anoff-items (loc context)
  "Filter relevant alignment anchors from CONTEXT.
Return all list context items before location specified by LOC, and
additionally further filtered by node symbol type.

LOC is an alist which provides the node and further location data.

Note: for anchor-offset pairs extracted from CONTEXT nodes directly,
the offset is assumed to be 0."
  (let* ((get-items (f90-ts--get-list-context-prop :get-items-fn context))
         (cur-line (alist-get 'line loc))
         (cntxt-line (f90-ts--node-line (or (cdr context) (car context))))
         (node-sym (alist-get 'nsym loc))
         (items-all (funcall get-items context loc))
         (prev-sym (f90-ts--align-prev-nsym items-all cur-line))
         ;; the tail uses the operands of operator expressions among the
         ;; items, as a comma is considered just another operator composing
         ;; a tuple; otherwise "(1 + &\n 2 &\n )" has no tail items, as the
         ;; expression starts on the head line
         (prev-sym-expanded
          (f90-ts--align-prev-nsym
           (f90-ts--align-expand-items-op-expr items-all)
           cur-line)))
    (cl-flet ((select (items pred)
                (f90-ts--align-nsym-filter-offset items pred loc)))
      (list :sym  (when node-sym
                    (select prev-sym
                            (lambda (nsym)
                              (f90-ts--align-nsym-compatible-p (cdr nsym)
                                                               node-sym))))
            :prev prev-sym
            :head (select prev-sym
                          (lambda (nsym)
                            (f90-ts--align-items-head-p nsym cntxt-line)))
            :tail (select prev-sym-expanded
                          (lambda (nsym)
                            (f90-ts--align-items-tail-p nsym cntxt-line)))))))


(defun f90-ts--align-anoff-other (loc anoff-items-plist context)
  "Return the other plist (:primary :force :others) for alignment in CONTEXT.
ANOFF-ITEMS-PLIST are some items lists of node items already obtained by
`f90-ts--align-anoff-items'.
These lists might be necessary to determine which primary and alternative
alignment positions to select.
LOC provides node and location data.
Return nil if the list context has no :get-other-fn.
See the calling convention of the align-list-other family."
  (when-let* ((get-other (f90-ts--get-list-context-prop :get-other-fn context)))
    (funcall get-other context loc anoff-items-plist)))


(defun f90-ts--align-anchor-offset (variant loc context)
  "Determine a pair (anchor offset) for alignment of a node in a list context.
Location data is given as an alist LOC containing node, column, line number and
node symbol.
To this end items on continued lines in the provided CONTEXT are
determined.  Additionally further anchors (like the first node of previous
statement by pstmt-1) are considered.
Finally use VARIANT to select one pair to align with."
  (let* ((node (alist-get 'node loc))
         ;; a comment aligned with the trailing comment on the previous line
         (anoff-comment (and node (f90-ts--trailing-comment-primary node)))
         (anoff-items-plist (f90-ts--align-anoff-items loc
                                                       context))
         ;;(_ (progn
         ;;     (f90-ts-log-msg :anoff "items-sym = %s"  (plist-get anoff-items-plist :sym))
         ;;     (f90-ts-log-msg :anoff "items-prev = %s" (plist-get anoff-items-plist :prev))
         ;;     (f90-ts-log-msg :anoff "items-head = %s" (plist-get anoff-items-plist :head))
         ;;     (f90-ts-log-msg :anoff "items-tail = %s" (plist-get anoff-items-plist :tail))))
         (anoff-items-sym (plist-get anoff-items-plist :sym))
         (anoff-other (f90-ts--align-anoff-other loc
                                                 anoff-items-plist
                                                 context))
         ;; always add default continued offset position as possible anchor
         (anoff-continued (f90-ts--align-pstmt1-anoff))
         ;; anoff-default becomes anoff-primary in a number of cases, but not always
         (anoff-default (plist-get anoff-other :primary))
         ;; determine whether anoff-default, anoff-continued or something else is used as primary:
         ;; 1. keep-or-continued-line forces the continued line position
         ;; 2. a comment following a matching trailing comment aligns with it
         ;; 3. the context may force its default
         ;; 4. if there are earlier items compatible with the current node,
         ;;    the one with the smallest column
         ;; 5. the default of the context
         ;; 6. the continued line position
         (anoff-primary
          (cond
           ((eq variant 'keep-or-continued-line)
            anoff-continued)
           (anoff-comment
            anoff-comment)
           ((plist-get anoff-other :force)
            (or anoff-default anoff-continued))
           (anoff-items-sym
            (f90-ts--align-smallest-anoff anoff-items-sym))
           (t
            (or anoff-default anoff-continued))))
         ;; final list of anchors (which are nodes or pairs (position offset))
         (anoff-final (append (list anoff-continued)
                              (and anoff-comment (list anoff-comment))
                              (and anoff-default (list anoff-default))
                              (plist-get anoff-other :others)
                              (unless (plist-get anoff-other :no-items-sym)
                                anoff-items-sym))))
    ;;(f90-ts-log-msg :anoff "items-other = %s" anoff-other)
    ;;(f90-ts-log-msg :anoff "items-final = %s" anoff-final)
    ;;(f90-ts-log-msg :anoff "item-primary = %s" anoff-primary)
    ;; select the anoff among all the options determined so far
    (f90-ts--align-select variant
                          (alist-get 'col loc)
                          anoff-primary
                          anoff-final)))


;;++++++++++++++
;; anchor functions: continued lines
;; determine whether list or standard case

;; get-items:
;; function to determine node items in the list context relevant for alignment
;;
;; get-other: (optional)
;; function which returns a plist (:primary :force :others) with the context
;; specific default primary position and further candidate columns for
;; alignment (see the calling convention above `f90-ts--align-other-paren-expr-core'),
;; if not provided, only earlier items and the standard continued line position are used
(defconst f90-ts--align-list-context-properties
  (let ((expr-prop    '(:get-items-fn f90-ts--align-items-op-expr
                        :get-other-fn f90-ts--align-other-op-expr))
        (paren-prop   '(:get-items-fn f90-ts--align-items-children-no-paren
                        :get-other-fn f90-ts--align-other-paren))
        (bind-prop    '(:get-items-fn f90-ts--align-items-children2))
        (io-item-prop '(:get-items-fn f90-ts--align-items-children-no-paren
                        :get-other-fn f90-ts--align-other-item-list)))
    (cl-remove-if
     #'null
     (list
      (cons "logical_expression"       expr-prop)
      (cons "math_expression"          expr-prop)
      (cons "relational_expression"    expr-prop)
      (cons "concatenation_expression" expr-prop)
      (cons "unary_expression"         expr-prop)
      (cons "binding_list"             bind-prop)
      (cons "final_statement"          bind-prop)
      (cons "parenthesized_expression" paren-prop)
      (cons "argument_list"            paren-prop)
      (cons "array_literal"
            '(:get-items-fn f90-ts--align-items-children-no-paren
              :get-other-fn f90-ts--align-other-array))
      (when (f90-ts--grammar-string-literal-decomposed-p)
        (cons "string_literal"
              '(:get-items-fn f90-ts--align-items-string
                :get-other-fn f90-ts--align-other-string)))
      (cons "parameters"               paren-prop)
      (cons "output_item_list"         io-item-prop)
      (cons "input_item_list"          io-item-prop)
      (cons "case_statement"
            '(:get-items-fn f90-ts--align-items-case
              :get-other-fn f90-ts--align-other-case))
      (cons "association_list"
            '(:get-items-fn f90-ts--align-items-assocation
              :get-other-fn f90-ts--align-other-association))
      (cons "variable_declaration"
            '(:get-items-fn f90-ts--align-items-var-decl
              :get-other-fn f90-ts--align-other-var-decl))
      (cons "init_declarator"
            '(:get-items-fn f90-ts--align-items-init-decl
              :get-other-fn f90-ts--align-other-init-decl))
      (cons "pointer_init_declarator"
            '(:get-items-fn f90-ts--align-items-init-decl
              :get-other-fn f90-ts--align-other-init-decl))
      (cons "import_statement"
            '(:get-items-fn f90-ts--align-items-children1)))))
  "List of tree-sitter node types presenting some kind of list context.
A list context is a node with children which are suitable for alignment if
spread over several lines in a continued line statement.
The value of the alist provides properties to deal with such list contexts,
in particular for how to extract relevant alignment positions.")


(defun f90-ts--get-list-context-prop (pkey context)
  "Lookup CONTEXT and return the property value for PKEY.
Lookup is done in alist `f90-ts--align-list-context-properties'."
  (let* ((list-context (car context))
         (properties (alist-get (treesit-node-type list-context)
                                f90-ts--align-list-context-properties
                                nil
                                nil
                                #'string=)))
    (plist-get properties pkey)))


(defun f90-ts--align-list-context-max (loc)
  "Determine maximal subtree related to node representing a list-context.
Node location data is given by LOC.
This is the maximal node for any further subtree searches."
  (let* ((pos (alist-get 'pos loc))
         (line (alist-get 'line loc))
         (ps-key (f90-ts--indent-prev-stmt-keyword))
         (stmt-beg (treesit-node-start ps-key))
         (stmt-root (when (<= stmt-beg pos)
                      (treesit-node-on (treesit-node-start ps-key) pos))))
    (when stmt-root
      (treesit-search-subtree
       stmt-root
       (lambda (n)
         (and (<= (f90-ts--node-line n) line)
              (<= line (line-number-at-pos (treesit-node-end n)))
              (assoc (treesit-node-type n)
                     f90-ts--align-list-context-properties)))))))


(defun f90-ts--align-list-context-op-expr (loc parent)
  "Determine, whether LOC is in an operator expression list context.
For expressions, we need to check node in LOC or PARENT (node might be
nil).  But we do not need to ascend further."
  (let* ((node (alist-get 'node loc))
         (line (alist-get 'line loc))
         (stmt-min
          (or (when (f90-ts--node-op-expr-p node)   node)
              (when (f90-ts--node-op-expr-p parent) parent)))
         ;; find root of expression
         (root-expr (and stmt-min (f90-ts--node-op-expr-chain-root stmt-min)))
         ;; if we are on the same line as the root, we might need to check
         ;; the previous item (assignment "=", parenthesis "(" or separator ",")
         ;; will be included, but there is no proper list-context node to include this)
         (psib-expr (and root-expr
                         (f90-ts--prev-sibling-proper root-expr))))
    (cond
     ;; note: unlike the second and third clauses below, this one does
     ;; not require root-expr itself to be before LINE; that is because an
     ;; assignment or opening parenthesis immediately preceding the expression
     ;; is a valid head even when the expression starts on the current line
     ;; (for example the first continued line after "x = a + &").
     ((and (f90-ts--node-before-line-p psib-expr line)
           (f90-ts--node-type-p psib-expr '("=" "(")))
      (cons root-expr psib-expr))

     ((and (f90-ts--node-before-line-p root-expr line)
           (f90-ts--node-before-line-p psib-expr line)
           (f90-ts--node-type-p psib-expr ","))
      ;; if root-expr is on the same line, there must be a more appropriate
      ;; list context the commata belongs to, use commata as head
      (cons root-expr psib-expr))

     ((f90-ts--node-before-line-p root-expr line)
      (cons root-expr nil)))))


(defun f90-ts--align-list-context-other (loc parent)
  "If LOC is in a list-context, other than expressions, return that context.
Use node from LOC or PARENT to start the search, which might require to
ascend a few steps.
This function returns nil as second auxiliary context node, except for a
case statement, where it is the opening parenthesis."
  (let* ((node (or (alist-get 'node loc)
                   parent))
         (line (alist-get 'line loc))
         (list-context (treesit-parent-until
                        node
                        (lambda (n)
                          (and (< (f90-ts--node-line n) line)
                               (assoc (treesit-node-type n)
                                      f90-ts--align-list-context-properties)))
                        t ; include node in search
                        )))
    ;; the search may ascend to an operator expression although neither node
    ;; nor parent is one, for example at `%bar' in
    ;;    x = aa + foo &
    ;;           %bar + cc
    ;; whose parent is a derived_type_member_expression; operator expressions
    ;; are handled by `f90-ts--align-list-context-op-expr', which provides the
    ;; required auxiliary node;
    ;; drop it, if it is found here, as it is not a usable context
    (cond
     ((null list-context)
      nil)
     ((f90-ts--node-op-expr-p list-context)
      nil)
     ;; no inner context exists, but the case statement includes its body:
     ;; the context is restricted to the header, which needs the parenthesis
     ((f90-ts--node-type-p list-context "case_statement")
      (f90-ts--align-list-context-case-aux list-context loc))
     (t
      (cons list-context nil)))))


(defun f90-ts--align-list-context-case-aux (case-stmt loc)
  "Return a context for LOC in the header of the case statement CASE-STMT.
The \"case_statement\" contains the body of the case branch, but is a list
context only between its parentheses, including the closing one.  Nodes
elsewhere, like comments and continued lines in the body, are not in this
context, return nil for these.  The opening parenthesis is the auxiliary node
of the context, which provides the head line of the list."
  (when-let* ((parens (f90-ts--align-items-case-paren-children case-stmt))
              (open  (car parens))
              (close (cdr parens))
              (pos   (alist-get 'pos loc))
              ((< (treesit-node-start open) pos))
              ((<= pos (treesit-node-start close)))
              ((< (f90-ts--node-line open) (alist-get 'line loc))))
    (cons case-stmt open)))


(defun f90-ts--align-list-context (loc parent)
  "Check whether LOC and PARENT are within a list context and return it.
A context is a cons pair of the list-context itself (root node of the
context) and optionally a second auxiliary node.

Often the list context is PARENT, but sometimes a related node like
grandparent, etc.

The smallest such context, starting on a previous line is returned.
Return value nil signals that position is not within a list context.

Only expressions and case statements return an auxiliary node, which is
assignment \"=\" or parenthesis \"(\".  It is required but not part of the list
context node."
  (when-let* ((stmt-max (f90-ts--align-list-context-max loc)))
    (or (f90-ts--align-list-context-op-expr loc parent)
        (f90-ts--align-list-context-other loc parent))))


(defun f90-ts--continued-line-anchor-offset (variant pstmt-1 loc parent)
  "Compute the anchor-offset for a continued line, or nil to use the default.
VARIANT controls alignment behavior.  PSTMT-1 is the previous statement first
node.  LOC is an list contain location data.  PARENT is the parent node.
It returns a pair (anchor offset), or nil if no proper list context could
be determined or variant prescribes the default continued line indentation."
  (unless (or (eq variant 'continued-line)
              (not pstmt-1))
    (when-let* ((context (f90-ts--align-list-context loc parent)))
      ;;(f90-ts-log-msg :anoff "loc = %s" loc)
      ;;(f90-ts-log-msg :anoff "context=%s" context)
      ;;(f90-ts-log-msg :anoff "parent = %s" parent)
      (cl-assert (< (f90-ts--node-line (or (cdr context)
                                           (car context)))
                    (alist-get 'line loc))
                 nil
                 "context not on some previous line: context=%s, loc-line=%s"
                 context
                 (alist-get 'line loc))
      (f90-ts--align-anchor-offset variant
                                   loc
                                   context))))


(defun f90-ts--continued-line-trailing-comment-anoff (variant loc)
  "Return anchor-offset for a comment following a trailing comment.
VARIANT controls alignment behavior, LOC is the location data of the node.
This is the case without list context, otherwise
`f90-ts--align-anchor-offset' takes care of comments.  Candidates are the
continued line position and the column of the trailing comment.  The latter is
primary if the comments match, see `f90-ts--trailing-comment-primary', except
for variant `keep-or-continued-line'.  Return nil if NODE is no such comment or
if VARIANT prescribes the default continued line indentation."
  (unless (eq variant 'continued-line)
    (when-let* ((node (alist-get 'node loc))
                ((f90-ts--node-type-p node "comment"))
                (prev-sib (f90-ts--trailing-comment-prev node)))
      (let ((continued (f90-ts--align-pstmt1-anoff))
            (trailing (cons prev-sib 0)))
        (f90-ts--align-select variant
                              (alist-get 'col loc)
                              (if (and (not (eq variant 'keep-or-continued-line))
                                       (f90-ts--trailing-comment-primary node))
                                  trailing
                                continued)
                              (list continued trailing))))))


(defun f90-ts--continued-line-anchor (node parent bol &rest _)
  "Determine anchor for a continued line given by NODE, PARENT and BOL.
If the current position is within a list like context, try to align with
list items like arguments in a procedure call or other related columns like
those of parenthesis.  Otherwise determine standard indentation for
continued lines.

Exact behaviour is determined by custom variables
`f90-ts-indent-list-line' and `f90-ts-indent-list-region'.

This anchor requires a matcher which succeeds only if on a subsequent continued
line.  The first statement line where the statement starts must not catched by
the continued line matcher."
  (let ((variant (or f90-ts--align-continued-variant-tab
                     f90-ts-indent-list-region))
        (pstmt-1 (f90-ts--indent-prev-stmt-first)))
    (cl-assert pstmt-1 nil
               "continued subsequent line must have a pstmt-1 node")
    (f90-ts--continued-line-cache-update
     (treesit-node-start pstmt-1))
    (let* ((loc (f90-ts--align-location node))
           (default-anchor-offset (if pstmt-1
                                      (list (treesit-node-start pstmt-1)
                                            f90-ts-indent-continued)
                                    (list bol 0)))
           (anchor-offset (or (f90-ts--continued-line-anchor-offset variant
                                                                    pstmt-1
                                                                    loc
                                                                    parent)
                              ;; if there is no list context and node is a comment
                              ;; following a trailing comment, then choose between
                              ;; the continued line position and the column of that
                              ;; comment (primary if both comments match)
                              (f90-ts--continued-line-trailing-comment-anoff
                               variant loc)
                              default-anchor-offset)))
      ;; there are two caches:
      ;;  * cache anchor and offset for offset function for the
      ;;    current line
      ;;  * cache computed offset for indentation of subsequent lines
      (let ((anchor (car anchor-offset))
            (offset (cadr anchor-offset)))
        ;; (strictly, anchor does not need to be cached)
        (f90-ts--indent-anchor-cache anchor)
        (f90-ts--indent-offset-cache offset)
        (f90-ts--continued-line-cache-put-subsequent bol anchor offset)
        anchor))))


;;++++++++++++++
;; simple indentation rules: expansion with f90-ts-- prefix

(defmacro f90-ts--map-rule (matcher anchor offset)
  "Map an indent rule triple with f90-ts-- prefix prepended where applicable.
MATCHER is either a bare symbol (direct matcher function) or a list
whose car is a constructor name.  ANCHOR and OFFSET are bare symbols or
literals.  Any symbol present in an internal list is prefixed, the rest is
passed through unchanged."
  (let ((prefix-syms '(;; matcher constructor
                       nopp-parent-is
                       nopp-n-p-gp
                       nopp-n-p-pstmtk
                       nopp-n-p-gp-pstmtk
                       n-p-ch-psibp
                       ;; dummy matcher (always fail, but do preparations)
                       populate-cache
                       continued-line-cache-start
                       ;; direct matcher
                       continued-subsequent-line-is
                       openmp-comment-is
                       preproc-node-is
                       preproc-at-toplevel-is
                       special-comment-is
                       comment-region-is
                       trailing-comment-region-is
                       ;; anchor
                       nopp-parent
                       nopp-grandparent
                       continued-line-anchor
                       previous-stmt-anchor
                       catch-all-anchor
                       cached-anchor
                       ;; offset
                       minus-block-offset
                       toplevel-offset
                       cached-offset)))
    (cl-flet ((mp (sym)
                (if (memq sym prefix-syms)
                    (intern (concat "f90-ts--" (symbol-name sym)))
                  sym)))
      (let* ((m (cond
                  ((and (listp matcher) matcher)
                   `'(,(mp (car matcher)) ,@(cdr matcher)))
                  ((symbolp matcher)
                   `',(mp matcher))
                  (t `',matcher)))
             (a (if (symbolp anchor) `',(mp anchor) anchor))
             (o (cond ((symbolp offset) `',(mp offset))
                      (t offset))))
        `(list ,m ,a ,o)))))

(defmacro f90-ts--with-map-rules (&rest triples)
  "Build a list of treesit indent rule triples.
Each element of TRIPLES is in most cases a (MATCHER ANCHOR OFFSET) form as
accepted by `f90-ts--map-rule'.  Return a list of all expanded
triples suitable for the treesit simple indentation engine.

The first exception is (log-state MSG), which expands to the
triple ((f90-ts-log-indent-print-state MSG) parent 0) directly.

The second exception are conditional triples of the
form (when COND (MATCHER ANCHOR OFFSET))
and (unless COND (MATCHER ANCHRO OFFSET)).
COND is evaluated at runtime, and the entry contributes its triple only when
COND is non-nil (when) or nil (unless).  As the condition is evaluated after
the grammar is loaded, it can be used to adjust for grammar variants (like
`f90-ts--grammar-string-literal-decomposed-p', which checks for string_literal
parser variant)."
 `(cl-loop
   for rule in (list ,@(mapcar
                        (lambda (entry)
                          (pcase entry
                            (`(log-state ,msg)
                             `(list '(f90-ts-log-indent-print-state ,msg) 'parent 0))
                            (`(when ,cond-form (,matcher ,anchor ,offset))
                             `(when ,cond-form
                                (f90-ts--map-rule ,matcher ,anchor ,offset)))
                            (`(unless ,cond-form (,matcher ,anchor ,offset))
                             `(unless ,cond-form
                                (f90-ts--map-rule ,matcher ,anchor ,offset)))
                            (`(,matcher ,anchor ,offset)
                             `(f90-ts--map-rule ,matcher ,anchor ,offset))))
                          triples))
   when rule collect rule))


;;++++++++++++++
;; simple indentation rules

(defun f90-ts--indent-rules-start ()
  "Indentation rules executed at start.
The main purpose is to fill the indentation cache for a new run."
  (f90-ts--with-map-rules
   ;; populate cache and then always fail
   (populate-cache parent 0)
   ;; info rules needs populated cache
   ;;(log-state "start")
   ))


(defun f90-ts--indent-rules-comments ()
  "Indentation rules for comments (excluding OpenMP statements)."
  (f90-ts--with-map-rules
   ;; there are special comments in `f90-ts-special-comment-rules',
   ;; distinguish between default and these special comments
   ;; (including openmp lines as well)
   ;;
   ;; default comments as well as special comments with property
   ;; `indented' are indented like code, these are not handled here
   ;; other comments are matched and anchor and offset caches are
   ;; loaded, according the the rule properties in
   ;; `f90-ts-special-comment-rules'
   ;;
   ;; if a comment follows another comment of the same kind (same
   ;; special rule), then alignment is with respect to the previous
   ;; comment in this comment region, the values are determined by the
   ;; matcher and saved in the cache
   (comment-region-is cached-anchor cached-offset)
   ;; indent separator comments like their parent nodes
   ;; this check is after the region check, hence previous sibling
   ;; is not a comment of same kind
   (special-comment-is cached-anchor cached-offset)
   ;; own-line comment following a trailing comment on the previous nonblank
   ;; line (not continued): align with that comment if both match the same
   ;; special comment rule, otherwise fail and use the default indentation.
   ;; On continued lines the continued line anchor deals with comments.
   (trailing-comment-region-is cached-anchor cached-offset)))


(defun f90-ts--indent-rules-preproc ()
  "Indentation rules for preprocessor directives."
  (f90-ts--with-map-rules
   ;; indent preprocessor directive
   ;; directive itself: no indent
   ;; otherwise, skip preproc nodes as if they are not present
   ;; (this is done by the nopp=no-preproc mechanics)
   (preproc-node-is column-0 0)))


(defun f90-ts--indent-rules-continued ()
  "Indentation rules for continued lines."
  (f90-ts--with-map-rules
   ;; handle continued lines
   ;; if on first line of a continued statement, reset the continued-line cache
   ;; but always fail;
   ;; (the cache stores offsets on previous lines, this is necessary in
   ;; indent-region operations with buffering)
   (continued-line-cache-start parent 0)

   ;; grammar variant without string_literal decomposition:
   ;; continued strings need special care, the ampersands do not appear as
   ;; separate nodes, (treesit-node-at pos) at indentation position returns
   ;; the string_literal, which starts at some previous line
   (unless (f90-ts--grammar-string-literal-decomposed-p)
     ((nopp-n-p-gp nil "string_literal" nil) nopp-parent 0))

   ;; it is easy to see whether we are on a continued line (not the first line
   ;; of a multiline statement, only subsequent lines), but handling specific
   ;; cases is not possible with just some simple n-p-gp pattern,
   (continued-subsequent-line-is f90-ts--continued-line-anchor cached-offset)))


(defun f90-ts--indent-rules-internal-proc ()
  "Indentation rules for internal_proc node.
This node occurs in conjunction with \"contain\" statements."
  (f90-ts--with-map-rules
   ;; contains statements in modules, programs, subroutines or functions,
   ;; no indentation for contains
   ((node-is         "internal_procedures")         nopp-parent 0)
   ((nopp-parent-is  "internal_procedures")         nopp-parent toplevel-offset)
   ((nopp-n-p-gp nil "ERROR" "internal_procedures") nopp-parent toplevel-offset)))


(defun f90-ts--indent-rules-prog-mod ()
  "Indentation rules for program and module nodes."
  (f90-ts--with-map-rules
   ;; program or module interface part (before contains) and end statement
   ;; in all cases: first match node with end_xyz_statement, and then only
   ;; whether parent is xyz, as parent is xyz in both cases
   ((node-is             "end_program_statement")    nopp-parent 0)
   ((nopp-parent-is      "program\\(_statement\\)?") nopp-parent toplevel-offset)
   ((nopp-n-p-pstmtk nil "ERROR" "program")          nopp-parent toplevel-offset)

   ;; parent-is uses regexp matching, thus use "^module" to avoid that it
   ;; matches "submodule"
   ((node-is             "end_module_statement")      nopp-parent 0)
   ((nopp-parent-is      "^module\\(_statement\\)?$") nopp-parent toplevel-offset)
   ((nopp-n-p-pstmtk nil "ERROR" "^module$")          nopp-parent toplevel-offset)

   ((node-is             "end_submodule_statement")      nopp-parent 0)
   ((nopp-parent-is      "^submodule\\(_statement\\)?$") nopp-parent toplevel-offset)
   ((nopp-n-p-pstmtk nil "ERROR" "^submodule$")          nopp-parent toplevel-offset)))


(defun f90-ts--indent-rules-function ()
  "Indentation rules for functions and subroutines."
  (f90-ts--with-map-rules
   ;; functions and subroutine bodies
   ((node-is         "end_subroutine_statement")         nopp-parent 0)
   ((node-is         "end_function_statement")           nopp-parent 0)
   ((node-is         "end_module_procedure_statement")   nopp-parent 0)
   ((nopp-parent-is  "subroutine")                       nopp-parent f90-ts-indent-block)
   ((nopp-parent-is  "function")                         nopp-parent f90-ts-indent-block)
   ((nopp-parent-is  "module_procedure")                 nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil nil "subroutine")               nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil nil "function")                 nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil nil "module_procedure")         nopp-parent f90-ts-indent-block)))


(defun f90-ts--indent-rules-translation-unit ()
  "Indentation rules related to root node translation_unit.
These occur for functions and subroutines not within a \"contains\" section,
and in case of ERROR nodes with incomplete code."
  (f90-ts--with-map-rules
   ;; statements related to toplevel subroutine or function statements,
   ;; and ERROR cases (might or might not be toplevel)
   ;; most rules are for incomplete code (like shown in resources/indent_line_incomplete.erts)
   ;; unfortunately, these are difficult to reproduce and test with erts, as the exact composition of
   ;; relatives in the tree returned by tree-sitter queries depend on presence of trailing blanks and
   ;; newline symbols after point at |, like in this simple code piece:
   ;;
   ;; subroutine sub()
   ;; |
   ;; (as string: "subroutine sub()\n|", with point at the very last position, no newline)
   ;;
   ;; in short: there are no testcases for the following rules, but there are sometimes very short
   ;; code pieces like the above which might require exactly one of these rules...
   ((n-p-ch-psibp nil "translation_unit" nil "subroutine_statement") nopp-parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "ERROR"            nil "subroutine_statement") nopp-parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "translation_unit" nil "function_statement")   nopp-parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "ERROR"            nil "function_statement")   nopp-parent f90-ts-indent-block)
   ;; rule for translation_unit and module_procedure_statement does not seem possible,
   ;; as module procedure statements are only allowed within contains section

   ((n-p-ch-psibp nil "ERROR"            nil "module_procedure_statement") nopp-parent f90-ts-indent-block)
   ((nopp-parent-is "translation_unit") column-0 0)))


(defun f90-ts--indent-rules-interface ()
  "Indentation rules for interface blocks."
  (f90-ts--with-map-rules
   ;; (abstract) interface bodies
   ((node-is         "end_interface_statement") nopp-parent 0)
   ((nopp-parent-is  "interface")               nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil "ERROR" "interface")   nopp-parent f90-ts-indent-block)))


(defun f90-ts--indent-rules-dtype-enum ()
  "Indentation rules for derived type and enumeration type statements."
  (f90-ts--with-map-rules
   ;; derived type definitions
   ((nopp-n-p-gp        "end_type_statement"      "derived_type_definition" nil)                      nopp-parent 0)
   ((n-p-ch-psibp       "derived_type_procedures" "derived_type_definition" "contains_statement" nil) nopp-parent 0)
   ((n-p-ch-psibp       "ERROR"                   "derived_type_definition" "contains_statement" nil) nopp-parent 0)
   ((nopp-n-p-gp-pstmtk nil                       "ERROR"                   "derived_type_definition" "contains")  previous-stmt-anchor f90-ts-indent-block)
   ((nopp-n-p-gp-pstmtk nil                       "ERROR"                   "derived_type_definition" "procedure") previous-stmt-anchor 0)
   ((nopp-parent-is     "derived_type_procedures") nopp-parent f90-ts-indent-block)
   ((nopp-parent-is     "derived_type_definition") nopp-parent f90-ts-indent-block)

   ;; enumeration types: enum and enumeration
   ((node-is         "end_enum_statement")                    nopp-parent 0)
   ((node-is         "end_enumeration_type_statement")        nopp-parent 0)
   ((nopp-parent-is  "^enum\\(eration_type\\)?$")             nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil "ERROR" "^enum\\(eration_type\\)?$") nopp-parent f90-ts-indent-block)))


(defun f90-ts--indent-rules-if ()
  "Indentation rules for if-then-else statements."
  (f90-ts--with-map-rules
   ;; if-then-else statements
   ;; this must be first, as its parent is an if-statement
   ;; but node=nil/something and parent=if_statement is possible (some line after if)
   ((nopp-n-p-gp     "end_if_statement" "if_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-gp     "elseif_clause"    "if_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-gp     "else_clause"      "if_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-pstmtk nil                "if_statement"  "^if$")      nopp-parent f90-ts-indent-block) ; line right after if
   ((nopp-n-p-pstmtk nil                "if_statement"  "^else$")    nopp-parent f90-ts-indent-block) ; line right after else, with empty else block
   ((nopp-n-p-pstmtk nil                "if_statement"  "^elseif$")  nopp-parent f90-ts-indent-block) ; line right after elseif, with empty elseif block
   ((nopp-n-p-pstmtk nil                "else_clause"   "^else$")    nopp-parent f90-ts-indent-block) ; line after else, with non-empty else block
   ((nopp-n-p-pstmtk nil                "elseif_clause" "^elseif$")  nopp-parent f90-ts-indent-block) ; line right after "elseif"
   ((nopp-n-p-pstmtk nil                "elseif_clause" "^else$")    nopp-parent f90-ts-indent-block) ; line right after "else if"

   ((nopp-n-p-pstmtk "elseif_clause"    "ERROR" "^if$") previous-stmt-anchor 0)
   ((nopp-n-p-pstmtk "elseif"           "ERROR" "^if$") previous-stmt-anchor 0)
   ((nopp-n-p-gp     "elseif_clause"    "ERROR" nil)    previous-stmt-anchor minus-block-offset) ; at elseif line, incomplete

   ((nopp-n-p-pstmtk "else_clause"      "ERROR" "^if$")     previous-stmt-anchor 0)
   ((nopp-n-p-pstmtk "else_clause"      "ERROR" "^elseif$") previous-stmt-anchor 0)
   ((nopp-n-p-gp     "else_clause"      "ERROR" nil)        previous-stmt-anchor minus-block-offset) ; at else line, incomplete
   ((nopp-n-p-pstmtk "^else$"           "ERROR" "^if$")     previous-stmt-anchor 0)
   ((nopp-n-p-gp     "^else$"           "ERROR" nil)        previous-stmt-anchor minus-block-offset)

   ((nopp-n-p-pstmtk nil                "ERROR" "^if$")     previous-stmt-anchor f90-ts-indent-block) ; empty line after if
   ((nopp-n-p-pstmtk nil                "ERROR" "^elseif$") previous-stmt-anchor f90-ts-indent-block) ; empty line after elseif
   ((nopp-n-p-pstmtk nil                "ERROR" "^else$")   previous-stmt-anchor f90-ts-indent-block) ; empty line after else
   ))


(defun f90-ts--indent-rules-where ()
  "Indentation rules for else-elsewhere statements."
  (f90-ts--with-map-rules
   ;; where-elsewhere statements
   ((nopp-n-p-gp     "end_where_statement" "where_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-gp     "elsewhere_clause"    "where_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-gp     "else_clause"         "where_statement"  nil)         nopp-parent 0)
   ((nopp-n-p-pstmtk nil                   "where_statement"  "where")     nopp-parent f90-ts-indent-block) ; line right after where
   ((nopp-n-p-pstmtk nil                   "where_statement"  "elsewhere") nopp-parent f90-ts-indent-block) ; line right after elsewhere
   ((nopp-n-p-pstmtk nil                   "elsewhere_clause" "elsewhere") nopp-parent f90-ts-indent-block) ; line after else, with non-empty else block

   ((nopp-n-p-pstmtk "elsewhere_clause"    "ERROR" "where") previous-stmt-anchor 0)
   ((nopp-n-p-pstmtk "elsewhere"           "ERROR" "where") previous-stmt-anchor 0)
   ((nopp-n-p-gp     "elsewhere_clause"    "ERROR" nil)     previous-stmt-anchor minus-block-offset) ; at elsewhere line, incomplete
   ((nopp-n-p-gp     "elsewhere"           "ERROR" nil)     previous-stmt-anchor minus-block-offset)

   ((nopp-n-p-pstmtk nil                "ERROR" "where")     previous-stmt-anchor f90-ts-indent-block) ; empty line after where
   ((nopp-n-p-pstmtk nil                "ERROR" "elsewhere") previous-stmt-anchor f90-ts-indent-block) ; empty line after elsewhere
   ))


(defun f90-ts--indent-rules-single-region ()
  "Indentation rules for single region structures.
These are do loops, block statements, associate construct and forall statements."
  (f90-ts--with-map-rules
   ;; structures with a single region block and linear execution
   ((nopp-n-p-gp     "end_do_loop" "do_loop"      nil)  nopp-parent 0)
   ((nopp-n-p-pstmtk nil                "do_loop" "do") nopp-parent f90-ts-indent-block)  ;; proper do block (with or without while)
   ((nopp-n-p-pstmtk nil                "ERROR"   "do") previous-stmt-anchor f90-ts-indent-block)

   ((nopp-n-p-gp     "end_block_construct_statement" "block_construct" nil)     nopp-parent 0)
   ((nopp-n-p-pstmtk nil                             "block_construct" "block") nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                             "ERROR"           "block") previous-stmt-anchor f90-ts-indent-block)

   ((nopp-n-p-gp     "end_associate_statement" "associate_statement" nil)         nopp-parent 0)
   ((nopp-n-p-pstmtk nil                       "association"         "associate") nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                       "associate_statement" "associate") nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                       "ERROR"               "associate") previous-stmt-anchor f90-ts-indent-block)

   ((nopp-n-p-gp "end_forall_statement"        "forall_statement" nil)      nopp-parent          0)
   ((nopp-n-p-pstmtk nil                       "forall_statement" "forall") nopp-parent          f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                       "ERROR"            "forall") previous-stmt-anchor f90-ts-indent-block)))


(defun f90-ts--indent-rules-select ()
  "Indentation rules for select statements (case and type)."
  (f90-ts--with-map-rules
   ;; control statements
   ((nopp-n-p-gp     "end_select_statement" "select_case_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk "case_statement"       "select_case_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk nil                    "select_case_statement" "case")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                    "case_statement"        "case")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     "ERROR"                "select_case_statement" nil)              nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     nil                    "ERROR"                 "case_statement") nopp-grandparent f90-ts-indent-block)
   ((nopp-parent-is                         "select_case_statement")                  nopp-parent 0)

   ((nopp-n-p-gp     "end_select_statement" "select_type_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk "type_statement"       "select_type_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk nil                    "select_type_statement" "type")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                    "select_type_statement" "class")          nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                    "type_statement"        "type")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                    "type_statement"        "class")          nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     "ERROR"                "select_type_statement" nil)              nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     nil                    "ERROR"                 "type_statement") nopp-grandparent f90-ts-indent-block)
   ((nopp-parent-is                         "select_type_statement")                  nopp-parent 0)

   ((nopp-n-p-gp     "end_select_statement" "select_rank_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk "rank_statement"       "select_rank_statement" nil)              nopp-parent 0)
   ((nopp-n-p-pstmtk nil                    "select_rank_statement" "rank")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                    "rank_statement"        "rank")           nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     "ERROR"                "select_rank_statement" nil)              nopp-parent f90-ts-indent-block)
   ((nopp-n-p-gp     nil                    "ERROR"                 "rank_statement") nopp-grandparent f90-ts-indent-block)
   ((nopp-parent-is                         "select_rank_statement")                  nopp-parent 0)))


(defun f90-ts--indent-rules-coarray ()
  "Indentation rules for coarray statements."
  (f90-ts--with-map-rules
   ;; structures with a single region block and linear execution
   ((nopp-n-p-gp     "end_coarray_critical_statement" "coarray_critical_statement" nil)        nopp-parent 0)
   ((nopp-n-p-pstmtk nil                              "coarray_critical_statement" "critical") nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                              "ERROR"                      "critical") previous-stmt-anchor f90-ts-indent-block)

   ((nopp-n-p-gp     "end_coarray_team_statement" "coarray_team_statement" nil)      nopp-parent 0)
   ((nopp-n-p-pstmtk nil                          "coarray_team_statement" "change") nopp-parent f90-ts-indent-block)
   ((nopp-n-p-pstmtk nil                          "ERROR"                  "change") previous-stmt-anchor f90-ts-indent-block)))


(defun f90-ts--indent-rules-catch-all ()
  "Final indentation rule to handle unmatched cases."
  (f90-ts--with-map-rules
   ;; final catch-all rule
   ;;(log-state "catch all")
   (catch-all catch-all-anchor 0)))


(defun f90-ts-indent-rules ()
  "List of all indentation rules.
Rules are evaluated in the given order, which is important in some cases.
In particular, comments and continued lines must be dealt with before most
other rules."
  `((fortran
     ,@(f90-ts--indent-rules-start)
     ,@(f90-ts--indent-rules-preproc)
     ,@(f90-ts--indent-rules-comments)
     ,@(f90-ts--indent-rules-continued)
     ,@(f90-ts--indent-rules-internal-proc)
     ,@(f90-ts--indent-rules-prog-mod)
     ,@(f90-ts--indent-rules-function)
     ,@(f90-ts--indent-rules-translation-unit)
     ,@(f90-ts--indent-rules-interface)
     ,@(f90-ts--indent-rules-dtype-enum)
     ,@(f90-ts--indent-rules-if)
     ,@(f90-ts--indent-rules-where)
     ,@(f90-ts--indent-rules-single-region)
     ,@(f90-ts--indent-rules-select)
     ,@(f90-ts--indent-rules-coarray)
     ,@(f90-ts--indent-rules-catch-all))))


;;;-----------------------------------------------------------------------------
;;; Smart end completion

(defvar-local f90-ts--indent-modified-outside-line nil
  "Non-nil if a line operation modified text outside the current line.
Set by operations like `f90-ts--complete-smart-end-indent' that indent
a whole block.  Checked by `f90-ts--with-check-modified-line' to prevent
incorrectly restoring the buffer-modified flag.

This assumes that a line operation using `f90-ts--with-check-modified-line'
might call a region based operation, but not the other way around.")


(defconst f90-ts--complete-end-structs
  '("end_program_statement"
    "end_module_statement"
    "end_submodule_statement"
    ;;"end_block_data_statement"
    "end_subroutine_statement"
    "end_function_statement"
    "end_module_procedure_statement"
    "end_type_statement"
    "end_interface_statement"
    ;;"end_do_label_loop_statement"
    "end_do_loop_statement"
    "end_if_statement"
    "end_where_statement"
    "end_forall_statement"
    "end_select_statement"
    "end_block_construct_statement"
    "end_associate_statement"
    "end_enum_statement"
    "end_enumeration_type_statement"
    "end_coarray_team_statement"
    "end_coarray_critical_statement")
    "List of type names used for end struct statements.
The parent of such a node represents the structure itself.
This list is used to find nodes elligible for smart end completion.
Statements not yet supported are commented out.")


(defun f90-ts--complete-replace-if-changed (beg end completion)
  "Replace text in region BEG...END with COMPLETION.
Replace only if completion is different than current text.
Return non-nil if something was changed and text actually replaced."
  (let* ((original (buffer-substring-no-properties beg end))
         (is-equal (string= original completion)))
    (if is-equal
        (goto-char end)
      (progn
        (delete-region beg end)
        (goto-char beg)
        (insert completion)))
    (not is-equal)))



(defconst f90-ts--complete-smart-end-query
  `(("program"                 . "(program (program_statement \"program\" @construct (_) * (name) @name))")
    ("module"                  . "(module (module_statement \"module\" @construct (_) * (name) @name))")
    ("submodule"               . "(submodule (submodule_statement \"submodule\" @construct (_) * (name) @name))")
    ("subroutine"              . "(subroutine (subroutine_statement \"subroutine\" @construct name: (_) @name))")
    ("function"                . "(function (function_statement \"function\" @construct name: (_) @name))")
    ("module_procedure"        . "(module_procedure (module_procedure_statement \"procedure\" @construct name: (_) @name))")
    ("interface"               . ,(concat "(interface (interface_statement (abstract_specifier)?"
                                          " \"interface\" @construct"
                                          " [((name) @name)"
                                          " ((operator) @name)"
                                          " ((assignment) @name)]?"
                                          "))"))
    ("derived_type_definition" . "(derived_type_definition (derived_type_statement \"type\" @construct (_) * (type_name) @name))")
    ("if_statement"            . "(if_statement (block_label_start_expression _ @name \":\")? \"if\" @construct)")
    ("where_statement"         . "(where_statement (block_label_start_expression _ @name \":\")? \"where\" @construct)")
    ("do_loop"                 . "(do_loop (block_label_start_expression _ @name \":\")? (do_statement \"do\" @construct))")
    ("block_construct"         . "(block_construct (block_label_start_expression _ @name \":\")? \"block\" @construct)")
    ("associate_statement"     . "(associate_statement (block_label_start_expression _ @name \":\")? \"associate\" @construct)")
    ("forall_statement"        . "(forall_statement (block_label_start_expression _ @name \":\")? \"forall\" @construct)")
    ("select_case_statement"   . "(select_case_statement (block_label_start_expression _ @name \":\")? \"select\" @construct)")
    ("select_type_statement"   . "(select_type_statement (block_label_start_expression _ @name \":\")? \"select\" @construct)")
    ("select_rank_statement"   . "(select_rank_statement (block_label_start_expression _ @name \":\")? \"select\" @construct)")
    ("enum"                    . "(enum (enum_statement \"enum\" @construct))")
    ("enumeration_type"        . ,(concat "(enumeration_type (enumeration_type_statement"
                                          " \"enumeration\" @construct \"type\" @construct2"
                                          " (_) *"
                                          " (type_name) @name"
                                          "))"))
    ("coarray_critical_statement" . "(coarray_critical_statement (block_label_start_expression _ @name \":\")? \"critical\" @construct)")
    ;; this is counter intuitive: "change team" is ended by "end team" not by "end change team"
    ("coarray_team_statement"     . "(coarray_team_statement (block_label_start_expression _ @name \":\")? \"change\" \"team\" @construct)"))
  "Treesitter queries to extract relevant nodes for smart end completion.")


(defun f90-ts--complete-smart-end-extract (node capture)
  "Extract relevant nodes from CAPTURE obtain by a treesitter query.

Somehow documented anchoring at NODE does not work, and it needs to be done by
hand.  First search for root=NODE.  Then extract subsequent matches, some of
which are optional and possibly not present or in different order."
  (cl-loop
   for (cap_sym . n) in capture
   with collecting = nil
   until (and collecting (eq cap_sym 'root))
   when (and (not collecting)
             (eq cap_sym 'root)
             (treesit-node-eq n node))
      do (setq collecting t)
   when (and collecting (not (eq cap_sym 'root)))
      collect (cons cap_sym n)))


(defun f90-ts--complete-smart-end-name (node)
  "Extract construct type and name from NODE.
Return a list containing name, construct type and second construct type if
present or nil.  For example, for \"end enumeration type xyz\", return
the list (\"xyz\" \"enumeration\" \"type\".
Depending on type of NODE, extraction is different, as subtrees are built
differently.  The construct type itself is fixed, but lower/upper/title case
and the related start of the structure are also required for completion."
  (when-let* ((query (alist-get (treesit-node-type node)
                                f90-ts--complete-smart-end-query
                                nil
                                nil
                                #'string=))
              (query-root (concat query " @root"))
              (capture-all (treesit-query-capture node query-root))
              (capture     (f90-ts--complete-smart-end-extract node capture-all)))
    ;; @root is added in order to also get the root node of the captured subtree,
    ;; capture result is an alist (('root, root), ('construct, construct) ('name, name),
    ;; ('root, root), ('construct, construct) ('name, name), ...),
    ;; where root and name are the captured nodes,
    ;; we need to make sure that root=node, which might not be the case in nested block structure
    ;; (where the inner loop, if etc. also matches),
    ;; this could be done with the :anchor pattern, but it is rejected... syntax not valid?
    (let* ((construct-node (alist-get 'construct capture))
           (construct2-node (alist-get 'construct2 capture))
           (name-node (alist-get 'name capture)))
      (cl-assert construct-node
                 nil
                 "complete-smart-end: no construct node found")
      ;; in general there is no second construct name, exception: "enumeration type"
      (list
       (and name-node (treesit-node-text name-node t))
       (and construct-node (treesit-node-text construct-node t))
       (and construct2-node (treesit-node-text construct2-node t))))))


(defun f90-ts--complete-smart-end-compose (node)
  "Create an \"end construct name\" completion from NODE if applicable.
NODE is assumed to be the structure node (parent of the end structure node).
Construct is a string like \"subroutine\", \"function\", \"module\",
\"do\", \"block\" etc.
Return nil if construct name could not be recovered.  In general, this should
not happen, as `f90-ts--complete-end-structs' lists all supported structures."
  (when-let* ((construct-name (f90-ts--complete-smart-end-name node)))
    (cl-assert construct-name
               nil
               "complete-smart-end: structure query failed")
    (let* ((name (car construct-name))
           (construct (cadr construct-name))
           (construct2 (caddr construct-name))
           (c0 (substring construct 0 1))
           (end
            (cond
             ((string= construct (upcase construct))
              "END")
             ((string= c0 (upcase c0))
              "End")
             (t
              "end"))))
      (string-join (delq nil (list end construct construct2 name))
                   " "))))


(defun f90-ts--complete-smart-end-show (node-struct)
  "Show the completion for root NODE-STRUCT of a structure statement.
Either jump to the start of the structure or show opening statement in the
message buffer.  Which action to perform depends on position and whether
`blink' is set."
  (unless (eq f90-ts-smart-end 'no-message)
    (let ((top-of-window (window-start))
          (start-struct (treesit-node-start node-struct)))
      (save-excursion
        (goto-char start-struct)
        (if (or (eq f90-ts-smart-end 'no-blink)
                (< start-struct top-of-window))
            (message "matches %s: %s"
                     (what-line)
                     (buffer-substring
                      (line-beginning-position)
                      (line-end-position)))
          (sit-for blink-matching-delay))))))


(defun f90-ts--complete-smart-end-end (node)
  "Find the end position of the end structure statement of NODE.
Together with the start position of the end structure statement it marks the
region to be replaced.
It should include trailing white space characters, but only if no comments or
commands (after a \";\") are present, in which case the original spacing should
be kept.  Moreover, incompletely typed text like in \"end subr_out\" should be
handled as well."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-forward "^;!\n")
    (let ((c (char-after)))
      (unless (or (eq c ?\n)
                  (null c))
        ;; this finally trims trailing white spaces if nothing else
        ;; is on the line
        (skip-chars-backward " \t")))
    (point)))


(defun f90-ts--complete-replace-end-struct (node-struct beg end completion)
  "Replace BEG..END by COMPLETION and return new NODE-STRUCT if updated.
If the completion is exactly what the buffer already contains, then nothing is
changed and the function returns nil, signalling that NODE-STRUCT is still
valid."
  (let (beg-marker
        end-marker)
    (unwind-protect
        (progn
          (setq beg-marker (copy-marker (treesit-node-start node-struct) t))
          (setq end-marker (copy-marker (treesit-node-end node-struct) t))
          (f90-ts--complete-replace-if-changed beg end completion)
          (when (treesit-node-check node-struct 'outdated)
            (treesit-node-on (marker-position beg-marker)
                             (marker-position end-marker))))
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


(defun f90-ts--complete-smart-end-node (node)
  "If NODE is an end struct statement, construct the completion.
It looks for structure type and name or label.

Example: complete \"end\" closing a subroutine of name mysub by
\"end subroutine mysub\".
After the changes, NODE will become stale.  The function returns the new
statement node representing the whole block.  This is used for the blink
option or statement indentation."
  ;; make sure that we are looking at and end statement, the parser might add
  ;; and end_xyz_statement node in error recovery mode (e.g. at end of file)
  (when (and (member (treesit-node-type node)
                     f90-ts--complete-end-structs)
             ;; t: ignore case in end prefix comparison
             (string-prefix-p "end" (treesit-node-text node) t))
    ;; the region check ensures that end statements spread over several lines
    ;; are left alone
    (let ((beg (treesit-node-start node))
          (end (f90-ts--complete-smart-end-end node)))
      (when (save-excursion
              (goto-char beg)
              (<= end (line-end-position)))
        (let ((node-struct (treesit-node-parent node)))
          (or (when-let* ((completion (f90-ts--complete-smart-end-compose node-struct)))
                (f90-ts--complete-replace-end-struct node-struct beg end completion))
              ;; if nothing has changed new node is nil, return the original one
              node-struct))))))


(defun f90-ts--complete-smart-end-indent (node-struct)
  "Indent block represented by NODE-STRUCT.
This can be optionally invoked after smart end completion to indent the whole
block.  If indentation changes something, the tree is updated and node-struct
becomes stale.  In this case, the function return the node-struct-new,
otherwise nil."
  (let ((variant-saved f90-ts--align-continued-variant-tab)
        (beg (treesit-node-start node-struct))
        ;; end position of node-struct sometimes reaches into next line
        ;; (including the eos token)
        (end (save-excursion
               (goto-char (treesit-node-end node-struct))
               (if (bolp)
                   (1- (point))
                 (point))))
        beg-marker
        end-marker
        node-struct-new)
    (unwind-protect
        (progn
          ;; use region variant for indentation (no rotation of list
          ;; items on continued lines)
          (setq f90-ts--align-continued-variant-tab nil)
          (setq beg-marker (copy-marker beg t))
          (setq end-marker (copy-marker end t))
          (f90-ts--indent-and-complete-region-aux beg-marker end-marker)

          ;; if something was modified, then treesit-node-on forces a reparse
          ;; and node-probe /= node-struct, in this case set node-struct-new to node-probe
          ;; otherwise leave node-struct-new at nil, signalling that the old node is still valid
          (let ((node-probe (treesit-node-on (marker-position beg-marker)
                                             (marker-position end-marker))))

            (unless (treesit-node-eq node-struct node-probe)
              (setq node-struct-new node-probe)
              ;; signal that the region has been modified in case it was invoked
              ;; by some line operation which wants to keep track of modifications
              (setq f90-ts--indent-modified-outside-line t))))
      ;; revert to saved tab variant
      (setq f90-ts--align-continued-variant-tab variant-saved)
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))
    node-struct-new))


;; The idea for smart end completion is taken from the classic f90-mode.
(defun f90-ts--complete-smart-tab (indent-struct)
  "Provide context-aware completion using tree-sitter after indentation by tab.
Currently it handles end statements.

If INDENT-STRUCT is true, then indent the whole block with `indent-region'."
  (when f90-ts-smart-end
    (when-let* ((indent-node (f90-ts--node-at-indent-pos (point)))
                (start (treesit-node-start indent-node))
                (node (treesit-parent-while
                       indent-node
                       (lambda (n) (= start (treesit-node-start n)))))
                (end (treesit-node-end node)))
      (when (= (line-number-at-pos)
               (f90-ts--node-line node))
        (when-let* ((node-struct (f90-ts--complete-smart-end-node node)))
          (when indent-struct
            ;; update node-struct, if indent changes anything
            (setq node-struct (or (f90-ts--complete-smart-end-indent node-struct)
                                  node-struct)))
          (f90-ts--complete-smart-end-show node-struct))))))


;;;------------------------------------------------------------------------------
;;; Indentation: auxiliary function
;;; mainly for handling leading ampersands in continued lines

(defun f90-ts--delete-trailing-whitespace-line (pos)
  "Delete trailing whitespace characters at the end of the line after POS."
  (save-excursion
    (let ((pos-last (f90-ts--pos-last-nonspace pos)))
      (goto-char pos)
      (delete-region (max pos pos-last)
                     (line-end-position)))))


(defun f90-ts--delete-trailing-whitespace-region (beg end)
  "Delete trailing whitespace characters on each line between BEG and END.
Uses the same line-boundary convention as
`f90-ts--indent-whitespace-leading-amp-or-label-region': if END is at the
beginning of a line, that line is excluded.
The function does nothing unless `f90-ts-indent-delete-trailing-whitespace'
is non-nil."
  (save-excursion
    (let ((beg-bol (progn (goto-char beg) (line-beginning-position))))
      ;; start at end and work backwards, so deleting whitespace on a line
      ;; does not shift the positions of the lines still to be processed
      (goto-char end)
      ;; if END is at the beginning of a line, that line is excluded;
      ;; if at the beginning of the buffer, then there is no previous line
      ;; and nothing to do
      (when (if (bolp)
                (zerop (forward-line -1))
              (beginning-of-line)
              t)
        (cl-loop
         while (>= (point) beg-bol)
         do (f90-ts--delete-trailing-whitespace-line (point))
         while (zerop (forward-line -1)))))))


(defmacro f90-ts--with-check-modified-line (&rest body)
  "Execute BODY, restoring unmodified status if current line is unchanged.

Note: do not use symbols starting with `wcml-'in BODY."
  (declare (indent 0))
  `(let* ((wcml-modified-before (buffer-modified-p))
          (wcml-beg (line-beginning-position))
          (wcml-end (line-end-position))
          (f90-ts--indent-modified-outside-line nil)
          (wcml-line-before (unless wcml-modified-before
                              (buffer-substring-no-properties
                               wcml-beg wcml-end)))
          wcml-bol-marker
          wcml-eol-marker)
     (unwind-protect
         (progn
           (setq wcml-bol-marker (copy-marker wcml-beg))
           (setq wcml-eol-marker (copy-marker wcml-end t))
           ,@body
           (unless (or wcml-modified-before
                       f90-ts--indent-modified-outside-line)
             (when (string= wcml-line-before
                            (buffer-substring-no-properties
                             wcml-bol-marker wcml-eol-marker))
               (restore-buffer-modified-p nil))))
       (set-marker wcml-bol-marker nil)
       (set-marker wcml-eol-marker nil))))


(defmacro f90-ts--with-check-modified-region (beg end &rest body)
  "Execute BODY, restoring unmodified status if region BEG..END is unchanged.

Note: do not use symbols starting with `wcmr-' in BODY."
  (declare (indent 2))
  `(let* ((wcmr-modified-before (buffer-modified-p))
          (wcmr-region-before (unless wcmr-modified-before
                                (buffer-substring-no-properties ,beg ,end)))
          wcmr-end-marker)
     (unwind-protect
         (progn
           (setq wcmr-end-marker (copy-marker ,end t))
           ,@body
           (unless wcmr-modified-before
             (when (string= wcmr-region-before
                            (buffer-substring-no-properties ,beg wcmr-end-marker))
               (restore-buffer-modified-p nil))))
       (set-marker wcmr-end-marker nil))))


(defun f90-ts--indent-leading-ampersand-column ()
  "Compute the column for a leading ampersand on the current line.
Uses `f90-ts-leading-ampersand-style' to determine placement.
For the `indent' style, uses the column of the first node of the
continued statement plus the configured offset."
  (pcase f90-ts-leading-ampersand-style
    (`(column . ,col)
     col)
    (`(indent . ,offset)
     (+ offset
        (let* ((node (f90-ts--node-at-indent-pos (point)))
               (parent (treesit-node-parent node))
               (pstmt-1 (f90-ts--previous-stmt-first node parent)))
          (f90-ts--node-column pstmt-1))))
    (_ (error "Invalid f90-ts-leading-ampersand-style: %S"
              f90-ts-leading-ampersand-style))))


(defun f90-ts--blank-ampersand-at-point ()
  "Blank a real leading ampersand at point, if present.
Point is assumed to be at the first non-blank character of the current line.
Return \\='(amp) if an ampersand was blanked, nil otherwise.
Does nothing if the ampersand is a virtual ampersand, part of a
string_literal, or if the line is just a trailing ampersand (optionally
followed by a comment), which is left untouched."
  ;; a line with just a trailing ampersand (and optional comment) is not
  ;; a leading ampersand to blank, so leave it as is
  (unless (looking-at "&\\s-*\\(?:!\\|$\\)")
    (let ((node (treesit-node-at (point))))
      (when (and node
                 (= (point) (treesit-node-start node))
                 (f90-ts--node-type-p node "&")
                 ;; make sure this is not part of a string_literal
                 (not (f90-ts--in-string-p (point))))
        (delete-char 1)
        (insert " ")
        '(amp)))))


(defun f90-ts--blank-label-at-point ()
  "Blank a statement label at point, if present.
Point is assumed to be at the first non-blank character of the current line.
Return \\='(label . label-text) if a label was blanked, nil otherwise."
  (when-let* ((node (treesit-node-at (point)))
              ((= (point) (treesit-node-start node)))
              ((f90-ts--node-type-p node "statement_label"))
              (text (treesit-node-text node)))
    (delete-char (length text))
    (insert (make-string (length text) ?\s))
    (cons 'label text)))


(defun f90-ts--indent-blank-leading-amp-or-label-line ()
  "Blank a leading ampersand or statement label on the current line.
Return nil if neither was found, \\='(amp) if an ampersand was
blanked, or \\='(label . label-text) if a statement label was blanked."
  (unless (f90-ts--point-on-empty-line-p)
    (save-excursion
      (back-to-indentation)
      (let ((c (char-after (point))))
        (cond
         ((and c (eq c ?&))
          (f90-ts--blank-ampersand-at-point))
         ((and c (>= c ?0) (<= c ?9))
          (f90-ts--blank-label-at-point)))))))


(defun f90-ts--indent-restore-leading-ampersand-line ()
  "Restore a leading ampersand after indentation.
It moves to the column determined by `f90-ts--indent-leading-ampersand-column'
inserting blanks and moving code to the right if necessary.
Then it pastes an ampersand at the determined column, leaving the code
positioned as it is."
  (let ((amp-col (f90-ts--indent-leading-ampersand-column)))
    (back-to-indentation)
    (let* ((indent-col (current-column))
           (delta (- indent-col amp-col)))
      (if (<= delta 0)
          (insert (make-string (- delta) ?\s))
        (backward-char delta )
        (delete-char 1))
      (insert "&"))))


(defun f90-ts--indent-restore-label-line (label-text)
  "Restore a statement_label after indentation.
It moves to the column determined by `f90-ts-stmt-label-column',
inserting blanks and moving code to the right if necessary.
Then it pastes LABEL-TEXT at the determined column, leaving the code
positioned as it is.  A blank is ensured after the label."
  (let* ((adj (car f90-ts-stmt-label-column))
         (col (cdr f90-ts-stmt-label-column))
         (label-len (length label-text))
         (label-col (if (eq adj 'right)
                        (max 0 (- col (1- label-len)))
                      col)))
    (back-to-indentation)
    (let* ((indent-col (current-column))
           (delta (- indent-col label-col)))
      (if (<= delta 0)
          (insert (make-string (- delta) ?\s)
                  label-text
                  " ")
        (backward-char delta)
        (insert label-text)
        ;; we need to remove label-len blank, currently are delta blanks,
        ;; hence if label-len <= delta-1 we can just remove the blanks and still have at least one left
        ;; otherwise we only can remove delta-1
        (delete-char (min label-len (1- delta)))))))


(defun f90-ts--indent-restore-leading-amp-or-label-line (amp-or-label)
  "Restore a leading ampersand or statement label from AMP-OR-LABEL.
AMP-OR-LABEL is a value previously returned by
`f90-ts--indent-blank-leading-amp-or-label-line'."
  (when (or amp-or-label
            (and f90-ts-leading-ampersand
                 (f90-ts--node-type-p
                  (f90-ts--first-node-on-line (point)) "&")))
    (pcase amp-or-label
      ((or 'nil '(amp))
       ;; if nil there was no ampersand but there is a virtual ampersand
       ;; signalling a continued line and f90-ts-leading-ampersand is non-nil
       (f90-ts--indent-restore-leading-ampersand-line))
      (`(label . ,label-text)
       (f90-ts--indent-restore-label-line label-text))
      (_ (cl-assert nil nil "unexpected value %S" amp-or-label)))))


(defun f90-ts--indent-blank-leading-amp-or-label-region (beg end)
  "Blank leading ampersands and statement labels for each line in BEG..END.
Return a vector (one entry per line) of values as returned by
`f90-ts--indent-blank-leading-amp-or-label-line'."
  (save-excursion
    (goto-char beg)
    (when (looking-at-p "[ \t]*$")
      (forward-line 1))
    (let* ((line-beg (line-number-at-pos (point)))
           (line-end-aux (line-number-at-pos end))
           ;; if at beginning of line, this line should be excluded,
           ;; this comes from the loop (while (< (point) end) ... (forward-line 1))
           ;; used in indentation, but we need number of lines before hand, so we
           ;; explicitely step back one line
           (line-end (save-excursion
                       (goto-char end)
                       (if (bolp)
                           (1- line-end-aux)
                         line-end-aux)))
           (n-lines  (- line-end line-beg -1))
           (vec      (make-vector n-lines nil)))
      ;; after blanking loop, each element of the vector vec will be one of:
      ;;   nil               – no amp, no label on the line
      ;;   (amp)             – had a leading & on the line
      ;;   (label . "12345") – had a statement label on the line
      (cl-loop
       for ix   from 0
       for line from line-beg to line-end
       do (progn
            (cl-assert (= (line-number-at-pos (point)) line)
                       nil
                       "line mismatch at index %d: expected %d, got %d"
                       ix line (line-number-at-pos (point)))
            (aset vec ix (f90-ts--indent-blank-leading-amp-or-label-line))
            (forward-line 1)))
      vec)))


(defun f90-ts--indent-restore-leading-amp-or-label-region (beg vec)
  "Restore leading ampersands and statement labels from BEG using VEC.
VEC is a vector as returned by
`f90-ts--indent-blank-leading-amp-or-label-region'."
  (save-excursion
    (goto-char beg)
    (when (looking-at-p "[ \t]*$")
      (forward-line 1))
    (let ((line-beg (line-number-at-pos (point))))
      (cl-loop
       for line from line-beg
       for amp-or-label across vec
       do (progn
            (cl-assert (= (line-number-at-pos (point)) line)
                       nil
                       "line mismatch at index %d: expected %d, got %d"
                       (- line line-beg) line (line-number-at-pos (point)))
            (f90-ts--indent-restore-leading-amp-or-label-line amp-or-label)
            (forward-line 1))))))


(defun f90-ts--indent-line-aux (&optional variant)
  "Indent a single line.
This is the default wrapper to invoke `treesit-indent' for indentation of a
single line.  Smart end completion or other extra stuff is not executed by
this function.
If provided VARIANT is the variant symbol for how to compute alignment in
multi-line statements.  Default value is `f90-ts-indent-list-line'.

Leading ampersands and statement labels on continuation lines are temporarily
removed before calling `treesit-indent', and restored afterwards at the
determined by `f90-ts-leading-ampersand-style' and `f90-ts-stmt-label-column'."
  (let ((f90-ts--align-continued-variant-tab
         (or variant f90-ts-indent-list-line))
        (amp-or-label (f90-ts--indent-blank-leading-amp-or-label-line)))
    ;; do indentation without leading ampersand and statement label
    (treesit-indent)
    ;; where applicable insert statement label or leading ampersand
    ;; (ampersand if there was already one or if f90-ts-leading-ampersand
    ;; is non-nil)
    (save-excursion
      (f90-ts--indent-restore-leading-amp-or-label-line amp-or-label))
    ;; delete trailing whitespace characters, if enabled
    (when f90-ts-indent-delete-trailing-whitespace
      (f90-ts--delete-trailing-whitespace-line (point)))))


(defun f90-ts--indent-region-aux (beg-marker end-marker)
  "Indent region from BEG-MARKER to END-MARKER.
This is the default wrapper to invoke `treesit-indent-region' for indentation
of a region.  Smart end completion or other extra stuff is not executed by
this function.

Leading ampersands and statement labels on continuation lines are temporarily
removed before calling `treesit-indent-region', and restored afterwards at the
determined by `f90-ts-leading-ampersand-style' and `f90-ts-stmt-label-column'.

Internally the continued-line cache is reset, so that regions, which cover only
part of a continued line, can be indented correctly."
  (let ((vec (f90-ts--indent-blank-leading-amp-or-label-region
              beg-marker end-marker)))
    (f90-ts--continued-line-cache-reset)
    (treesit-indent-region beg-marker end-marker)
    ;; restore ampersands or labels: beg-marker still points to the
    ;; first line (insertion-type nil keeps it before any text
    ;; treesit-indent may have inserted at the start).
    (f90-ts--indent-restore-leading-amp-or-label-region
     beg-marker vec)
    ;; delete trailing whitespace characters on each line, if enabled
    (when f90-ts-indent-delete-trailing-whitespace
      (f90-ts--delete-trailing-whitespace-region beg-marker end-marker))))


(defun f90-ts--indent-and-complete-line-aux (variant indent-struct)
  "Auxiliary wrapper for indent-and-complete-line function.
VARIANT is the tab variant to be used.
If INDENT-STRUCT is true, and point is at some end statement, then
indent the whole block closed by the end statement after smart end
completion."
  ;; indent line, taking leading ampersand into account if necessary
  (f90-ts--indent-line-aux variant)
  ;; if end statement, do smart end completion
  (f90-ts--complete-smart-tab indent-struct))


(defun f90-ts--indent-stmt-region (beg end)
  "Apply indent region from begin of line at BEG to end of line at END.
Return true if the region was already properly indented (nothing was
changed)."
  (let (beg-marker end-marker)
    (unwind-protect
        (progn
          ;; beg marker should stay before inserted text
          ;; end marker should stay after inserted text
          (let ((beg-reg (save-excursion
                           (goto-char beg)
                           (line-beginning-position)))
                (end-reg (save-excursion
                           (goto-char end)
                           (line-end-position))))
            (setq beg-marker (copy-marker beg-reg))
            (setq end-marker (copy-marker end-reg t))
            (let ((old-text (buffer-substring-no-properties beg-reg end-reg)))
              (f90-ts--indent-region-aux beg-marker end-marker)
              ;; place point properly on last line, but where does treesit-indent-region and
              ;; the restore function (which uses a save-excursion) put point?
              (skip-chars-forward "& \t")
              (string= old-text (buffer-substring-no-properties beg-marker end-marker)))))
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


(defun f90-ts--indent-and-complete-region-aux (beg end)
  "Indent region and execute smart end completion in region from BEG to END.
This is an auxiliary function for `f90-ts-indent-and-complete-region'.  But it
can also be called by line based operations triggering indentation and
completion for a region, like indent-stmt operations on an end struct line."
  (let (beg-marker end-marker)
    (unwind-protect
        (progn
          ;; beg marker should stay before inserted text
          ;; end marker should stay after inserted text
          (setq beg-marker (copy-marker beg))
          (setq end-marker (copy-marker end t))
          (f90-ts--with-check-modified-region beg-marker end-marker
            (f90-ts-complete-smart-end-region beg-marker end-marker)
            (f90-ts--indent-region-aux beg-marker end-marker)))
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


(defun f90-ts--indent-abbrev-expand ()
  "Expand abbrev at point if there is one.
This is done as part of `f90-ts--indent-and-complete-line', to allow hitting
TAB right after typing an abbrevation with proper expansion and indentation."
  (when abbrev-mode
    (expand-abbrev)))


;;;------------------------------------------------------------------------------
;;; Indentation and smart end completion

;;; line indentation for <tab>, C-<tab> etc.
;;;  * f90-ts-indent-and-complete-stmt
;;;  * f90-ts-indent-and-complete-line
;;;  * f90-ts-indent-line
;;;  * f90-ts-indent-for-tab-command{-[2,3]}
;;;
;;; region indentation:
;;;  * f90-ts-indent-and-complete-region

(defun f90-ts-indent-and-complete-line ()
  "Indent and apply smart end completion to current line.
This is the default function for indent and smart complete of end lines,
bound to <tab>.  More advanced indentations of involved continued lines and
block structures is done by `f90-ts--indent-and-complete-stmt'"
  (interactive)
  (f90-ts--with-check-modified-line
    (f90-ts--indent-abbrev-expand)
    (f90-ts--indent-and-complete-line-aux
     f90-ts-indent-list-line
     nil)))


(defun f90-ts-indent-line (&optional variant)
  "Indent a single line.
This is the default function for indentation of a single line.  Smart end
completion or other extra stuff is not executed by this function.
If provided VARIANT is the variant symbol for how to compute alignment in
multi-line statements.  Default value is `f90-ts-indent-list-line'."
  (interactive)
  (f90-ts--with-check-modified-line
    (f90-ts--indent-line-aux variant)))


(defun f90-ts--complete-end-node-p (node pos end-marker at-col0)
  "Predicate for `treesit-search-forward' in smart end completion loop.
Return non-nil if NODE is a completion target within POS and END-MARKER.
If AT-COL0 is non-nil, do no accept nodes at starting at END-MARKER.
Thus do not work on nodes at the line if end marker is at column 0.

It throws `f90-ts--past-end' if END-MARKER has been passed, as there is
no abort mechanism for `treesit-search-foward'."
  (let ((start-n (treesit-node-start node))
        (end-n  (f90-ts--node-end-trimmed node)))
    (when (or (> start-n end-marker)
              (and at-col0 (= start-n end-marker)))
      ;; treesit-search-forward has no early abort option,
      ;; without this, the search exhausts the whole remaining tree
      (throw 'f90-ts--past-end nil))
    (and (<= pos end-n)
         (member (treesit-node-type node)
                 f90-ts--complete-end-structs))))


;; used by f90-ts-indent-and-complete-region
(defun f90-ts-complete-smart-end-region (beg end)
  "Execute smart end completion in region from BEG to END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))

  (let* ((beg-pos (if (markerp beg) (marker-position beg) beg))
         (end-pos (if (markerp end) (marker-position end) end))
         ;; skip the final line if end-pos is at zeroth column
         (at-col0 (= 0 (f90-ts--column-number-at-pos end-pos)))
         (end-marker nil))
    (unwind-protect
        (progn
          (setq end-marker (copy-marker end-pos))
          (catch 'f90-ts--past-end
            (cl-loop
             with pos = beg-pos
             for node = (treesit-search-forward
                         (treesit-node-at pos)
                         (lambda (n)
                           (f90-ts--complete-end-node-p n pos end-marker at-col0))
                         nil  ; search forward
                         nil) ; only named nodes (end-structs are always named nodes)
             while node
             do (let ((node-start (treesit-node-start node)))
                  (f90-ts--complete-smart-end-node node)
                  (goto-char node-start)
                  (forward-line 1))
             until (eobp)
             do (setq pos (point))))
          ;; after finishing move to end of region
          (goto-char end-marker))
      (set-marker end-marker nil))))


(defun f90-ts-indent-region (beg end)
  "Indent region from BEG to END.
It is based on the treesitter tree overlapping that region.

Leading ampersands and statement labels on continuation lines are temporarily
removed before calling `treesit-indent-region', and restored afterwards at the
determined by `f90-ts-leading-ampersand-style' and `f90-ts-stmt-label-column'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))

  (let (beg-marker end-marker)
    (unwind-protect
        (progn
          ;; beg marker should stay before inserted text
          ;; end marker should stay after inserted text
          (setq beg-marker (copy-marker beg))
          (setq end-marker (copy-marker end t))
          (f90-ts--with-check-modified-region beg-marker end-marker
            (f90-ts--indent-region-aux beg-marker end-marker)))
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


(defun f90-ts-indent-and-complete-region (beg end)
  "Indent region and execute smart end completion in region from BEG to END.
It is based on the treesitter tree overlapping that region.
For region based indentation, smart end completion should be executed first,
as this might repair the tree.  Only then indentation should be done.
For single line the order does not matter.  Example:

module mod
contains
 subroutine sub()
 end function sub
end module mod

At the end function line, \"end\" represents the end subroutine statement,
hence indentation as well as smart end completion both work.  However,
the keyword \"function\" after \"end\" starts a new function and muddles the
subsequent tree.

Leading ampersands and statement labels on continuation lines are temporarily
removed before calling `treesit-indent-region', and restored afterwards at the
determined by `f90-ts-leading-ampersand-style' and `f90-ts-stmt-label-column'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (f90-ts--indent-and-complete-region-aux beg end))


(defun f90-ts-indent-and-complete-stmt ()
  "Perform indentation and smart end completion for a whole statement.
In most cases, this just behaves like `f90-ts--indent-and-complete-line'.
However, in two cases this indents and complete a region associated with
the statement or block at point.

If point is on an \"end\" statement of some structure like a \"subroutine\",
a \"do\" or an \"if\" block, then the whole structure is indented as a region.

If point is on a line within a continued statement, it first indents the
statement starting at its first up to the current line as a region.
If the operation has not changed anything, then and only then it indents
the current line by invoking `f90-ts--indent-and-complete' to apply rules
like rotation of list context items.

If a region is active, then just invoke `f90-ts-indent-and-complete-region'"
  (interactive)
  (cond
   ((use-region-p)
    (f90-ts-indent-and-complete-region (region-beginning)
                                       (region-end)))

   ((not (f90-ts--pos-within-continued-stmt-p (point)))
    ;; just do indent-and-complete plus block indentation if applicable
    (f90-ts--with-check-modified-line
     (f90-ts--indent-and-complete-line-aux
      f90-ts-indent-list-line ; use default line variant
      t)))

   (t
    ;; multi-line statement
    (let* ((end (point))
           (node (f90-ts--first-node-on-line end))
           (first (f90-ts--first-node-of-stmt node))
           (beg (treesit-node-start first))
           (variant-line f90-ts-indent-list-line))
      (f90-ts--with-check-modified-region beg end
        (if (f90-ts--indent-stmt-region beg end)
            ;; no indentation applied, invoke indentation for current line
            ;; like doing rotational alignment,
            ;; no smart end completion necessary, as there is no end is involved
            (f90-ts--indent-line-aux variant-line)
          ;; statement up to current line was changed by indent statement region
          (back-to-indentation)
          (skip-chars-forward "& \t")))))))


(defun f90-ts-indent-for-tab-command-2 ()
  "Invoke `indent-for-tab-command' with variant `f90-ts-indent-list-line-2'.
The variant to be used can be customized.  Intended for use in key bindings."
  (interactive)
  (let ((f90-ts-indent-list-line f90-ts-indent-list-line-2))
    (indent-for-tab-command)))


(defun f90-ts-indent-for-tab-command-3 ()
  "Invoke `indent-for-tab-command' with variant `f90-ts-indent-list-line-3'.
The variant to be used can be customized.  Intended for use in key bindings."
  (interactive)
  (let ((f90-ts-indent-list-line f90-ts-indent-list-line-3))
    (indent-for-tab-command)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-indent)

;;; f90-ts-indent.el ends here
