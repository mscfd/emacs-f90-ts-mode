;;; f90-ts-mark-region.el --- Mark region operations for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide tree-sitter based mark region operations for `f90-ts-mode'.
;; Provided operations are shrinking and enlaring a region based on the tree
;; structure, moving a marked region to some sibling, and extending or
;; reducing a marked region by siblings.

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
;;; treesitter based region functions

(defun f90-ts--mark-region-reversed-p (region-order)
  "Return non-nil if region should be marked with point at start.
REGION-ORDER is as provided by `f90-ts-mark-region-order'."
  (pcase region-order
    ('start    t)
    ('end      nil)
    ('preserve (and (use-region-p) (> (mark) (point))))
    (_         nil)))


(defun f90-ts--mark-region (beg end &optional region-order)
  "Mark region BEG..END.
REGION-ORDER controls where point is placed after marking,
see `f90-ts-mark-region-order'."
  (let ((order (f90-ts--mark-region-reversed-p region-order)))
    (push-mark beg t t)
    (goto-char end)
    (when order
      (exchange-point-and-mark))))


(defun f90-ts--node-or-extent-span (node-or-extent)
  "Return trimmed span (BEG . END) of NODE-OR-EXTENT.
NODE-OR-EXTENT is either a single node or a pair (FIRST . LAST), in which
case the span reaches from the start of FIRST to the end of LAST."
  (if (consp node-or-extent)
      (cons (car (f90-ts--node-span-trimmed (car node-or-extent)))
            (cdr (f90-ts--node-span-trimmed (cdr node-or-extent))))
    (f90-ts--node-span-trimmed node-or-extent)))


(defun f90-ts--mark-region-node (node &optional region-order)
  "Mark the region spanned by NODE, trimmed of leading and trailing whitespace.
If REGION-ORDER is non-nil, then put point at start of region, otherwise point
is at end of region.
This is `f90-ts--mark-region-node-or-extent' for a single node."
  (f90-ts--mark-region-node-or-extent node region-order))


(defun f90-ts--mark-region-node-or-extent (node-or-extent &optional region-order)
  "Mark region given by NODE-OR-EXTENT, trimmed of leading and trailing whitespace.
It is either a single NODE or a pair (FIRST . LAST).
If it is a pair, mark the whole region from start of FIRST to end of LAST.

If REGION-ORDER is non-nil, then put point at start of region, otherwise point
is at end of region."
  (let ((span (f90-ts--node-or-extent-span node-or-extent)))
    (f90-ts--mark-region (car span) (cdr span) region-order)))


(defun f90-ts--smallest-named-node-containing-region (beg end trim)
  "Return the smallest named node fully containing region from BEG to END.
The node must be strictly larger than the region (BEG END).
If TRIM is non-nil, then use trimmed span of named nodes for comparison
with BEG..END region."
  (when-let* ((node (treesit-node-on beg end))
              (cover (treesit-parent-until
                      node
                      (lambda (n)
                        (let ((nb (if trim
                                      (f90-ts--node-start-trimmed n)
                                    (treesit-node-start n)))
                              (ne (if trim
                                      (f90-ts--node-end-trimmed n)
                                    (treesit-node-end n))))
                          (and (treesit-node-check n 'named)
                               (<= nb beg)
                               (>= ne end)
                               (< (- end beg)
                                  (- ne nb)))))
                      t)))
    (f90-ts--largest-node-same-span cover)))


(defun f90-ts--group-block-extent (node)
  "Return (FIRST . LAST) nodes for the group containing NODE."
  (let* ((first (cl-loop
                 for cur = node then next
                 for next = (treesit-node-prev-sibling cur)
                 while (and next (f90-ts--nodes-same-group-p next node))
                 finally return cur))
         (last  (cl-loop
                 for cur = node then next
                 for next = (treesit-node-next-sibling cur)
                 while (and next (f90-ts--nodes-same-group-p next node))
                 finally return cur)))
    (cons first last)))


(defun f90-ts--node-aligned-at-beg (beg end named)
  "Return the largest node starting exactly at BEG and ending before END.
If NAMED is non-nil, consider only named nodes."
  (when-let* ((node (f90-ts--node-on-pos beg 'right named)))
    (treesit-parent-while
     node
     (lambda (n)
       (and (=  beg (f90-ts--node-start-trimmed n))
            (<= (f90-ts--node-end-trimmed n) end))))))


(defun f90-ts--node-aligned-at-end (beg end named)
  "Return the largest node ending exactly at END and starting after BEG.
If NAMED is non-nil, consider only named nodes."
  (when-let* ((node (f90-ts--node-on-pos end 'left named)))
    (treesit-parent-while
     node
     (lambda (n)
       (and (<= beg (f90-ts--node-start-trimmed n))
            (= (f90-ts--node-end-trimmed n) end))))))


(defun f90-ts--group-block-p (node-beg node-end)
  "Check whether NODE-BEG and NODE-END are within a node group.
This is the case if both are siblings (have the same parent),
belong to the same group, and all siblings in between the two
nodes also belong to the same group."
  (and node-beg
       node-end
       (treesit-node-eq (treesit-node-parent node-beg)
                        (treesit-node-parent node-end))
       (f90-ts--nodes-same-group-p node-beg node-end)
       (cl-loop
        for cur = node-beg then (treesit-node-next-sibling cur t)
        while (and cur (not (treesit-node-eq cur node-end)))
        always (f90-ts--nodes-same-group-p cur node-beg))))


(cl-defstruct (f90-ts--region-info
               (:constructor f90-ts--region-info-create)
               (:copier nil))
  "Classification of a region with respect to tree-sitter nodes and groups.
BEG and END delimit the (trimmed) region.  FIRST and LAST are the nodes at
the start and end of the region, they are the same node for kind `single'.
KIND is one of:
  `single'    region is exactly one node
  `block'     region is exactly the full group from FIRST to LAST
  `partial'   region is within a group but neither single nor full
  `disparate' not a group, region starts and ends with disparate nodes
  nil         nodes at BEG or END could not be determined (FIRST and LAST
              are nil as well)"
  beg end kind first last)


(defun f90-ts--group-block-region-classify (beg end node-beg node-end)
  "Classify BEG..END within a confirmed group from NODE-BEG to NODE-END.
Return a `f90-ts--region-info'."
  (let* ((extent (f90-ts--group-block-extent node-beg))
         (node-first (car extent))
         (node-last (cdr extent))
         (first-beg (f90-ts--node-start-trimmed node-first))
         (last-end (f90-ts--node-end-trimmed node-last)))
    (cl-flet ((make-info (kind first last)
                (f90-ts--region-info-create
                 :beg beg :end end :kind kind :first first :last last)))
      (cond
       ((and (treesit-node-eq node-beg node-end)
             (= beg first-beg)
             (= end last-end))
        (make-info 'single node-beg node-beg))
       ((and (= beg first-beg) (= end last-end))
        (make-info 'block node-first node-last))
       (t
        (make-info 'partial node-beg node-end))))))


(defun f90-ts--group-block-region (beg end)
  "Classify type of region BEG..END with respect to groups of nodes.

If nodes at BEG and END exists but do not form a group, return them marked
as disparate, as these nodes are still useful for further operations.

Return a `f90-ts--region-info' (see there for the kinds), or nil if the node
at BEG or END could not be determined."
  (let* ((node-beg (or (f90-ts--node-aligned-at-beg beg end 'named)
                       (f90-ts--node-on-pos beg 'left 'named)))
         (node-end (or (f90-ts--node-aligned-at-end beg end 'named)
                       (f90-ts--node-on-pos end 'right 'named))))
    (cond
     ((f90-ts--group-block-p node-beg node-end)
      (f90-ts--group-block-region-classify beg end node-beg node-end))
     ((and node-beg node-end)
      (f90-ts--region-info-create
       :beg beg :end end :kind 'disparate :first node-beg :last node-end))
     (t
      nil))))


(defun f90-ts--group-block-eq (group1 group2)
  "Determine whether two groups GROUP1 and GROUP2 of nodes are equal.
Note that equality of the two first nodes is equivalent to the
equality of the two last nodes.
If at least one group is nil, return value is nil as well."
  (and group1
       group2
       (treesit-node-eq (car group1) (car group2))))


(defun f90-ts--active-region-info ()
  "Return `f90-ts--region-info' for the trimmed active region.
If the nodes at the region boundaries cannot be determined, then slots kind,
first and last are nil.  This function assumes that a region is active."
  (let* ((region (f90-ts--region-trimmed))
         (beg    (car region))
         (end    (cdr region)))
    (or (f90-ts--group-block-region beg end)
        (f90-ts--region-info-create :beg beg :end end))))


(defmacro f90-ts--with-active-region (slots &rest body)
  "Evaluate BODY with SLOTS of the active region info bound as variables.
SLOTS is a list of symbols naming slots of `f90-ts--region-info' (any of
beg, end, kind, first, last).  They are bound to the values for the trimmed
active region, see `f90-ts--active-region-info'.
If there is no active region, show a message and return nil without
evaluating BODY."
  (declare (indent 1) (debug (sexp body)))
  `(if (use-region-p)
       (pcase-let (((cl-struct f90-ts--region-info ,@slots)
                    (f90-ts--active-region-info)))
         ,@body)
     (message "no active region")))


(defun f90-ts-mark-region-enlarge-active ()
  "Expand active region to next larger node."
  (cl-assert (use-region-p)
             nil
             "active region required")

  (f90-ts--with-active-region (beg end kind first)
    ;; for a part of a comment block expand to the full block, unless the
    ;; block is just one line, then fall through to tree-sitter ascent
    (let ((extent (when (eq kind 'partial)
                    (f90-ts--group-block-extent first))))
      (if (and extent
               (not (treesit-node-eq (car extent) (cdr extent))))
          (f90-ts--mark-region-node-or-extent extent f90-ts-mark-region-order)
        (if-let* ((node (f90-ts--smallest-named-node-containing-region beg end 'trim)))
            (f90-ts--mark-region-node node f90-ts-mark-region-order)
          (message "no tree-sitter node found enlarging current region"))))))


(defun f90-ts-mark-region-enlarge-initial ()
  "Mark region of smallest named node at point.
This operation assumes that no region is active."
  (cl-assert (not (use-region-p))
             nil
             "active region already present")

  (let* ((pos (f90-ts--pos-nonspace (point)))
         (node-at (treesit-node-at pos)))
    (if (f90-ts--node-type-p node-at "comment")
        (f90-ts--mark-region-node node-at f90-ts-mark-region-order)
      (if-let* ((node-on (f90-ts--node-on-pos pos 'right 'named))
                (node    (f90-ts--largest-node-same-span node-on)))
          (f90-ts--mark-region-node node f90-ts-mark-region-order)
        (message "no tree-sitter node found at point")))))


(defun f90-ts-mark-region-enlarge ()
  "Expand region to next larger node.
If no region is active, select the smallest named node at point.
If region is active, expand to the smallest named node that is larger
than current region.
If current node is a comment belonging to a block of comments all
starting with the same comment prefix enlarge to the block of comments.
The comment prefix is matched by `f90-ts-openmp-prefix-regexp' and
`f90-ts-comment-prefix-regexp'."
  (interactive)
  (if (use-region-p)
      (f90-ts-mark-region-enlarge-active)
    (f90-ts-mark-region-enlarge-initial)))


(defun f90-ts--mark-region-shrink-child (direction)
  "Core routine for shrinking region to a child node.
Find smallest node covering region.  Then reduce region to its first child (for
DIRECTION `backward') or its last child (for DIRECTION `forward').  If there
are further grand-children with the same span, return the smallest grandchild
in the tree.
If the region is a full block of grouped nodes (comments), reduce it to the
first or last node of the block instead."
  (cl-assert (member direction '(backward forward))
             nil "direction is not backward or forward")
  (f90-ts--with-active-region (beg end kind first last)
    (let ((backward (eq direction 'backward)))
      (if (eq kind 'block)
          (f90-ts--mark-region-node (if backward first last)
                                    f90-ts-mark-region-order)
        (if-let* ((node-on (treesit-node-on beg end))
                  (node    (f90-ts--smallest-node-same-span node-on))
                  (child   (treesit-node-child node (if backward 0 -1) t)))
            (f90-ts--mark-region-node child f90-ts-mark-region-order)
          (message "no tree-sitter %s child found for current region"
                   (if backward "first" "last")))))))


(defun f90-ts-mark-region-shrink-child-first ()
  "Core routine for shrinking region to a child node.
Find smallest node covering region.  Then reduce region to its first child.
If there are further grand-children with the same span, return the smallest
grandchild in the tree."
  (interactive)
  (f90-ts--mark-region-shrink-child 'backward))


(defun f90-ts-mark-region-shrink-child-last ()
  "Core routine for shrinking region to a child node.
Find smallest node covering region.  Then reduce region to its last child.
If there are further grand-children with the same span, return the smallest
grandchild in the tree."
  (interactive)
  (f90-ts--mark-region-shrink-child 'forward))


(defun f90-ts--relative-or-group (anchor get-relative)
  "Return the relative of ANCHOR via GET-RELATIVE and group handling.
If the relative returned by GET-RELATIVE is in the same group as ANCHOR,
return the relative node itself, and do not extent to their group.
Otherwise return the full group extent of the relative.

Note that each node always forms a \"group\" on its own if it does not
belong to any proper block."
  (when-let* ((relative      (funcall get-relative anchor))
              (group-anchor   (f90-ts--group-block-extent anchor))
              (group-relative (f90-ts--group-block-extent relative)))
    (if (f90-ts--group-block-eq group-anchor group-relative)
        relative
      group-relative)))


(defun f90-ts--mark-region-relative (get-relative direction)
  "Mark relative of node determined by current active region and GET-RELATIVE.
DIRECTION is `backward' or `forward', used to resolve the anchor node when the
region spans a full group block.
For `backward' the first node of the group is used as anchor for GET-RELATIVE.
For `forward' the last node of the group is used as anchor for GET-RELATIVE.

First find smallest node covering currently active region.
If the node spans the current region, then mark its relative determined
by GET-RELATIVE.  GET-RELATIVE may return a node or a cons (FIRST . LAST).
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (cl-assert (member direction '(backward forward))
             nil "direction is not backward or forward")
  (f90-ts--with-active-region (beg end first last)
    (if-let* ((node-on (treesit-node-on beg end))
              (node    (f90-ts--largest-node-same-span node-on))
              (anchor  (if (eq direction 'backward) first last)))
        (let ((node-mark (or (f90-ts--relative-or-group anchor get-relative)
                             node)))
          (f90-ts--mark-region-node-or-extent node-mark f90-ts-mark-region-order))
      (message "tree-sitter relative not found for current region"))))


(defun f90-ts--relative-first-sibling (node)
  "Return the first named sibling of NODE (possibly NODE itself)."
  (when-let* ((parent (treesit-node-parent node)))
    (treesit-node-child parent 0 t)))


(defun f90-ts--relative-prev-sibling (node)
  "Return the previous named sibling of NODE."
  (treesit-node-prev-sibling node t))


(defun f90-ts--relative-next-sibling (node)
  "Return the next named sibling of NODE."
  (treesit-node-next-sibling node t))


(defun f90-ts--relative-last-sibling (node)
  "Return the last named sibling of NODE (possibly NODE itself)."
  (when-let* ((parent (treesit-node-parent node)))
    (treesit-node-child parent -1 t)))


(defun f90-ts-mark-region-first-sibling ()
  "Mark first sibling of node determined by current active region.
If the node spans the current region, then mark its first sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (f90-ts--mark-region-relative #'f90-ts--relative-first-sibling 'backward))


(defun f90-ts-mark-region-prev-sibling ()
  "Mark prev sibling of node determined by current active region.
If the node spans the current region, then mark its prev sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (f90-ts--mark-region-relative #'f90-ts--relative-prev-sibling 'backward))


(defun f90-ts-mark-region-next-sibling ()
  "Mark next sibling of node determined by current active region.
If the node spans the current region, then mark its next sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (f90-ts--mark-region-relative #'f90-ts--relative-next-sibling 'forward))


(defun f90-ts-mark-region-last-sibling ()
  "Mark last sibling of node determined by current active region.
If the node spans the current region, then mark its last sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (f90-ts--mark-region-relative #'f90-ts--relative-last-sibling 'forward))


;;;-----------------------------------------------------------------------------
;;; extend or reduce region by siblings

(defun f90-ts--region-extend (beg end at-start anchor get-relative)
  "Return region (BEG . END) extended by the relative of ANCHOR, or nil.
If AT-START is non-nil the start of the region moves, otherwise the end.
GET-RELATIVE is as in `f90-ts--mark-region-relative'.  Comment groups
are handled by `f90-ts--relative-or-group'."
  (when-let* ((relative (f90-ts--relative-or-group anchor get-relative))
              (span     (f90-ts--node-or-extent-span relative)))
    (if at-start
        (cons (min beg (car span)) end)
      (cons beg (max end (cdr span))))))


(defun f90-ts--region-reduce-start (beg end first last jump)
  "Return new start position for region BEG..END reduced at its start, or nil.
FIRST and LAST are the first and last node of the region.
If JUMP is nil, drop the first node, or its whole group if the group is
completely inside the region and something else remains.  If JUMP is
non-nil, reduce to the last node of the region, or to its whole group if the
group is completely inside the region and something else remains.  So if the
region is exactly one group, the last node of the group remains."
  (if jump
      (let ((gext (f90-ts--group-block-extent last)))
        (if (and (treesit-node-eq last (cdr gext))
                 (> (f90-ts--node-start-trimmed (car gext)) beg))
            (f90-ts--node-start-trimmed (car gext))
          (f90-ts--node-start-trimmed last)))
    (let* ((gext (f90-ts--group-block-extent first))
           (drop-group (and (treesit-node-eq first (car gext))
                            (< (f90-ts--node-end-trimmed (cdr gext)) end)))
           (next (treesit-node-next-sibling (if drop-group (cdr gext) first) t)))
      (when (and next
                 (<= (f90-ts--node-end-trimmed next) end))
        (f90-ts--node-start-trimmed next)))))


(defun f90-ts--region-reduce-end (beg end first last jump)
  "Return new end position for region BEG..END reduced at its end, or nil.
Mirror of `f90-ts--region-reduce-start': with JUMP reduce to node FIRST (or
its group, if something else remains) of the region, otherwise drop
node LAST (or its group)."
  (if jump
      (let ((gext (f90-ts--group-block-extent first)))
        (if (and (treesit-node-eq first (car gext))
                 (< (f90-ts--node-end-trimmed (cdr gext)) end))
            (f90-ts--node-end-trimmed (cdr gext))
          (f90-ts--node-end-trimmed first)))
    (let* ((gext (f90-ts--group-block-extent last))
           (drop-group (and (treesit-node-eq last (cdr gext))
                            (> (f90-ts--node-start-trimmed (car gext)) beg)))
           (prev (treesit-node-prev-sibling (if drop-group (car gext) last) t)))
      (when (and prev
                 (>= (f90-ts--node-start-trimmed prev) beg))
        (f90-ts--node-end-trimmed prev)))))


(defun f90-ts--mark-region-extend-or-reduce (get-relative direction jump)
  "Extend or reduce the active region by siblings, keeping the active end.
Only the end of the region where point is located is moved.  Whether the
region is extended or reduced depends on DIRECTION (`backward' or `forward')
and the position of point:

  point at start, `backward': extend (start moves to previous sibling)
  point at start, `forward':  reduce (start moves towards end)
  point at end,   `forward':  extend (end moves to next sibling)
  point at end,   `backward': reduce (end moves towards start)

GET-RELATIVE is as in `f90-ts--mark-region-relative' and used for extending.
If JUMP is non-nil, move to the extreme: when extending, to the first or
last sibling, when reducing, such that only the node (or group) at the
opposite end of the region remains.  If that group is the whole region, only
its single node at the opposite end remains.  After the operation point is
still at the same end of the region."
  (cl-assert (member direction '(backward forward))
             nil "direction is not backward or forward")
  (f90-ts--with-active-region (beg end first last)
    (if (not (and first last))
        (message "tree-sitter nodes not found for current region")
      (let* ((at-start (< (point) (mark)))
             (extend   (eq at-start (eq direction 'backward)))
             (new (if extend
                      (f90-ts--region-extend beg end at-start
                                             (if at-start first last)
                                             get-relative)
                    (if at-start
                        (when-let* ((nb (f90-ts--region-reduce-start
                                         beg end first last jump)))
                          (cons nb end))
                      (when-let* ((ne (f90-ts--region-reduce-end
                                       beg end first last jump)))
                        (cons beg ne))))))
        (if (or (null new)
                (equal new (cons beg end)))
            (message "region cannot be %s further"
                     (if extend "extended" "reduced"))
          (f90-ts--mark-region (car new) (cdr new)
                               (if at-start 'start 'end)))))))


(defun f90-ts-mark-region-extend-first-sibling ()
  "Extend or reduce active region towards the first sibling.
Point at start of region: extend start to the first sibling.
Point at end of region: reduce region to its first node (or group).
Point stays at its end of the region.  See
`f90-ts--mark-region-extend-or-reduce'."
  (interactive)
  (f90-ts--mark-region-extend-or-reduce
   #'f90-ts--relative-first-sibling 'backward t))


(defun f90-ts-mark-region-extend-prev-sibling ()
  "Extend or reduce active region by one step towards the previous sibling.
Point at start of region: extend start to the previous sibling.
Point at end of region: drop the last node (or group) from the region.
Point stays at its end of the region.  See
`f90-ts--mark-region-extend-or-reduce'."
  (interactive)
  (f90-ts--mark-region-extend-or-reduce
   #'f90-ts--relative-prev-sibling 'backward nil))


(defun f90-ts-mark-region-extend-next-sibling ()
  "Extend or reduce active region by one step towards the next sibling.
Point at end of region: extend end to the next sibling.
Point at start of region: drop the first node (or group) from the region.
Point stays at its end of the region.  See
`f90-ts--mark-region-extend-or-reduce'."
  (interactive)
  (f90-ts--mark-region-extend-or-reduce
   #'f90-ts--relative-next-sibling 'forward nil))


(defun f90-ts-mark-region-extend-last-sibling ()
  "Extend or reduce active region towards the last sibling.
Point at end of region: extend end to the last sibling.
Point at start of region: reduce region to its last node (or group).
Point stays at its end of the region.  See
`f90-ts--mark-region-extend-or-reduce'."
  (interactive)
  (f90-ts--mark-region-extend-or-reduce
   #'f90-ts--relative-last-sibling 'forward t))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-mark-region)

;;; f90-ts-mark-region.el ends here
