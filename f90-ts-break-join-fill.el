;;; f90-ts-indent.el --- Break, join and fill operations for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide tree-sitter based break, join and fill operations for `f90-ts-mode'.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)

;; indentation, sometimes performed in conjunction with breaking a line
(require 'f90-ts-indent)


;;;-----------------------------------------------------------------------------

(defconst f90-ts--join-line-prev-valid-actions
  '(empty-line
    beg-of-buffer
    remove-blank-lines-between
    join-continued-lines
    join-continued-string
    join-comments-same-prefix-trimmed
    join-comments-same-prefix-with-blanks
    append-comment-after-amp
    append-comment-after-stmt)
  "All valid action symbols returned by `f90-ts--join-line-prev-aux'.")


(defconst f90-ts--join-line-next-valid-actions
  '(empty-line
    end-of-buffer
    remove-blank-lines-between
    join-continued-lines
    join-continued-string
    join-comments-same-prefix-trimmed
    join-comments-same-prefix-with-blanks
    append-comment-after-amp
    append-comment-after-stmt)
  "All valid action symbols returned by `f90-ts--join-line-next-aux'.")


(defun f90-ts--break-line-insert-amp-at-end ()
  "If not yet present, insert ampersand at end of line."
  (or (eq (char-before) ?&)
      (insert " &")))


;; Portions of the following code are adapted from `f90.el',
;; which is part of GNU Emacs.
(defun f90-ts-break-line ()
  "Break line at point, insert continuation marker where necessary and indent."
  (interactive "*")
  (cond
   ((f90-ts--bol-to-point-blank-p)
    ;; this results in an empty line, no ampersand
    (delete-horizontal-space)
    (insert "\n"))

   ((f90-ts--in-openmp-p (point))
    ;; looks like a comment, but starting with special "!$" sequence,
    ;; breaking openmp lines requires a continuation symbol
    (let* ((node (treesit-node-at (point)))
           (prefix (f90-ts--comment-prefix node 'with-blanks)))
      (delete-horizontal-space)
      (f90-ts--break-line-insert-amp-at-end)
      (insert "\n" prefix)))

   ((f90-ts--in-comment-p (point))
    (cl-assert (not (f90-ts--in-openmp-p (point))) nil "unexpectedly within openmp comment")
    (let* ((node (treesit-node-at (point)))
           (prefix (f90-ts--comment-prefix node 'with-blanks)))
      (delete-horizontal-space)
      (insert "\n" prefix)))

   ;; comments can be within continued strings, so comments must be checked
   ;; before the in-string predicate (in fac in-comment-p and in-string-p can both be true
   ((f90-ts--in-string-p (point))
    (cl-assert (not (f90-ts--in-comment-p (point))) nil "unexpectedly within comment")
    (if (not (f90-ts--grammar-string-literal-decomposed-p))
        ;; old style continued string (this does not handle all cases properly
        (insert "&\n&")
      ;; first determine where point is within string
      (let ((node (treesit-node-on (1- (point)) (point))))
        (if (or (f90-ts--node-type-p node '("string_content"
                                            "\""
                                            "\\"))
                (and (f90-ts--node-type-p node "&")
                     (string= (treesit-node-text node) "&") ; exclude virtual ampersand
                     (= (treesit-node-start node)
                        (f90-ts--pos-first-nonspace (point)))))
            (insert "&\n&")
          ;; point is after a continuation symbol &, delete horizontal space and
          ;; just add an empty line with a leading ampersand
          (delete-horizontal-space)
          (newline 1)
          (insert "&&")
          (backward-char)))))

   (t
    (delete-horizontal-space)
    ;; note that a leading ampersand (depend on f90-ts-leading-ampersand)
    ;; is inserted by the indentation function, and thus does not need to
    ;; be added here
    (f90-ts--break-line-insert-amp-at-end)
    (newline 1)))
  ;; finally indent the new line after insertion of the newline
  (indent-according-to-mode))


(defun f90-ts--join-line-empty-prev ()
  "For point on an empty line, remove all blank lines before point.
After the operation point is after the last character on the non-blank line."
  (cl-assert (f90-ts--point-on-empty-line-p)
             nil "join-line-empty-prev requires point to be on an empty line")
  (let* ((prev (save-excursion
                 (skip-chars-backward " \t\n")
                 (point)))
         (end (line-end-position)))
    (delete-region prev end)))


(defun f90-ts--join-line-empty-next ()
  "For point on an empty line, remove all blank lines after point.
Move point to first character on the line after it was originally placed."
  (cl-assert (f90-ts--point-on-empty-line-p)
             nil "join-line-empty-next requires point to be on an empty line")
  (let* ((next (save-excursion
                 (skip-chars-forward " \t\n")
                 (line-beginning-position)))
         (beg (line-beginning-position)))
    (delete-region beg next)
    (back-to-indentation)))


(defun f90-ts--join-line-string (node pos1 pos2)
  "Join previous line with current line both belonging to a continued string.
Positions POS1 and POS2 are end of previous line and start of current line.

If the grammar variant does not decompose the string_literal, then NODE is
of type string_literal.  For the decomposed variant, NODE is the first
ampersand before POS1 and within the string_literal to be joined."
  (cl-assert (if (f90-ts--grammar-string-literal-decomposed-p)
                 (and (f90-ts--node-type-p node '("&" "string_content" "\""))
                      (f90-ts--node-type-p (treesit-node-parent node) "string_literal"))
               (f90-ts--node-type-p node "string_literal"))
             nil "node is not within a string literal")
  (cl-assert (or (not (f90-ts--node-type-p node "&"))
                 (eq (char-before pos1) ?&))
             nil "character before pos1 is not an ampersand")

  (let ((beg (if (eq (char-before pos1) ?&)
                 (1- pos1)
             ;; the continued string is missing the ampersand on the first line,
             ;; we still can safely join, but do not delete blanks after pos1
              (save-excursion
                (goto-char pos1)
                (line-end-position))))
        (end (if (eq (char-after pos2) ?&)
                 (1+ pos2)
               pos2)))
    (delete-region beg end)
    (goto-char beg)))


(defun f90-ts--join-action-trimmed-end (node)
  "Return the end position of NODE, moved back over trailing blanks."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-backward " \t")
    (point)))


(defun f90-ts--join-action-replace-with-blank (beg end action)
  "Return (THUNK . ACTION), with THUNK replacing region BEG..END by one blank."
  (cons (lambda ()
          (delete-region beg end)
          (goto-char beg)
          (insert " "))
        action))


(defun f90-ts--join-line-action-common (first secnd is-prev)
  "Compute an action pair for joining lines at FIRST and SECND.
The pair (THUNK . ACTION) provides a THUNK to perform the join and an ACTION
symbol describing what is done.

The ACTION symbol is used by the caller to decide whether to execute the thunk,
for example in a `fill-region' loop context.

Argument IS-PREV signals whether the function was called from the
`join-line-prev' or the `join-line-next' function."
  (let* ((is-amp-1st     (f90-ts--node-type-p first "&"))
         (is-amp-2nd     (f90-ts--node-type-p secnd "&"))
         (is-comment-1st (f90-ts--node-type-p first "comment"))
         (is-comment-2nd (f90-ts--node-type-p secnd "comment"))
         ;; a comment can be put after FIRST, except when FIRST is an
         ;; ampersand continuing a string_literal;
         ;; a comment after the ampersand is part of the string literal,
         ;; and thus not a comment anymore
         (appendable-comment (and is-comment-2nd
                                  (not is-comment-1st)
                                  (not (f90-ts--node-string first)))))
    (cond
     ((and is-amp-1st is-amp-2nd)
      (let ((beg (treesit-node-start first))
            (end (treesit-node-end secnd)))
        (cons (lambda ()
                (delete-region beg end)
                (goto-char beg)
                (fixup-whitespace)
                (skip-chars-forward " \t"))
              'join-continued-lines)))

     (appendable-comment
      (f90-ts--join-action-replace-with-blank
       (f90-ts--join-action-trimmed-end first)
       (treesit-node-start secnd)
       (if is-amp-1st
           'append-comment-after-amp
         'append-comment-after-stmt)))

     ((and is-comment-1st
           is-comment-2nd
           (string= (f90-ts--comment-prefix first 'trimmed)
                    (f90-ts--comment-prefix secnd 'trimmed)))
      (f90-ts--join-action-replace-with-blank
       (f90-ts--join-action-trimmed-end first)
       (+ (treesit-node-start secnd)
          (length (f90-ts--comment-prefix secnd 'with-blanks)))
       ;; join and fill behave differently: do not join in fill operation
       ;; if indentation differs, so signal whether the prefixes match
       ;; including blanks or only trimmed
       (if (string= (f90-ts--comment-prefix first 'with-blanks)
                    (f90-ts--comment-prefix secnd 'with-blanks))
           'join-comments-same-prefix-with-blanks
         'join-comments-same-prefix-trimmed)))

     (t
      (let ((beg (save-excursion
                   (goto-char (treesit-node-end first))
                   (line-beginning-position 2)))
            (end (save-excursion
                   (goto-char (treesit-node-start secnd))
                   (line-beginning-position))))
        (cons (lambda ()
                (delete-region beg end)
                (goto-char beg)
                (if is-prev
                    (skip-chars-forward " \t")
                  (skip-chars-backward " \t\n")))
              'remove-blank-lines-between))))))


(defun f90-ts--join-line-is-cont-str (first secnd)
  "Check whether the join is performed within a continued string literal.
FIRST and SECND are the leaf nodes at the first and second join positions,
which are at end of first and beginning of second line.
The predicate only returns non-nil if FIRST and SECND are ampersand nodes,
without any comments in-between (the non-decomposed grammar variant fails to
parse comments within strings)."
  (if (f90-ts--grammar-string-literal-decomposed-p)
      (and (f90-ts--node-type-p first '("&"
                                        ;; these are only posssible within a
                                        ;; malformed continued string without &
                                        "string_content" "\""))
           (f90-ts--node-type-p secnd '("&"
                                        ;; these are only posssible within a
                                        ;; malformed continued string without &
                                        "string_content" "\"" "\\"))
           (f90-ts--node-type-p (treesit-node-parent first) "string_literal"))
    (and (f90-ts--node-type-p first "string_literal")
         (f90-ts--node-type-p secnd "string_literal")
         (treesit-node-eq first secnd))))


(defun f90-ts--join-line-prev-aux ()
  "Join previous line with the current one, if part of a continued statement.
Return a cons (THUNK . ACTION); the caller is responsible for executing
THUNK and for messaging on failure."
  (cond
   ((f90-ts--point-on-empty-line-p)
    (cons (lambda () (f90-ts--join-line-empty-prev))
          'empty-line))

   ((save-excursion
      (back-to-indentation)
      (bobp))
    (cons (lambda () (beginning-of-line))
          'beg-of-buffer))

   (t
    (let* ((pos1 (save-excursion
                   (beginning-of-line)
                   (skip-chars-backward " \t\n")
                   (point)))
           (pos2 (save-excursion
                   (beginning-of-line)
                   (skip-chars-forward " \t\n")
                   (point)))
           (first (f90-ts--node-on-pos pos1 'left))
           (secnd (f90-ts--node-on-pos pos2 'right))
           ;; this is nil if first and secnd are within a string_literal,
           ;; but not within its proper parts (for example first is ampersand
           ;; and second is a comment)
           (is-cont-str (f90-ts--join-line-is-cont-str first secnd)))
      ;;(f90-ts-log-msg :joinprev "pos1: %s, %s" pos1 first)
      ;;(f90-ts-log-msg :joinprev "pos2: %s, %s" pos2 secnd)
      ;;(f90-ts-log-msg :joinprev "node-str: %s, %s" (f90-ts--node-string first) (f90-ts--node-string secnd))
      ;;(f90-ts-log-msg :joinprev "is-cont-str: %s" is-cont-str)
      (cl-assert (or first (= pos1 (point-min)))
                 nil "first node is nil with wrong pos1")
      (cl-assert (or (null first)
                     is-cont-str
                     (or (= pos1 (treesit-node-end first))
                         (and (f90-ts--node-type-p first "comment")
                              (string-blank-p (buffer-substring pos1 (treesit-node-end first))))))
                 nil "first node has wrong end position %s" first)
      (cl-assert secnd nil "secnd node is nil")
      (cl-assert (or (= pos2 (treesit-node-start secnd))
                     is-cont-str)
                 nil "secnd node has wrong start position")
      (cond
       (is-cont-str
        (cons (lambda () (f90-ts--join-line-string first pos1 pos2))
              'join-continued-string))

       (first
        (f90-ts--join-line-action-common first secnd t))

       (t
        (cons (lambda ()
                (back-to-indentation)
                (delete-region pos1 (point)))
              'beg-of-buffer)))))))


(defun f90-ts-join-line-prev ()
  "Join previous line with the current one, if part of a continued statement.
This is (partially) a counterpart to `f90-ts-break-line'.
If previous line has comments (at end, next line etc.) joining is not done."
  (interactive)
  (let* ((pair (f90-ts--join-line-prev-aux))
         (process-fn (car pair))
         (action (cdr pair)))
    (cl-assert (member action f90-ts--join-line-prev-valid-actions)
               nil "invalid action from f90-ts--join-line-prev-aux, got %s" action)
    (funcall process-fn)))


(defun f90-ts--join-line-next-aux ()
  "Join current line with the next one, if part of a continued statement.
Return a cons (THUNK . ACTION); the caller is responsible for executing
THUNK and for messaging on failure."
  (cond
   ((f90-ts--point-on-empty-line-p)
    (cons (lambda () (f90-ts--join-line-empty-next))
          'empty-line))

   ((save-excursion
      (end-of-line)
      (eobp))
    (cons (lambda () (end-of-line))
          'end-of-buffer))

   (t
    (let* ((pos1 (save-excursion
                   (end-of-line)
                   (skip-chars-backward " \t\n")
                   (point)))
           (pos2 (save-excursion
                   (end-of-line)
                   (skip-chars-forward " \t\n")
                   (point)))
           (first (f90-ts--node-on-pos pos1 'left))
           (secnd (f90-ts--node-on-pos pos2 'right))
           (is-cont-str (f90-ts--join-line-is-cont-str first secnd)))
      ;;(f90-ts-log-msg :joinnext "pos1: %s, %s" pos1 first)
      ;;(f90-ts-log-msg :joinnext "pos2: %s, %s" pos2 secnd)
      ;;(f90-ts-log-msg :joinnext "is-cont-str: %s" is-cont-str)
      (cl-assert first nil "first node is nil")
      (cl-assert (or (= pos1 (treesit-node-end first))
                     is-cont-str
                     (and (f90-ts--node-type-p first "comment")
                          (string-blank-p (buffer-substring pos1 (treesit-node-end first)))))
                 nil "first node has wrong end position")
      (cl-assert secnd nil "secnd node is nil")
      (cl-assert (or (not (f90-ts--node-type-p secnd "translation_unit"))
                     (= pos2 (point-max)))
                 nil "secnd node is translation_unit with wrong pos2")
      (cl-assert (or (f90-ts--node-type-p secnd "translation_unit")
                     is-cont-str
                     (= pos2 (treesit-node-start secnd)))
                 nil "secnd node has wrong start position")
      (cond
       ((f90-ts--node-type-p secnd "translation_unit")
        (cons (lambda ()
                (end-of-line)
                (delete-region (point) pos2))
              'end-of-buffer))

       (is-cont-str
        (cons (lambda () (f90-ts--join-line-string first pos1 pos2))
              'join-continued-string))

       (t
        (f90-ts--join-line-action-common first secnd nil)))))))


(defun f90-ts-join-line-next ()
  "Join current line with the next one, if part of a continued statement.
This is (partially) a counterpart to `f90-ts-break-line'.
If continued line has comments (at end, next line etc.) joining is not done."
  (interactive)
  (let* ((pair (f90-ts--join-line-next-aux))
         (process-fn (car pair))
         (action (cdr pair)))
    (cl-assert (member action f90-ts--join-line-next-valid-actions)
               nil "invalid action from f90-ts--join-line-next-aux, got %s" action)
    (funcall process-fn)))


(defun f90-ts-shift-line-break ()
  "Shift the break point of current line to point.
It breaks line at point and then joins the trailing part with the next line.
If point is at end of line, then it does not nothing except to move point to
indentation of next line."
  (interactive)
  (if (save-excursion
        (skip-chars-forward " \t")
        (eolp))
      (when (zerop (forward-line 1))
        (back-to-indentation)
        (let ((node (treesit-node-at (point))))
          (when (f90-ts--node-type-p node "comment")
            (f90-ts--comment-forward-prefix node)
            (skip-chars-forward " \t"))))
    (f90-ts-break-line)
    (f90-ts-join-line-next)))


;;;-----------------------------------------------------------------------------
;;; fill operations
;;; the code is inspired and partially copied from the f90-mode.el in emacs core

(defconst f90-ts--join-line-fill-auto-actions
  '(join-continued-lines
    join-continued-string
    join-comments-same-prefix-with-blanks)
  "Set of join actions for non-interactive join during fill operations.
It is a subset of `f90-ts--join-line-next-valid-actions'.")


(defconst f90-ts--join-line-fill-interactive-actions
  '(join-continued-lines
    join-continued-string
    join-comments-same-prefix-trimmed
    join-comments-same-prefix-with-blanks
    append-comment-after-amp
    append-comment-after-stmt)
  "Set of join actions for interactive join during fill operations.
It is a subset of both `f90-ts--join-line-prev-valid-actions'
and `f90-ts--join-line-next-valid-actions' and used for interactive join with
previous and with next line.")


(defvar-local f90-ts--breakpoint-seed-markers nil
  "List of markers recording positions where continuation lines were joined.
It has the format (cons POS-MARKER SEED-MARKERS), where pos-marker stores
current point as preferred initial position, seed-markers are other seeds.
The other seeds are populated by `f90-ts--join-line-fill' during the join
phase and consumed by `f90-ts--find-breakpoint' during the subsequent break
phase to choose initial interactive position in case pos-marker is not
available.  Cleared after each statement is joined and broken up completely.")


(defun f90-ts--breakpoint-seed-markers-reset ()
  "Reset `f90-ts--breakpoint-seed-markers', invalidating all markers."
  (when-let* ((m (car f90-ts--breakpoint-seed-markers)))
    (set-marker m nil))
  (cl-loop for m in (cdr f90-ts--breakpoint-seed-markers)
           do (set-marker m nil))
  (setq f90-ts--breakpoint-seed-markers (cons nil nil)))


(defun f90-ts--seed-markers-pos-marker ()
  "Return the pos-marker from `f90-ts--breakpoint-seed-markers', or nil."
  (car f90-ts--breakpoint-seed-markers))


(defun f90-ts--seed-markers-seeds ()
  "Return the seed marker list from `f90-ts--breakpoint-seed-markers'."
  (cdr f90-ts--breakpoint-seed-markers))


(defun f90-ts--seed-markers-set-pos (pos)
  "Set or update the pos-marker to POS in `f90-ts--breakpoint-seed-markers'."
  (unless f90-ts--breakpoint-seed-markers
    (setq f90-ts--breakpoint-seed-markers (cons nil nil)))
  (if-let* ((m (car f90-ts--breakpoint-seed-markers)))
      (set-marker m pos)
    (setcar f90-ts--breakpoint-seed-markers (set-marker (make-marker) pos))))


(defun f90-ts--seed-markers-push ()
  "Push a seed marker at the current non-whitespace position.
Skip backwards over blanks before recording the position.
Does nothing if a seed marker at that position already exists."
  (save-excursion
    (skip-chars-backward " \t")
    (let ((pos (point)))
      (unless (cl-loop for m in (f90-ts--seed-markers-seeds)
                       when (= (marker-position m) pos) return t)
        (unless f90-ts--breakpoint-seed-markers
          (setq f90-ts--breakpoint-seed-markers (cons nil nil)))
        (push (copy-marker pos) (cdr f90-ts--breakpoint-seed-markers))))))


(defun f90-ts--seed-markers-invalidate-pos ()
  "Invalidate the pos-marker in `f90-ts--breakpoint-seed-markers'."
  (when-let* ((m (car f90-ts--breakpoint-seed-markers)))
    (set-marker m nil)
    (setcar f90-ts--breakpoint-seed-markers nil)))


(defun f90-ts--seed-markers-cleanup-before (pos)
  "Discard seed markers at or before POS, keeping those beyond it."
  (when f90-ts--breakpoint-seed-markers
    (setcdr f90-ts--breakpoint-seed-markers
            (cl-loop
             for m in (f90-ts--seed-markers-seeds)
             if (> (marker-position m) pos)
             collect m
             else do (set-marker m nil)))))


(defun f90-ts--breakpoint-node-within-fill-col-p (node pos fill-col)
  "Return non-nil if POS is within the effective fill column for NODE.
If NODE is a comment, no continuation ampersand is added, so FILL-COL
is compared directly.
If NODE is a string_literal, the continuation ampersand is added without a
blank, hence FILL-COL needs to be reduced by 1.
Otherwise two characters are subtracted to
account for the blank and ampersand appended by the break operation."
  (let ((fill-col-adjusted
         (cond
          ((f90-ts--node-type-p node "comment")
           fill-col)
          ((f90-ts--node-type-p node '("string_content"))
           (- fill-col 1))
          (t
           (- fill-col 2)))))

    (<= (f90-ts--column-number-at-pos pos)
        fill-col-adjusted)))


(defun f90-ts--breakpoint-pos-within-fill-col-p (pos fill-col)
  "Return non-nil if POS is within the effective fill column.
Depending on whether pos is within a statement, within a comment
or within a string, FILL-COL needs to be adjusted.
Like `f90-ts--breakpoint-node-within-fill-col-p', but queries the node at
POS via `treesit-node-at'."
  (f90-ts--breakpoint-node-within-fill-col-p (treesit-node-at pos) pos fill-col))


(defun f90-ts--find-breakpoint-prefer-p (node)
  "Decide whether the end point of NODE is a preferred breakpoint.
If possible, certain positions, like directly left of a comma, opening
parenthesis, before or after \"%\" in component selection should be avoided.
This predicate is ignored in break point selection only if no break point can
be found otherwise."
  (let* ((pos-next (save-excursion
                     (goto-char (treesit-node-end node))
                     (skip-chars-forward " \t\n")
                     (point)))
         (node-next (when pos-next
                      (treesit-node-at pos-next))))
    (not (or
          ;; do not split right before any of these delimiters
          (f90-ts--node-type-p node-next '("," "(" "%"))
          ;; do not split after this delimiter
          (f90-ts--node-type-p node "%")))))


(defun f90-ts--find-breakpoint-in-comment (node fill-col)
  "Find all break positions inside comment NODE at column <= FILL-COL.
Scan the comment text for word boundaries (whitespace preceded by a
non-whitespace character) that fall within the fill budget.  The
comment prefix (matched by `f90-ts-comment-prefix-regexp') is skipped
before searching for break candidates.
Return a sorted list of buffer positions, or nil."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "\"comment\" node expected")
  (let* ((end (f90-ts--node-end-trimmed node)))
    (save-excursion
      (f90-ts--comment-forward-prefix node)
      (sort
       (cl-loop
        with prefix-end = (point)
        while (re-search-forward "\\S-\\(\\s-+\\)\\S-" end t)
        for pos = (match-beginning 1)
        ;; go back to avoid skipping the last \\S- match, which is
        ;; captured in the next loop as the first \\S-, otherwise we skip
        ;; one letter words like "a"
        do (goto-char (match-end 1))
        when (<= (f90-ts--column-number-at-pos pos) fill-col)
        collect pos
        into positions
        finally return (if (eq f90-ts-fill-select-breakpoint-by 'interactive)
                           (cons end (cons prefix-end positions))
                         positions))
       #'<))))


(defun f90-ts--find-breakpoint-in-string (node beg fill-col &optional prefer)
  "Find all break positions inside string literal NODE at column <= FILL-COL.
Start the search at position BEG.  NODE is the whole string_literal and can
start well before the current line.
Scan the string content for word boundaries (whitespace preceded by a
non-whitespace character) that fall within the fill budget and are on the
current line.  Return a sorted list of buffer positions, or nil.
If PREFER is non-nil, select only some positions (like after a word etc.),
otherwise return all positions."
  (cl-assert (f90-ts--node-type-p node "string_literal")
             nil "\"string_literal\" node expected")
  (save-excursion
    ;; make sure that we stay on the line and within node boundaries
    ;; (string_literal can start before current line, argument beg
    ;; already restrict start to after default indentation column)
    (let* ((start (max beg
                       (save-excursion
                         (back-to-indentation)
                         (skip-chars-forward "&")
                         (point))))
           (end (min (f90-ts--node-end-trimmed node)
                     (progn
                       (goto-char (line-end-position))
                       ;; add one additional character (blank or &, which is matched
                       ;; in re-search-forward below to ensure that position is eligible
                       (move-to-column (min (1+ fill-col) (current-column)))
                       (point)))))
      (if (null prefer)
          ;; take all positions, allow breaking at any point within the string
          (cl-loop for pos from (1+ start) below end
                   when (<= (f90-ts--column-number-at-pos pos) (1- fill-col))
                   collect pos)
        (goto-char start)
        (cl-loop
         ;; at whitespace boundary (\S- followed by \s-), or after punctuation
         ;;do (f90-ts-log-line :brpstr "match" ?\u25C6)
         ;;do (f90-ts-log-line :brpstr "match" ?\x1f48e)
         while (re-search-forward
                "\\S-\\(\\s-\\|[^[:alnum:]]\\)\\|[^[:alnum:]]"
                end t)
         for pos = (if (match-beginning 1)
                       (match-beginning 1)
                     (match-end 0))
         ;; advance by at least one position, do not use pos, as leading characters
         ;; in the match might just be used for assertions
         ;; (make it general so that we can possibly make the regexp a defcustom)
         do (goto-char (1+ (match-beginning 0)))
         when (and (< start pos)
                   (<= (f90-ts--column-number-at-pos pos) (1- fill-col)))
         collect pos
         into positions
         finally return positions)))))


(defun f90-ts--find-breakpoint-collect (sparse-tree)
  "Collect all break-point positions from SPARSE-TREE.
SPARSE-TREE is a cons of a list of position integers and a possibly empty list
of children of the same format as SPARSE-TREE.
It returns a sorted list of buffer positions of prospective breakpoints."
  (let ((collected (cl-labels
                       ((collect (tree)
                          (when (consp tree)
                            (append (car tree)
                                    (seq-mapcat #'collect (cdr tree))))))
                     (collect sparse-tree))))
    (delete-dups
     (sort collected #'<))))


(defun f90-ts--find-breakpoints-for-node (node beg fill-col &optional prefer)
  "Return a list of break positions for NODE after BEG and before FILL-COL.
In general, it returns the (trimmed) end position of the node as a
singleton list, if the column is below (- FILL-COL 2).  Two is subtracted to
account for a blank and the continuation symbol, which will be added by
breaking at this position.  However, for comment nodes and string literals,
there might be several prospective positions within the node.  These are
collected by `f90-ts--find-breakpoint-in-comment' and by
`f90-ts--find-breakpoint-in-string'

If PREFER is non-nil, then break points within string literals is
restricted to preferred positions."
  (cond
   ((f90-ts--node-type-p node "comment")
    (f90-ts--find-breakpoint-in-comment node fill-col))

   ((and (f90-ts--grammar-string-literal-decomposed-p)
         (f90-ts--node-type-p node "string_content"))
    (f90-ts--find-breakpoint-in-string (treesit-node-parent node)
                                       beg fill-col
                                       prefer))

   (t
    (let* ((pos (f90-ts--node-end-trimmed node)))
      (when (f90-ts--breakpoint-node-within-fill-col-p node pos fill-col)
        (list pos))))))


(defun f90-ts--find-breakpoint-sparse-tree (root beg end fill-col &optional prefer)
  "Return sparse tree of break points within tree ROOT.
The sparse tree is obtained by filtering relevant nodes and then mapping those
nodes to a list of possible break points.

In most cases, the function filters for leaf nodes whose end position is a
possible break point.  Nodes are selected by their (trimmed) end position.
In particular, the trimmed end of node should be between BEG and END and respect
the fill column FILL-COL.
If PREFER is non-nil, then further reduce selection of nodes and break points
to preferred positions, using `f90-ts--find-breakpoint-prefer-p'.

Comment and string literal nodes require special care, as these nodes can be
broken in the middle."
  ;; first map root to a sparse tree of prospective positions,
  ;; then collect and sort the list
  (treesit-induce-sparse-tree
   root
   (lambda (n)
     (let* ((end-node (f90-ts--node-end-trimmed n)))
       ;; only leaf nodes whose trimmed end falls on the
       ;; current line, the column check is done in the process-fn
       ;; function, as comment and string_literal nodes need special care
       (and (null (treesit-node-children n))
            (<= beg end-node)
            (<= end-node end)
            (not (f90-ts--node-type-p n "&"))
            (or (null prefer) (f90-ts--find-breakpoint-prefer-p n)))))
   (lambda (n) (f90-ts--find-breakpoints-for-node n beg fill-col prefer))))


(defun f90-ts--find-breakpoint-rightmost-idx (positions fill-col)
  "Return index in POSITIONS of the rightmost candidate within FILL-COL.
A candidate satisfies the constraint according to
`f90-ts--breakpoint-pos-within-fill-col-p'.  Return nil if no candidate
in POSITIONS satisfies it."
  (when-let* ((target (cl-loop
                        for p in positions
                        when (f90-ts--breakpoint-pos-within-fill-col-p p fill-col)
                        maximize p)))
    (cl-position target positions)))


(defun f90-ts--find-breakpoint-read-key (idx positions fill-col at-end)
  "Read one key and return (IDX ACTION) for candidate POSITIONS.
ACTION is nil to continue, or a symbol: `confirm', `join', `skip',
`abort', `continue'.  AT-END controls whether SPC is is just a skip
line operation or whether it extends the region by one line.
FILL-COL is used to determine the target position for the \"r\" key,
which jumps to the rightmost candidate at or before FILL-COL."
  (let ((prompt (concat "Select break point: left/right/home/end or C-p/C-n, "
                        "r rightmost, "
                        "BACKSPACE/DEL join, RET confirm"
                        (if at-end
                            ", q skip line, SPC skip and extend region"
                          ", q/SPC skip line")
                        ", C-g abort")))
    (message prompt))
  (let ((key (read-key))
        (num-pos (length positions)))
    (cond
     ((or (eq key 'left)  (eq key ?\C-p)) (list (mod (1- idx) num-pos) nil))
     ((or (eq key 'right) (eq key ?\C-n)) (list (mod (1+ idx) num-pos) nil))
     ((eq key 'home)                      (list 0                      nil))
     ((eq key 'end)                       (list (1- num-pos)           nil))
     ((eq key ?r)
      (list (or (f90-ts--find-breakpoint-rightmost-idx positions fill-col) idx)
            nil))
     ((memq key '(return ?\r ?\n))        (list idx                    'confirm))
     ((eq key ?\d)                        (list idx                    'join-prev))
     ((eq key 'deletechar)                (list idx                    'join-next))
     ((eq key ?q)                         (list idx                    'skip))
     ((eq key ?\C-g)                      (list idx                    'abort))
     ((eq key ?\s)                        (list idx                    (if at-end 'continue 'skip)))
     ;; ignore any other keys, and keep idx
     (t                                   (list idx                    nil)))))


(defun f90-ts--find-breakpoint-initial-pos (positions fill-col)
  "Return initial position for interactive breakpoint selection from POSITIONS.
If the pos-marker in `f90-ts--breakpoint-seed-markers' is valid, return
the closest candidate at or before it.  Otherwise, return the closest
candidate at or before the rightmost seed marker within FILL-COL.
If no seed markers are available, return the rightmost candidate within
FILL-COL."
  (or
   ;; pos-marker: closest candidate at or before it
   (when-let* ((m (f90-ts--seed-markers-pos-marker))
               (ref (marker-position m)))
     (cl-loop
      for p in positions
      when (<= p ref)
      maximize p into best
      finally return best))

   ;; other seed-markers: rightmost marker within fill-col and then
   ;;                     closest candidate at or before it
   (when-let* ((ref (cl-loop
                     for m in (f90-ts--seed-markers-seeds)
                     for mpos = (marker-position m)
                     when (and mpos
                               (f90-ts--breakpoint-pos-within-fill-col-p mpos fill-col))
                     maximize mpos into best
                     finally return best)))
     (cl-loop
      for p in positions
      when (<= p ref)
      maximize p into best
      finally return best))

   ;; fallback: rightmost candidate in positions within fill-col
   (cl-loop
    for p in positions
    when (f90-ts--breakpoint-pos-within-fill-col-p p fill-col)
    maximize p into best
    finally return best)))


(defun f90-ts--find-breakpoint-interactive (positions fill-col allow-continue)
  "Interactively select a break point from POSITIONS.
Moves point to each candidate position as the user navigates with
LEFT/RIGHT or \\[previous-line]/\\[next-line].  Return the selected position
on RET, or nil on \\[keyboard-quit].
Use POSITIONS to determine a candidate closest to FILL-COL but before that.
ALLOW-CONTINUE controls whether SPC is offered to extend the fill region.

Remark: the `read-key' loop in `f90-ts--find-breakpoint-read-key' blocks other
interaction (like scrolling in another buffer with mouse).  This might need
some improvements."
  (when-let* ((initial-pos (or (f90-ts--find-breakpoint-initial-pos positions fill-col)
                               ;; no previous breakpoint available, then look into positions
                               ;; before fill-col
                               (cl-loop for p in positions
                                        when (<= (f90-ts--column-number-at-pos p) fill-col)
                                        maximize p)
                               ;; use smallest position available as fallback
                               (car positions)))
              (idx (cl-position initial-pos positions))
              (orig-point (point))
              (orig-cursor cursor-type))
    (unwind-protect
        (progn
          ;; make cursor bar of roughly one third of character width
          (setq cursor-type (cons 'bar (max 2 (/ (frame-char-width) 3))))
          (goto-char (nth idx positions))
          (cl-loop
           for (new-idx action) = (f90-ts--find-breakpoint-read-key idx positions fill-col
                                                                    allow-continue)
           do (setq idx new-idx)
           until action
           do (goto-char (nth idx positions))
           ;; track current position in pos-marker in seed-markers list
           do (f90-ts--seed-markers-set-pos (point))
           finally return (pcase action
                            ;; return value nil signals that no position is available,
                            ;; do not break this line
                            ('confirm    (nth idx positions))
                            ('join-prev  'join-prev)
                            ('join-next  'join-next)
                            ('skip       nil)
                            ('abort      'abort)
                            ('continue   'continue))))
      (setq cursor-type orig-cursor)
      (goto-char orig-point))))


(defun f90-ts--find-breakpoints-indent ()
  "Estimate the indentation column of the next line after breaking current line.
This estimate is reuqired to make sure a break point is chosen after the
new indentation level, so that breaking the line does not does not make the
layout worse.
The return value is a buffer position on the current line indicating the
guessed indentation column on the next line.  Any break point on the current
is after that buffer position."
  ;; TODO: what kind of estimate do we really need here??
  ;; possible contexts: single line statement, multiline statements,
  ;;           comments, statement with trailing comment, ...
  ;; example: breaking
  ;; x=very_long_unbreakable_variable_name
  ;; could give
  ;; x= &
  ;;      very_long_unbreakable_variable_name
  ;; due to indentation rules, making it worse than before
  (save-excursion
    (back-to-indentation)
    (let ((node (treesit-node-at (point))))
      (+ (point)
         (cond
          ((f90-ts--subsequent-line-of-continued-stmt-p (point))
           1)
          ((f90-ts--node-type-p node "comment")
           0)
          (t
           f90-ts-indent-continued))))))


(defun f90-ts--find-breakpoints-all (fill-col fill-col-select)
  "Collect all candidate break points on the current line.
In general a break point is any end position of a leaf node on the current
line, at column at most FILL-COL-SELECT minus 2 and beyond the estimated
indentation of the continued line.  FILL-COL is the fill column,
which can be ignored in interactive mode, but is still used to preselect
prospective break positions and for some cursor movements in interactive mode.

Return a list of candidate POSITIONS.  There are three cases:
- no preferred positions exist: fall back to all positions, bounded by
  FILL-COL-SELECT;
- in interactive mode, preferred positions exist but none are within
  FILL-COL: merge all positions within FILL-COL with the preferred
  positions (which may lie beyond FILL-COL), so navigating past fill-col
  only offers sensible break points;
- otherwise (preferred positions exist, and either at least one is
  within FILL-COL, or selection mode is not interactive): use the preferred
  positions as-is.

If no positions exist, then it returns nil."
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (indent (f90-ts--find-breakpoints-indent))
         (root (treesit-node-on beg end))
         (pos-prefer (f90-ts--find-breakpoint-collect
                      (f90-ts--find-breakpoint-sparse-tree
                       root indent end fill-col-select
                       'prefer)))

         (positions (if (and pos-prefer
                             (f90-ts--breakpoint-pos-within-fill-col-p (car pos-prefer) fill-col))
                        pos-prefer
                      (f90-ts--find-breakpoint-collect
                       (f90-ts--find-breakpoint-sparse-tree
                        root indent end fill-col-select
                        nil)))))

    (if (eq f90-ts-fill-select-breakpoint-by 'interactive)
        (let ((marker-pos (f90-ts--find-breakpoint-initial-pos pos-prefer fill-col)))
          (if (and marker-pos (not (member marker-pos positions)))
              (cl-sort (cons marker-pos positions) #'<)
            positions))
      positions)))


(defun f90-ts--find-breakpoint (fill-col allow-continue)
  "Find a break point on the current line.
A break point is any end position of a leaf node on the current line and at
column at most FILL-COL minus 2.  Return the buffer position to break at,
or nil if no suitable break point exists.

The selection strategy is controlled by `f90-ts-fill-select-breakpoint-by'.

For interactive selection, the rightmost marker in
`f90-ts--breakpoint-seed-markers' that falls among the eligible positions is
used as the initial suggestion.
Interactive mode allows to select and break after FILL-COL.
ALLOW-CONTINUE is passed to `f90-ts--find-breakpoint-interactive' to
control whether SPC is offered to extend the fill region."
  (when (or (> (f90-ts--column-number-at-pos (line-end-position)) fill-col)
            (eq f90-ts-fill-select-breakpoint-by 'interactive))
    (let* ((fill-col-select (if (eq f90-ts-fill-select-breakpoint-by 'interactive)
                                most-positive-fixnum
                              fill-col))
           (positions (f90-ts--find-breakpoints-all fill-col fill-col-select)))
      (when positions
        (pcase f90-ts-fill-select-breakpoint-by
          ('rightmost
           (apply #'max positions))
          ('interactive
           (f90-ts--find-breakpoint-interactive positions
                                                fill-col
                                                allow-continue)))))))


(defun f90-ts--break-line-wrap (fill-col allow-continue)
  "Break the current line once if it exceeds FILL-COL.
Find a breakpoint via `f90-ts--find-breakpoint' according to
`f90-ts-find-breakpoint-select' and call `f90-ts-break-line' with position.
Return non-nil if a break was performed, nil if the line already fits
or no breakpoint can be found.  The function assumes that the line has
no trailing blanks.

ALLOW-CONTINUE controls whether SPC is offered in interactive mode to
extend the fill region; when selected, returns the symbol `continue'.

Join markers in `f90-ts--breakpoint-seed-markers', which are before the
break point, are discarded; those beyond it are kept as candidates for
subsequent interactive break point selection."
  (cl-assert (= (line-end-position)
                (f90-ts--pos-last-nonspace (point)))
             nil "line has trailing blanks at buffer position %s" (point))
  (let ((pos (f90-ts--find-breakpoint fill-col allow-continue)))
    (cond
     ((null pos)          'skip)  ; no break point found or selected
     ((eq pos 'join-prev) 'join-prev)
     ((eq pos 'join-next) 'join-next)
     ((eq pos 'abort)     'abort) ; abort the fill operation
     ((eq pos 'continue)  'continue)
     ((or (= pos (line-end-position))
          (save-excursion
            (goto-char pos)
            (skip-chars-forward " \t")
            (f90-ts--node-type-p (f90-ts--node-on-pos (point) 'right)
                                 "&")))
      ;; do not break if at end of line or at continuation symbol ampersand
      'skip)
     (t ; position value determined by action 'confirm
      (goto-char pos)
      (f90-ts-break-line)
      ;; discard markers at or before the break point,
      ;; keep only those beyond it (still valid candidates)
      (let ((break-pos (point)))
        (f90-ts--seed-markers-invalidate-pos)
        (f90-ts--seed-markers-cleanup-before break-pos))
      'broken))))


(defun f90-ts--join-line-fill-p (join-col end-marker)
  "Return non-nil if current line should be joined with next line for filling.
It checks that the current line is shorter than JOIN-COL, that next line does
not start after END-MARKER and that neither the current nor next line is empty
or an empty comment.  This is done to preserve any comment structure.

Note: most checks whether lines can be joined are done in
`f90-ts--join-line-action-common', which returns an action symbol for deciding
whether a join operations fits the fill case.  This predicate provides a
pre-check to exclude column related cases and some specific corner-case."
  (not
   (or (>= (- (line-end-position)
              (line-beginning-position))
           join-col)
       (>= (save-excursion (forward-line 1) (point))
           end-marker)
       (f90-ts--next-line-empty-p (point))
       (f90-ts--line-empty-comment-p (point))
       (f90-ts--next-line-empty-comment-p (point)))))


(defun f90-ts--join-line-fill (fill-col end-marker &optional requested)
  "Join current line with the next one, as part of a fill operation.
The join operation is performed as long as current line does not exceed
column FILL-COL and the next line does not start after END-MARKER.

In interactive mode, REQUESTED can be non-nil.  In this case, the fill
pre-check `f90-ts--join-line-fill-p' is bypassed and the full set of join
actions is accepted.
If REQUESTED is `join-prev' then `f90-ts-join-line-prev' is invoked.
If REQUESTED is `join-next' then `f90-ts-join-line-next' is invoked.

The function adds the position of the join in `f90-ts--breakpoint-seed-markers'
after each successful join."
  (cl-assert (memq requested '(nil join-prev join-next))
             nil "invalid join action, requested is %s" requested)
  (when (or requested (f90-ts--join-line-fill-p fill-col end-marker))
    (let* ((prev-p (eq requested 'join-prev))
           (pair (if prev-p
                     (f90-ts--join-line-prev-aux)
                   (f90-ts--join-line-next-aux)))
           (process-fn (car pair))
           (action     (cdr pair))
           (valid-actions (if prev-p
                              f90-ts--join-line-prev-valid-actions
                            f90-ts--join-line-next-valid-actions))
           (accepted-actions (if requested
                                 f90-ts--join-line-fill-interactive-actions
                               f90-ts--join-line-fill-auto-actions)))
      (cl-assert (member action valid-actions)
                 nil "invalid join action, got %s" action)
      (when (member action accepted-actions)
        (funcall process-fn)
        (f90-ts--seed-markers-push)
        ;; signal that a join took place
        t))))


(defun f90-ts--fill-region-update-end-overlay (ov end-marker)
  "Update OV to show the END-MARKER with an indicator.
END-MARKER is assumed to be at beginning of line position.
The overlay is placed on the end of the previous line, which is the
last line to be filled."
  (save-excursion
    (goto-char (marker-position end-marker))
    (if (eobp)
        (move-overlay ov (1- (point)) (point))
      (let ((pos (1- (point))))         ; newline character of preceding line
        (move-overlay ov (1- pos) pos)))
    ;; 25C4 is a left pointing filled triangle
    (overlay-put ov 'after-string (propertize " \u25C4" 'face 'warning))))


(defun f90-ts--fill-region-continue (end-marker end-overlay)
  "Advance END-MARKER past empty and empty-comment lines.
Called after a \\='continue action to position END-MARKER at the
first line where a break candidate can exist and interactive session
can resume.
The function updates END-OVERLAY to match the new end marker position."
  ;; assertion also works if at final line at eob with and without
  ;; trailing newline
  (cl-assert (= (marker-position end-marker) (line-beginning-position 2))
             nil "end-marker not at start of next line: marker=%s lbp2=%s"
             (marker-position end-marker) (line-beginning-position 2))
  (unless (= (marker-position end-marker) (point-max))
    (save-excursion
      (goto-char (line-beginning-position 2))
      (while (and (not (eobp))
                  (f90-ts--line-empty-p (point)))
        (forward-line 1))
      ;; either at end of buffer or at first non-empty-like line,
      ;; place marker at start of next line
      (forward-line 1)
      (set-marker end-marker (point)))
    (when end-overlay
      (f90-ts--fill-region-update-end-overlay end-overlay end-marker))))


(defun f90-ts--fill-region-join-step (broke end-marker fill-col)
  "Attempt to join the current line with its continuation.
BROKE is the result of the previous break step; when it is the symbol
`join', the user has requested a standard join with next line and fill
heuristics are bypassed.  END-MARKER is the region end marker and is moved
to the start of the next line if it falls inside the newly joined line, so that
the main loop's while condition does not abort prematurely.  FILL-COL is the
target fill column passed to `f90-ts--join-line-fill'.
Return non-nil if a join was performed or signalled."
  (unless (f90-ts--point-on-empty-line-p)
    (let ((joined (f90-ts--join-line-fill fill-col end-marker
                                          (and (memq broke '(join-prev join-next))
                                               broke))))
      (when joined
        ;; if end-marker landed inside the newly joined line, move it to
        ;; the start of the next line so the while condition steps past cleanly
        (when (< (marker-position end-marker) (line-end-position))
          (set-marker end-marker (line-beginning-position 2))))
      ;; pass on the signal that a join took place (even if
      ;; f90-ts-join-line-next has not joined, but this skips any subsequent
      ;; actions in the main loop like the break step to be performed)
      joined)))


(defun f90-ts--fill-region-break-step (joined fill-col end-marker)
  "Attempt to break the current line if it exceeds the fill column FILL-COL.
JOINED is the result of the join step; if it is non-nil, or the current
line is empty, no break is attempted.  END-MARKER is used to determine
whether the `continue' action should be offered: it is only available
when END-MARKER is exactly at the start of the next line, avoiding
accidental region extension when the boundary is not visible.
Return the result of `f90-ts--break-line-wrap', or nil if skipped."
  (unless (or joined (f90-ts--point-on-empty-line-p))
    (end-of-line)
    (delete-horizontal-space)
    (let ((allow-continue (= (marker-position end-marker)
                             (line-beginning-position 2))))
      (f90-ts--break-line-wrap fill-col allow-continue))))


(defun f90-ts--fill-region-advance ()
  "Advance past the current line and reset the join-marker list.
Called when neither a join nor a break was performed on the current line,
indicating it needed no modification and the loop should move on.
Clears `f90-ts--breakpoint-seed-markers' and moves point to the next line."
  (f90-ts--breakpoint-seed-markers-reset)
  (forward-line 1))


(defun f90-ts--fill-region-aux (beg end fill-col)
  "Fill every line in region BEG..END up to column FILL-COL.
Break overlong lines exceeding FILL-COL and join continued lines or
comments where possible.

For line-based fill operations, an initial break point marker for
interactive mode can be injected via `f90-ts--breakpoint-seed-markers'."
  ;; extend beg/end to canonical line-beginning positions
  (save-excursion
    (goto-char beg)
    (unless (bolp) (setq beg (line-beginning-position))))
  (save-excursion
    (goto-char end)
    (unless (bolp) (setq end (line-beginning-position 2))))

  (when (eq f90-ts-fill-select-breakpoint-by 'interactive)
    (deactivate-mark))

  (save-excursion
    (f90-ts--with-check-modified-region beg end
      (let (end-marker end-overlay)
        (unwind-protect
            (progn
              (setq end-marker (copy-marker end t))
              (when (eq f90-ts-fill-select-breakpoint-by 'interactive)
                (setq end-overlay (make-overlay 1 1))
                (f90-ts--fill-region-update-end-overlay end-overlay end-marker))

              (cl-loop
               ;; note: broke is initialised with nil, it is first accessed
               ;; in the next line, before first evaluated afterwards
               initially (goto-char beg)

               ;; at most one action per loop step: join, break or advance
               for joined = (f90-ts--fill-region-join-step broke end-marker fill-col)
               do (when end-overlay
                    (f90-ts--fill-region-update-end-overlay end-overlay end-marker))

               for broke = (f90-ts--fill-region-break-step joined fill-col end-marker)
               do (when (eq broke 'continue)
                    (f90-ts--fill-region-continue end-marker end-overlay))

               until (eq broke 'abort)
               unless (or joined (memq broke '(broken join-prev join-next)))
               do (f90-ts--fill-region-advance)
               while (or (< (point) end-marker)
                         (memq broke '(join-prev join-next)))))
          ;; clean up overlay and markers
          ;; (end-marker and prior breakpoint markers for join)
          (when end-overlay (delete-overlay end-overlay))
          (f90-ts--breakpoint-seed-markers-reset)
          (set-marker end-marker nil))))))


(defun f90-ts-fill-region ()
  "Fill active region or whole buffer up to `fill-column'."
  (interactive)
  (f90-ts--fill-region-aux
   (if (use-region-p) (region-beginning) (point-min))
   (if (use-region-p) (region-end)       (point-max))
   fill-column))


(defun f90-ts-fill-at-line ()
  "Break and join current line at point according to fill operations.
Joining is done in interactive mode and allows to combine current with
subsequent lines in multiline statements or comment blocks.
In interactive mode, the current point is used as the initial break point
suggestion."
  (interactive)
  (unwind-protect
      (progn
        (when (eq f90-ts-fill-select-breakpoint-by 'interactive)
          (f90-ts--seed-markers-set-pos (point)))
        (f90-ts--fill-region-aux (line-beginning-position)
                                 (line-end-position)
                                 fill-column))
    (f90-ts--breakpoint-seed-markers-reset)))


(defun f90-ts-fill-prev-line ()
  "Fill the current line and the previous line."
  (interactive)
  (let ((beg (save-excursion
               (forward-line -1)
               (line-beginning-position)))
        (end (line-end-position)))
    (f90-ts--fill-region-aux beg end fill-column)))


(defun f90-ts-fill-next-line ()
  "Fill the current line and the next line."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line 1)
               (line-end-position))))
    (f90-ts--fill-region-aux beg end fill-column)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-break-join-fill)

;;; f90-ts-break-join-fill.el ends here
