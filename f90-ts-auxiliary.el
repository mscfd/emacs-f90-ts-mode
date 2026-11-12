;;; f90-ts-auxiliary.el --- Auxiliary functions for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Auxiliary predicates, tree walking and query functions used by
;; `f90-ts-mode'.  This includes helpers for node spans and positions,
;; comment prefix handling, continuation lines and ampersands, sibling
;; and previous-statement lookup, and the node predicates used in thing
;; queries.
;;
;; This file is loaded by `f90-ts-mode' and is not meant to be used on its
;; own.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; defface and defcustom stuff, in particular the comment prefix regexps,
;; `f90-ts-special-comment-rules' and `f90-ts-special-var-regexp'
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)


;;;-----------------------------------------------------------------------------
;;; debug: load f90-ts-log if these are required for development or debugging

(defun f90-ts-log-msg (_category _fmt &rest _args)
  "Logging stub.
Load f90-ts-log.el to enable logging.  This function is replaced by the real
implementation when the logging package is loaded.

Set `f90-ts-allow-log' to allow log instruction as no-op without `f90-ts-log'.
This is used by the Makefile to run ert tests during development."
  (unless (bound-and-true-p f90-ts-allow-log)
    (error "Function f90-ts-log-msg not available: load f90-ts-log.el first")))


(defun f90-ts-log-line (_category _msg &optional _pos-marker _pos)
  "Logging stub.
Load f90-ts-log.el to enable logging.  This function is replaced by the real
implementation when the logging package is loaded.

Set `f90-ts-allow-log' to allow log instruction as no-op without `f90-ts-log'.
This is used by the Makefile to run ert tests during development."
  (unless (bound-and-true-p f90-ts-allow-log)
    (error "Function f90-ts-log-line not available: load f90-ts-log.el first")))


(defun f90-ts-log-inspect-node (_category _node _info)
  "Node inspection stub.
Load f90-ts-log.el to enable logging.  This function is replaced by the real
implementation when the logging package is loaded.

Set `f90-ts-allow-log' to allow log instruction as no-op without `f90-ts-log'.
This is used by the Makefile to run ert tests during development."
  (unless (bound-and-true-p f90-ts-allow-log)
    (error "Function f90-ts-log-inspect-node not available: load f90-ts-log.el first")))


(defun f90-ts-log-indent-print-state (_msg)
  "Debug indent rule stub.
Load f90-ts-log.el to enable logging.  This function is replaced by the real
implementation when the logging package is loaded.

Set `f90-ts-allow-log' to allow log instruction as no-op without `f90-ts-log'.
This is used by the Makefile to run ert tests during development."
  (lambda (_node _parent _bol &rest _)
    (unless (bound-and-true-p f90-ts-allow-log)
      (error "Function f90-ts-log-indent-print-state not available: load f90-ts-log.el first"))
    ;; return nil to ensure that the dummy rule fails (as the real log function also does)
    nil))


;;;-----------------------------------------------------------------------------

(defun f90-ts--node-named-p (node)
  "Check whether NODE is named or anonymous.

Used as additional predicate in thing queries.  Checking with regexp is not
always sufficient, as main structure node and keyword are sometimes equal.
But for thing queries, only the named structure node should match."
  (treesit-node-check node 'named))


(defun f90-ts--node-line (node)
  "Determine line number of start position of NODE."
  (line-number-at-pos (treesit-node-start node)))


(defun f90-ts--node-before-line-p (node line)
  "Return non-nil if NODE is on a line strictly before LINE.
NODE may be nil, in which case the result is nil."
  (and node (< (f90-ts--node-line node) line)))


(defun f90-ts--node-column (node)
  "Determine column number of start position of NODE."
  (f90-ts--column-number-at-pos (treesit-node-start node)))


(defun f90-ts--column-number-at-pos (pos)
  "Compute column at position POS."
  (save-excursion
    (goto-char pos)
    (current-column)))


(defun f90-ts--indentation-at-pos (pos)
  "Compute indentation of line at POS."
  (save-excursion
    (goto-char pos)
    (current-indentation)))


(defun f90-ts--line-number-at-node-or-pos (node)
  "Return line number of NODE or point.
If NODE is non-nil, return line number at which start position is
located, otherwise return line number of current point position."
  (or (and node (f90-ts--node-line node))
      (line-number-at-pos)))


(defun f90-ts--common-prefix-length (str pos)
  "Return length of the common prefix of STR and buffer text at POS."
  (cl-loop
   for i below (length str)
   while (and (< (+ pos i) (point-max))
              (eq (aref str i)
                  (char-after (+ pos i))))
   finally return i))


(defun f90-ts--node-length (node)
  "Return length of text of NODE."
  (- (treesit-node-end node) (treesit-node-start node)))


(defun f90-ts--node-start-trimmed (node)
  "Return the position of the first non-blank character of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (skip-chars-forward " \t\n")
    (point)))


(defun f90-ts--node-end-trimmed (node)
  "Return the position of the last non-blank character of NODE."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-backward " \t\n")
    (point)))


(defun f90-ts--node-span-trimmed (node)
  "Return the trimmed span of NODE as cons cell (START . END).
The trimmed span starts at the first non-blank character
and ends at the last non-blank character of NODE."
  (save-excursion
    (cons (progn
            (goto-char (treesit-node-start node))
            (skip-chars-forward " \t\n\r\f")
            (point))
          (progn
            (goto-char (treesit-node-end node))
            (skip-chars-backward " \t\n\r\f")
            (point)))))


(defun f90-ts--empty-comment-node-p (node)
  "Return non-nil if comment NODE has no content after its prefix."
  (and (f90-ts--node-type-p node "comment")
       (null (f90-ts--comment-content node))))


(defun f90-ts--line-empty-p (pos)
  "Return non-nil if the line at POS is empty or has only blanks."
  (save-excursion
    (goto-char pos)
    (looking-at-p "^[ \t]*$")))


(defun f90-ts--next-line-empty-p (pos)
  "Return non-nil if the next line to POS is empty or has only blanks."
  (save-excursion
    (goto-char pos)
    (and (zerop (forward-line 1))
         (f90-ts--line-empty-p (point)))))


(defun f90-ts--line-empty-comment-p (pos)
  "Return non-nil if the line at POS has an empty comment node."
  (f90-ts--empty-comment-node-p
   (f90-ts--node-at-indent-pos pos)))


(defun f90-ts--next-line-empty-comment-p (pos)
  "Return non-nil if the next line after POS has an empty comment node."
  (save-excursion
    (goto-char pos)
    (when (zerop (forward-line 1))
      (f90-ts--line-empty-comment-p (point)))))


(defun f90-ts--same-line-p (pos1 pos2)
  "Return non-nil if POS1 and POS2 are on the same line.
The implementation using line numbers, which might be expensive
to determine."
  (save-excursion
    (goto-char (min pos1 pos2))
    (<= (max pos1 pos2) (pos-eol))))


(defun f90-ts--region-trimmed ()
  "Return (BEG . END) of trimmed active region.
The trimming removes leading and trailing whitespace."
  (cons (save-excursion
          (goto-char (region-beginning))
          (skip-chars-forward " \t\n")
          (point))
        (save-excursion
          (goto-char (region-end))
          (skip-chars-backward " \t\n")
          (point))))


(defun f90-ts--pos-first-nonspace (pos)
  "Determine position of first non-space character of line at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))


(defun f90-ts--pos-last-nonspace (pos)
  "Determine position right after last non-space character of line at POS."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (skip-chars-backward " \t")
    (point)))


(defun f90-ts--pos-nonspace (pos)
  "Move point to first or last non-space character on current line.
If POS is before the first non-space character, move to it.
If POS is after the last non-space character, move to just after it.
Otherwise return POS."
  (let ((first (f90-ts--pos-first-nonspace pos))
        (last (f90-ts--pos-last-nonspace pos)))
    (cond
     ((< pos first) first)
     ((> pos last)  last)
     (t             pos))))


(defun f90-ts--node-type-p (node type)
  "Compare type of NODE with TYPE.
If TYPE is nil, return true and ignore NODE.
If NODE is nil and TYPE is non-nil, return nil.
If TYPE is a string, return true if NODE is non-nil and is of type TYPE.
If TYPE is a list of strings, return true if NODE is non-nil and its
type is among the elements of TYPE."
  (or (null type)
      (and node
           (let ((type-n (treesit-node-type node)))
             (if (stringp type)
                 (string= type-n type)
               (member type-n type))))))


(defun f90-ts--node-type-match-p (node type-rx)
  "Match type of NODE with TYPE-RX.
If TYPE_RX is nil, return non-nil and ignore NODE.
If NODE is nil and TYPE_RX is non-nil, return nil.
If TYPE-RX and NODE are both non-nil return non-nil if the type of NODE
is matched by TYPE-RX."
  (or (null type-rx)
      (and node
           (let ((type-n (treesit-node-type node)))
             (string-match-p type-rx type-n)))))


(defun f90-ts--nodes-same-span-p (node1 node2)
  "Return non-nil if NODE1 and NODE2 have the same span."
  (and (= (treesit-node-start node1) (treesit-node-start node2))
       (= (treesit-node-end node1)   (treesit-node-end node2))))


(defun f90-ts--node-span-p (node beg end)
  "Return non-nil if NODE has the span BEG..END."
  (and (= beg (treesit-node-start node))
       (= end (treesit-node-end node))))


(defun f90-ts--comment-omp-prefix-regexp ()
  "Return the combined comment and openmp prefix regexp.
The returned regexp also captures trailing blanks, which serves as an assertion
that the prefix is properly separated by a blank.  If accessing the captured
prefix, match group 1 needs to be used.

Note: this regexp cannot be used to match directly in the buffer, as it uses
an anchor to match only at start of a comment.  It is intended to be used on
the text of a comment node."
  ;; first match openmp as comment prefix might just take the initial ! and
  ;; ignore the following $omp part in openmp statements
  (concat "^\\(?:"
          "\\(" f90-ts-openmp-prefix-regexp "\\)\\(?:\\s-\\|$\\)"
          "\\|\\(" f90-ts-comment-prefix-regexp "\\)"
          (when f90-ts-comment-prefix-separator-regexp
            (concat "\\(?:" f90-ts-comment-prefix-separator-regexp "\\|$\\)"))
          ;; if nothing matches, then extract the leading comment starter
          "\\|\\(!\\)"
          "\\)"))


(defun f90-ts--match-comment-omp-prefix (text)
  "Match a comment prefix at the start of TEXT.
Return the prefix in the comment TEXT if matched, nil otherwise.
It uses `f90-ts--comment-omp-prefix-regexp' to capture the prefix."
  (when (string-match (f90-ts--comment-omp-prefix-regexp) text)
    (or (match-string 1 text)
        (match-string 2 text)
        (match-string 3 text))))


(defun f90-ts--comment-omp-prefix-with-blanks (text prefix)
  "Return PREFIX extended by any blanks following it in TEXT.
Used to capture trailing blanks in comment and openmp prefixes."
  (let ((pos (length prefix)))
    (string-match "\\s-*" text pos)
    (substring text 0 (match-end 0))))


(defun f90-ts--comment-prefix (node trim)
  "Extract the starting character sequence from a comment NODE.
NODE is assumed to be of type comment.  It uses `f90-ts-openmp-prefix-regexp'
and `f90-ts-comment-prefix-regexp' to identify the prefix to extract.
If TRIM is `with-blanks', trailing blanks are included in the returned prefix.
If TRIM is `trimmed', only the matched prefix is returned."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "comment node expected")
  (cl-assert (memq trim '(with-blanks trimmed)) nil "argument trim has invalid value, got %s" trim)
  (let* ((text (treesit-node-text node))
         (prefix (f90-ts--match-comment-omp-prefix text)))
    ;; there should always be a prefix, as a single "!" is captured as well
    (cl-assert prefix nil "comment has no prefix")
    (if (eq trim 'with-blanks)
        (f90-ts--comment-omp-prefix-with-blanks text prefix)
      prefix)))


(defun f90-ts--comment-content (node)
  "Extract the content of a comment NODE following the comment prefix.
NODE is assumed to be of type comment.  It uses `f90-ts-openmp-prefix-regexp'
and `f90-ts-comment-prefix-regexp' to identify the prefix to extract.
It always removes blanks after the prefix.  Thus the first character in the
returned content is a non-blank character, or content is nil."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "comment node expected")
  (let* ((text (treesit-node-text node))
         (prefix (f90-ts--match-comment-omp-prefix text)))
    (cl-assert prefix nil "comment has no prefix")
    (let ((pos (length prefix)))
      (string-match "\\s-*" text pos)
      (let ((content-start (match-end 0)))
        (when (< content-start (length text))
          (substring text content-start))))))


(defun f90-ts--comment-forward-prefix (node)
  "Position point at end of comment prefix matched at start of NODE.
NODE is assumed to be of type comment.  It uses `f90-ts-openmp-prefix-regexp'
and `f90-ts-comment-prefix-regexp' to identify the prefix to extract."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "comment node expected")
  ;; first match openmp as comment prefix would just take the initial ! and ignoring
  ;; following $omp part in openmp statements
  (let ((start (treesit-node-start node))
        (text (treesit-node-text node)))
    (if (string-match (f90-ts--comment-omp-prefix-regexp) text)
        (progn
          (goto-char (+ start (match-end 0)))
          (skip-chars-backward " \t"))
      (cl-assert nil nil "failed to match comment prefix")
      ;; skip the leading comment starter
      (goto-char (1+ start)))))


(defun f90-ts--nodes-same-group-p (node1 node2)
  "Return non-nil if NODE1 and NODE2 belong to the same group.

Currently this predicate only groups comment nodes sharing the same comment
prefix.  It is used for indentation and mark region operations."
  ;; even if not belonging to any kind of group, a node is at least grouped with itself,
  ;; this avoids any additional branches in other functions
  (or (treesit-node-eq node1 node2)
      (and (f90-ts--node-type-p node1 "comment")
           (f90-ts--node-type-p node2 "comment")
           (string= (f90-ts--comment-prefix node1 'trimmed)
                    (f90-ts--comment-prefix node2 'trimmed)))))


(defun f90-ts--node-special-var-p (node)
  "Check if NODE is an identifier and matches the special variable regexp.
Note that the parse uses identifier not just for variables, but for types etc."
  (when (f90-ts--node-type-p node "identifier")
    ;; we do not prepend or append symbol start or end assertions, as it should also
    ;; work with more general regexps (like highlight all variables with a certain prefix)
    (string-match-p (concat "^"
                            f90-ts-special-var-regexp
                            "$")
                    (treesit-node-text node))))


(defun f90-ts--node-openmp-p (node)
  "Check if NODE is a comment node and has a OpenMP comment prefix."
  (when (f90-ts--node-type-p node "comment")
    (string-match-p (concat "^" f90-ts-openmp-prefix-regexp "\\(\\s-+\\|$\\)")
                    (treesit-node-text node))))


(defun f90-ts--node-line-leading-p (node)
  "Check whether NODE is the first node on its source line.
It checks whether there are any non-blank characters between start of line
and start of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (skip-chars-backward " \t")
    (bolp)))


(defun f90-ts--node-leading-real-ampersand-p (node)
  "Check whether NODE is a non-virtual ampersand at the start of its line.
Note that leading ampersand are optional.  If missing, the grammar injects
a virtual zero-length ampersand token."
  (and (f90-ts--node-type-p node "&")
       (= 1 (f90-ts--node-length node))
       (f90-ts--node-line-leading-p node)))


(defun f90-ts--node-block-label-ancestor (node)
  "Return block_label_start_expression ancestor of leaf NODE if there is one.
NODE is assumed to be the leaf node at the start of a line.  The function
checks whether it is part of a \"block_label_start_expression\"."
  ;; Unfortunately, this comes in two time two variants. First, a label name,
  ;; which is a language keyword, parses slightly differently. Second, the
  ;; grammar added a named node to capture the block_label itself.
  ;; Old grammar (label is an anonymous node):
  ;;
  ;;   (block_label_start_expression 'label' :)
  ;;   (block_label_start_expression 'label' keyword :)
  ;;
  ;; New grammar (label is the named node block_label):
  ;;
  ;;   (block_label_start_expression (block_label) :)
  ;;   (block_label_start_expression (block_label keyword) :)
  ;;
  ;; Both variants are handled by `f90-ts--block-label-types'.  The function
  ;; returns \"block_label_start_expression\", or nil.
  (cl-assert (or (not node)
                 (zerop (treesit-node-child-count node nil)))
             nil "node is not a leaf node: %s" node)
  (let* ((parent (and node (treesit-node-parent node)))
         (grandparent (and parent (treesit-node-parent parent)))
         (block-label-types '("label" "block_label")))
    (cond
     ((and (f90-ts--node-type-p node block-label-types)
           (f90-ts--node-type-p parent "block_label_start_expression"))
      parent)
     ((and (f90-ts--node-type-p parent block-label-types)
           (f90-ts--node-type-p grandparent "block_label_start_expression"))
      grandparent)
     (t nil))))


(defun f90-ts--node-preproc-p (node)
  "Check if NODE is a preprocessor directive node.
This is done by checking whether the node text starts with a \"#\".
For indentation, only these directive nodes are relevant.
This function should cover both anonymous leaf nodes as well as named nodes.
Hence checking the type of the node works only partially."
    (let ((start (treesit-node-start node)))
      ;; only fetch the first character instead of the whole node text
      (eq (char-after start) ?#)))


(defconst f90-ts--builtin-functions
  '(;; integer/real
    "abs" "aimag" "aint" "anint" "ceiling" "conjg" "dble" "dim" "dprod"
    "floor" "hypot" "max" "min" "mod" "modulo" "nearest" "nint" "real"
    "rrspacing" "scale" "sign" "spacing"
    ;; exponential and logarithmic
    "exp" "log" "log10" "log_gamma" "gamma" "sqrt"
    ;; trigonometric (radians)
    "acos" "asin" "atan" "atan2" "cos" "sin" "tan"
    ;; trigonometric (degrees)
    "acosd" "asind" "atand" "atan2d" "cosd" "sind" "tand"
    ;; trigonometric (half-revolutions)
    "acospi" "asinpi" "atanpi" "atan2pi" "cospi" "sinpi" "tanpi"
    ;; hyperbolic
    "acosh" "asinh" "atanh" "cosh" "sinh" "tanh"
    ;; error functions
    "erf" "erfc" "erfc_scaled"
    ;; bessel functions
    "bessel_j0" "bessel_j1" "bessel_jn"
    "bessel_y0" "bessel_y1" "bessel_yn"
    ;; Floating-point model inquiry
    "digits" "epsilon" "exponent" "fraction" "huge" "maxexponent"
    "minexponent" "precision" "radix" "range" "rrspacing" "tiny"
    ;; numeric type conversion
    "cmplx" "int" "logical" "transfer"
    ;; bit operations
    "btest" "iand" "ibclr" "ibits" "ibset" "ieor" "ior"
    "ishft" "ishftc" "leadz" "maskl" "maskr" "merge_bits"
    "mvbits" "not" "popcnt" "poppar" "shifta" "shiftl" "shiftr"
    "trailz" "bit_size"
    ;; array inquiry
    "allocated" "is_contiguous" "lbound" "rank" "shape" "size" "ubound"
    ;; array construction, manipulation and reduction
    "all" "any" "count" "cshift" "dot_product" "eoshift" "iall" "iany"
    "iparity" "matmul" "maxloc" "maxval" "merge" "minloc" "minval" "norm2"
    "pack" "parity" "product" "reduce" "reshape" "spread" "sum"
    "transpose" "unpack"
    ;; array location
    "findloc" "maxloc" "minloc"
    ;; character
    "achar" "char" "iachar" "ichar" "index" "len" "len_trim"
    "new_line" "repeat" "scan" "trim" "verify"
    ;; kind and type inquiry
    "kind" "selected_char_kind" "selected_int_kind"
    "selected_logical_kind" "selected_real_kind" "storage_size"
    "out_of_range"
    ;; type inquiry
    "extends_type_of" "same_type_as"
    ;; pointer and allocation
    "associated" "move_alloc" "null"
    ;; coarray
    "event_post" "event_query" "event_wait"
    "failed_images" "get_team" "image_index" "image_status" "lcobound"
    "num_images" "stopped_images" "team_number" "this_image" "ucobound"
    "co_broadcast" "co_max" "co_min" "co_reduce" "co_sum"
    ;; coarray atomic subroutines
    "atomic_add" "atomic_and" "atomic_cas" "atomic_define"
    "atomic_fetch_add" "atomic_fetch_and" "atomic_fetch_or"
    "atomic_fetch_xor" "atomic_or" "atomic_ref" "atomic_xor"
    ;; random numbers
    "random_init" "random_number" "random_seed"
    ;; system and environment
    "command_argument_count" "cpu_time" "date_and_time"
    "get_command" "get_command_argument" "get_environment_variable"
    "is_iostat_end" "is_iostat_eor" "system_clock"
    ;; miscellaneous
    "present" )
  "List of all builtin keywords for font-lock highlighting.
Used with face `font-lock-builtin-face'.")


(defun f90-ts--builtin-function-p (node)
  "Return non-nil if NODE represents a builtin function.
The function assumes that NODE is an identifier and only checks the text of the
node."
;; Remark: the regexp engine invoked by tree-sitter query command :match is
;; case-sensitive, but the emacs regexp engine itself is case-insensitive.
;; So plugging (:match ,(regexp-opt f90-ts--builtin-functions 'symbols) ...)
;; into the font lock rule, as was originally done, does not work if the
;; function name in the node contains some uppercase letters.
  (cl-assert (f90-ts--node-type-p node "identifier")
             nil "builtin-function-p: identifier expected")
  (let ((text (treesit-node-text node))
        (rx (regexp-opt f90-ts--builtin-functions 'symbols)))
    (string-match-p rx text)))


;; this works for both old and new string_literal parsing rules
(defun f90-ts--node-string (node)
  "Return the string_literal associated to NODE or nil.
The associated string_literal is NODE itself or a parent of this type."
  ;; only ascend if node type is possible within a string
  (and (f90-ts--node-type-p node '("string_literal"
                                   "string_content"
                                   "comment"
                                   "&"
                                   "\""
                                   "\\"))
       (treesit-parent-until
        node
        (lambda (n) (f90-ts--node-type-p n "string_literal"))
        t)))


;; this works for both old and new string_literal parsing rules
(defun f90-ts--in-string-p (pos)
  "Non-nil if POS is inside a string."
  (when-let* ((node (treesit-node-at pos))
              (string-node (f90-ts--node-string node)))
    (let ((start (treesit-node-start string-node))
          (end   (treesit-node-end   string-node)))
      ;; start/end are the opening/closing quotation characters
      (and (< start pos) (< pos end)))))


(defun f90-ts--in-openmp-p (pos)
  "Non-nil if POS is inside an OpenMP statement.
OpenMP statements are parsed as comment nodes, but always start with !$.
The grammar does not parse OpenMP currently."
  (when-let* ((node (treesit-node-at pos)))
    (when (f90-ts--node-openmp-p node)
      (let ((start (treesit-node-start node)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts--in-comment-p (pos)
  "Non-nil if POS is after start of comment.
This excludes OpenMP statements, which look like comments and are currently not
parsed by the treesitter grammar."
  (when-let* ((node (treesit-node-at pos)))
    (when (and (f90-ts--node-type-p node "comment")
               (not (f90-ts--node-openmp-p node)))
      (let ((start (treesit-node-start node)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts--bol-to-point-blank-p ()
  "Return non-nil if only blank characters exist between BOL and point.
Note that in fortran, a continuation symbol shall not be used on blank lines."
  (looking-back "^\\s-*" (line-beginning-position)))


(defun f90-ts--point-on-empty-line-p ()
  "Return non-nil if point is on an empty line, containing at most blanks."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))


(defun f90-ts--node-not-number-literal-p (node)
  "Return non-nil if NODE is not a number_literal.

This predicate is necessary, as the :pred in queries does not seem to
work with lambda expressions."
  (not (string= (treesit-node-type node) "number_literal")))


(defun f90-ts--node-ampersand-p (node)
  "Check whether NODE is continuation symbol &."
  (f90-ts--node-type-p node "&"))


(defun f90-ts--node-not-comment-p (node)
  "Return non-nil if NODE is not of type comment."
  (not (f90-ts--node-type-p node "comment")))


(defun f90-ts--node-not-error-p (node)
  "Return non-nil if NODE is not of type error."
  (not (f90-ts--node-type-p node "ERROR")))


(defun f90-ts--node-not-comment-or-preproc-p (node)
  "Return non-nil if NODE is not of type comment or a preproc NODE."
  (and (f90-ts--node-not-comment-p node)
       (not (f90-ts--node-preproc-p node))))


(defun f90-ts--node-not-comment-error-preproc-p (node)
  "Return non-nil if NODE is not of type comment, error or preproc."
  (and (f90-ts--node-not-comment-p node)
       (f90-ts--node-not-error-p node)
       (not (f90-ts--node-preproc-p node))))


(defconst f90-ts--node-op-expr-types
  '("logical_expression"
    "math_expression"
    "relational_expression"
    "concatenation_expression"
    "unary_expression")
  "Operator expression types for alignment purposes.
These are required to have a field named operator.  Note that logical and math
expression overlap, as user defined operators are always interpreted as math
expression, even if operating on booleans (for example: .imply.  operator).")


(defun f90-ts--node-op-expr-p (node)
  "Return non-nil if NODE is of some expression type."
  (member (treesit-node-type node)
          f90-ts--node-op-expr-types))


(defun f90-ts--comment-matching-rule (node)
  "Return the first rule in `f90-ts-special-comment-rules' which matches.
A rule matches if text of NODE matches the regexp or the predicate of the rule.
NODE is assumed to be of type comment."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "comment-matching-rule: comment node expected")
  (let ((text (treesit-node-text node t)))
    (seq-find
     (lambda (rule)
       (let ((match (plist-get rule :match)))
         (cond
          ((stringp match)   (string-match-p match text))
          ((functionp match) (funcall match node)))))
     f90-ts-special-comment-rules)))


(defun f90-ts--comments-same-rule-p (node1 node2)
  "Return non-nil if comments NODE1 and NODE2 match the same special rule.
Comments matching no rule never match in this predicate."
  (when-let* ((rule1 (f90-ts--comment-matching-rule node1))
              (rule2 (f90-ts--comment-matching-rule node2)))
    (eq rule1 rule2)))


(defun f90-ts--comments-match-p (node1 node2)
  "Return non-nil if kind of comment NODE1 and comment NODE2 match.
A special rule of `f90-ts-special-comment-rules' is the first criterion:
if either comment matches a rule, both must match the same rule.
Comments matching no rule belong together if they share the same prefix,
see `f90-ts--nodes-same-group-p'.

Note that special comment rules might allow different prefixes (like matching
both \"!<\" and \"!>\" for documentation text).
On the other hand different rules never match, even if the prefixes
agree, (for example `!$omp' versus `!$omp simd' can be different rules with
different indentation etc.)."
  (let ((rule1 (f90-ts--comment-matching-rule node1))
        (rule2 (f90-ts--comment-matching-rule node2)))
    (if (or rule1 rule2)
        (eq rule1 rule2)
      (f90-ts--nodes-same-group-p node1 node2))))


(defun f90-ts--node-on-pos (pos side &optional named)
  "Return the smallest node covering position POS.
If NAMED is non-nil, consider only named nodes, otherwise include anonymous
nodes as well.

Function `treesit-node-on' works on half-open regions.
It returns a node spanning the half-open region [beg, end).
If POS is at end position of node, then the region [POS,POS) is empty and
`treesit-node-on' returns some parent node properly including POS.
Also note that if POS is not within the span of the root node, then
`treesit-node-on' returns the root node, even so it does not cover POS.

In contrast to `treesit-node-on', this function uses the closed interval logic
and looks for nodes really covering [POS,POS].  If there is more than one such
node (one ending at POS, another starting at POS), SIDE is is used to resolve
the ambiguity.  SIDE is either `left' or `right'.  This selects the node which
ends at POS and starts at POS, respectively.

Example
subroutine sub()
   if (cond) then
   end if|
end subroutine sub

If point is at |, then the smallest named node is the end_statement
for \"end if\".  However, treesit-node-on returns the subroutine node.
Querying with [POS-1,POS) gives the expected answer."
  (cl-assert (member side '(left right))
             nil "invalid argument side, got %s" side)
  (let* ((right-on (treesit-node-on pos pos nil named))
         (left-on  (and (> pos (point-min))
                        (treesit-node-on (1- pos) pos nil named)))
         ;; filter nodes which do not cover POS
         (right (when (and right-on
                           (<= (treesit-node-start right-on) pos)
                           (<= pos (treesit-node-end right-on)))
                  right-on))
         (left (when (and left-on
                          (<= (treesit-node-start left-on) pos)
                          (<= pos (treesit-node-end left-on)))
                 left-on)))
    (cond
     ;; only one candidate
     ((null left) right)
     ((null right) left)

     ;; ambiguity case
     ((and (= (treesit-node-end left) pos)
           (= pos (treesit-node-start right)))
      (if (eq side 'left) left right))

     ;; return node with smaller span (which should be a descendant of the large node)
     (t
      (let ((len-left (- (treesit-node-end left)
                         (treesit-node-start left)))
            (len-right (- (treesit-node-end right)
                          (treesit-node-start right))))
        (if (< len-left len-right) left right))))))


(defun f90-ts--smallest-node-same-span (node)
  "Find the smallest named descendant of NODE which spans the same region."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (sparse (treesit-induce-sparse-tree
                  node
                  (lambda (n) (and (treesit-node-check n 'named)
                                   (f90-ts--node-span-p n beg end))))))
    ;; get the first leaf, descend the first child until a leaf is reached
    (cl-loop for current = sparse then (car (cdr current))
             while (cdr current)
             finally return (car current))))


(defun f90-ts--largest-node-same-span (node)
  "Find the largest ancestor of NODE which spans the same region.
Example: for (contains_statement \"contains\") `treesit-node-on' returns
\"contains\", but for navigation, we usually want the largest node with
same bounds, which is the contains_statement node."
  (treesit-parent-while
   node
   (lambda (n)
     (f90-ts--nodes-same-span-p n node))))


(defun f90-ts--siblings-between (node1 node2 &optional named)
  "Return sibling between NODE1 and NODE2, including NODE1 and excluding NODE2.
It is assumed that node1 and node2 are sibling (have the same parent).
If NAMED is non-nil, return only named nodes."
  (cl-assert (treesit-node-eq (treesit-node-parent node1)
                              (treesit-node-parent node2))
             nil
             "node1 and node2 are not siblings, node1=%s, node2=%s" node1 node2)

  (let ((parent (treesit-node-parent node1))
        (ix1 (treesit-node-index node1))
        (ix2 (treesit-node-index node2)))
    (cl-loop
     for i from ix1 below ix2
     for sib = (treesit-node-child parent i)
     when (or (not named)
              (treesit-node-check sib 'named))
     collect sib)))


(defun f90-ts--sibling-predicate (node sibling-fn predicate)
  "Find sibling of NODE satisfying PREDICATE using SIBLING-FN to walk siblings.
Repeat SIBLING-FN until a suitable sibling is found,
or return nil."
  (cl-loop
   for sibling = (funcall sibling-fn node)
            then (funcall sibling-fn sibling)
   while sibling
   when (funcall predicate sibling)
   return sibling))


(defun f90-ts--prev-sibling-proper (node)
  "Determine previous \"proper\" sibling of NODE.
Nodes of type comments and ampersand are not considered \"proper\"."
  (f90-ts--sibling-predicate
   node
   #'treesit-node-prev-sibling
   (lambda (n)
     (not (f90-ts--node-type-p n '("comment" "&"))))))


(defun f90-ts--next-sibling-proper (node)
  "Determine next \"proper\" sibling of NODE.
Nodes of type comments and ampersand are not considered \"proper\"."
  (f90-ts--sibling-predicate
   node
   #'treesit-node-next-sibling
   (lambda (n)
     (not (f90-ts--node-type-p n '("comment" "&"))))))


(defun f90-ts--nodes-on-prev-lines (nodes cur-line &optional predicate)
  "Filter NODES by PREDICATE and line number.
Accept only NODES which are on a line prior to CUR-LINE.
If PREDICATE is provided, additionally filter by predicate.
For empty NODES, return an empty list."
  (seq-filter
   (lambda (node)
     (and (or (not predicate)
              (funcall predicate node))
          (< (f90-ts--node-line node)
             cur-line)))
   nodes))


(defun f90-ts--first-node-on-line (pos)
"Return the first node on the line at POS, or nil if the line is empty.
If the line is part of a continued line, it starts with a real or a virtual
zero-width ampersand.  Return this ampersand as first node if present.

`treesit-node-on' for (point) (point) usually returns the virtual
ampersand directly.  Walking back over previous siblings starting at the
same position is still necessary in one situation:
Several zero-width nodes start at the same position, for example after a
trailing comma in a call, with an incomplete argument list, like:

      call foo(x, &
               y, &
               )

Error recovery inserts a zero-width `assumed_size' node (with a MISSING
\"*\") between the virtual ampersand and the closing parenthesis.
`treesit-node-on' then returns the parenthesis, and the ampersand is
reached only by walking back via `assumed_size'."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (let* ((line (line-number-at-pos))
           (on-node (treesit-node-on (point) (point)))
           (at-node (treesit-node-at (point))))
      (cond
       ((and on-node
             (= (point) (treesit-node-start on-node)))
        ;; usually, (treesit-node-on (point) (point)) already returns the
        ;; virtual ampersand if present, but in some cases, there might be
        ;; other virtual nodes, or treesit-node-on returns a proper node
        ;; starting at (point), walk back as long as start of node is at point
        (cl-loop
         for node = on-node
         then (treesit-node-prev-sibling node)
         while (and node
                    (= (treesit-node-start node) (point)))
         for last = node ; updates only if while condition is true
         finally return last))

       ((and at-node
             (= line (f90-ts--node-line at-node)))
        at-node)))))


(defun f90-ts--last-node-on-line (pos)
  "Go to end of line of POS and obtain the node at this end of line position."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (skip-chars-backward " \t")
    (unless (bolp)
      (when-let* ((node (treesit-node-at (point))))
        (when (= (line-number-at-pos pos)
                 (f90-ts--node-line node))
          node)))))


(defun f90-ts--skip-comments-to-ampersand (node sibling-fn)
  "Return final ampersand after a sequence of comment nodes.
Walk from NODE via SIBLING-FN across comment nodes, and return
the ampersand node reached once comments end.  Return nil if NODE
is nil, not a comment, or if the node reached after skipping comments
is not an ampersand."
  (cl-loop
   for n = node then (funcall sibling-fn n)
   while (f90-ts--node-type-p n "comment")
   finally return (and (f90-ts--node-ampersand-p n) n)))


(defun f90-ts--find-ampersand-boundary (node sibling-fn)
  "Find the ampersand at the far end of an &-comment*-& sequence.

Walking siblings is done via SIBLING-FN, which determines the direction of
the walk \(`treesit-node-prev-sibling' or `treesit-node-next-sibling'\).

If NODE is an ampersand or a comment that is part of such a sequence, return
the ampersand at the far end of it.  Otherwise, including if NODE is
of any other type, return nil."
  (cond
   ((f90-ts--node-ampersand-p node)
    ;; try to skip subsequent comments, otherwise return node,
    ;; which is already the boundary ampersand
    (or (f90-ts--skip-comments-to-ampersand
         (funcall sibling-fn node) sibling-fn)
        node))
   ((f90-ts--node-type-p node "comment")
    (f90-ts--skip-comments-to-ampersand node sibling-fn))
   (t nil)))


(defun f90-ts--sibling-skip-continuation (node sibling-fn)
  "Return NODE's sibling, skipping a &-comment*-& sequence if present.

Walking siblings is done via SIBLING-FN, which determines the direction of
the walk \(`treesit-node-prev-sibling' or `treesit-node-next-sibling'\).

If sibling of NODE is an ampersand or comment that is part of such a sequence,
return the node directly following the sequence's second ampersand (nil if
there is no such sibling).  Otherwise return the sibling of NODE itself."
  (when-let* ((nsib (funcall sibling-fn node)))
    (if-let* ((amp2 (f90-ts--find-ampersand-boundary
                     nsib sibling-fn)))
        (funcall sibling-fn amp2)
      nsib)))


(defun f90-ts--skip-continuation-forward (node)
  "Return NODE's next sibling, skipping a &-comment*-& sequence if present.
See `f90-ts--sibling-skip-continuation'."
  (f90-ts--sibling-skip-continuation node #'treesit-node-next-sibling))


(defun f90-ts--find-first-ampersand (node)
  "Find the first ampersand if at a &-comment*-& sequence.
In continued lines, continuation symbol ampersands appears in
sequences like &, (comment)*, &.  This routines checks whether NODE
is any of those ampersand or comment nodes, and returns the first
ampersand node of this sequence."
  (f90-ts--find-ampersand-boundary node #'treesit-node-prev-sibling))


(defun f90-ts--first-node-of-stmt (node)
  "Return the first node of the statement at which NODE is placed.
Use `f90-ts--first-node-on-line', check for continuation symbol and
if present, further go back, skipping comments and empty lines until
beginning of statement is found."
  (cl-loop
   for namp = node then next-namp
   for first = (progn
                 (f90-ts--first-node-on-line
                  (treesit-node-start namp)))
   for next-namp = (f90-ts--find-first-ampersand first)
   while next-namp
   finally return first))


(defun f90-ts--previous-stmt-first-line (node line-num)
  "Return previous statement determined by NODE.
First search for any reasonable (leaf) node, which is before line with
line number LINE-NUM, and then go to start of line.  If on a continued line,
follow it to the start of the statement.
If current point is within a continued line, then previous leaf node
belongs to the continued line as well, and previous-stmt return the
node at the start of the continued line.

In order to find the most previous leaf node start at node and ascend
the tree until a previous sibling on a previous line can be found.  Just
taking a sibling of node is not possible, as node might be nil (empty
line), or node is part of an expression tree with deep nesting or
similar.  We really want to go to previous line with a proper node on
it.  Once we have a proper node, descend the previous sibling to further
narrow it down among its children.
Finally return the leaf node at the start of the line.  Follow continued
lines to first line of continued statement.

Ignore nodes which do not satisfy the predicate
`f90-ts--node-not-comment-error-preproc-p' during ascend or descend.
For ascend, it suffices to ignore errors, but for descend we also need to
exclude comment and preprocessor nodes in `f90-ts--before-child'."
  (let* ((predicate #'f90-ts--node-not-comment-error-preproc-p)
         ;; ascend until a previous ancestor is found
         (prev-sib-of-anc
          (cl-loop
           for ancestor = node then (treesit-node-parent ancestor)
           while ancestor
           for relative = (f90-ts--before-child ancestor line-num predicate)
           when relative return relative))
         ;; descend prev-sib-of-anc to find the deepest node still before current line
         (prev-descend
          (and prev-sib-of-anc
               (cl-loop
                for sib = prev-sib-of-anc then next
                for next = (and (> (treesit-node-child-count sib) 0)
                                (f90-ts--before-child sib line-num predicate))
                ;; if there is a next, continue and shift next to sib
                ;; with "for sib=next" in the first cl-loop line
                while next
                finally return sib)))
         ;; take continuation lines into account and go to beginning of statement
         (first (and prev-descend
                     (f90-ts--first-node-of-stmt prev-descend))))
    ;; check for statement_label node and skip if possible
    (when first
      (if (not (f90-ts--node-type-p first "statement_label"))
          ;; standard case, no statement label present
          first
        ;; first should be the unnamed node in (statement_label "statement_label")
        ;; we need the parent, first ensure that this assumption is correct
        (cl-assert (f90-ts--node-type-p (treesit-node-parent first) "statement_label")
                   nil "unexpected tree structure at leaf node of type statement_label")
        (let ((next (treesit-node-next-sibling
                     (treesit-node-parent first))))
          (if (and next
                   (= (f90-ts--node-line first)
                      (f90-ts--node-line next)))
              ;; retrieve the leaf node starting at next
              (treesit-node-at (treesit-node-start next))
            first))))))


(defun f90-ts--previous-stmt-first (node parent)
  "Return previous non-preproc statement determined by NODE and PARENT.
It uses `f90-ts--previous-stmt-first' to step back statement by statement,
until a non-preprocessor statement is found.

It starts at NODE, and if it is nil, it uses PARENT.  The function is used by
the indent engine, which does not have a NODE but always has a PARENT (for
example on empty lines)."
  (cl-loop
   for ancestor = (or node parent) then pstmt-1
   for cur-line =    (f90-ts--line-number-at-node-or-pos node)
   then (f90-ts--node-line pstmt-1)
   for pstmt-1 = (f90-ts--previous-stmt-first-line ancestor cur-line)
   while pstmt-1
   when (not (f90-ts--node-preproc-p pstmt-1)) return pstmt-1))


(defun f90-ts--previous-stmt-keyword-by-first (first)
  "Return keyword of previous statement.
The returned leaf node is usually some keyword like \"if\", \"elseif\", \"do\".
In case of a block label the first leaf node FIRST is the label, not the
keyword.  For use as anchor, the label is required.  For use as matcher, the
keyword is relevant."
  ;; if the statement starts with a block label, then first is unnamed
  ;; node label, and its parent is block_label_start_expression. Its
  ;; next sibling is a keyword like if or do (or some continuation line bustle)
  (if-let* ((block-label (f90-ts--node-block-label-ancestor first))
	        (next (f90-ts--skip-continuation-forward block-label)))
      (cl-loop
       for n = next then child
       for child = (treesit-node-child n 0)
       while child
       finally return n)
    ;; not a label expression, just return first
    first))


(defun f90-ts--before-child (node line predicate)
  "Return child of NODE, which is on a previous LINE and satisfies PREDICATE.
Take the last of all children satisfying this condition."
  (when-let* ((children (treesit-node-children node))
              (children-prev (f90-ts--nodes-on-prev-lines children line predicate)))
    (car (last children-prev))))


(defun f90-ts--prev-sib-by-parent (parent)
  "Previous sibling based on position of point and PARENT.
Especially for an empty line, it often happens that node=nil, but parent is
some relevant node, whose children, which are kind of siblings to
nil-node-position, can be used to determine things like indentation.
If the sibling is an ERROR node, then descend (always the first sibling) to
find a relevant structure type, like subroutine_statement or similar.
Usually only one step is required to skip the ERROR node.

Comment and preprocessor nodes are ignored as previous siblings."
  (let* ((cur-line (line-number-at-pos))
         (psib (f90-ts--before-child parent cur-line
                                     #'f90-ts--node-not-comment-or-preproc-p)))
    ;; if psib=nil, just return nil
    ;; if psib=ERROR node, descend and try to find some non-error node
    (cl-loop
     for current = psib then child
     for child = (and current (treesit-node-child current 0 t))
     while (and child
                (f90-ts--node-type-p current "ERROR"))
     finally return current)))


(defun f90-ts--line-continued-at-end-p (last pos)
  "Check whether line at POS is continued.
It usually ends in &, but might be followed by a comment.  First check that
there is a next line after current line.
LAST is expected to be the last node on the line, and can be obtained
by `f90-ts--last-node-on-line'"
  ;; if there is an ampersand (or ampersand (comment)) at end of line but
  ;; no other sibling follows, we are probably at end of file
  (when (treesit-node-next-sibling last)
    (cond
     ((f90-ts--node-ampersand-p last)
      t)

     ((f90-ts--node-type-p last "comment")
      ;; if last comment is node, check whether previous one is an ampersand,
      ;; but it must be on the same line
      (let ((prev (treesit-node-prev-sibling last)))
        (and prev
             (f90-ts--node-ampersand-p prev)
             (= (line-number-at-pos pos)
                (f90-ts--node-line prev)))))

     (t
      ;; in all other cases, we are not on a continued line
      nil))))


(defun f90-ts--first-line-of-continued-stmt-p (pos)
  "Check whether POS is on the first line of a continued statement.
To this end, check that it does not start with and ampersand, but
is continued at end."
  (when-let* ((first-node (f90-ts--first-node-on-line pos))
              (last-node (f90-ts--last-node-on-line pos)))
    (and (not (f90-ts--node-type-p first-node "&"))
         (f90-ts--line-continued-at-end-p last-node pos))))


(defun f90-ts--subsequent-line-of-continued-stmt-p (pos)
  "Check whether line at POS is within a subsequent continued line.
Return nil for the first line of a continued statement or if the
statement is not continued.
Such a subsequent line either has a node \"&\" at start of line, is a comment,
or an empty line. If it is an empty line, it is necessary to look backwards and
forwards and check, whether an ampersand can be found."
  ;; if line is empty, n-first is nil and n-next is on some subsequent
  ;; line; if the line is after some continuation ampersand
  ;; then n-start is either a comment or the second ampersand,
  ;; from which we can go backwards
  (let* ((first-node (f90-ts--first-node-on-line pos))
         (next-node (treesit-node-at pos))
         (start-node (or first-node next-node)))
    (when start-node
      (f90-ts--find-first-ampersand start-node))))


(defun f90-ts--pos-within-continued-stmt-p (pos)
  "Check whether POS is on some line of a continued statement.
This needs to check forward or backward, as first and last line
must also match."
  (or (f90-ts--subsequent-line-of-continued-stmt-p pos)
      (when-let* ((last (f90-ts--last-node-on-line pos)))
        (f90-ts--line-continued-at-end-p last pos))))


(defun f90-ts--parent-no-preproc (parent)
  "Ascend until a non-preproc ancestor PARENT is found.
If PARENT is a normal node, then return PARENT."
  (treesit-parent-until parent
                        (lambda (n) (not (f90-ts--node-type-match-p n "preproc.*")))
                        t))


(defun f90-ts--grandparent-no-preproc (parent)
  "Ascend until a non-preproc ancestor PARENT is found.
If PARENT is a normal node, then return PARENT."
  (when-let* ((parent-nopp (f90-ts--parent-no-preproc parent))
              (gp (treesit-node-parent parent-nopp)))
    (f90-ts--parent-no-preproc gp)))


(defun f90-ts--indent-pos-at-node (node)
  "Determine indentation position of line where start of NODE is located.
Besides blanks, skip leading ampersands as well.
This does not anchor at NODE, just at indentation of first statement node
on the line.

Note: ampersands and statement_label's are removed for indented lines, but
node might not be on such a line (for line indentation of region indention
with relevant nodes outside of marked region), thus we need to actively skip
them.

Note: the determined position can be used as anchor in indentation even within
`treesit-indent-region', which uses buffering of computed (anchor offset).

TODO: besides ampersands also skip statement_label's."
  (save-excursion
    (goto-char (treesit-node-start node))
    (beginning-of-line)
    (skip-chars-forward "& \t")
    (point)))


(defun f90-ts--node-at-indent-pos (pos)
  "Determine node at indentation position of line determined by POS.
If the line is empty, return nil."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (let ((node (treesit-node-at (point))))
      (when (and node
                 (= (point) (treesit-node-start node)))
        node))))


;;;-----------------------------------------------------------------------------
;;; Imenu and navigation queries

(defconst f90-ts--nav-queries
  `(("program"
     :label   "program"
     :capture name_program
     :query   "(program (program_statement \"program\" (name) @name_program))")

    ("module"
     :label   "module"
     :capture name_module
     :query   "(module (module_statement \"module\" (name) @name_module))")

    ("submodule"
     :label   "submodule"
     :capture name_submodule
     :query   "(submodule (submodule_statement \"submodule\" (name) @name_submodule))")

    ("subroutine"
     :label   "subroutine"
     :capture name_subroutine
     :query   "(subroutine (subroutine_statement \"subroutine\" name: (name) @name_subroutine))")

    ("function"
     :label   "function"
     :capture name_function
     :query   "(function (function_statement \"function\" name: (name) @name_function))")

    ("module_procedure"
     :label   "module procedure"
     :capture name_module_proc
     :query   "(module_procedure (module_procedure_statement \"module\" \"procedure\" name: (name) @name_module_proc))")

    ("derived_type_definition"
     :label   "derived type"
     :capture name_dt_type
     :query   "(derived_type_definition (derived_type_statement \"type\" (_) * (type_name) @name_dt_type))")

    ("interface"
     :label   "interface"
     :capture name_interface
     :query   ,(concat "(interface (interface_statement (abstract_specifier)?"
                       " \"interface\""
                       " [((name) @name_interface)"
                       "  ((operator) @name_interface)"
                       "  ((assignment) @name_interface)]?"
                       "))"))

    ("variable_declaration"
     :label   "variable"
     :capture name_var_decl
     :query   ,(concat "(variable_declaration"
                       " declarator:"
                       " [((identifier) @name_var_decl)"
                       "  (init_declarator left: (identifier) @name_var_decl)"
                       "  (pointer_init_declarator left: (identifier) @name_var_decl)"
                       "  (sized_declarator (identifier) @name_var_decl)])")
     :leaf t))
  "Query specification and properties for Imenu and the TOC side panel buffer.
Each entry is a list (KEY PLIST) where KEY is a string returned by the related
Tree-sitter node type, and PLIST may contain:
  :label   Display string used in Imenu and menus.
  :capture Symbol matching the Tree-sitter capture name for the node name.
  :query   Tree-sitter query string capturing the relevant node name.
  :leaf    Non-nil if entries should be treated as leaf nodes (no children).")


(defconst f90-ts--nav-capture-key-alist
  (cl-loop
   for (key . plist) in f90-ts--nav-queries
   collect (cons (plist-get plist :capture) key))
  "Alist mapping Tree-sitter capture symbols to query-key.
Each entry is (CAPTURE-SYMBOL . KEY) where KEY is the
car of the corresponding entry in `f90-ts--nav-queries'.")


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-auxiliary)

;;; f90-ts-auxiliary.el ends here
