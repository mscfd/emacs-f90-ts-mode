;;; f90-ts-mode.el --- Tree-sitter based Fortran 90 mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein
;; Maintainer: Martin Stein
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

;; f90-ts-mode is a major mode for editing Fortran 90/2003 (and newer) source
;; files, based on Emacs's built-in tree-sitter support (requires Emacs 30,
;; but might run with Emacs 29 as well).
;;
;; Features:
;;   - Syntax highlighting
;;   - Indentation
;;   - Alignment for multiline statements where applicable
;;   - Smart end completion
;;   - Break and join continued lines
;;   - Comment region (un)commenting with configurable prefixes
;;   - OpenMP and preprocessor directive handling
;;   - Region selection based on tree-sitter nodes
;;
;; Note: feature might only be partially implemented.
;;
;; Installation requires the tree-sitter Fortran grammar.  It might be
;; necessary to use the master branch at
;;   https://github.com/mscfd/tree-sitter-fortran
;; instead of official tree-sitter repository at
;;   https://github.com/stadelmanma/tree-sitter-fortran
;;
;; Note: compile the grammar against tree-sitter 0.25.x; Emacs does not yet
;; support 0.26.
;;
;; To verify the setup:
;;   M-: (treesit-library-abi-version)           ; should return 15
;;   M-: (treesit-language-abi-version 'fortran) ; should return 15
;;   ldd bin_path_to_emacs/emacs | grep libtree-sitter
;;                                               ; should show libtree-sitter.so.0.25
;;
;; Basic setup with use-package:
;;
;;   (use-package f90-ts-mode
;;     :mode ("\\.f90\\'" . f90-ts-mode))
;;
;; See the README at https://github.com/mscfd/emacs-f90-ts-mode for full
;; documentation on indentation options, keybindings, and testing.

(require 'cl-lib)
(require 'treesit)

;;;-----------------------------------------------------------------------------

;;; Code:

(defgroup f90-ts nil
  "Fortran (F90+) major mode using Tree-sitter."
  :group 'languages)


(defgroup f90-ts-indent nil
  "Indentation in free format Fortran for treesitter f90-ts mode."
  :prefix "f90-ts-"
  :group  'f90-ts)


(defcustom f90-ts-indent-toplevel 0
  "Extra indentation applied to contain sections at toplevel."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-contain 3
  "Extra indentation applied to contain sections."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-block 3
  "Extra indentation applied to most blocks.
These are function and subroutine bodies, control statements (do, if,
associate ...) etc."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-continued 5
  "Extra indentation applied to continued lines."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defconst f90-ts-indent-list-options
  '(("keep if aligned or align to primary column" . keep-or-primary)
    ("keep if aligned or rotate to next column" . keep-or-rotate)
    ("always align with primary column" . primary)
    ("indent to first line of statement with offset `f90-ts-indent-continued'" . continued-line)
    ("rotate columns" . rotate))
  "Options for indentation of list like structures on continued lines.")


(defconst f90-ts--indent-list-radio
  `(radio ,@(mapcar (lambda (x)
                      `(const :tag ,(car x) ,(cdr x)))
                    f90-ts-indent-list-options))
  "Prepared options list for defcustoms.")


(defcustom f90-ts-indent-list-region 'keep-or-primary
  "Select indentation column for continued lines in list-like context.
Used as default setting in `indent-region' and similar operations."
  :type  f90-ts--indent-list-radio
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line 'rotate
  "Select indentation column for continued lines in list-like context.
Used as default setting in `indent-for-tab-command' and similar
operations (indentation of a single line)."
  :type  f90-ts--indent-list-radio
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line-2 'continued-line
  "Select indentation column for continued lines in list-like context.
Used as secondary setting in `indent-for-tab-command'.  Can be be bound
to <backtab> (S-<tab>), A-<tab>, C-S-<tab> or other."
  :type  f90-ts--indent-list-radio
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line-3 'primary
  "Select indentation column for continued lines in list-like context.
Used as ternary setting in `indent-for-tab-command'.  Can be be bound
to <backtab> (S-<tab>), A-<tab>, C-S-<tab> or other."
  :type  f90-ts--indent-list-radio
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-always-include-default t
  "Always include the default continued-line column in selected columns.
This column is offered as additional choice for alignment in a list-like
context.  It is the column of the first line of the continued statement
plus the value of `f90-ts-indent-continued'."
  :type  'boolean
  :safe  'booleanp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-paren-default 1
  "Additional offset applied for alignment with opening parenthesis.
The default is for all items except the closing parenthesis.

Example:
   call sub(       arg1, &
            .not.  arg2)
Primary alignment column for the second line column of parenthesis plus
`f90-ts-indent-paren-default'."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-paren-close 0
  "Additional offset applied for alignment with opening parenthesis.
This is for the closing parenthesis.

Example:
   x = x + (y + &
            z &
           )

Primary alignment column for the closing parenthesis is column of
opening parenthesis plus `f90-ts-indent-paren-close'."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-expr-assign-default 2
  "Additional offset applied for alignment at assignment.
This is applied for alignment with symbol \"=\" for all items
except for associative operators.

Example:
x =      & ! some comment
    some_expression

Primary alignment column for the second line column of assignment plus
`f90-ts-indent-expr-assign-default' for non-operators."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-expr-assign-assoc-op 0
  "Additional offset applied for alignment at assignment.
This is applied for alignment with symbol \"=\" for all associative
operators (logical_expression, math_expression).

Example:
x = value1 &
  + some_expression

Primary alignment column for the second line column of assignment plus
`f90-ts-indent-expr-assign-assoc-op' for associative operators."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-declaration 3
  "Additional offset applied for alignment with declaration delimiter \"::\".
This is applied in variable declarations.

Example:
integer, parameter :: ! some parameter &
                      param = 0.1234

Primary alignment column for the second line of a declaration plus
`f90-ts-indent-declaration'."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


;;;-----------------------------------------------------------------------------

(defcustom f90-ts-smart-end 'blink
  "Determine whether and how to complete an end statement.
If set to blink, perform completion and then jump to the opening clause of the
completed statement.
If set to no-blink perform completion without jumping.
Value nil turns off smart end completion.

Copied from prog mode `f90-mode'."
  :type  '(choice (const blink) (const no-blink) (const nil))
  :safe  (lambda (value) (memq value '(blink no-blink nil)))
  :group 'f90-ts)


;; same as in legacy f90 mode
(defcustom f90-ts-beginning-ampersand nil
  "Non-nil gives automatic insertion of `&' at start of continuation line."
  :type  'boolean
  :safe  'booleanp
  :group 'f90-ts)


;;;-----------------------------------------------------------------------------

(defface f90-ts-font-lock-delimiter-face
  '((t :foreground "Sienna4"
       :weight medium))
  "Face for custom font-lock highlighting."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-bracket-face
  '((t :foreground "BlueViolet"
       :weight bold))
  "Face for custom font-lock highlighting."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-operator-face
  '((t :foreground "Brown3"
       :weight bold))
  "Face for custom font-lock highlighting."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-openmp-face
  '((t :foreground "turquoise4"
       :weight medium))
  "Face for openmp statements."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-special-var-face
  '((t :foreground "blue4"
       :weight semi-bold))
  "Face for special variables like self or this."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-separator-comment-face
  '((t :foreground "Sienna4"
       :weight bold))
  "Face for separator comments."
  :group 'f90-ts-font-lock)


;;;-----------------------------------------------------------------------------
;;; keymap and syntax table

(defvar f90-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<tab>") #'f90-ts-indent-and-complete-stmt)
    ; <tab> is bound to indent-for-tab-command by default
    (define-key map (kbd "<backtab>")         #'f90-ts-indent-for-tab-command-2) ; S-<tab>
    (define-key map (kbd "C-S-<iso-lefttab>") #'f90-ts-indent-for-tab-command-3) ; Linux
    (define-key map (kbd "C-<backtab>")       #'f90-ts-indent-for-tab-command-3) ; Windows?

    (define-key map (kbd "A-<return>") 'f90-ts-break-line)
    (define-key map (kbd "A-<backspace>") #'f90-ts-join-line-prev)
    (define-key map (kbd "A-<delete>") #'f90-ts-join-line-next)
    (define-key map (kbd "A-\\") #'f90-ts-enlarge-region)
    (define-key map (kbd "A-0") #'f90-ts-child0-region)
    (define-key map (kbd "A-[") #'f90-ts-prev-region)
    (define-key map (kbd "A-]") #'f90-ts-next-region)
    map)
  "Keymap for `f90-ts-mode'.")


(defvar f90-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; --- symbols and words ---
    ;; '_' is a symbol constituent in Fortran
    (modify-syntax-entry ?_ "_" table)

    ;; --- string delimiters ---
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; --- comments ---
    ;; '!' starts a comment. '<' means start, 'b' means it's a
    ;; "Style B" comment (standard for line-based comments).
    (modify-syntax-entry ?! "< b" table)
    ;; newline ends the comment. '>' means end.
    (modify-syntax-entry ?\n "> b" table)

    ;; delimiters, newline, continuation
    (modify-syntax-entry ?\r " "  table) ; return is whitespace
    (modify-syntax-entry ?&  "."  table) ; continuation line
    (modify-syntax-entry ?%  "."  table) ; component reference

    ;; --- Arithmetic/Logic Punctuation ---
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?. "." table)  ; this is difficult, as dot in ".and." for example
                                        ; should belong to the symbol class and not punctuation,
                                        ; but having this as symbol would interfer with for example
                                        ; "this_flag.and.other_flag", appearing as one big symbol,
                                        ; likewise it could be difficult with floating point numbers?

    table)
  "Syntax table for `f90-ts-mode'.")


;;;-----------------------------------------------------------------------------
;;; other options

(defcustom f90-ts-special-var-regexp "\\_<\\(self\\|this\\)\\_>"
  "Regular expression for matching special variables.
This is used for syntax highlighting of variables like \"self\" and \"this\".
For matching identifiers the face `f90-ts-font-lock-special-var' is used."
  :type  'regexp
  :safe  'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-prefix-regexp "!\\S-*\\s-+"
  "Regular expression for matching and capturing comment starts.
This is used to extract the comment prefix for `f90-ts-break-line'.
OpenMP prefix is matched by `f90-ts-openmp-prefix-regexp'.

The variable should also add trailing whitespace characters to
preserve indentation within comments with `f90-ts-break-line'."
  :type  'regexp
  :safe  'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-region-prefix "!!$"
  "Comment prefix."
  :type  'string
  :group 'f90-ts)


(defcustom f90-ts-extra-comment-prefixes '("!" "!$omp" "!$acc" "!!!" "!>" "!<")
  "List of additional comment prefixes for interactive selection."
  :type  '(repeat string)
  :group 'f90-ts)


(defcustom f90-ts-mark-region-reversed nil
  "Mark region operations place point at end of region.
If this option is set, then point is placed at start of region."
  :type  'boolean
  :safe  'booleanp
  :group 'f90-ts)


(defcustom f90-ts-openmp-prefix-regexp "!\\$\\(?:omp\\)?\\s-+"
  "Regular expression for matching OpenMP starts.
This is used for line break operations, as openmp statements require
continuation symbols.
The defcustom should also add trailing whitespace characters to preserve
indentation within OpenMP statements."
  :type  'regexp
  :safe  'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-keyword-regexp "\\<\\(TODO\\|FIXME\\|Remarks?\\)\\>"
  "Regexp matching keywords within comments to highlight.
The matched part of the comment is highlighted with `font-lock-warning-face'.
Set to nil to disable keyword highlighting in comments."
  :type '(choice (const :tag "Disabled" nil)
                 (regexp :tag "Regexp"))
  :group 'f90-ts)


(defcustom f90-ts-special-comment-rules
  '((:name "openmp simd rule"
     :match "^!\\$omp simd\\b"
     :indent indented
     :face f90-ts-font-lock-openmp-face)
    (:name "general openmp rule"
     :match "^!\\$\\(?:omp\\)?\\b"
     :indent column-0
     :face f90-ts-font-lock-openmp-face)
    (:name "separator comment rule"
     :match "^!={40,}"
     :indent context
     :face f90-ts-font-lock-separator-comment-face)
    (:name "ford documentation"
     :match "^![<>]"
     :indent indented
     :face font-lock-doc-face))
  "Rules for special comment node indentation in `f90-ts-mode'.

Each element is a plist with the following keys:

  :name    A string naming the rule for documentation.

  :match   Either a regexp string matched against the comment line
           text, or a predicate function called with one argument
           (the comment node) that returns non-nil for a match.

  :indent  One of the following symbols:
           `column-0'  — always indent to column 0,
           `context'   — indent aligned to enclosing construct,
           `indented'  — indent like normal code and comments.

  :face    face symbol used to highlight matching comments.

Rules are tested in order; the first match determines indentation
and font lock face.
If no rule matches, the comment is indented normally.

Indentation hints of special comment rules are ignored within continued
lines, except for the column-0 option.  The other two options does not
seem to make much sense."
  :type '(repeat
          (list :tag "Rule"
                (const :format "" :value :name)
                (string :tag "Name")
                (const :format "" :value :match)
                (choice :tag "Match"
                        (regexp   :tag "Regexp")
                        (function :tag "Predicate"))
                (const :format "" :value :indent)
                (choice :tag "Indentation"
                        (const :tag "Column 0"                          column-0)
                        (const :tag "Context (parent block)"            context)
                        (const :tag "Indented (like code and comments)" indented))

                (const :format "" :value :face)
                (choice :tag "Face"
                        (const :tag "font-lock-comment-face"
                               font-lock-comment-face)
                        (const :tag "font-lock-doc-face"
                               font-lock-doc-face)
                        (const :tag "f90-ts-font-lock-separator-comment-face"
                               f90-ts-font-lock-separator-comment-face)
                        (const :tag "f90-ts-font-lock-openmp-face"
                               f90-ts-font-lock-openmp-face)
                        (face :tag "other face"))))
  :group 'f90-ts)


;;;-----------------------------------------------------------------------------
;;; auxiliary predicates

(defun f90-ts--node-type-p (node type)
  "If TYPE is nil, return true and ignore NODE.
If NODE is nil and TYPE is non-nil, return nil.
If TYPE is a string, return true if NODE is non-nil and is of type TYPE.
If TYPE is a list of strings, return true if NODE is non-nil and its
type is among the elements of TYPE."
  (or (not type)
      (and node
           (let ((type-n (treesit-node-type node)))
             (if (stringp type)
                 (string= type-n type)
               (member type-n type))))))


(defun f90-ts--node-type-match-p (node type-rx)
  "If TYPE_RX is nil, return non-nil and ignore NODE.
If NODE is nil and TYPE_RX is non-nil, return nil.
If TYPE-RX and NODE are both non-nil return non-nil if the type of NODE
is matched by TYPE-RX."
  (or (not type-rx)
      (and node
           (let ((type-n (treesit-node-type node)))
             (string-match-p type-rx type-n)))))


(defun f90-ts--comment-prefix (node)
  "Extract the starting character sequence from a comment NODE.
NODE is assumed to be of type comment.  It uses `f90-ts-openmp-prefix-regexp'
and `f90-ts-comment-prefix-regexp' to identify the prefix to extract."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "comment-prefix: comment node expected")
  ;; first match openmp as comment prefix would just take the initial ! and ignoring
  ;; following $omp part in openmp statements
  (let ((rx-comment (concat "^\\(?:" f90-ts-openmp-prefix-regexp "\\)\\|\\(?:" f90-ts-comment-prefix-regexp "\\)")))
    (when (string-match rx-comment (treesit-node-text node))
      (match-string 0 (treesit-node-text node)))))


(defun f90-ts-special-var-p (node)
  "Check if NODE is an identifier and matches the special variable regexp.
Note that the parse uses identifier not just for variables, but for types etc."
  (when (f90-ts--node-type-p node "identifier")
    ;; we do not prepend or append symbol start or end assertions, as it should also
    ;; work with more general regexps (like highlight all variables with a certain prefix)
    (string-match-p (concat "^"
                            f90-ts-special-var-regexp
                            "$")
                    (treesit-node-text node))))


(defun f90-ts-openmp-node-p (node)
  "Check if NODE is a comment node and has a OpenMP comment prefix."
  (when (f90-ts--node-type-p node "comment")
    (string-match-p (concat "^" f90-ts-openmp-prefix-regexp)
                    (treesit-node-text node))))


(defun f90-ts-preproc-node-p (node)
  "Check if NODE is a preprocessor node and has the '#' prefix."
    (let ((type (treesit-node-type node)))
      (or (string-match "^preproc_" type)
          (string-prefix-p "#" type))))


;; the regexp engine is lacking a case insensitive switch, so we need to
;; lowercase the identifier by hand
(defun f90-ts-builtin-p (node)
  "Return non-nil if NODE represents an builtin function.
The function assumes that NODE is an identifier and only checks the text of the
node."
  (cl-assert (f90-ts--node-type-p node "identifier")
             nil "builtin-p: identifier expected")
  (let ((text (downcase (treesit-node-text node)))
        (rx (regexp-opt
             '("abs" "acos" "aimag" "aint" "allocated" "anint" "any" "asin"
               "associated" "atan" "atan2" "btest" "ceiling" "char" "cmplx"
               "conjg" "cos" "cosh" "count" "cshift" "date_and_time" "dble"
               "dim" "dot_product" "dprod" "eoshift" "epsilon" "exp" "exponent"
               "floor" "fraction" "huge" "iand" "ibclr" "ibits" "ibset" "ichar"
               "ieor" "index" "int" "ior" "ishft" "ishftc" "kind" "lbound"
               "len" "len_trim" "log" "log10" "logical" "matmul" "max"
               "maxexponent" "maxloc" "maxval" "merge" "min" "minexponent"
               "minloc" "minval" "mod" "modulo" "mvbits" "nearest" "nint" "not"
               "null" "pack" "precision" "present" "product" "radix"
               "random_number" "random_seed" "range" "rank" "real" "repeat"
               "reshape" "rrspacing" "scale" "scan" "selected_int_kind"
               "selected_real_kind" "set_exponent" "shape" "sign" "sin" "sinh"
               "size" "spacing" "spread" "sqrt" "sum" "system_clock" "tan"
               "tanh" "tiny" "transfer" "transpose" "trim" "ubound" "unpack"
               "verify")
             'symbols)))
    (string-match-p rx text)))


(defun f90-ts-in-string-p ()
  "Non-nil if point is inside a string."
  (when-let ((node (treesit-node-at (point))))
    (when (f90-ts--node-type-p node "string_literal")
      (let ((start (treesit-node-start node))
            (end (treesit-node-end node))
            (pos (point)))
        ;; start and end position are the quotation characters
        (and (< start pos) (< pos end))))))


(defun f90-ts-in-openmp-p ()
  "Non-nil if point is inside an OpenMP statement.
OpenMP statements are parsed as comment nodes, but always start with !$.
The grammar does not parse OpenMP currently."
  (when-let ((node (treesit-node-at (point))))
    (when (f90-ts-openmp-node-p node)
      (let ((start (treesit-node-start node))
            (pos (point)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts-in-comment-p ()
  "Non-nil if point is after start of comment.
This excludes OpenMP statements, which look like comments and are currently not
parsed by the treesitter grammar."
  (when-let ((node (treesit-node-at (point))))
    (when (and (f90-ts--node-type-p node "comment")
               (not (f90-ts-openmp-node-p node)))
      (let ((start (treesit-node-start node))
            (pos (point)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts-bol-to-point-blank-p ()
  "Return non-nil if only blank characters exist between BOL and point.
Note that in fortran, a continuation symbol shall not be used on blank lines."
  (looking-back "^\\s-*" (line-beginning-position)))


(defun f90-ts--node-not-number-literal-p (node)
  "Return non-nil if NODE is not a number_literal.

This predicate is necessary, as the :pred in queries does not seem to
work with lambda expressions."
  (not (string= (treesit-node-type node) "number_literal")))


(defun f90-ts--node-is-ampersand-p (node)
  "Check whether NODE is continuation symbol &."
  (f90-ts--node-type-p node "&"))


(defun f90-ts--node-not-comment-p (node)
  "Return non-nil if NODE is not of type comment."
  (not (f90-ts--node-type-p node "comment")))


(defun f90-ts--node-not-comment-or-error-p (node)
  "Return non-nil if NODE is not of type comment or error."
  (not (f90-ts--node-type-p node '("comment" "ERROR"))))


(defun f90-ts-node-overlap-region-p (node start end)
  "Return non-nil if NODE overlaps with region START END."
  (and (< (treesit-node-start node) end)
       (> (treesit-node-end node)   start)))


(defconst f90-ts--node-op-expr-types
  '("logical_expression"
    "math_expression"
    "relational_expression"
    "concatenation_expression"
    "unary_expression")
  "Operator expression types for alignment purposes.
These are required to have a field named operator.  Note that logical and math
expression overlap, as user defined operators are always interpreted as math
expression, even if operating on bools (for example: .imply.  operator).")


(defun f90-ts--node-is-op-expr-p (node)
  "Return non-nil if NODE is of some expression type."
  (member (treesit-node-type node)
          f90-ts--node-op-expr-types))


;;;-----------------------------------------------------------------------------
;;; auxiliary walk and query functions

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


;; currently not used, but might be useful
(defun f90-ts--search-subtree (root pred &optional start end prune reversed)
  "Collect nodes within subtree of ROOT for which PRED shall return non-nil.
ROOT can be any node and is not necessarily the root node of the tree.
If START and END are non-nil, only visit nodes overlapping that region.
If PRUNE is non-nil, do not descend into children of nodes that satisfy PRED.
If REVERSED is true, return in reversed order."
  (let (nodes)
    (cl-labels
        ((traverse (node)
           (when (or (null start)
                     (null end)
                     (f90-ts-node-overlap-region-p node start end))
             (let ((match (funcall pred node)))
               (when match
                 (push node nodes))
               (unless (and match prune)
                 (dolist (child (treesit-node-children node))
                   (traverse child)))))))
      (traverse root))
    (if reversed
        nodes
      (nreverse nodes))))


;; currently not used, but might be useful
(defun f90-ts--statement-at-node (node)
  "Find the most relevant ancestor node starting at the same position as NODE.
This is done by ascending to parent nodes until start position becomes different
or some ERROR or translation_unit node is encountered."
  ;; ascend as long as parent starts at the same position and is not an ERROR node
  (cl-loop
   for current = node then parent
   for parent = (treesit-node-parent current)
   while (and parent
              (= (treesit-node-start parent)
                 (treesit-node-start current))
              (not (f90-ts--node-type-p parent
                                        '("ERROR" "translation_unit"))))
   finally return current))


(defun f90-ts--prev-sibling-predicate (node predicate)
  "Find previous sibling of NODE satisfying PREDICATE.
Repeat `treesit-node-prev-sibling' until a suitable sibling is found,
or return nil."
  (cl-loop
   for sibling = (treesit-node-prev-sibling node)
            then (treesit-node-prev-sibling sibling)
   while sibling
   when (funcall predicate sibling) return sibling))


(defun f90-ts--prev-sibling-proper (node)
  "Determine previous \"proper\" sibling of NODE.
Nodes of type comments and ampersand are not considered \"proper\"."
  (f90-ts--prev-sibling-predicate
   node
   (lambda (n)
     (not (f90-ts--node-type-p n '("comment" "&"))))))


(defun f90-ts--previous-stmt-keyword-by-first (first)
  "Return keyword of previous statement.
Use node FIRST which provides the very first leaf node of previous statement.
Auxiliary function for `f90-ts--previous-stmt-keyword' or when
first statement is known."
  ;; block structure statements like if, do, associate etc. can start
  ;; with a label, if a (optional) label is present, skip it and extract
  ;; the keyword node;
  ;; in general, the AST looks like:
  ;;
  ;; (do_loop
  ;;  (block_label_start_expression
  ;;   'label'
  ;;   :)
  ;;  (do_statement do
  ;;  ...))
  ;;
  ;; but if label is a reserved keyword, it becomes
  ;;
  ;; (do_loop
  ;;  (block_label_start_expression
  ;;   'label'
  ;;    keyword
  ;;   :)
  ;;  (do_statement do
  ;;  ...))
  ;;
  ;; in both cases 'label' is an anonymous node;
  ;; in the second case, it has a child,
  ;;
  ;; we want to extract the anonymous node "do" of the statement
  ;; following the block_label_start expression,
  ;; so first check we are within a block_label_start_expression
  (let* ((parent (and first (treesit-node-parent first)))
         (grandparent (and parent (treesit-node-parent parent)))
         (block-label (cond
                       ((and (f90-ts--node-type-p first "label")
                             (f90-ts--node-type-p parent
                                                  "block_label_start_expression"))
                        parent)
                       ((and (f90-ts--node-type-p parent "label")
                             (f90-ts--node-type-p grandparent
                                                  "block_label_start_expression"))
                      grandparent)
                       (t nil))))
    (if block-label
	    (let ((fp-next (treesit-node-next-sibling block-label)))
          ;; next sibling is a statement label, we descend to find
          ;; first leaf
          (cl-loop
           for n = fp-next then child
           for child = (treesit-node-child n 0)
           while child
           finally return n))
      ;; not a label expression, just return first
      first)))


(defun f90-ts--previous-stmt-keyword (node parent)
  "Return the previous statement leaf node.
Use NODE and PARENT to deteremine previous statement or start of
statement in a multiline statement.
Usually some keyword like if, elseif, do, etc. for `f90-ts-prev-stmt-first'.
In case of a block label the first leaf node is the label, not the keyword.
For use as anchor, the label is required.  For use as matcher, we need the
keyword.
Keyword nodes become relevant for incomplete code with ERROR nodes."
  ;; if the statement starts with a block label, then first is unnamed
  ;; node label, and its parent is block_label_start_expression. Its
  ;; next sibling is a keyword like if or do
  (let ((first (f90-ts--previous-stmt-first node parent)))
    (f90-ts--previous-stmt-keyword-by-first first)))


(defun f90-ts--previous-stmt-first (node parent)
  "Return previous statement determined by NODE and PARENT.
First search for any reasonable (leaf) node, which is before current
line, and then go to start of line.  If on a continued line, follow it
to the start of the statement.
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
`f90-ts--node-not-comment-or-error-p' during ascend or descend,
for example comment nodes."
  (let* ((cur-line (f90-ts--line-number-at-node-or-pos node))
         (predicate #'f90-ts--node-not-comment-or-error-p)
         ;; ascend until a previous ancestor is found
         (prev-sib-of-anc
          (cl-loop for ancestor = (or node parent) then (treesit-node-parent ancestor)
                   while ancestor
                   for relative = (f90-ts--before-child ancestor cur-line predicate)
                   when relative return relative))
         ;; descend prev-sib-of-anc to find the deepest node still before current line
         (prev-descend
          (and prev-sib-of-anc
               (cl-loop for sib = prev-sib-of-anc then next
                        for next = (and (> (treesit-node-child-count sib) 0)
                                        (f90-ts--before-child sib cur-line predicate))
                        ;; if there is a next, continue and shift next to sib
                        ;; with for sib=next in the first line
                        while next
                        finally return sib))))
    ;; take continuation lines into account and go to beginning of statement
    (and prev-descend
         (f90-ts--first-node-of-stmt prev-descend))))


(defun f90-ts--prev-sib-by-parent (parent)
  "Previous sibling based on position of point and PARENT.
Especially for empty line, it often happens that node=nil, but parent is some
relevant node, whose children, which are kind of siblings to nil-node-position,
can be used to determine things like indentation.
For the first sibling itself, we do not exclude ERROR nodes.  Then the
ERROR node is descended (usually just one step) to find a node with relevant
structure type, like subroutine_statement or similar.
Comment nodes and ampersand are ignored"
  (let* ((cur-line (line-number-at-pos))
         (predicate #'f90-ts--node-not-comment-p)
         (psib (f90-ts--before-child parent cur-line predicate)))
    ;; if psib=nil, just return nil
    ;; if psib=ERROR node, descend and try to find some non-error node
    (cl-loop
     for current = psib then child
     for child = (and current (treesit-node-child current 0 t))
     while (and child
                (f90-ts--node-type-p current "ERROR"))
     finally return current)))


(defun f90-ts--before-child (node line predicate)
  "Return child of NODE, which is on a previous LINE and satisfies PREDICATE.
Take the last of all children satisfying this condition."
  (when-let* ((children (treesit-node-children node))
              (children-prev (f90-ts--nodes-on-prev-lines children line predicate)))
    (car (last children-prev))))


(defun f90-ts--first-node-on-line (pos)
  "Return the first node on the line at POS.
Take virtual ampersand nodes into account, these are not returned by
`treesit-node-at' and require extra work.  If the line is empty, return nil."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (let* ((line (line-number-at-pos))
           (on-node (treesit-node-on (point) (point)))
           (at-node (treesit-node-at (point))))
      (cond
       ((and on-node
             (= (point) (treesit-node-start on-node)))
        ;; in some cases, there might be other virtual nodes,
        ;; or treesit-node-on returns a proper node starting at (point),
        ;; walk back as long as start of node does not change
        (cl-loop for node = on-node
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
      (when-let ((node (treesit-node-at (point))))
        (when (= (line-number-at-pos pos)
                 (f90-ts--node-line node))
          node)))))


(defun f90-ts--find-first-ampersand (first)
  "Find the first ampersand if at a ampersand-comment*-ampersand sequence.
In continued lines, continuation symbol ampersands appears in
sequences like &, (comment)*, &.  This routines checks whether node
FIRST is any of those ampersand or comment nodes, and returns the first
ampersand node of this sequence.  FIRST is assumed to be the first node
on its line!"
  (when-let ((nprev
              (cond
               ((f90-ts--node-is-ampersand-p first)
                ;; go one step back to first ampersand or sequence
                ;; of comments
                (treesit-node-prev-sibling first))

               ((f90-ts--node-type-p first "comment")
                first)

               (t
                ;; in all other cases, we are not on a continued line
                nil))))
    ;; if necessary skip comments (but only comments) to find the first ampersand,
    ;; if we are in a sequence of comments not being part of a continued line,
    ;; then the final non-comment node is not an ampersand and we return nil
    (cl-loop for node = nprev then (treesit-node-prev-sibling node)
             while (f90-ts--node-type-p node "comment")
             finally return (and (f90-ts--node-is-ampersand-p node)
                                 node))))


(defun f90-ts--line-continued-at-end-p (last pos)
  "Check whether line at POS is continued.
It usually ends in &, but might be followed by a comment.  First check that
there is a next line after current line.
LAST is expected to be the last node on the line, and can be obtained
by `f90-ts--last-node-line'"
  ;; if there is an ampersand (or ampersand (comment)) at end of line but
  ;; no other sibling follows, we are probably at end of file
  (when (treesit-node-next-sibling last)
    (cond
     ((f90-ts--node-is-ampersand-p last)
      t)

     ((f90-ts--node-type-p last "comment")
      ;; if last comment is node, check whether previous one is an ampersand,
      ;; but it must be on the same line
      (let ((prev (treesit-node-prev-sibling last)))
        (and prev
             (f90-ts--node-is-ampersand-p prev)
             (= (line-number-at-pos pos)
                (f90-ts--node-line prev)))))

     (t
      ;; in all other cases, we are not on a continued line
      nil))))


(defun f90-ts--first-line-of-continued-stmt-p (pos)
  "Check whether POS is on the first line of a continued statement.
To this end, check that it does not start with and ampersand, but
is continued at end."
  (when-let ((first-node (f90-ts--first-node-on-line pos))
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
      (when-let ((last (f90-ts--last-node-on-line pos)))
        (f90-ts--line-continued-at-end-p last pos))))


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


(defun f90-ts--after-stmt-line1-p (node pos)
  "Check whether position POS is on the next line after NODE.
NODE is assumed to being part of a statement possibly spread over several lines.
Empty lines are automatically skipped as those are not present in the tree."
  ;; strategy: get last node on the same line as NODE, check whether it is &,
  ;; goto next node, which is & on next line and compare with line number at pos;
  ;; note that if "&" is at end of line, then there is always a second "&"
  ;; at beginning of the next non-empty/non-comment line or at EOF,
  ;; hence (treesit-next-sibling last) below can always be executed.
  (when-let* ((cur-line (line-number-at-pos pos))
              (pos-node (treesit-node-start node))
              (last (f90-ts--last-node-on-line pos-node)))
    (when (f90-ts--line-continued-at-end-p last pos-node)
      (let ((nsib (treesit-node-next-sibling last)))
        (and nsib
             (not (< (f90-ts--node-line nsib) cur-line)))))))


(defun f90-ts--indent-pos-at-node (node)
  "Determine indentation position of line where start of NODE is located."
  ;; TODO: is this correct even with treesit-indent-region?
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))


(defun f90-ts--node-at-indent-pos (pos)
  "Determine node at indentation position of line determined by POS."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (treesit-node-at (point))))


(defun f90-ts--node-line (node)
  "Determine line number of start position of NODE."
  (line-number-at-pos (treesit-node-start node)))


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


;;;-----------------------------------------------------------------------------
;;; Font-locking: auxiliary

(defun f90-ts--fontify-comment (node override _start _end &rest _)
  "Fontify NODE assumed to be a comment.
Check whether NODE satisfies a special comment rule, and if it does,
use the face provided by the first matching rule.
If no rule matches, use `font-lock-comment-face'.

Keyword matches from `f90-ts-comment-keyword-regexp' are additionally
highlighted with `font-lock-warning-face' on top of the base face.

Argument OVERRIDE is passed to `treesit-fontify-with-override' for the comment
rule but not for matched keywords, which are enforced with override=t."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "fontify-comment: comment node expected")
  (let* ((rule (f90-ts--comment-matching-rule node))
         (face (or (and rule (plist-get rule :face))
                   'font-lock-comment-face))
         (start (treesit-node-start node))
         (end   (treesit-node-end node)))

    ;; apply base face to the whole comment node
    (treesit-fontify-with-override start end face override)

    ;; overlay keyword matches on top
    (when f90-ts-comment-keyword-regexp
      (save-excursion
        (goto-char start)
        (cl-loop while (re-search-forward f90-ts-comment-keyword-regexp end t)
                 do (treesit-fontify-with-override
                       (match-beginning 0) (match-end 0)
                       'font-lock-warning-face t))))))


;;;-----------------------------------------------------------------------------
;;; Font-locking: treesitter rules

(defun f90-ts--font-lock-rules-comment ()
  "Font-lock rules for comments."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'comment
   '(;; default comments as well as special comments and openmp
     ;; statements, declared by `f90-ts-special-comment-rules'
     ((comment) @f90-ts--fontify-comment))))


(defun f90-ts--font-lock-rules-intrinsic ()
  "Font-lock rules for Fortran intrinsic functions."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'builtin
   `((print_statement
      "print" @font-lock-builtin-face)
     (read_statement
      "read" @font-lock-builtin-face)
     (write_statement
      "write" @font-lock-builtin-face))

   :language 'fortran
   :feature 'builtin
   `((call_expression
      (identifier) @font-lock-builtin-face
      (:pred f90-ts-builtin-p @font-lock-builtin-face)))))


(defun f90-ts--font-lock-rules-keyword ()
  "Font-lock rules for Fortran keywords."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'keyword
   ;; match type qualifiers/attributes
   '((type_qualifier) @font-lock-keyword-face)

   :language 'fortran
   :feature 'keyword
   '(;; match keywords in select case statements
     ;; (which are not covered by simple keywords below
     (case_statement
      "case"    @font-lock-keyword-face
      (default) @font-lock-keyword-face)
     ;; match keywords in select type statements
     ;; (which are not covered by simple keywords below
     (type_statement
      (default) @font-lock-keyword-face)
     ;; match keywords in select rank statements
     ;; (which are not covered by simple keywords below
     (rank_statement
      (default) @font-lock-keyword-face))

   :language 'fortran
   :feature 'keyword
   ;; "none" is a named node in "implicit none", but an anonymous node
   ;; in "do concurrent(...) default(none)"
   '((implicit_statement
      (none) @font-lock-keyword-face))

   :language 'fortran
   :feature 'keyword
   ;; match keywords exposed by the grammar
   ;; note that this matches anonymous nodes representing the keyword,
   ;; not the keyword text itself, and these nodes are always stored lower-case
   ;; (hence no need to match case insensitive as is necessary with builtins)
   '((["program" "module" "submodule"
      "function" "subroutine" "procedure"
      "bind" "result" "end" "call"
      "public" "private" "protected" "contains"
      "use" "only" "implicit" "none" "external"
      "pure" "impure" "elemental" "recursive"
      "type" "class" "is" "typeof" "classof"
      "if" "then" "else" "elseif" "endif"
      "do" "while"
      "cycle" "exit"
      "associate" "block" "critical"
      "enum" "enumeration" "enumerator"
      "where" "elsewhere" "forall" "concurrent"
      "select" "case" "rank" "default"
      "shared" "local" "local_init" "reduce"
      "extends" "abstract"
      "pass" "nopass" "deferred"
      "operator" "assignment" "generic" "final"
      "interface" "return"
      "allocate" "deallocate" "allocatable"
      "intent" "in" "out" "inout"
      "parameter" "save" "target" "pointer" "optional"
      "dimension" "contiguous" "volatile"]
      @font-lock-keyword-face))))


(defun f90-ts--font-lock-rules-preproc ()
  "Font-lock rules for preprocessor directives."
  (treesit-font-lock-rules
;   :language 'fortran
;   :feature 'preproc
;   ;; preprocessor directive keywords as tokens (not entire nodes)
;   '((["#include" "#define" "#if" "#ifdef" "#ifndef" "#endif" "#else" "#elif" "#elifdef"]) @font-lock-preprocessor-face)

   :language 'fortran
   :feature 'preproc
   ;; highlight macro names in definitions
   '((preproc_include
      "#include"             @font-lock-preprocessor-face
      path: (string_literal) @font-lock-string-face)
     (preproc_def
      "#define"            @font-lock-preprocessor-face
      name: (identifier)   @font-lock-constant-face)
     (preproc_function_def
      "#define"            @font-lock-preprocessor-face
      name: (identifier)   @font-lock-function-name-face)
     (preproc_if
      "#if"                @font-lock-preprocessor-face
      "#endif"             @font-lock-preprocessor-face)
     (preproc_ifdef
      "#ifdef"             @font-lock-preprocessor-face
      name: (identifier)   @font-lock-constant-face
      "#endif"             @font-lock-preprocessor-face)
     (preproc_ifdef
      "#ifndef"            @font-lock-preprocessor-face
      name: (identifier)   @font-lock-constant-face
      "#endif"             @font-lock-preprocessor-face)
     (preproc_elif
      "#elif"              @font-lock-preprocessor-face)
     (preproc_elifdef
      "#elifdef"           @font-lock-preprocessor-face
      name: (identifier)   @font-lock-constant-face)
     (preproc_else
      "#else"              @font-lock-preprocessor-face))))


(defun f90-ts--font-lock-rules-prog-mod ()
  "Font-lock rules for program and (sub)modules."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((program_statement
      "program"
      (name)                  @font-lock-function-name-face)
     (module_statement
      "module"
      (name)                  @font-lock-function-name-face)
     (submodule_statement
      "submodule"
      ancestor: (module_name
                 (name)       @font-lock-function-name-face)
      (name)                  @font-lock-function-name-face))))


(defun f90-ts--font-lock-rules-type ()
  "Font-lock rules for type declarations."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'type
   ;; match type keywords
   '(["integer"
      "real" "complex" "double" "precision"
      "logical"
      "character"] @font-lock-type-face)

   :language 'fortran
   :feature 'type
   ;; match type keywords
   '(((type_name)                @font-lock-type-face)
     ((derived_type_statement
       base: (base_type_specifier
              (identifier)       @font-lock-type-face)))
     (end_type_statement
      (name)                     @font-lock-type-face))

   :language 'fortran
   :feature 'type
   ;; special declarations (e.g. within allocate statements)
   ;; TODO: should the grammar use type_name instead of identifier as done elsewhere?
   '(((allocate_statement
      type: (identifier)        @font-lock-type-face))
     (type_statement
      type: (identifier)        @font-lock-type-face))))


(defun f90-ts--font-lock-rules-function ()
  "Font-lock rules for functions and subroutines."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((subroutine_statement
      name: (name)                 @font-lock-function-name-face)
     (function_statement
      name: (name)                 @font-lock-function-name-face)
     (module_procedure_statement
      name: (name)                 @font-lock-function-name-face)
     (function_result
      (identifier)                 @default)
     (subroutine_call
      subroutine: [
                   ((identifier)     @font-lock-function-name-face)
                   ((derived_type_member_expression
                    [(identifier)
                     (derived_type_member_expression)]
                    "%"
                    (type_member) @font-lock-function-name-face))
                   ])

     ;; within derived type declarations
     (variable_declaration
      type: (procedure
             "procedure"
             (procedure_interface)        @font-lock-function-name-face)
      ;; is this always a pointer to a procedure?
      declarator: (identifier)            @font-lock-function-name-face)
     (procedure_statement
      declarator: [
                   ((method_name)         @font-lock-function-name-face)
                   ((binding
                     (binding_name
                      [
                       ((identifier)      @font-lock-function-name-face)
                       ((operator
                         (operator_name)  @f90-ts-font-lock-operator-face))
                       ((assignment "="   @f90-ts-font-lock-operator-face))
                       ])
                     (method_name)        @font-lock-function-name-face))
                   ])
     (generic_statement
      declarator: (binding_list
                    (binding_name
                     [
                      ((identifier)      @font-lock-function-name-face)
                      ((operator
                        (operator_name)  @f90-ts-font-lock-operator-face))
                      ((assignment "="   @f90-ts-font-lock-operator-face))
                      ])
                    (method_name)        @font-lock-function-name-face))
     (final_statement
      declarator: (method_name)          @font-lock-function-name-face))))


(defun f90-ts--font-lock-rules-interface ()
  "Font-lock rules for interface blocks."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((interface_statement
      "interface"
      (name)                      @font-lock-function-name-face)
     (procedure_statement
      declarator: (method_name)   @font-lock-function-name-face))))


(defun f90-ts--font-lock-rules-end ()
  "Apply font-lock rules for end statements of structures."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((end_program_statement
      "end"
      "program"
      (name)       @font-lock-function-name-face)

     (end_module_statement
      "end"
      "module"
      (name)       @font-lock-function-name-face)

     (end_submodule_statement
      "end"
      "submodule"
      (name)       @font-lock-function-name-face)

     (end_function_statement
      "end"
      "function"
      (name)       @font-lock-function-name-face)

     (end_subroutine_statement
      "end"
      "subroutine"
      (name)       @font-lock-function-name-face)

     (end_module_procedure_statement
      "end"
      "procedure"
      (name)       @font-lock-function-name-face)

     (end_interface_statement
      "end"
      "interface"
      (name)       @font-lock-function-name-face))))


(defun f90-ts--font-lock-rules-variable ()
  "Font-lock rules for variables."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'variable
   '(
     ((identifier) @f90-ts-font-lock-special-var-face
      (:pred f90-ts-special-var-p @f90-ts-font-lock-special-var-face)))))


(defun f90-ts--font-lock-rules-value ()
  "Font-lock rules for numbers, strings, booleans etc."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'string
   '(((string_literal) @font-lock-string-face)
     (preproc_include path: (string_literal) @font-lock-string-face))

   :language 'fortran
   :feature 'number
   '(((number_literal) @font-lock-number-face)
     ((complex_literal) @font-lock-number-face))

   :language 'fortran
   :feature 'constant
   '(((boolean_literal) @font-lock-constant-face)
     ((null_literal) @font-lock-constant-face))))


(defun f90-ts--font-lock-rules-delimiter ()
  "Font-lock rules for brackets and delimiters."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'bracket
   '(["(" ")" "[" "]" "(/" "/)"] @f90-ts-font-lock-bracket-face)

   :language 'fortran
   :feature 'delimiter
   '(["," ":" ";" "::" "=>" "&"] @f90-ts-font-lock-delimiter-face)))


(defun f90-ts--font-lock-rules-operator ()
  "Font-lock rules for operators."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'operator
   '((logical_expression       operator: _  @f90-ts-font-lock-operator-face)
     (math_expression          operator: _  @f90-ts-font-lock-operator-face)
     (relational_expression    operator: _  @f90-ts-font-lock-operator-face)
     (concatenation_expression operator: _  @f90-ts-font-lock-operator-face)
     ("="                                   @f90-ts-font-lock-operator-face)
     ("%"                                   @f90-ts-font-lock-operator-face)
     ;; unary: only match user_defined_operator nodes with a non-number_literal argument
     ;; (note: :pred does not seem to accept lambda expressions)
     (unary_expression
      operator: (user_defined_operator) @f90-ts-font-lock-operator-face)
     ((unary_expression
      operator: _ @f90-ts-font-lock-operator-face
      argument: (_) @_unary_arg
      (:pred f90-ts--node-not-number-literal-p @_unary_arg))))))


(defvar f90-ts-font-lock-rules
  (list
   (f90-ts--font-lock-rules-comment)
   (f90-ts--font-lock-rules-intrinsic)
   (f90-ts--font-lock-rules-keyword)
   (f90-ts--font-lock-rules-preproc)
   (f90-ts--font-lock-rules-prog-mod)
   (f90-ts--font-lock-rules-type)
   (f90-ts--font-lock-rules-function)
   (f90-ts--font-lock-rules-interface)
   (f90-ts--font-lock-rules-end)
   (f90-ts--font-lock-rules-operator)
   (f90-ts--font-lock-rules-variable)
   (f90-ts--font-lock-rules-value)
   (f90-ts--font-lock-rules-delimiter))
  "List of font-lock rules.")


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


(defconst f90-ts--indent-cache-size 8
  "Fixed size for `f90-ts--indent-cache'.")


;; slot indices of indent cache
;; nodes
(defconst f90-ts--indent-slot-node               0)
(defconst f90-ts--indent-slot-parent             1)
(defconst f90-ts--indent-slot-child0             2)
(defconst f90-ts--indent-slot-prev-sib-by-parent 3)
(defconst f90-ts--indent-slot-prev-stmt-first    4)
(defconst f90-ts--indent-slot-prev-stmt-keyword  5)
;; anchor and/or offset data
;; note: matcher or anchor can sometimes determine anchor/offset
;; in more complex rules, like for continued lines, and it is not
;; possible to pass any results from matcher to anchor to offset
;; in simple indent rules machinery
(defconst f90-ts--indent-slot-anchor             6)
(defconst f90-ts--indent-slot-offset             7)


(defun f90-ts--indent-cache-print ()
  "Print out current cache state."
  (if (null f90-ts--indent-cache)
      (f90-ts-log :indent "cache: nil" f90-ts--indent-cache)
    (f90-ts-inspect-node :indent (f90-ts--indent-cached-node)        "node@cache")
    (f90-ts-inspect-node :indent (f90-ts--indent-cached-parent)      "parent@cache")
    (f90-ts-inspect-node :indent (f90-ts--indent-child0)             "child0@cache")
    (f90-ts-inspect-node :indent (f90-ts--indent-prev-sib-by-parent) "psibp@cache")
    (f90-ts-inspect-node :indent (f90-ts--indent-prev-stmt-first)    "pstmt-1@cache")
    (f90-ts-inspect-node :indent (f90-ts--indent-prev-stmt-keyword)  "pstmt-k@cache")
    (f90-ts-log :indent "anchor@cache = %s" (f90-ts--indent-cached-anchor))
    (f90-ts-log :indent "offset@cache = %s" (f90-ts--indent-cached-offset))))


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


(defun f90-ts--indent-cache-valid-p (node parent)
  "Check whether cache is valid and not empty or stale.
Note that NODE and PARENT are live nodes, so a simple
comparison is sufficient."
  (and f90-ts--indent-cache
       (eq node   (f90-ts--indent-cached-node))
       (eq parent (f90-ts--indent-cached-parent))))


(defun f90-ts--indent-ensure-cache (node parent)
  "Return cache for NODE and PARENT, reusing it if the cache is valid."
  (when (not (f90-ts--indent-cache-valid-p node parent))
      (setq f90-ts--indent-cache (make-vector f90-ts--indent-cache-size 'unset))
      (aset f90-ts--indent-cache f90-ts--indent-slot-node   node)
      (aset f90-ts--indent-cache f90-ts--indent-slot-parent parent))
  f90-ts--indent-cache)


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
  "Return result of `f90-ts--previous-stmt-keyword' for cached node and parent.
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
The cache is an alist with entries (LINE . (BOL-COL DELTA))
for already indented lines of a continued statement.

For alignment operations, we need node column numbers of nodes on
previously (already indented) lines to select the proper one.
The cache is required for `indent-region' operations, which work in a
batch mode.  If a previous line has not already been flushed, which is
the default case, we need to compute the column number ourselves after
not-yet-applied indentation.  To this end, we store the computed delta
in the cache.
Moreover, we also store BOL-COL to detect, whether internal indentation
buffer of `treesit-indent-region' has already been flushed for a line.
Except for the first line, we can compute the indentation delta and
cache it.  For the first line, the delta is not known and initially
stored as DELTA=0. Once we detect a flush using cached column at bol,
we can compute the delta and add to all cached lines.  This is necessary
to have consistent indentation across all previous lines

As mentioned above, the cache is an alist (LINE . (BOL-COL DELTA)),
mapping buffer LINE numbers to BOL-COL and DELTA, where:
  BOL-COL: indentation column at cache time, used as flush detector:
           if current indentation at line == BOL-COL, line is not yet
           flushed
  DELTA:   delta of original to new indentation, if line is not
           flushed, then the column after applying indentation is
           column number of node + DELTA,
           and if the line has been flushed, it is just current node
           column

The first line of the statement initially is stored with DELTA=0 and
current BOL-COL.  For each new line, the current indentation of the
first line is checked and if a buffer flush is detected, the applied
delta is computed and added to the delta of all subsequent cached
lines.

The state of `f90-ts--align-continued-variant-tab' is used to decide
whether `indent-region' or a line variant is in use.  The cache is
required only if `indent-region' with buffering is done.")


(defun f90-ts--continued-line-cache-reset ()
  "Reset the continued-line indentation cache."
  (setq f90-ts--continued-line-cache nil))


(defun f90-ts--continued-line-cache-put-first (bol)
  "Store a cache entry for the first line of a continued statement.
BOL is the beginning of that line.  Always store DELTA=0.
If a flush is detected (actual BOL is different from cached BOL),
the DELTA is computed and added to all other cached line."
  (f90-ts--continued-line-cache-reset)
  (unless f90-ts--align-continued-variant-tab
    (let* ((bol-col (save-excursion
                      (goto-char bol)
                      (current-indentation)))
           (line (line-number-at-pos bol)))
      (setq f90-ts--continued-line-cache
            (list (cons line (list bol-col 0)))))))


(defun f90-ts--continued-line-cache-put-subsequent (bol anchor offset)
  "Compute and store the indent delta for a subsequent line at BOL.
ANCHOR is the anchor position returned by the anchor function.
OFFSET is the offset from that anchor.
Resolves delta via the anchor's cache entry:
  delta = delta-at-anchor-line + OFFSET"
  (unless f90-ts--align-continued-variant-tab
    (let* ((anchor-line (line-number-at-pos anchor))
           (anchor-entry (cdr (assq anchor-line f90-ts--continued-line-cache)))
           (anchor-delta (if anchor-entry (cadr anchor-entry) 0))
           (bol-current  (f90-ts--indentation-at-pos bol))
           (bol-new (f90-ts--column-number-at-pos (+ anchor anchor-delta offset)))
           (delta (- bol-new bol-current))
           (line (line-number-at-pos bol)))
      (push (cons line (list bol-current delta))
            f90-ts--continued-line-cache))))



(defun f90-ts--continued-line-cache-get-first ()
  "Find entry for first line (smallest line number) in the cache.
Cache is reverese ordered, so we can simply return the last entry."
  ;; cache is constructed by push, the last entry is the first line
  (car (last f90-ts--continued-line-cache)))
  ;; (cl-loop for line-entry in f90-ts--continued-line-cache
  ;;          for line-min = line-entry then (if (< (car line-entry) (car line-min))
  ;;                                             line-entry
  ;;                                           line-min)
  ;;        finally return line-min)


(defun f90-ts--continued-line-cache-update (first-pos)
  "Check whether first line at FIRST-POS has been flush.
This is the case if current bol and cached bol are different.
If it has, update the entry and apply delta to all other cached lines.
Argument FIRST-POS is used to jump to this line efficiently (jumping
to a line is more expensive)."
  (unless f90-ts--align-continued-variant-tab
    (let* ((first (f90-ts--continued-line-cache-get-first))
           (first-line (car first))
           (first-bol-col (cadr first))
           (first-bol-current (f90-ts--indentation-at-pos first-pos))
           (first-delta (- first-bol-current first-bol-col)))
      ;; indentation of first line has been flushed, add delta to all
      ;; other lines
      (when (/= first-bol-col first-bol-current)
        (setq f90-ts--continued-line-cache
              (seq-map (lambda (entry)
                         (let ((line (car entry))
                               (bol-col (cadr entry))
                               (delta (caddr entry)))
                           (cons line
                                 (if (= line first-line)
                                     (list first-bol-current 0)
                                   (list bol-col (+ delta first-delta))))))
                       f90-ts--continued-line-cache))))))


(defun f90-ts--continued-line-cache-get-col (node)
  "Return the final column of NODE in a continued statement.
NODE is a potential anchor on some previous line, for which indentation
has already been computed.
Lookup the cache for the line node is placed on.  If current and cached
indentation is equal, then apply cached delta to current column,
otherwise return column as is."
  (let* ((pos (treesit-node-start node))
         (col (f90-ts--column-number-at-pos pos)))
    (if f90-ts--align-continued-variant-tab
        ;; line based indentation, no caching
        col
      (let* ((line (f90-ts--node-line node))
             (entry (cdr (assq line f90-ts--continued-line-cache)))
             (delta (cadr entry))
             (bol-col (car entry))
             (bol-current (f90-ts--indentation-at-pos pos)))
        (cl-assert entry nil "cache-get-col: no entry for line in cache")
        (if (= bol-col bol-current)
            ;; not yet flushed
            (+ col delta)
          col)))))


;;++++++++++++++
;; matchers

(defun f90-ts--populate-cache (node parent _bol &rest _)
  "Always fail, but prepare the indent cache.
This dummy matcher should be the very first rule to be executed to reset the
cache for a new indentation run.
The cache stores NODE and PARENT and prepares further internal values."
  (f90-ts--indent-ensure-cache node parent)
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
    (when-let ((psibp (f90-ts--indent-prev-sib-by-parent)))
      ;; prev-sib-by-parent already excludes comment node, hence we can directly
      ;; check whether there is an ampersand
      (f90-ts--node-type-p psibp "&")))))


(defun f90-ts--openmp-comment-is (node _parent _bol &rest _)
  "Succeed if NODE is an OpenMP comment."
  (and (f90-ts--node-type-p node "comment")
       (f90-ts-openmp-node-p node)))


(defun f90-ts--preproc-node-is (node _parent _bol &rest _)
  "Succeed if NODE a fortran preprocessor statement."
  (and node (f90-ts-preproc-node-p node)))


(defun f90-ts--preproc-at-toplevel-is (_node parent _bol &rest _)
  "Succeed if PARENT is a toplevel preprocessor node.
It is a toplevel node if its first non-preprocessor ancestor is a toplevel
node (like program, module, etc.)."
  ;; Content inside preprocessor:
  ;; - Search up the tree for the first "Ancestor" that is NOT a
  ;;   preprocessor node.
  ;; - If that ancestor is a module or program -> toplevel indent
  (and parent
       (f90-ts-preproc-node-p parent)
       (let ((ancestor (treesit-parent-until
                        parent
                        (lambda (n)
                          (not (f90-ts-preproc-node-p n))))))
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
  ;; when within continued line statement, then cache anchor offset
  (when (f90-ts--continued-subsequent-line-is node parent bol)
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
Comments are classified by rules in `f90-ts-special-comment-rules' (including
both matching none=default comment).  The operation requires PARENT and BOL
for possible caching of anchor and offset values."
  (when (f90-ts--node-type-p node "comment")
    (when-let* ((prev-sib (treesit-node-prev-sibling node))
                (prev-line (f90-ts--first-node-on-line
                            (treesit-node-start prev-sib))))
      (when (and (f90-ts--node-type-p prev-line "comment")
                 (eq (f90-ts--comment-matching-rule node)
                     (f90-ts--comment-matching-rule prev-line)))
        (let ((anchor (treesit-node-start prev-line)))
          (f90-ts--cache-anchor-offset node parent bol anchor 0)
          ;; signal match
          t)))))


(defun f90-ts--n-p-pstmtk (type-n type-p type-pstmtk)
  "Succeed if types of nodes match provided types.
TYPE-N, TYPE-P and TYPE-PSTMTK are expected to be regular expressions
or nil.  If nil, everything is matched, hence the type of the corresponding
node is ignored.
TYPE-N is matched against type of (cached) node,
TYPE-P is matched against type of (cached) parent and
TYPE-PSTMTK is matched against type of (cached) pstmtk, which is
the previous statement keyword node."
  (lambda (node parent _bol &rest _)
    (let ((pstmt-k (f90-ts--indent-prev-stmt-keyword)))
      (and (f90-ts--node-type-match-p node type-n)
           (f90-ts--node-type-match-p parent type-p)
           (f90-ts--node-type-match-p pstmt-k type-pstmtk)))))


(defun f90-ts--n-p-ch-psibp (type-n type-p type-ch type-psibp)
  "Succeed if types of nodes match provided types.
TYPE-N, TYPE-P TYPE-CH and TYPE-PSIBP are expected to be regular expressions
or nil.  If nil, everything is matched, hence the type of the corresponding
node is ignored.
TYPE-N is matched against type of (cached) node,
TYPE-P is matched against type of (cached) parent,
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

(defun f90-ts--previous-stmt-anchor (_node _parent bol &rest _rest)
  "Return anchor at previous statements indentation or BOL.
If no previous statement can be found (for example at start of buffer),
then BOL is returned as anchor position."
  (if-let ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
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


(defun f90-ts--toplevel-offset (_node parent _bol &rest _rest)
  "Return offset for stuff right below top level nodes.
If inside module, submodule or program then return `f90-ts-indent-toplevel',
otherwise return `f90-ts-indent-contain'.
Deciding which of the two cases is present requires to access grandparent as
well as grand-grandparent, which are obtained via PARENT."
  (let* ((grandparent (treesit-node-parent parent))
         (ggparent    (and grandparent (treesit-node-parent grandparent))))
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
;; anchor functions: lists on continued lines
;; rotation logic

(defun f90-ts--align-node-symbol (node)
  "Return a symbol for NODE to group and select nodes for alignment selection.
Nodes for alignment are mostly selected among nodes with the same symbol.
If NODE is nil return nil."
  (when node
    (cond
     ((f90-ts--node-type-p node "ERROR")
      'error)

     ((f90-ts--node-type-p node "comment")
      'comment)

     ((f90-ts--node-type-p node "unary_expression")
      ;; is this the right approach and symbol?
      ;; (not that of unary expressions, node is not the operator
      ;; but the unary_expression node itself)
      'operator-unary)

     ((string= (treesit-node-field-name node) "operator")
      (let* ((parent (treesit-node-parent node))
             (type (and parent (treesit-node-type parent))))
      (pcase type
        ("logical_expression"        'operator-logical)
        ("math_expression"           'operator-math)
        ("relational_expression"     'operator-relation)
        ;; map concatenation to math_expression, as user defined
        ;; operators are also handled by that way
        ("concatentation_expression" 'operator-math)
        ;("unary_expression"          'operator-unary) ; can this happen here
        (_                           'operator) ; anything still missing?
        )))

     ((f90-ts--node-type-p node "&")
      ;; text is not always "&" (like virtual ampersand at beginning of line)
      'ampersand)

     (t
      (let ((text (treesit-node-text node)))
        (pcase text
          ("("  'parenthesis)
          (")"  'parenthesis)
          (","  'comma)
          ("&"  'ampersand)
          ("=>" 'associate)
          ("="  'assignment)
          ;; default: argument for anything else (this is probably a named node
          ;; of type identifier, number_literal, call_expression etc.
          (_    'named)))))))


(defun f90-ts--align-list-location (node)
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
   #'f90-ts--node-is-op-expr-p))


;;++++++++++++++
;; list context: association_list

(defun f90-ts--align-list-expand-assoc (nodes)
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


(defun f90-ts--align-list-items-assocation (list-context _loc)
  "Determine relevant childrens of LIST-CONTEXT = \"association_list\"."
  (cl-assert (f90-ts--node-type-p list-context "association_list")
             nil "expected list context: association_list, got '%s'" list-context)
  ;; expand it, as the it contains a list of nodes, whose children are required
  (when-let ((children (treesit-node-children list-context)))
    (f90-ts--align-list-expand-assoc children)))


;;++++++++++++++
;; list context: operator expressions

(defun f90-ts--align-list-expand-op-expr (node)
  "Recursively compute relevant children of a logical expression tree NODE.
An expression like (A and B and C) is represented as
logical_expression(logical_expression(A and B) and C).
The routine returns the five children A, and, B, and, C.
It does not descend into parenthesized_expressions."
  (if (f90-ts--node-is-op-expr-p node)
      (mapcan #'f90-ts--align-list-expand-op-expr
              (treesit-node-children node))
    (list node)))


(defun f90-ts--align-list-items-op-expr (list-context _loc)
  "Determine relevant children of LIST-CONTEXT being some operator expression."
  (cl-assert (f90-ts--node-is-op-expr-p list-context)
             nil
             "expected list context: some expression node, got list-context '%s'"
             list-context)
  ;; assert that we are already at the root of an op-expr chain
  (cl-assert (eq list-context
                 (f90-ts--node-op-expr-chain-root list-context))
             nil
             "list context not root of chain of op-expr, list-context is '%s'"
             list-context)
  (f90-ts--align-list-expand-op-expr list-context))


;;++++++++++++++
;; list context: parameters

(defun f90-ts--align-list-items-parameters (list-context _loc)
  "Determine relevant childrens of LIST-CONTEXT = \"parameters\"."
  (cl-assert (f90-ts--node-type-p list-context "parameters")
             nil "expected list context: parameters, got '%s'" list-context)
  ;; this also includes the opening and closing parenthesis
  (treesit-node-children list-context))


;;++++++++++++++
;; list context: binding_list, final_statement

(defun f90-ts--align-list-items-binding (list-context _loc)
  "Determine relevant children of LIST-CONTEXT of binding type.
These are nodes of type \"binding_list\" or \"final_statement\".  Both occur
in the contains part of a derived type definition."
  (cl-assert (or (f90-ts--node-type-p list-context "binding_list")
                 (f90-ts--node-type-p list-context "final_statement"))
             nil "expected list context: binding_list or final_statement, got '%s'" list-context)
  (when-let ((children (treesit-node-children list-context)))
    ;; drop the (binding_name ...) part, and the => binding symbol
    (seq-drop children 2)))


;;++++++++++++++
;; list context: variable_declarations

(defun f90-ts--align-list-items-var-decl (list-context loc)
  "Determine relevant children of LIST-CONTEXT = \"variable_declaration\".
Use LOC to determine whether attribute or declarator items are relevant (those
before and after \"::\")."
  (cl-assert (f90-ts--node-type-p list-context "variable_declaration")
             nil "expected list context: variable_declaration, got '%s'" list-context)
  (when-let ((children (treesit-node-children list-context)))
    (let* ((pos (alist-get 'pos loc))
           (attr-children (seq-filter (lambda (n)
                                        (string= (treesit-node-field-name n) "attribute"))
                                      children))
           (decl-children (seq-filter (lambda (n)
                                        (string= (treesit-node-field-name n) "declarator"))
                                      children))

           ;;(attr-end (seq-max (seq-map (lambda (child) (treesit-node-end child))
           ;;                            attr-children)))
           (decl-start (seq-min (seq-map (lambda (child) (treesit-node-start child))
                                         decl-children))))
      (if (< pos decl-start)
          attr-children
        decl-children))))


;;++++++++++++++
;; list context: arguments

(defun f90-ts--align-list-items-arguments (list-context _loc)
  "Determine relevant children of LIST-CONTEXT = \"argument_list\"."
  (cl-assert (f90-ts--node-type-p list-context "argument_list")
             nil "expected list context: argument_list, got '%s'" list-context)
  (treesit-node-children list-context))


;;++++++++++++++
;; additional anchor for aligned lists:

(defmacro f90-ts--collecting-anoff (&rest body)
  "Execute BODY with `collect' available to accumulate (anchor offset) pairs.
Pairs can be collected either by executing \"(collect anchor offset)\"
 or \"(collect anoff-pair)\".

In this context, anchor must be a buffer position and offset an integer offset.
Collecting nodes instead of their start position is not supported here.

The macro deals with nil as pair input as well, so using it like
\"(collect (pair-or-nil ...))\" is valid."
  (let ((acc (gensym "acc")))
    `(let ((,acc nil))
       (cl-flet ((collect (arg1 &optional arg2)
                   (when arg1
                     (push (if arg2 (list arg1 arg2) arg1)
                           ,acc))))
         ,@body
         ;; reverse order, the first time collected is used as primary,
         ;; it must be the first in the final list
         (nreverse ,acc)))))


(defun f90-ts--align-list-pstmt1-anoff ()
  "Return the standard continued line indentation.
This is start position of pstmt-1 as anchor and `f90-ts-indent-continued'
as offset."
  (let ((pstmt-1 (f90-ts--indent-prev-stmt-first)))
    (list (treesit-node-start pstmt-1)
          f90-ts-indent-continued)))


(defun f90-ts--align-list-smallest-anoff (items)
  "Return the item with the smallest column number in a list of ITEMS.
ITEMS is not asssumed to be sorted.  An item is a pair (anchor offset).
For this routine it is assumed that anchor is a node (in general it is a
node or a buffer position)."
  (car (seq-sort-by #'f90-ts--continued-line-cache-get-col
                    #'<
                    items)))


(defun f90-ts--align-list-other-default (_list-context items _loc)
  "Return a list of default/fallback alignment positions (anchors offset).
These depend on LIST-CONTEXT, NODE-SYM and whether ITEMS has any nodes.
The first entry of the \"other\" list is used as primary anchor.
If ITEMS has entries, then return the entry with the smallest column.
If ITEMS is empty, return (pos-of-pstmt-1 f90-ts-indent-continued) for default
continued line indentation ."
  (f90-ts--collecting-anoff
   (if items
        ;; find item with smallest column number (used as primary one)
        (collect (f90-ts--align-list-smallest-anoff items))
     (collect (f90-ts--align-list-pstmt1-anoff)))))


(defun f90-ts--align-list-other-tuple (list-context items loc)
  "Return a list of default alignmnet positions (anchors offset) for tuples.
These anchros depend on LIST-CONTEXT, node-sym (nsym in LOC) and whether
ITEMS has any nodes.

For argument lists (call sub(...)) and parameters (subroutine sub (...)),
a default and fallback position is to align with initial opening parenthesis
plus offset.  This primary position is returned as first element of the list."
  ;; for argument_list, there should always be the opening parenthesis
  (cl-assert (or (f90-ts--node-type-p list-context "argument_list")
                 (f90-ts--node-type-p list-context "parameters"))
             nil
             "expected list context: argument_list or parameters, got '%s'"
             list-context)

  (f90-ts--collecting-anoff
   ;; for closing parenthesis, align to opening parenthesis,
   ;; for other node types, align one position to the right of it
   (collect (treesit-node-start list-context)
            (if (eq (alist-get 'nsym loc) 'parenthesis)
                f90-ts-indent-paren-close
              f90-ts-indent-paren-default))

   ;; add default continued line indentation if items is empty
   (unless items
     (collect (f90-ts--align-list-pstmt1-anoff)))))


(defun f90-ts--align-list-other-op-expr (list-context items loc)
  "Return a list of default positions (anchors offset) for expressions.
These depend on LIST-CONTEXT, node-sym (nsym in LOC) and whether ITEMS has
any nodes.

For logical expressions a default and fallback position is to align with the
start of the expression.  This primary position is returned as first element
of the list."
  ;; for argument_list, there should always be the opening parenthesis
  (cl-assert (f90-ts--node-is-op-expr-p list-context)
             nil
             "expected list context: some expression, got '%s'"
             list-context)

  (f90-ts--collecting-anoff
   ;; use as primary anchor if item of compatible types are available
   (when items
     (collect (f90-ts--align-list-smallest-anoff items)))

   ;; for "(a .and. b & ...)", the expression starts at a,
   ;; first check previous sibling of list-context to see whether we
   ;; can align with that, which often is an opening parenthesis
   ;; or an equal sign in assignment statements;
   ;; use as primary anchor if no items of compatible type are available
   (when-let* ((psib-context (f90-ts--prev-sibling-proper list-context))
               (psib-type (treesit-node-type psib-context)))
     (pcase psib-type
       ;; "(": parent is parenthesized_expression or argument_list
       ;; "=": parent is assignment_statement
       ("(" (let* ((node-sym (alist-get 'nsym loc))
                   (offset (if (eq node-sym 'parenthesis)
                               f90-ts-indent-paren-close
                             f90-ts-indent-paren-default)))
              (collect (treesit-node-start psib-context) offset)))
       ("=" (let* ((node-sym (alist-get 'nsym loc))
                   (offset (if (member node-sym '(operator-logical
                                                  operator-math))
                               f90-ts-indent-expr-assign-assoc-op
                             f90-ts-indent-expr-assign-default)))
              (collect (treesit-node-start psib-context) offset)))))

   ;; add default continued line indentation if items list is empty
   (unless items
     (collect (f90-ts--align-list-pstmt1-anoff)))))


(defun f90-ts--align-list-other-association (list-context items loc)
  "Return a list of default positions (anchors offset) for association nodes.
These depend on LIST-CONTEXT, node-sym (nsym in LOC) and whether ITEMS has
any nodes.

For association lists (associate(x => y, ...)), a default and fallback
position is to align with the initial opening parenthesis plus some offset.
However, if node is the =>, then apply indent with
`f90-ts-indent-continued' relative to opening parenthesis.
This primary position is returned as first element of the list."
  ;; for argument_list, there should always be the opening parenthesis
  (cl-assert (f90-ts--node-type-p list-context "association_list")
             nil
             "expected list context: association_list, got '%s'"
             list-context)

  (f90-ts--collecting-anoff
   (when items
     (collect (f90-ts--align-list-smallest-anoff items)))

   (let ((nsym (alist-get 'nsym loc))
         (child-paren (treesit-node-child list-context 0 nil)))
     (cl-assert (f90-ts--node-type-p child-paren "(")
                nil
                "expected opening parenthesis as child-0, got '%s'"
                child-paren)
     (collect (treesit-node-start child-paren)
              (pcase nsym
                ('associate   f90-ts-indent-continued)     ; indents "=>"
                ('parenthesis f90-ts-indent-paren-close)   ; indents ")"
                (_            f90-ts-indent-paren-default) ; all other kind of nodes
                )))

   ;; add default continued line indentation if items is empty
   (unless items
     (collect (f90-ts--align-list-pstmt1-anoff)))))


(defun f90-ts--align-list-other-var-decl (_list-context items loc)
  "Return a list of default/fallback alignment positions (anchors offset).
The list context is a variable_declaration.  For this, the continued line
position is always added.
If ITEMS has entries, then return the entry with the smallest column as
primary anchor.
Always return (pos-of-pstmt-1 f90-ts-indent-continued) for default continued
line indentation ."
  (let* ((smallest-anoff (f90-ts--align-list-smallest-anoff items))
         (psibp (f90-ts--indent-prev-sib-by-parent))
         (prev (if (f90-ts--node-type-p psibp "&")
                   (treesit-node-prev-sibling psibp)
                 psibp)))
    (f90-ts--collecting-anoff
     (collect smallest-anoff)
     (when (f90-ts--node-type-p prev "::")
       (collect (treesit-node-start prev)
                ;; add one because "::" as two and not one character as "="
                f90-ts-indent-declaration))
     ;; always add default continued line indentation
     (collect (f90-ts--align-list-pstmt1-anoff)))))


;;++++++++++++++
;; list context: items and columns

(defun f90-ts--align-list-filter-items (items node-sym)
  "From a list ITEMS of node items select compatible nodes.
Currently these are nodes with the same NODE-SYM."
  (when node-sym
    (let ((pred-node-sym
           (lambda (n) (eq (f90-ts--align-node-symbol n)
                           node-sym))))
      (seq-filter pred-node-sym
                  items))))


(defun f90-ts--align-list-map-col-pos (anchor)
  "Map an ANCHOR to a triple (column position offset).
ANCHOR is either a node or an otherwise obtained pair (position offset), .
For a node, position is start of node and offset is zero."
  (if (treesit-node-p anchor)
      (list
       ;; some node, compute column number and buffer position, offset=0
       (f90-ts--continued-line-cache-get-col anchor)
       (treesit-node-start anchor)
       0)
    ;; a pair (buffer position, offset) add column
    ;; compute column at position and then add offset
    ;; (do not compute column at position + offset, this might not be valid)
    (cons
     (+ (f90-ts--column-number-at-pos (car anchor)) (cadr anchor))
     anchor)))


(defun f90-ts--align-list-cp-sort (cp-alist)
  "Make CP-ALIST of (column position offset) triples unique by column position.
For several entries with same column number, take the element with the largest
buffer position."
  (let ((cp-alist-unique
         (seq-map (lambda (group)
                    ;; elements produced by seq-group-by are:
                    ;; group = (col (col pos1 offset1) (col pos2 offset2) ...)
                    ;; ((cdr group) gets rid of initial col)
                    (cl-reduce (lambda (cp1 cp2)
                                 (if (> (cadr cp1) (cadr cp2)) cp1 cp2))
                               (cdr group)))
                  (seq-group-by #'car cp-alist))))
    (seq-sort (lambda (a b) (< (car a) (car b))) cp-alist-unique)))


(defun f90-ts--align-list-select (variant cur-col primary items)
  "Select (anchor offset) from PRIMARY and ITEMS.
Depending on VARIANT and current column CUR-COL, select the relevant
anchor and offset.

PRIMARY is a default/fallback anchor (position offset).  Depending on
VARIANT (like keep-or-primary) and current alignment, PRIMARY is
selected."
  ;; note that entries in col-pos are triples
  ;; cp = (column, buffer position, offset),
  ;; where buffer position must be start of a previous node to ensure
  ;; that treesitter buffering in indent-region works as expected,
  (let* ((col-pos-unsorted (seq-map #'f90-ts--align-list-map-col-pos items))
         (col-pos (f90-ts--align-list-cp-sort col-pos-unsorted))
         (aligned-at (seq-find (lambda (cp) (= cur-col (car cp)))
                               col-pos))
         (primary-col-pos (f90-ts--align-list-map-col-pos primary)))
    ;; :get-other-fn should always return some fallback position
    ;; (like pstmt-1+default indent for continued lines), and thus
    ;; there must always be some anchors in col-pos
    (cl-assert col-pos nil "no relevant columns found")

    ;; the selection process selects a triple, drops the column and returns
    ;; the two remaining elements (buffer position, offset)
    (cond
     ;; special case (e.g. after inserting newline by <return> or f90-line-break)
     ;; check whether we are before first entry in col-pos,
     ;; if this is the case we go to primary, not to first entry in col-pos
     ((< cur-col (caar col-pos))
      (cdr primary-col-pos))

     ;; cases: (aligned, rotate), (not-aligned, rotate),
     ;;        (not-aligned,keep-or-rotate)
     ((or (eq variant 'rotate)
          (and (not aligned-at)
               (eq variant 'keep-or-rotate)))
      ;; go to next column or wrap around,
      ;; recall that col-pos is sorted by columns
      (let ((aligned-next (seq-find (lambda (cp) (< cur-col (car cp)))
                                    col-pos)))
        ;; next if there is a next, otherwise first entry (not necessarily primary)
        (or (cdr aligned-next)
            (cdar col-pos))))

     ;; cases: (not-aligned, keep-or-primary),
     ;;        (aligned, primary), (not-aligned, primary)
     ((or (not aligned-at)
          (eq variant 'primary))
      (cdr primary-col-pos))

     ;; cases: (aligned, keep-or-primary)
     ;;        (aligned, keep-or-rotate)
     ((and aligned-at
           (member variant '(keep-or-primary
                             keep-or-rotate)))
      ;; aligned, keep current column, but use proper element from col-pos
      ;; as anchor, otherwise indent-region does not take indentation of anchor
      ;; position into account
      (cdr aligned-at))

     (t
      ;; all eight cases plus node before minimal column are covered above
      (cl-assert col-pos nil "cond logic not complete")
      (cdr primary-col-pos)))))


(defun f90-ts--align-list-anoff-items (loc list-context)
  "Filter relevant alignment anchors from LIST-CONTEXT.
LOC is an alist which provides the node and further location data.
Note: for anchor-offset pairs extracted from LIST-CONTEXT, the offset is 0 in
general."
  ;; parenthesis are handled by anoff-other, as we want to
  ;; add a custom offset and use it as primary anchor
  (unless (eq (alist-get 'nsym loc)
              'parenthesis)
    (let* ((get-items (f90-ts--get-list-context-prop :get-items-fn list-context))
           (items-all (funcall get-items list-context loc))
           ;; filter by line number, use only items on some previous line
           (cur-line (alist-get 'line loc))
           (items-prev (seq-filter
                        (lambda (n) (< (f90-ts--node-line n)
                                       cur-line))
                        items-all)))
      ;; further filter by symbol type node-sym of current node at point
      (f90-ts--align-list-filter-items items-prev
                                       (alist-get 'nsym loc)))))


(defun f90-ts--align-list-anoff-other (loc anchors-context list-context)
  "Return other relevant anchor-offset pairs for alignment in LIST-CONTEXT.
ANCHORS-CONTEXT are anchor-offset items already obtained by
`f90-ts--align-list-anoff-items'.  These might determine which alternative
alignment positions to select.
LOC provides node and location data."
  (let ((get-other (or (f90-ts--get-list-context-prop :get-other-fn list-context)
                       #'f90-ts--align-list-other-default)))
    (funcall get-other
             list-context
             anchors-context
             loc)))


(defun f90-ts--align-list-anchor-offset (variant loc list-context)
  "Determine a pair (anchor offset) for alignment of a node in a list context.
Location data is given as an alist LOC containing node, column, line number and
node symbol.
To this end items on continued lines in the provided LIST-CONTEXT are
determined.  Additionally further anchors (like the first node of previous
statement by pstmt-1) are considered.
Finally use VARIANT to select one pair to align with."
  (let* ((anoff-items (f90-ts--align-list-anoff-items loc
                                                      list-context))
         (anoff-other (f90-ts--align-list-anoff-other loc
                                                      anoff-items
                                                      list-context))
         ;; if selected, add default continued offset position as anchor
         (anoff-extra (and f90-ts-indent-list-always-include-default
                           (f90-ts--align-list-pstmt1-anoff)))
         ;; anoff-primary is used as 'primary' in keep-or-primary, primary etc.
         (anoff-primary (car anoff-other))
         ;; final list of anchors (which are nodes or pairs (position offset))
         (anoff-final (append (and anoff-extra (list anoff-extra))
                              anoff-other
                              anoff-items)))
    (f90-ts--align-list-select variant
                               (alist-get 'col loc)
                               anoff-primary
                               anoff-final)))


;;++++++++++++++
;; anchor functions: continued lines
;; determine whether list or standard case

;; get-items: function to determine node items in the list context
;;            relevant for alignment
;; get-other: other columns for alignment (default and fallback values),
;;            must return a non-empty list of anchors, the primary anchor
;;            in the list is used as primary/fallback position (for example
;;            in keep-or-primary option)
(defconst f90-ts--align-list-context-config
  (let ((expr-options '(:get-items-fn f90-ts--align-list-items-op-expr
                        :get-other-fn f90-ts--align-list-other-op-expr))
        (bind-options '(:get-items-fn f90-ts--align-list-items-binding)))
     (list
      (cons "logical_expression"       expr-options)
      (cons "math_expression"          expr-options)
      (cons "relational_expression"    expr-options)
      (cons "concatenation_expression" expr-options)
      (cons "unary_expression"         expr-options)
      (cons "binding_list"             bind-options)
      (cons "final_statement"          bind-options)
      (cons "argument_list"
            '(:get-items-fn f90-ts--align-list-items-arguments
              :get-other-fn f90-ts--align-list-other-tuple))
      (cons "parameters"
            '(:get-items-fn f90-ts--align-list-items-parameters
              :get-other-fn f90-ts--align-list-other-tuple))
      (cons "association_list"
            '(:get-items-fn f90-ts--align-list-items-assocation
              :get-other-fn f90-ts--align-list-other-association))
      (cons "variable_declaration"
            '(:get-items-fn f90-ts--align-list-items-var-decl
              :get-other-fn f90-ts--align-list-other-var-decl))))
  "List of tree-sitter node types presenting some kind of list context.
A list context is a node with children which are suitable for alignment if
spread over several lines in a continued line statement.
The value of the alist provides properties to deal with such list contexts,
in particular for how to extract relevant alignment positions.")


(defun f90-ts--get-list-context-prop (pkey list-context)
  "Lookup LIST-CONTEXT and return the property value for PKEY.
Lookup is done in alist `f90-ts--align-list-context-config'."
  (let ((properties (alist-get (treesit-node-type list-context)
                               f90-ts--align-list-context-config
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
         (stmt-root (treesit-node-on (treesit-node-start ps-key)
                                     pos)))
    (treesit-search-subtree
     stmt-root
     (lambda (n)
       (and (<= (f90-ts--node-line n) line)
            (<= line (line-number-at-pos (treesit-node-end n)))
            (assoc (treesit-node-type n)
                   f90-ts--align-list-context-config))))))


(defun f90-ts--align-list-context-op-expr (loc parent)
  "Determine, whether LOC is in an operator expression list context.
For expressions, we need to check node in LOC or PARENT (node might be
nil).  But we do not need to ascend further."
  ;; always looking at PARENT fails for expressions like in
  ;; x = &
  ;;     y
  ;; z = ( &
  ;;      u)
  ;; in both cases, node is already the root of the expression chain,
  ;; looking at parent is wrong
  (let* ((node (alist-get 'node loc))
         (line (alist-get 'line loc))
         (stmt-min
          (or (and (f90-ts--node-is-op-expr-p node)
                   node)
              (and (f90-ts--node-is-op-expr-p parent)
                   parent)))
         ;; find root of expression
         (expr-root (and stmt-min (f90-ts--node-op-expr-chain-root stmt-min)))
         (psib-root (and expr-root
                         (f90-ts--prev-sibling-proper expr-root))))
    ;; if expr-root is on the same line, return this as list-context,
    ;; however, if the previous sibling is on a previous line
    ;; and is "(" or "=" (which starts the context and is used for
    ;; alignment, see the two examples above), then also return it
    ;; otherwise return nil
    (when (and expr-root
               (or (< (f90-ts--node-line expr-root) line)
                   (and (< (f90-ts--node-line psib-root) line)
                        (f90-ts--node-type-p psib-root '("(" "=")))))
      expr-root)))


(defun f90-ts--align-list-context-other (loc parent)
  "Determine, whether LOC is in a list-context, other than expressions.
Use node from LOC or PARENT to start the search, which might require to
ascend a few steps."
  (let* ((node (or (alist-get 'node loc)
                   parent))
         (line (alist-get 'line loc))
         (list-context (treesit-parent-until
                        node
                        (lambda (n)
                          (and (< (f90-ts--node-line n) line)
                               (assoc (treesit-node-type n)
                                      f90-ts--align-list-context-config)))
                        t ; include node in search
                        )))
    ;; list-context might be of some operator expression type, but
    ;; neither node nor parent are (already excluded), so we are at
    ;; some other kind of node like a call_expression or
    ;; parenthesized_expression, which are allowed within expressions,
    ;; but this is another context
    (unless (f90-ts--node-is-op-expr-p list-context)
      list-context)))


(defun f90-ts--align-list-context (loc parent)
  "Check whether LOC and PARENT are within a list context and return the node.
Often the list context is PARENT, but sometimes a related node like
grandparent, etc.
The smallest such context, starting on a previous line is returned.
Return value nil signals that position is not within a list context."
  (let ((stmt-max (f90-ts--align-list-context-max loc)))
    (when stmt-max
      (or (f90-ts--align-list-context-op-expr loc parent)
          (f90-ts--align-list-context-other loc parent)))))


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
    (cl-assert pstmt-1 nil "continued subsequent line must have a pstmt-1 node")
    (f90-ts--continued-line-cache-update
     (treesit-node-start pstmt-1))
    (let* ((loc (f90-ts--align-list-location node))
           (default-anchor-offset (if pstmt-1
                                     (list (treesit-node-start pstmt-1)
                                           f90-ts-indent-continued)
                                   (list bol 0)))
           (anchor-offset
            (if (or (eq variant 'continued-line)
                    (not pstmt-1))
                default-anchor-offset
              (let ((list-context (f90-ts--align-list-context loc parent)))
                (if list-context
                    (f90-ts--align-list-anchor-offset variant
                                                      loc
                                                      list-context)
                  ;; default continued line indentation
                  default-anchor-offset)))))
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
;; debug stuff

(defun f90-ts--fail-info-is (msg)
  "Always fail as indentation matcher, but print a separator line.
Additionally if MSG=\"start\" or \"catch all\" print detailed data of position
and nodes for debugging purposes into the exclusive log buffer."
  (lambda (node parent bol &rest _)
    (f90-ts-log :indent "---------info %s--------------" msg)
    (when (or (string= msg "start") (string= msg "catch all"))
      (let* ((grandparent (and parent (treesit-node-parent parent)))
             (psibp (f90-ts--indent-prev-sib-by-parent))
             (pstmt-k (f90-ts--indent-prev-stmt-keyword))
             (child0 (f90-ts--indent-child0)))

        (f90-ts-log :indent "position: point=%d, bol=%d, lbp=%d, line=%d"
                    (point) bol (line-beginning-position) (line-number-at-pos))
        (let ((tttttt (format "types n-p-gp-psibp-pstmtk-ch = %s, %s, %s, %s, %s, %s"
                              (and node (treesit-node-type node))
                              (and parent (treesit-node-type parent))
                              (and grandparent (treesit-node-type grandparent))
                              (and psibp (treesit-node-type psibp))
                              (and pstmt-k (treesit-node-type pstmt-k))
                              (and child0 (treesit-node-type child0)))))
          (f90-ts-log :indent (propertize tttttt 'face '(:foreground "brown2")))
          (f90-ts--indent-cache-print))))
    nil))


(defun f90-ts-indent-rules-info (msg)
  "Create an indentation rule, which never matches, but prints a MSG header.
If MSG is \"start\" or \"catch all\" print additional position and node info.

Used as \",@(f90-ts-indent-rules-info message\"') in indentation rules."
  `(;; for testing purposes
    ((f90-ts--fail-info-is ,msg) parent 0)))


;;++++++++++++++
;; simple indentation rules: expansion with f90-ts-- prefix

(defmacro f90-ts--map-rule (matcher anchor offset)
  "Map an indent rule triple with f90-ts-- prefix prepended were applicable.
MATCHER is either a bare symbol (direct matcher function) or a list
whose car is a constructor name.  ANCHOR and OFFSET are bare symbols or
literals.  Any symbol present in an internal list are prefixed, the rest is
passed through unchanged."
  (let ((prefix-syms '(;; matcher constructor
                       n-p-pstmtk
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
                       ;; anchor
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
Each element of TRIPLES is a (MATCHER ANCHOR OFFSET) form as accepted
by `f90-ts--map-rule'.  Returns a list of all expanded triples suitable for
use as the body of a `defvar' ruleset."
  `(list ,@(mapcar (lambda (triple)
                     `(f90-ts--map-rule ,(nth 0 triple)
                                        ,(nth 1 triple)
                                        ,(nth 2 triple)))
                   triples)))


;;++++++++++++++
;; simple indentation rules

(defvar f90-ts-indent-rules-start
  (f90-ts--with-map-rules
   ;; populate cache and then always fail
   (populate-cache parent 0)
   ;; info rules needs populated cache
   ;;,@(f90-ts-indent-rules-info "start")
   )
  "Indentation rules executed at start.
The main purpose is to fill the indentation cache for a new run.")


(defvar f90-ts-indent-rules-comments
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
   (special-comment-is cached-anchor cached-offset))
  "Indentation rules for comments (excluding OpenMP statements).")


(defvar f90-ts-indent-rules-preproc
  (f90-ts--with-map-rules
   ;; indent preprocessor directive.
   ;; directive itself: no indent
   (preproc-node-is column-0 0)
   ;; directive with preproc parent at toplevel
   (preproc-at-toplevel-is grand-parent f90-ts-indent-toplevel)
   ;; other
   ((n-p-gp nil "preproc_.*" nil) grand-parent f90-ts-indent-block))
  "Indentation rules for preprocessor directives.")


(defvar f90-ts-indent-rules-continued
  (f90-ts--with-map-rules
   ;; handle continued lines
   ;; if on first line of a continued statement, reset the continued-line cache
   ;; but always fail;
   ;; (the cache stores offsets on previous lines, this is necessary in
   ;; indent-region operations with buffering)
   (continued-line-cache-start parent 0)
   ;; special case, first node is closing parenthesis means this is a continued line
   ((n-p-gp ")" "parenthesized_expression" nil) parent f90-ts-indent-paren-close)
   ;; it is easy to see whether we are on a continued line (not the first line
   ;; of a multiline statement, only subsequent lines), but handling specific
   ;; cases is not possible with just some simple n-p-gp like matches,
   ;; in particular on conjunction with ERROR cases of incomplete code
   ;;
   ;; moreover, using previous line indentation for all but the first continued line
   ;; does not work in conjunction with list alignment, if a statement has continued
   ;; lines after a list part, for example:
   ;;
   ;; function fun(aa, x1, x2,&
   ;;                  x3, x4, x5) &
   ;;      result(val)
   ;; by how much should result be indented? x3 is not a good anchor!
   (continued-subsequent-line-is f90-ts--continued-line-anchor f90-ts--cached-offset))
  "Indentation rules for continued lines.")


(defvar f90-ts-indent-rules-internal-proc
  (f90-ts--with-map-rules
   ;; contains statements in modules, programs, subroutines or functions,
   ;; no indentation for contains
   ((node-is    "internal_procedures")         parent 0)
   ((parent-is  "internal_procedures")         parent f90-ts--toplevel-offset)
   ((n-p-gp nil "ERROR" "internal_procedures") parent f90-ts--toplevel-offset))
  "Indentation rules for internal_proc node.
This node occurs in conjunction with \"contain\" statements.")


(defvar f90-ts-indent-rules-prog-mod
  (f90-ts--with-map-rules
   ;; program or module interface part (before contains) and end statement
   ;; in all cases: first match node with end_xyz_statement, and then only
   ;; whether parent is xyz, as parent is xyz in both cases
   ((node-is    "end_program_statement")        parent 0)
   ((parent-is      "program\\(_statement\\)?") parent f90-ts--toplevel-offset)
   ((n-p-pstmtk nil "ERROR" "program")          parent f90-ts--toplevel-offset)

   ;; parent-is uses regexp matching, thus use "^module" to avoid that it
   ;; matches "submodule"
   ((node-is        "end_module_statement")      parent 0)
   ((parent-is      "^module\\(_statement\\)?$") parent f90-ts--toplevel-offset)
   ((n-p-pstmtk nil "ERROR" "^module$")          parent f90-ts--toplevel-offset)

   ((node-is        "end_submodule_statement")      parent 0)
   ((parent-is      "^submodule\\(_statement\\)?$") parent f90-ts--toplevel-offset)
   ((n-p-pstmtk nil "ERROR" "^submodule$")          parent f90-ts--toplevel-offset))
  "Indentation rules for program and module nodes.")


(defvar f90-ts-indent-rules-function
  (f90-ts--with-map-rules
   ;; functions and subroutine bodies
   ((node-is    "end_subroutine_statement")       parent 0)
   ((node-is    "end_function_statement")         parent 0)
   ((node-is    "end_module_procedure_statement") parent 0)
   ((parent-is  "subroutine")                     parent f90-ts-indent-block)
   ((parent-is  "function")                       parent f90-ts-indent-block)
   ((parent-is  "module_procedure")               parent f90-ts-indent-block)
   ((n-p-pstmtk nil nil "subroutine")             parent f90-ts-indent-block)
   ((n-p-pstmtk nil nil "function")               parent f90-ts-indent-block)
   ((n-p-pstmtk nil nil "module_procedure")       parent f90-ts-indent-block))
  "Indentation rules for functions and subroutines.")


(defvar f90-ts-indent-rules-translation-unit
  (f90-ts--with-map-rules
   ;; statements related to toplevel subroutine or function statements,
   ;; and ERROR cases (might or might not be toplevel)
   ((n-p-ch-psibp nil "translation_unit" nil "subroutine_statement") parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "ERROR"            nil "subroutine_statement") parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "translation_unit" nil "function_statement") parent f90-ts-indent-block)
   ((n-p-ch-psibp nil "ERROR"            nil "function_statement") parent f90-ts-indent-block)
   ;; rule for translation_unit and module_procedure_statement does not seem possible,
   ;; as module procedure statements are only allowed within contains section
   ((n-p-ch-psibp nil "ERROR"            nil "module_procedure_statement") parent f90-ts-indent-block)
   ((parent-is "translation_unit") column-0 0))
  "Indentation rules related to root node translation_unit.
These occur for functions and subroutines not within a \"contains\" section,
and in case of ERROR nodes with incomplete code.")


(defvar f90-ts-indent-rules-interface
  (f90-ts--with-map-rules
   ;; (abstract) interface bodies
   ((node-is    "end_interface_statement") parent 0)
   ((parent-is  "interface")               parent f90-ts-indent-block)
   ((n-p-pstmtk nil "ERROR" "interface")   parent f90-ts-indent-block))
  "Indentation rules for interface blocks.")


(defvar f90-ts-indent-rules-dtype-enum
  (f90-ts--with-map-rules
   ;; derived type definitions
   ((n-p-gp       "end_type_statement"      "derived_type_definition" nil)                      parent 0)
   ((n-p-ch-psibp "derived_type_procedures" "derived_type_definition" "contains_statement" nil) parent 0)
   ((n-p-ch-psibp "ERROR"                   "derived_type_definition" "contains_statement" nil) parent 0)
   ((parent-is    "derived_type_procedures") parent f90-ts-indent-block)
   ((parent-is    "derived_type_definition") parent f90-ts-indent-block)

   ;; enumeration types: enum and enumeration
   ((node-is    "end_enum_statement")                    parent 0)
   ((node-is    "end_enumeration_type_statement")        parent 0)
   ((parent-is  "^enum\\(eration_type\\)?$")             parent f90-ts-indent-block)
   ((n-p-pstmtk nil "ERROR" "^enum\\(eration_type\\)?$") parent f90-ts-indent-block))
  "Indentation rules for derived type and enumeration type statements.")


(defvar f90-ts-indent-rules-if
  (f90-ts--with-map-rules
   ;; if-then-else statements
   ;; this must be first, as its parent is an if-statement
   ;; but node=nil/something and parent=if_statement is possible (some line after if)
   ((n-p-gp     "end_if_statement" "if_statement"  nil)      parent 0)
   ((n-p-gp     "elseif_clause"    "if_statement"  nil)      parent 0)
   ((n-p-gp     "else_clause"      "if_statement"  nil)      parent 0)
   ((n-p-pstmtk nil                "if_statement"  "if")     parent f90-ts-indent-block) ; line right after if
   ((n-p-pstmtk nil                "if_statement"  "elseif") parent f90-ts-indent-block) ; line right after elseif
   ((n-p-pstmtk nil                "if_statement"  "else")   parent f90-ts-indent-block) ; line right after else, with empty else block
   ((n-p-pstmtk nil                "else_clause"   "else")   parent f90-ts-indent-block) ; line after else, with non-empty else block
   ((n-p-pstmtk nil                "elseif_clause" "elseif") parent f90-ts-indent-block)

   ((n-p-pstmtk "elseif_clause"    "ERROR" "if") previous-stmt-anchor 0)
   ((n-p-pstmtk "elseif"           "ERROR" "if") previous-stmt-anchor 0)
   ((n-p-gp     "elseif_clause"    "ERROR" nil)  previous-stmt-anchor minus-block-offset) ; at elseif line, incomplete

   ((n-p-pstmtk "else_clause"      "ERROR" "if")     previous-stmt-anchor 0)
   ((n-p-pstmtk "else_clause"      "ERROR" "elseif") previous-stmt-anchor 0)
   ((n-p-gp     "else_clause"      "ERROR" nil)      previous-stmt-anchor minus-block-offset) ; at else line, incomplete
   ((n-p-pstmtk "else"             "ERROR" "if")     previous-stmt-anchor 0)
   ((n-p-gp     "else"             "ERROR" nil)      previous-stmt-anchor minus-block-offset)

   ((n-p-pstmtk nil                "ERROR" "if")     previous-stmt-anchor f90-ts-indent-block) ; empty line after if
   ((n-p-pstmtk nil                "ERROR" "elseif") previous-stmt-anchor f90-ts-indent-block) ; empty line after elseif
   ((n-p-pstmtk nil                "ERROR" "else")   previous-stmt-anchor f90-ts-indent-block) ; empty line after else
   )
  "Indentation rules for if-then-else statements.")


(defvar f90-ts-indent-rules-where
  (f90-ts--with-map-rules
   ;; where-elsewhere statements
   ((n-p-gp     "end_where_statement" "where_statement"  nil)         parent 0)
   ((n-p-gp     "elsewhere_clause"    "where_statement"  nil)         parent 0)
   ((n-p-gp     "else_clause"         "where_statement"  nil)         parent 0)
   ((n-p-pstmtk nil                   "where_statement"  "where")     parent f90-ts-indent-block) ; line right after where
   ((n-p-pstmtk nil                   "where_statement"  "elsewhere") parent f90-ts-indent-block) ; line right after elsewhere
   ((n-p-pstmtk nil                   "elsewhere_clause" "elsewhere") parent f90-ts-indent-block) ; line after else, with non-empty else block

   ((n-p-pstmtk "elsewhere_clause"    "ERROR" "where") previous-stmt-anchor 0)
   ((n-p-pstmtk "elsewhere"           "ERROR" "where") previous-stmt-anchor 0)
   ((n-p-gp     "elsewhere_clause"    "ERROR" nil)     previous-stmt-anchor minus-block-offset) ; at elsewhere line, incomplete
   ((n-p-gp     "elsewhere"           "ERROR" nil)     previous-stmt-anchor minus-block-offset)

   ((n-p-pstmtk nil                "ERROR" "where")     previous-stmt-anchor f90-ts-indent-block) ; empty line after where
   ((n-p-pstmtk nil                "ERROR" "elsewhere") previous-stmt-anchor f90-ts-indent-block) ; empty line after elsewhere
   )
  "Indentation rules for else-elsewhere statements.")


(defvar f90-ts-indent-rules-single-region
  (f90-ts--with-map-rules
   ;; structures with a single region block and linear execution
   ((n-p-gp     "end_do_loop" "do_loop" nil)  parent 0)
   ((n-p-pstmtk nil           "do_loop" "do") parent f90-ts-indent-block)  ;; proper do block (with or without while)
   ((n-p-pstmtk nil           "ERROR"   "do") previous-stmt-anchor f90-ts-indent-block)

   ((n-p-gp     "end_block_construct_statement" "block_construct" nil)     parent 0)
   ((n-p-pstmtk nil                             "block_construct" "block") parent f90-ts-indent-block)
   ((n-p-pstmtk nil                             "ERROR"           "block") previous-stmt-anchor f90-ts-indent-block)

   ((n-p-gp "end_associate_statement" "associate_statement" nil)         parent 0)
   ((n-p-pstmtk nil                       "association"         "associate") parent f90-ts-indent-block)
   ((n-p-pstmtk nil                       "associate_statement" "associate") parent f90-ts-indent-block)
   ((n-p-pstmtk nil                       "ERROR"               "associate") previous-stmt-anchor f90-ts-indent-block)

   ((n-p-gp "end_forall_statement"        "forall_statement" nil)      parent               0)
   ((n-p-pstmtk nil                       "forall_statement" "forall") parent               f90-ts-indent-block)
   ((n-p-pstmtk nil                       "ERROR"            "forall") previous-stmt-anchor f90-ts-indent-block))
  "Indentation rules for single region structures.
These are do loops, block statements, associate construct and forall statements.")


(defvar f90-ts-indent-rules-select
  (f90-ts--with-map-rules
   ;; control statements
   ((n-p-gp     "end_select_statement" "select_case_statement" nil)              parent 0)
   ((n-p-pstmtk "case_statement"       "select_case_statement" nil)              parent 0)
   ((n-p-pstmtk nil                    "select_case_statement" "case")           parent f90-ts-indent-block)
   ((n-p-pstmtk nil                    "case_statement"        "case")           parent f90-ts-indent-block)
   ((n-p-gp     "ERROR"                "select_case_statement" nil)              parent f90-ts-indent-block)
   ((n-p-gp     nil                    "ERROR"                 "case_statement") grand-parent f90-ts-indent-block)
   ((parent-is                         "select_case_statement")                  parent 0)

   ((n-p-gp     "end_select_statement" "select_type_statement" nil)              parent 0)
   ((n-p-pstmtk "type_statement"       "select_type_statement" nil)              parent 0)
   ((n-p-pstmtk nil                    "select_type_statement" "type")           parent f90-ts-indent-block)
   ((n-p-pstmtk nil                    "select_type_statement" "class")          parent f90-ts-indent-block)
   ((n-p-pstmtk nil                    "type_statement"        "type")           parent f90-ts-indent-block)
   ((n-p-pstmtk nil                    "type_statement"        "class")          parent f90-ts-indent-block)
   ((n-p-gp     "ERROR"                "select_type_statement" nil)              parent f90-ts-indent-block)
   ((n-p-gp     nil                    "ERROR"                 "type_statement") grand-parent f90-ts-indent-block)
   ((parent-is                         "select_type_statement")                  parent 0)

   ((n-p-gp     "end_select_statement" "select_rank_statement" nil)              parent 0)
   ((n-p-pstmtk "rank_statement"       "select_rank_statement" nil)              parent 0)
   ((n-p-pstmtk nil                    "select_rank_statement" "rank")           parent f90-ts-indent-block)
   ((n-p-pstmtk nil                    "rank_statement"        "rank")           parent f90-ts-indent-block)
   ((n-p-gp     "ERROR"                "select_rank_statement" nil)              parent f90-ts-indent-block)
   ((n-p-gp     nil                    "ERROR"                 "rank_statement") grand-parent f90-ts-indent-block)
   ((parent-is                         "select_rank_statement")                  parent 0))
  "Indentation rules for select statements (case and type).")


(defvar f90-ts-indent-rules-catch-all
  (f90-ts--with-map-rules
   ;; final catch-all rule
   ;;,@(f90-ts-indent-rules-info "catch all")
   (catch-all catch-all-anchor 0))
  "Final indentation rule to handle unmatched cases.")


(defvar f90-ts-indent-rules
  `((fortran
     ,@f90-ts-indent-rules-start
     ,@f90-ts-indent-rules-preproc
     ,@f90-ts-indent-rules-comments
     ,@f90-ts-indent-rules-continued
     ,@f90-ts-indent-rules-internal-proc
     ,@f90-ts-indent-rules-prog-mod
     ,@f90-ts-indent-rules-function
     ,@f90-ts-indent-rules-translation-unit
     ,@f90-ts-indent-rules-interface
     ,@f90-ts-indent-rules-dtype-enum
     ,@f90-ts-indent-rules-if
     ,@f90-ts-indent-rules-where
     ,@f90-ts-indent-rules-single-region
     ,@f90-ts-indent-rules-select
     ,@f90-ts-indent-rules-catch-all))
  "List of all indentation rules in its proper sequence.")


;;;-----------------------------------------------------------------------------
;;; Smart end completion

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
    ;;"end_coarray_team_statement"
    ;;"end_coarray_critical_statement"
    )
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
                                          "))")))
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
  (when-let ((construct-name (f90-ts--complete-smart-end-name node)))
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
        (sit-for blink-matching-delay)))))


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


(defun f90-ts--complete-smart-end-node (node)
  "If NODE is an end struct statement, construct the completion.
It looks for structure type and name or label.

Example: complete \"end\" closing a subroutine of name mysub by
\"end subroutine mysub\".
After the changes, NODE will become stale.  The function returns the new
statement node representing the whole block.  This is used for the blink
option or statement indentation."
  ;; the region check ensures that end statements on continued lines are left alone
  (let ((type (treesit-node-type node))
        (beg (treesit-node-start node))
        (end (f90-ts--complete-smart-end-end node))
        (text (treesit-node-text node)))
    ;; make sure that we are looking at and end statement, the parser might add
    ;; and end_xyz_statement node in error recovery mode (e.g. at end of file)
    (when (and (= (line-number-at-pos beg) (line-number-at-pos end))
               (and text (string-match-p "^end" text))
               (member type f90-ts--complete-end-structs))

      (let ((node-struct (treesit-node-parent node))
            node-struct-new)
        (when-let ((completion (f90-ts--complete-smart-end-compose node-struct)))
          (let (beg-marker
                end-marker)
            (unwind-protect
                (progn
                  ;; beg marker should move with inserted/deleted text
                  ;; to stay at start of node
                  (setq beg-marker (copy-marker (treesit-node-start node-struct) t))
                  (setq end-marker (copy-marker (treesit-node-end node-struct) t))
                  ;; returns true if text was changed
                  (f90-ts--complete-replace-if-changed beg end completion)
                  (when (treesit-node-check node-struct 'outdated)
                    (setq node-struct-new (treesit-node-on (marker-position beg-marker)
                                                          (marker-position end-marker)))))
              (when beg-marker (set-marker beg-marker nil))
              (when end-marker (set-marker end-marker nil)))))
        ;; if nothing has changed new node is nil, return the original one
        (or node-struct-new node-struct)))))


(defun f90-ts--complete-smart-end-indent (node-struct)
  "Indent block represented by NODE-STRUCT.
This can be optionally invoked after smart end completion to indent the whole
block.  If indentation changes something, the tree is updated and node-struct
becomes stale.  In this case, the function return the node-struct-new,
otherwise nil."
  (let ((variant-saved f90-ts--align-continued-variant-tab)
        beg-marker
        end-marker
        node-struct-new)
    (unwind-protect
        (progn
          ;; use region variant for indentation (no rotation of list
          ;; items on continued lines)
          (setq f90-ts--align-continued-variant-tab nil)
          (setq beg-marker (copy-marker (treesit-node-start node-struct) t))
          (setq end-marker (copy-marker (treesit-node-end node-struct) t))
          (treesit-indent-region beg-marker end-marker)
          (when (treesit-node-check node-struct 'outdated)
            (setq node-struct-new (treesit-node-on (marker-position beg-marker)
                                                  (marker-position end-marker)))))
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
        (when-let ((node-struct (f90-ts--complete-smart-end-node node)))
          (when indent-struct
            ;; update node-struct, if indent changes anything
            (setq node-struct (or (f90-ts--complete-smart-end-indent node-struct)
                                 node-struct)))
          (f90-ts--complete-smart-end-show node-struct))))))


;;;------------------------------------------------------------------------------
;;; Indentation and smart end completion

;;; line indentation for <tab>, C-<tab> etc.
;;;  * f90-ts-indent-and-complete-stmt
;;;  * f90-ts-indent-and-complete-line{-[2,3]}
;;;  * f90-ts-indent-line{-[2,3]}
;;;
;;; region indentation:
;;;  * f90-ts-indent-and-complete-region

(defun f90-ts--indent-stmt-region (beg end)
  "Apply indent region from begin of line at BEG to end of line at END.
Return true if the region was already properly indented (nothing was
changed)."
  (let ((beg-reg (save-excursion
                    (goto-char beg)
                    (line-beginning-position)))
        (end-reg (save-excursion
                    (goto-char end)
                    (line-end-position))))
    ;; possibly slow if continued lines are very long (but safer)
    (let ((old-text (buffer-substring-no-properties beg-reg end-reg)))
      (treesit-indent-region beg-reg end-reg)
      (string= old-text (buffer-substring-no-properties beg-reg end-reg)))))


(defun f90-ts--indent-and-complete-line-aux (variant indent-struct)
  "Auxiliary wrapper for indent-and-complete-line function.
VARIANT is the tab variant to be used.
If INDENT-STRUCT is true, and point is at some end statement, then
indent the whole block closed by the end statement after smart end
completion."
  (let ((f90-ts--align-continued-variant-tab variant))
    (treesit-indent)
    (f90-ts--complete-smart-tab indent-struct)))


(defun f90-ts-indent-and-complete-line ()
  "Indent and apply smart end completion to current line.
This is the default function for indent and smart complete of end lines,
bound to <tab>.  More advanced indentations of involved continued lines and
block structures is done by `f90-ts--indent-and-complete-stmt'"
  (interactive)
  (f90-ts--indent-and-complete-line-aux
   f90-ts-indent-list-line
   nil))


(defun f90-ts-indent-line (&optional variant)
  "Indent a single line.
This is the default function for indentation of a single line.  Smart end
completion or other extra stuff is not executed by this function.
If provided VARIANT is the variant symbol for how to compute alignment in
multi-line statements.  Default value is `f90-ts-indent-list-line'."
  (interactive)
  (let ((f90-ts--align-continued-variant-tab
         (or variant f90-ts-indent-list-line)))
    (treesit-indent)))


(defun f90-ts-indent-and-complete-stmt ()
  "Perform indentation and smart end completion for a whole statement.
In general, this just calls `f90-ts--indent-and-complete-line-aux'
with non-nil for indent-struct.
However, if within a continued line region, it determines the first line of
the current statement and performs indent region from this first line up to
and including the current line.  If indentation of current line has not
changed, then it indents the current line by invoking
`f90-ts--indent-and-complete' to apply rules like rotation of list context
items.  Otherwise it indents  with default line choice."
  (interactive)
  (cond
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (f90-ts-complete-smart-end-region beg end)))

   ((not (f90-ts--pos-within-continued-stmt-p (point)))
    ;; just do indent-and-complete plus block indentation if applicable
    (f90-ts--indent-and-complete-line-aux
     f90-ts-indent-list-line ; use default line variant
     t))

   (t
    ;; multi-line statement
    (let* ((end (point))
           (node (f90-ts--first-node-on-line end))
           (first (f90-ts--first-node-of-stmt node))
           (beg (treesit-node-start first)))

      (if (f90-ts--indent-stmt-region beg end)
          ;; no indentation applied, invoke indentation for current line
          ;; like doing rotational alignment,
          ;; no smart end completion necessary, as there is no end is involved
          (f90-ts-indent-line)
        ;; statement up to current line was changed by indent statement region
        (back-to-indentation))))))


;; used by f90-ts-indent-and-complete-region
(defun f90-ts-complete-smart-end-region (beg end)
  "Execute smart end completion in region from BEG to END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))

  (let ((beg-pos (if (markerp beg) (marker-position beg) beg))
        (end-pos (if (markerp end) (marker-position end) end))
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
                          (let ((start-n (treesit-node-start n)))
                            ;; treesit-search-forward has no early abort
                            ;; option, without this, the search exhausts
                            ;; the whole remaining tree
                            (when (> start-n end-marker)
                              (throw 'f90-ts--past-end nil))
                            (and (<= pos start-n)
                                 (member (treesit-node-type n)
                                         f90-ts--complete-end-structs))))
                             nil ; search forward
                             nil ; only named nodes (end-structs are always named nodes)
                             )
            while node
            do (let ((node-start (treesit-node-start node)))
                 (f90-ts--complete-smart-end-node node)
                 (progn ; or better save-excursion (seems unnecessary?)
                   (goto-char node-start)
                   (forward-line 1)
                   (setq pos (point))))))
          ;; after finishing move to end of region
          (goto-char end-marker))
      (set-marker end-marker nil))))


(defun f90-ts-indent-and-complete-region (beg end)
  "Indent region and execute smart end completion in region from BEG to END.
It is based on the treesitter tree overlapping that region.
For region based indentation, smart end completion should be executed first,
as this might repair the tree.  Only then indentation should be done.
For single line the order does not matter.  Example:

module do_mod
contains
 subroutine do_sub()
 end function do_sub
end module do_mod

At the end function line, \"end\" represents the end subroutine statement,
hence indentation as well as smart end completetion both work.  However,
the keyword \"function\" after \"end\" starts a new function and muddles the
subsequent tree."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))

  ;; we need markers as indent-region changes the positions
  ;; of beg and end in general
  (let (beg-marker end-marker)
    (unwind-protect
        (progn
          ;; beg marker should stay before inserted text
          ;; end marker should stay after inserted text
          (setq beg-marker (copy-marker beg))
          (setq end-marker (copy-marker end t))
          (f90-ts-complete-smart-end-region beg-marker end-marker))
          (treesit-indent-region beg-marker end-marker)
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


(defun f90-ts-indent-for-tab-command-2 ()
  "Involve `indent-for-tab-command' with variant `f90-ts-indent-list-line-2'.
The variant to be used can be customized.  Intended for use in key bindings."
  (interactive)
  (let ((f90-ts-indent-list-line f90-ts-indent-list-line-2))
    (indent-for-tab-command)))


(defun f90-ts-indent-for-tab-command-3 ()
  "Involve `indent-for-tab-command' with variant `f90-ts-indent-list-line-3'.
The variant to be used can be customized.  Intended for use in key bindings."
  (interactive)
  (let ((f90-ts-indent-list-line f90-ts-indent-list-line-3))
    (indent-for-tab-command)))


;;;-----------------------------------------------------------------------------
;;; Break lines and add continuation symbol

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
   ((f90-ts-in-string-p)
    (insert "&\n&"))

   ((f90-ts-in-openmp-p)
    ;; looks like a comment, but starting with special "!$" sequence,
    ;; breaking openmp lines requires a continuation symbol
    (let* ((node (treesit-node-at (point)))
           (prefix (f90-ts--comment-prefix node)))
      (delete-horizontal-space)
      (f90-ts--break-line-insert-amp-at-end)
      (insert "\n" prefix)))

   ((f90-ts-in-comment-p)
    (let* ((node (treesit-node-at (point)))
           (prefix (f90-ts--comment-prefix node)))
      (delete-horizontal-space)
      (insert "\n" prefix)))

   ((f90-ts-bol-to-point-blank-p)
    ;; this results in an empty line, no ampersand
    (delete-horizontal-space)
    (insert "\n"))

   (t
    (delete-horizontal-space)
    (f90-ts--break-line-insert-amp-at-end)
    (newline 1)
    (if f90-ts-beginning-ampersand (insert "&"))))

  (indent-according-to-mode))


;; TODO for both join variants:
;; * joining of comment line and openmp statements
;; * empty lines within continued statements
;;
;; note: due to comments and empty lines, a simple forward-line or backward-line
;; does not seem possibly easily, instead we should check and reconstruct the
;; (&, comment*, &) sequence of nodes and use it for navigation and changes,
;; and to unify the two join variants
(defun f90-ts-join-line-prev ()
  "Join previous line with the current one, if part of a continued statement.
This is (partially) a counterpart to `f90-ts-break-line'.
If previous line has comments (at end, next line etc.) joining is not done."
  (interactive)
  (let* ((first-node (or (f90-ts--first-node-on-line (point))
                         (treesit-node-at (point))))
         (amp-node (and (f90-ts--node-type-p first-node "&")
                        first-node))
         (prev-node (and amp-node
                         (treesit-node-prev-sibling amp-node)))
         (bol (save-excursion
                (back-to-indentation)
                (point))))
    ;; if amp-node is non-nil it is the first node on line and thus the second
    ;; (possibly virtual) ampersand of a continued statement, comments might be
    ;; in between those two ampersands
    (cl-assert (or (null first-node)
                   (and amp-node
                        (f90-ts--node-type-p prev-node '("&" "comment"))))
               nil "internal error: prev node is not an ampersand or comment?")
    (if (and amp-node
             prev-node
             (f90-ts--node-type-p prev-node "&"))
        (let ((end (treesit-node-end amp-node)))
          (if (= bol end)
              ;; point is on the line of the ampersand, join completely
              (let ((beg (treesit-node-start prev-node)))
                (delete-region beg end)
                (goto-char beg)
                (fixup-whitespace))
            ;; next proper continued line is not at point but later
            (let ((beg (treesit-node-end prev-node)))
              (cl-assert (< (line-number-at-pos bol)
                            (line-number-at-pos end))
                         nil "internal error: bol and end at same line?")
              ;; do not delete the first &, as we also do not delete second &
              (delete-region beg bol)
              (goto-char beg)
              (fixup-whitespace))))
      (message "join failed: not a simple continued line"))))


(defun f90-ts-join-line-next ()
  "Join current line with the next one, if part of a continued statement.
This is (partially) a counterpart to `f90-ts-break-line'.
If continued line has comments (at end, next line etc.) joining is not done."
  (interactive)
  (let* ((last-node (f90-ts--last-node-on-line (point)))
         (amp-node (and (f90-ts--node-type-p last-node "&")
                        last-node))
         (next-node (and amp-node (treesit-node-next-sibling amp-node))))
    ;; if amp-node is non-nil it is the last node on line and an ampersand,
    ;; thus next node is either (possibly virtual) ampersand of a continued
    ;; statement, or a comment (which are allowed to be in between those two
    ;; ampersands)
    (cl-assert (or (null last-node)
                   (and amp-node
                        (f90-ts--node-type-p next-node '("&" "comment"))))
               nil "internal error: next node is not an ampersand or comment?")
    (if (and amp-node
             next-node
             (f90-ts--node-type-p next-node "&"))
        (let ((beg (treesit-node-start amp-node))
              (end (treesit-node-end next-node)))
          (delete-region beg end)
          (goto-char beg)
          (fixup-whitespace))
      (message "join failed: not a simple continued line"))))


;;;-----------------------------------------------------------------------------
;;; treesitter based region functions

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


(defun f90-ts--mark-region-node (node &optional reversed)
  "Mark the region spanned by NODE.
If REVERSED is non-nil, then put point at start of region, otherwise point
is at end of region."
  (let ((beg (treesit-node-start node))
        (end (treesit-node-end node)))
    (if reversed
        (progn
          (goto-char beg)
          (push-mark end t t))
      (push-mark beg t t)
      (goto-char end))))


(defun f90-ts--smallest-child0-same-span (node)
  "Find the smallest child0 of NODE which spans the same region."
  (cl-loop for current = node then child
           for child = (treesit-node-child current 0 t)
           while (and child
                      (= (treesit-node-start child) (treesit-node-start current))
                      (= (treesit-node-end child) (treesit-node-end current)))
           finally return current))


(defun f90-ts--largest-node-same-span (node)
  "Find the largest ancestor of NODE which spans the same region.
Example: for (contains_statement \"contains\") `treesit-node-on' returns
\"contains\", but for navigation, we usually want the largest node with
same bounds, which is the contains_statement node."
  (treesit-parent-while
   node
   (lambda (n)
     (and (= (treesit-node-start n) (treesit-node-start node))
          (= (treesit-node-end n) (treesit-node-end node))))))


(defun f90-ts--smallest-named-node-containing-region (beg end)
  "Return the smallest named node fully containing region from BEG to END.
The node must be strictly larger than the region (BEG END)."
  (when-let* ((node (treesit-node-on beg end))
              (cover (treesit-parent-until
                      node
                      (lambda (n)
                        (and (treesit-node-check n 'named)
                             (<= (treesit-node-start n) beg)
                             (>= (treesit-node-end n) end)
                             (< (- end beg)
                                (- (treesit-node-end n) (treesit-node-start n)))))
                      t)))
    (f90-ts--largest-node-same-span cover)))


(defun f90-ts--node-on-pos (pos)
  "Function `treesit-node-on' works on half-open regions.
It returns a node spanning the half-open region [beg, end).
If pos is at end position of node, then the region [pos,pos) is empty and
`treesit-node-on' returns a larger node, which is not expected in this context.
This function queries at POS and at POS-1, and selects the smallest node
containing POS in the closed interval logic.
Example
subroutine sub()
   if (cond) then
   end if|
end subroutine sub
If point is at |, then the smallest named no is the end_statement node
for \"end if\". However, treesit-node-on returns the subroutine node.
Querying at POS-1 gives the expected answer."
  (let* ((nodes (list (treesit-node-on pos      pos      nil 'named)
                      (treesit-node-on (1- pos) (1- pos) nil 'named)))
         (filtered (seq-filter (lambda (n) (and (<= (treesit-node-start n) pos)
                                                (<= pos (treesit-node-end n))))
                               nodes))
         (sorted (seq-sort (lambda (n1 n2) (< (- (treesit-node-end n1)
                                                 (treesit-node-start n1))
                                              (- (treesit-node-end n2)
                                                 (treesit-node-start n2))))
                           filtered)))
    (car sorted)))


(defun f90-ts-enlarge-region ()
  "Expand region to next larger node.
If no region is active, select the smallest named node at point.
If region is active, expand to the smallest named node that is larger
than current region."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (node (f90-ts--smallest-named-node-containing-region beg end)))
        (if node
            (f90-ts--mark-region-node node
                                      f90-ts-mark-region-reversed)
          (message "no tree-sitter node found enlarging current region")))
    ;; no active region, select smallest named node at point
    ;; (but move point to nonspace part first)
    (if-let* ((pos (f90-ts--pos-nonspace (point)))
              (node-on (f90-ts--node-on-pos pos))
              (node (f90-ts--largest-node-same-span node-on)))
        (f90-ts--mark-region-node node
                                  f90-ts-mark-region-reversed)
      (message "no tree-sitter node found at point"))))


(defun f90-ts-child0-region ()
  "Find smallest node covering region.
Then reduce region to its first child.  If there are further first child with
same region, return the smallest of these grandchildren."
  (interactive)
  (if (use-region-p)
      (if-let* ((beg (region-beginning))
                (end (region-end))
                (node-on (treesit-node-on beg end))
                (node (f90-ts--smallest-child0-same-span node-on))
                (child0 (treesit-node-child node 0 t)))
          (f90-ts--mark-region-node child0
                                    f90-ts-mark-region-reversed)
        (message "no tree-sitter child0 found for current region"))
    (message "no active region")))


(defun f90-ts-prev-region ()
  "Find smallest node covering current marked region.
If the node spans the current region, then mark its previous sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (if (use-region-p)
      (if-let* ((beg (region-beginning))
                (end (region-end))
                (node-on (treesit-node-on beg end))
                (node (f90-ts--largest-node-same-span node-on))
                (node-mark (if (and (= beg (treesit-node-start node))
                                    (= end (treesit-node-end node)))
                               (treesit-node-prev-sibling node t)
                             node)))
          (f90-ts--mark-region-node node-mark
                                    f90-ts-mark-region-reversed)
        (message "no tree-sitter previous sibling found for current region"))
    (message "no active region")))


(defun f90-ts-next-region ()
  "Find smallest node covering current marked region.
If the node spans the current region, then mark its next sibling.
Otherwise mark the region spanned by the node itself (like enlarge-region)."
  (interactive)
  (if (use-region-p)
      (if-let* ((beg (region-beginning))
                (end (region-end))
                (node-on (treesit-node-on beg end))
                (node (f90-ts--largest-node-same-span node-on))
                (node-mark (if (and (= beg (treesit-node-start node))
                                    (= end (treesit-node-end node)))
                               (treesit-node-next-sibling node t)
                             node)))
          (f90-ts--mark-region-node node-mark
                                    f90-ts-mark-region-reversed)
        (message "no tree-sitter next sibling found for current region"))
    (message "no active region")))


;;;-----------------------------------------------------------------------------
;;; Comment region using some prefix

;; The following code are adapted from `f90.el', which is part of GNU Emacs.
(defun f90-ts-comment-region-with-prefix (beg-region end-region prefix)
  "Comment/uncomment every line in the region using comment PREFIX.
Region is given by BEG-REGION and END-REGION.
Insert comment prefix at the start of every line in the region.
If the prefix is already present, then remove it and uncomment the line."
  (let ((end (copy-marker end-region)))
    (goto-char beg-region)
    (beginning-of-line)
    (if (looking-at (regexp-quote prefix))
        (delete-region (point) (match-end 0))
      (insert prefix))
    (while (and (zerop (forward-line 1))
                (< (point) end))
      (if (looking-at (regexp-quote prefix))
          (delete-region (point) (match-end 0))
        (insert prefix)))
    (set-marker end nil)))


(defun f90-ts-comment-region-default (beg-region end-region)
  "Comment/uncomment every line in the region using default !!$ prefix.
Region is given by BEG-REGION and END-REGION."
  (interactive "*r")
  (f90-ts-comment-region-with-prefix beg-region
                                     end-region
                                     f90-ts-comment-region-prefix))


(defun f90-ts-comment-region-custom (beg-region end-region prefix)
  "Comment/uncomment every line in the region using custom PREFIX.
Region is given by BEG-REGION and END-REGION.
If called interactively, prompt for a prefix from
`f90-ts-extra-comment-prefixes' and `f90-ts-comment-region-prefix'."
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (region-beginning))
    (region-end)
    (completing-read "choose comment prefix: "
                          (append f90-ts-extra-comment-prefixes
                                  (list f90-ts-comment-region-prefix))
                          nil t nil nil f90-ts-comment-region-prefix)))
  (f90-ts-comment-region-with-prefix beg-region
                                     end-region
                                     prefix))


;;;-----------------------------------------------------------------------------

;;;###autoload
(define-derived-mode f90-ts-mode prog-mode "F90[TS]"
  "Major mode for editing Fortran 90+ files, using tree-sitter library."
  :group 'f90-ts
  ;; check if treesit has a ready parser for 'fortran
  (unless (treesit-available-p)
    (error "Tree-sitter support is not available"))

  ;; create parser or report error
  (if (treesit-ready-p 'fortran)
      (treesit-parser-create 'fortran)
    (error
     (concat "Tree-sitter parser for 'fortran not ready. "
             "Run `M-x treesit-install-language-grammar RET fortran RET' "
             "or ensure treesit-language-source-alist points to a built grammar.")))

  ;; font-lock feature list controls what features are enabled for highlighting
  (setq-local treesit-font-lock-feature-list
              '((comment preproc)                                ; level 1
                (builtin keyword string type)                    ; level 2
                (constant number)                                ; level 3
                (function variable operator bracket delimiter))) ; level 4

  ;; use the pre-defined font-lock rules variable
  (setq-local treesit-font-lock-settings
              (apply #'append f90-ts-font-lock-rules))

  ;; use the pre-defined indentation rules variable
  (setq-local treesit-simple-indent-rules f90-ts-indent-rules)

  ;; basic setup helper provided by emacs for tree-sitter powered modes,
  ;; this must be called after setting setq-local variables above!
  (treesit-major-mode-setup)

  ;; set indentation functions (both add smart end completion before
  ;; indentation, so no hook available); this must be done AFTER
  ;; calling treesit-major-mode-setup
  (setq-local indent-line-function #'f90-ts-indent-and-complete-line)
  (setq-local indent-region-function #'f90-ts-indent-and-complete-region)

  ;; provide a simple mode name in the modeline
  (setq-local mode-name "F90-TS"))


;;;-----------------------------------------------------------------------------
;; debug: log buffer

(defconst f90-ts-log-buffer "*f90-ts-log*"
  "Buffer name used for f90 tree-sitter logging.")


(defun f90-ts--log-get-buffer ()
  "Return the log buffer and if it does not yet exists, create a new one."
  (or (get-buffer f90-ts-log-buffer)
      (with-current-buffer (get-buffer-create f90-ts-log-buffer)
        (f90-ts-log-mode)
        (current-buffer))))


(defun f90-ts-log (category fmt &rest args)
  "Insert a message into the dedicated *f90-ts-log* log buffer.
The message is computed from FMT and ARGS (using `format') and prefixed
by CATEGORY and a time stamp."
  (let ((buf (f90-ts--log-get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        ;; insert time stamp
        (let* ((time (current-time))
               (microseconds (nth 2 time))
               (centiseconds (/ microseconds 10000))
               (time-string (format-time-string "%H:%M:%S" time)))
          (insert (format "[%s.%02d] " time-string centiseconds)))

        ;; insert coloured category keyword
        (let* (;; remove initial ':' from category keyword
               (cat-name (substring (symbol-name category) 1))
               ;; fixed width (default 10)
               (cat-fix (format "%-10s" cat-name)))
          (insert (propertize cat-fix 'face '(:foreground "blue"))))

        (insert (apply #'format fmt args))
        (insert "\n")
        (goto-char (point-max))))

    ;; scroll window if buffer is displayed
    (when-let ((win (get-buffer-window buf t))) ; visible in any frame
      (with-selected-window win
        (goto-char (point-max))))))


(defun f90-ts-log-clear ()
  "Clear the f90-ts log buffer."
  (interactive)
  (when (get-buffer f90-ts-log-buffer)
    (with-current-buffer f90-ts-log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))


;;;###autoload
(defun f90-ts-log-show ()
  "Show the f90-ts log buffer in the current frame."
  (interactive)
  (let ((buf (f90-ts--log-get-buffer)))
    (with-selected-frame (selected-frame)
      (switch-to-buffer buf))))


(defvar f90-ts-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") #'f90-ts-log-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `f90-ts-log-mode'.")


(define-derived-mode f90-ts-log-mode special-mode "F90-TS-Log"
  "Major mode for the f90-tree-sitter log buffer."
  (setq-local truncate-lines t)      ; keep logs on one line per entry
  (setq-local buffer-read-only t)    ; ensure it stays read-only
  (buffer-disable-undo)              ; improve performace, no undo required
)


;;;-----------------------------------------------------------------------------
;; debug: node inspection and other stuff

(defun f90-ts-treesit-inspect-node (node-inspect)
  "Construct a descriptive node name for NODE-INSPECT.
This is mostly a copy of `treesit-inspect-node-at-point', but this variation
highlights provided NODE-INSPECT and uses its start position as point."
  ;; NODE-LIST contains all the node that starts at point.
  (let* ((node-start (treesit-node-start node-inspect))
         (node-list
          (cl-loop
           for node = (treesit-node-at node-start)
           then (treesit-node-parent node)
           while node
           if (eq (treesit-node-start node)
                  node-start)
           collect node))
         (largest-node (car (last node-list)))
         (parent (treesit-node-parent largest-node))
         ;; node-list-ascending contains all the node bottom-up, then
         ;; the parent.
         (node-list-ascending
          (if (null largest-node)
              ;; If there are no nodes that start at point, just show
              ;; the node at point and its parent.
              (list (treesit-node-at node-start)
                    (treesit-node-parent
                     (treesit-node-at node-start)))
            (append node-list (list parent))))
         (name ""))
    ;; We draw nodes like (parent field-name: (node)) recursively,
    ;; so it could be (node1 field-name: (node2 field-name: (node3))).
    (dolist (node node-list-ascending)
      (setq
       name
       (concat
        (if (treesit-node-field-name node)
            (format " %s: " (treesit-node-field-name node))
          " ")
        (if (treesit-node-check node 'named) "(" "\"")
        (propertize (or (treesit-node-type node) "N/A")
                    'face
                    (if (treesit-node-eq node node-inspect)
                        'bold nil))
        name
        (if (treesit-node-check node 'named) ")" "\""))))
    name))


(defun f90-ts-inspect-node (category node info)
  "Show inspect info of treesitter NODE as a one-liner in the log buffer.
Prefix the line with CATEGORY and `inspect<info>' using INFO."
  (if node
      (let* ((type  (treesit-node-type node))
             (start (treesit-node-start node))
             (end   (treesit-node-end node))
             (len   (- end start))
             (line  (f90-ts--node-line node))
             (inspect-name  (f90-ts-treesit-inspect-node node)))
        (f90-ts-log category "inspect<%s>: type= %s  -  name= %s - start=%d  end=%d  len=%d  line=%d"
                    info type inspect-name start end len line))
    (f90-ts-log category "inspect<%s>: nil" info)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-mode)

;;; f90-ts-mode.el ends here
