;;; f90-ts-custom.el --- Customisation for f90-ts-mode -*- lexical-binding: t; -*-

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

;; f90-ts-custom provides all customisation options related to f90-ts-mode:
;; These are defface, defcustom related constants and functions.

;;; Code:

(require 'cl-lib)

;;;-----------------------------------------------------------------------------

(defgroup f90-ts nil
  "Fortran (F90+) major mode using Tree-sitter."
  :group 'languages)


(defgroup f90-ts-font-lock nil
  "Font-locking options used by `f90-ts-mode'.
This group defines additional faces used for F90-specific syntax.
Standard font-lock faces are used as well."
  :prefix "f90-ts-"
  :group  'f90-ts)


(defgroup f90-ts-indent nil
  "Indentation options used by `f90-ts-mode'."
  :prefix "f90-ts-"
  :group  'f90-ts)


(defgroup f90-ts-comment nil
  "Comment and openmp options used by `f90-ts-mode'."
  :prefix "f90-ts-"
  :group  'f90-ts)


;;;-----------------------------------------------------------------------------

(defcustom f90-ts-indent-toplevel 0
  "Extra indentation applied to contain sections at toplevel."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-contain 3
  "Extra indentation applied to contain sections."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-block 3
  "Extra indentation applied to most blocks.
These are function and subroutine bodies, control statements (do, if,
associate ...) etc."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-continued 5
  "Extra indentation applied to continued lines."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defconst f90-ts--indent-options-alist
  '(("continued-line: indent relative to the first line of the statement" . continued-line)
    ("keep-or-continued-line: preserve alignment or indent relative to the first line" . keep-or-continued-line)
    ("primary: align with the primary column" . primary)
    ("keep-or-primary: preserve alignment or align with the primary column" . keep-or-primary)
    ("rotate: cycle through indentation columns" . rotate)
    ("keep-or-rotate: preserve alignment or cycle to the next indentation column" . keep-or-rotate))
  "Available indentation options for continued lines in list-like structures.")


(defconst f90-ts--indent-region-options-alist
  (cl-remove-if (lambda (x)
                  (memq (cdr x) '(rotate keep-or-rotate)))
                f90-ts--indent-options-alist)
  "Options for region indentation (excludes rotation).")


(defconst f90-ts--indent-values
  (mapcar #'cdr f90-ts--indent-options-alist)
  "List of valid symbols for single-line indentation.")


(defconst f90-ts--indent-region-values
  (mapcar #'cdr f90-ts--indent-region-options-alist)
  "List of valid symbols for region indentation (excludes rotation).")


(defun f90-ts--indent-make-radio-type (options)
  "Generate a customize radio type from an alist of indent OPTIONS."
  `(radio ,@(mapcar (lambda (x)
                      `(const :tag ,(car x) ,(cdr x)))
                    options)))


(defcustom f90-ts-indent-list-region 'keep-or-primary
  "Method to select indentation column for continued lines in list-like context.
Used as default setting in `indent-region' and similar operations."
  :type (f90-ts--indent-make-radio-type f90-ts--indent-region-options-alist)
  :safe (lambda (val) (memq val f90-ts--indent-region-values))
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line 'rotate
  "Method to select indentation column for continued lines in list-like context.
Used as default setting in `indent-for-tab-command' and similar
operations (indentation of a single line)."
  :type (f90-ts--indent-make-radio-type f90-ts--indent-options-alist)
  :safe (lambda (val) (memq val f90-ts--indent-values))
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line-2 'continued-line
  "Method to select indentation column for continued lines in list-like context.
Used as secondary setting in `indent-for-tab-command'.  Can be be bound
to <backtab> (S-<tab>), A-<tab>, C-S-<tab> or other."
  :type (f90-ts--indent-make-radio-type f90-ts--indent-options-alist)
  :safe (lambda (val) (memq val f90-ts--indent-values))
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-list-line-3 'primary
  "Method to select indentation column for continued lines in list-like context.
Used as ternary setting in `indent-for-tab-command'.  Can be be bound
to <backtab> (S-<tab>), A-<tab>, C-S-<tab> or other."
  :type (f90-ts--indent-make-radio-type f90-ts--indent-options-alist)
  :safe (lambda (val) (memq val f90-ts--indent-values))
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-paren-default 1
  "Additional offset applied for alignment with opening parenthesis.
The default is used for all items except the closing parenthesis.

Example:
   call sub(       arg1, &
            .not.  arg2)
Primary alignment column for the second line column of parenthesis plus
`f90-ts-indent-paren-default'."
  :type  'integer
  :safe  #'integerp
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
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-expr-assign-default 2
  "Additional offset applied for alignment at assignment.
This is applied for alignment with symbol \"=\" for all items
except for associative operators.

Example (with offset 2, which aligns some_expr two columns to the right of =):
x =      & ! some comment
    some_expr

Primary alignment column for the second line is column of assignment \"=\" plus
`f90-ts-indent-expr-assign-default'.  Applied to all nodes except associate
operators, which are handled by `f90-ts-indent-expr-assign-assoc-op'."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-expr-assign-assoc-op 0
  "Additional offset applied for alignment at assignment.
This is applied for alignment with symbol \"=\" for all associative
operators (logical_expression, math_expression).

Example (with offset 0, which aligns = and + at the same colum):
x = value1 &
  + some_expr

Primary alignment column for the second line is column of assignment \"=\" plus
`f90-ts-indent-expr-assign-assoc-op' for associative operators.
All other cases are handled by `f90-ts-indent-expr-assign-default'."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-declaration 3
  "Additional offset applied for alignment with declaration delimiter \"::\".
For declarations without the delimiter offset `f90-ts-indent-declaration'
minus 2 is used (subtracting the length of \"::\").

This is applied in variable declarations.

Example:
integer, parameter :: ! some parameter &
                      param = 0.1234

integer ! offset of \"x\" is end position of type specifier + 3 - 2
        x

Primary alignment column for the second line of a declaration plus
`f90-ts-indent-declaration'."
  :type  'integer
  :safe  #'integerp
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-array-align 'left
  "Alignment of the first element on continued lines in array literals.
The value selects which alignment positions are available for rotation,
keep and primary selection, and which one is the primary position:

`left'               left-aligned only: position the first element on a
                     continued line relative to earlier elements (usual
                     left alignment).
`right'              right-aligned only: right-align the comma after the
                     first element of a line with commata on previous
                     lines.  This is always the primary position, the
                     left-aligned positions are not candidates.
`both-left-primary'  left- and right-aligned positions are candidates,
                     the left-aligned position is primary.
`both-right-primary' left- and right-aligned positions are candidates,
                     the right-aligned position is primary.

Example for right alignment in matrices:

   arr = reshape( &
          [      g,      1.0_dp,     1.0_dp, &
                -g,      2.0_dp,     2.0_dp, &
            1.0_dp,           g,         -g, &
           -1.0_dp,          -g,          g], &
          [3,4])

With `both-left-primary', the right alignment can be achieved by rotation,
and `keep' preserves the alignment:

   Msym33 = [  (x+y),      f(x,q),   1.23, &
                       -(x+2.0*y),      y, &
                                     4.56]

Other secondary positions like at the opening bracket remain available for
rotation and keep options.

This applies only to array literals, delimited by \"[...]\" or \"(/.../)\"."
  :type  '(radio (const :tag "Left-aligned with previous elements" left)
                 (const :tag "Right-aligned at previous commata" right)
                 (const :tag "Both, left-aligned is primary" both-left-primary)
                 (const :tag "Both, right-aligned is primary" both-right-primary))
  :safe  (lambda (val)
           (memq val '(left right both-left-primary both-right-primary)))
  :group 'f90-ts-indent)


(defcustom f90-ts-indent-delete-trailing-whitespace nil
  "If non-nil, delete trailing whitespace characters after indentation.
This applies whenever an indentation operation is performed on a single
line, a statement (which may span several lines), or a region.
Besides `f90-ts-indent-line', `f90-ts-indent-and-complete-line',
`f90-ts-indent-and-complete-stmt', `f90-ts-indent-region' and
`f90-ts-indent-and-complete-region', this also applies to operations
performing indentation, like `f90-ts-comment-region-default' etc."
  :type  'boolean
  :safe  #'booleanp
  :group 'f90-ts-indent)


(defcustom f90-ts-leading-ampersand-style '(indent . 3)
  "Indentation style for leading ampersands in continued lines.
Value is a list of the form (TYPE VALUE) where TYPE is either:
- `column': place the ampersand at column VALUE (0-based)
- `indent': place the ampersand with offset VALUE relative to first
  line of the continued statement."
  :type '(choice (cons :tag "Fixed column"
                       (const :tag "" :format "" column)
                       (integer :tag "Fixed column number"))
                 (cons :tag "Indent offset"
                       (const :tag "" :format "" indent)
                       (integer :tag "Offset to first line")))
  :safe (lambda (v) (and (consp v)
                         (memq (car v) '(column indent))
                         (integerp (cdr v))))
  :group 'f90-ts)


(defcustom f90-ts-stmt-label-column '(left . 0)
  "Column specification for Fortran statement labels.
A cons cell (ADJUSTMENT . COLUMN) where ADJUSTMENT is either
`right-adjusted' or `left-adjusted', and COLUMN is a 0-based column number.
With `right', the label's last digit lands on COLUMN (right-adjusted),
so (right . 4) fills columns 0-4, the classic Fortran label field.
With `left', the label's first digit starts on COLUMN (left-adjusted)."
  :type '(cons
          (choice
           (const :tag "Right-adjusted (last digit at column)" right)
           (const :tag "Left-adjusted  (first digit at column)" left))
          (integer :tag "Column (0-based)"))
  :safe (lambda (v) (and (consp v)
                         (memq (car v) '(right left))
                         (integerp (cdr v))))
  :group 'f90-ts)


;;;-----------------------------------------------------------------------------

(defface f90-ts-font-lock-delimiter-face
  '((t :inherit font-lock-delimiter-face))
  "Face used to highlight delimiter symbols (e.g. commas)."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-bracket-face
  '((t :inherit font-lock-bracket-face))
  "Face used to highlight brackets and parenthesis."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-operator-face
  '((t :inherit font-lock-operator-face))
  "Face used to highlight operators (e.g. +, -, *, /)."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-escape-face
  '((t :inherit font-lock-escape-face))
  "Face used to highlight quotes in strings used as escape character.
This is the first quote in double single- or double- quotes within strings."
  :group 'f90-ts-font-lock)

(defface f90-ts-font-lock-openmp-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight OpenMP statements.
Openmp statements are determined by `f90-ts-special-comment-rules'."
  :group 'f90-ts-font-lock
  :group 'f90-ts-comment)


(defface f90-ts-font-lock-special-var-face
  '((t :inherit font-lock-keyword-face :slant italic))
  "Face used to highlight special variables like \"self\" or \"this\".
Special variables are determined by regexp custom variable
`f90-ts-special-var-regexp'."
  :group 'f90-ts-font-lock)


(defface f90-ts-font-lock-separator-comment-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face used to highlight separator comments.
Special comments such as separators are determined by rules in
`f90-ts-special-comment-rules'."
  :group 'f90-ts-font-lock
  :group 'f90-ts-comment)


(defface f90-ts-font-lock-error-face
  '((t (:underline (:style wave :color "OrangeRed"))))
  "Additional face properties applied to Tree-sitter ERROR nodes.

The face is appended to the existing syntax highlighting, so it is
intended to specify only UI-related attributes such as slant,
underline, or a subtle background."
  :group 'f90-ts-font-lock)


(defcustom f90-ts-font-lock-error-show 'all
  "Extent of highlighting applied to tree-sitter ERROR nodes.

The value controls how much of an ERROR node is highlighted:
- nil, do not apply any additional highlighting to ERROR nodes
- symbol `all' highlight the complete span of the ERROR
- positive integer: highlight this many lines from the start of the ERROR node

Highlighting uses the face `f90-ts-font-lock-error-face'.

- nil: do not apply any additional highlighting to ERROR nodes.
- the symbol `all': highlight the whole ERROR node.
- a positive integer: highlight only that many lines from the start
  of the ERROR node."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Whole node" all)
          (integer :tag "Number of lines from start"))
  :safe (lambda (val)
          (or (null val)
              (eq val 'all)
              (and (integerp val) (> val 0))))
  :group 'f90-ts-font-lock)


;;;-----------------------------------------------------------------------------
;;; other options

(defcustom f90-ts-smart-end 'blink
  "Determine whether and how to complete an end statement.
If set to blink, perform completion and then jump to the opening clause of the
completed statement.
If set to no-blink perform completion without jumping, but print a message.
If set to no-message just perform completion without jumping and any message.
Value nil turns off smart end completion.

Copied from prog mode `f90-mode'."
  :type  '(choice (const blink) (const no-blink) (const no-message) (const nil))
  :safe  (lambda (value) (memq value '(blink no-blink no-message nil)))
  :group 'f90-ts)


;; same as f90-beginning-ampersand in legacy f90 mode
(defcustom f90-ts-leading-ampersand nil
  "Non-nil gives automatic insertion of leading `&' at a continued line."
  :type  'boolean
  :safe  #'booleanp
  :group 'f90-ts)


(defcustom f90-ts-mark-region-order 'preserve
  "Mark region operations need to place point at beginning or end of region.
If `start', point is placed at start of region.
If `end', point is placed at end of region.
If `preserve', the point/mark order of the active region is preserved,
defaulting to end of region if there is no active region present."
  :type  '(choice (const :tag "Point at start" start)
                  (const :tag "Point at end" end)
                  (const :tag "Preserve order" preserve))
  :safe  (lambda (v) (memq v '(start end preserve)))
  :group 'f90-ts)


(defcustom f90-ts-fill-select-breakpoint-by 'rightmost
  "How to select the break point when filling a Fortran line.
- rightmost:   automatically pick the rightmost eligible break point.
- interactive: rotate through possible break points with left/right or
               \\[previous-line]/\\[next-line], confirming with RET.
               Start at previous break point if there was one."
  :type '(choice (const :tag "Rightmost" rightmost)
                 (const :tag "Interactive" interactive))
  :safe (lambda (v) (memq v '(rightmost interactive)))
  :group 'f90-ts)


(defcustom f90-ts-special-var-regexp "\\_<\\(self\\|this\\)\\_>"
  "Regular expression for matching special variables.
This is used for syntax highlighting of variables like \"self\" and \"this\".
For matching identifiers the face `f90-ts-font-lock-special-var' is used."
  :type  'regexp
  :safe  #'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-prefix-regexp "!\\S-*"
  "Regular expression for matching and capturing comment starts.
Together with `f90-ts-comment-prefix-separator-regexp', this is used to extract
the comment prefix for `f90-ts-break-line', `f90-ts-join-line-prev',
`f90-ts-join-line-next' and various fill operations.
OpenMP prefix is matched by `f90-ts-openmp-prefix-regexp'.

Note: do *NOT* use capturing groups in the regular expression, otherwise
comment prefix matching fails.  If a group is required, then use non-capturing
groups with \"\\(?:regexp\\)\".  Example: \"!\\(?:doc\\|remark\\|!\\$\\)\".

The default value \"!\\S-*\" with separator \"\\s-\" assumes that the comment
prefixes are always separated by a blank.  If comment content starts right
after the comment start \"!\", then these two regexp must be adjusted to
properly match only desired comment starters without any separator."
  :type  'regexp
  :safe  #'stringp
  :group 'f90-ts-comment)


(defcustom f90-ts-comment-prefix-separator-regexp "\\s-"
  "Regular expression for separating comment prefix and its content.
End-of-string always serves as separator as well and does not need to provided.

If comment content regularly starts right after comment start \"!\", then this
should be disabled and `f90-ts-comment-prefix-regexp' properly formulate to
match only exactly desired prefixes."
  :type '(choice (regexp :tag "Separator regexp")
                 (const :tag "No separator" nil))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'f90-ts-comment)


(defcustom f90-ts-openmp-prefix-regexp "!\\$\\(?:omp\\)?"
  "Regular expression for matching OpenMP starts.
It is used for identifying openmp statements, which are just comment nodes.
For example, this is relevant for line break operations, as openmp statements
require continuation symbols."
  :type  'regexp
  :safe  #'stringp
  :group 'f90-ts-comment)


(defcustom f90-ts-comment-region-prefix "!!$ "
  "Default comment prefix for commenting regions.
Trailing blank(s) are not inserted automatically, but can be provided
in the string."
  :type  'string
  :safe  #'stringp
  :group 'f90-ts-comment)


(defcustom f90-ts-extra-comment-prefixes '("! " "!$omp " "!$acc " "!!!" "!> " "!< ")
  "List of additional comment prefixes for interactive selection.
Trailing blank(s) are not inserted automatically, but can be provided
in the string."
  :type  '(repeat string)
  :safe (lambda (v) (and (listp v) (cl-every #'stringp v)))
  :group 'f90-ts-comment)


(defcustom f90-ts-comment-prefix-keep-indent t
  "Keep indentation in comment region operation when inserting comment prefix.
If nil, just insert the prefix.  If non-nil, remove blanks up to length of
prefix before commented command starts.  This preserves the original
indentation."
  :type  'boolean
  :safe  #'booleanp
  :group 'f90-ts-comment)


(defcustom f90-ts-comment-keyword-regexp "\\<\\(TODO\\|FIXME\\|Remarks?\\)\\>"
  "Regexp matching keywords within comments to highlight.
The matched part of the comment is highlighted with `font-lock-warning-face'.
Set to nil to disable keyword highlighting in comments."
  :type '(choice (const :tag "Disabled" nil)
                 (regexp :tag "Regexp"))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'f90-ts-font-lock
  :group 'f90-ts-comment)


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
     :face font-lock-doc-face)
    (:name "comment region prefixes: column-0"
     :match "^!!\\$"
     :indent column-0
     :face font-lock-comment-face)
    (:name "comment region prefixes: context"
     :match "^!!!"
     :indent context
     :face font-lock-comment-face))
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

Matches are only done at start of comment if the regexp starts with \"^\".
Otherwise matches also succeed if the it matches somewhere within the comment.

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
  :safe (lambda (v)
          (and (listp v)
               (cl-every (lambda (rule)
                            (and (listp rule)
                                 (stringp (plist-get rule :name))
                                 (or (stringp (plist-get rule :match))
                                     (functionp (plist-get rule :match)))
                                 (memq (plist-get rule :indent)
                                       '(column-0 context indented))
                                 (symbolp (plist-get rule :face))))
                          v)))
  :group 'f90-ts-font-lock
  :group 'f90-ts-indent
  :group 'f90-ts-comment)


;;;-----------------------------------------------------------------------------

(declare-function f90-ts--abbrev-refresh "f90-ts-mode")

(defun f90-ts--abbrev-set-extra (sym val)
  "Setter for `f90-ts-extra-abbrevs'.
Set SYM to VAL and refresh the abbrev table.
When the abbrev code is not loaded yet, only set the value."
  (when (fboundp 'f90-ts--abbrev-validate)
    (f90-ts--abbrev-validate val))
  (set-default sym val)
  (when (fboundp 'f90-ts--abbrev-refresh)
    (f90-ts--abbrev-refresh)))


(defcustom f90-ts-extra-abbrevs nil
  "Alist of (ABBREV . EXPANSION) added to or overriding the default abbrevs.
ABBREV is one or two characters from [a-z0-9].  The leading accent is
added automatically, so write \"my\" for the abbrev \"`my\".
An entry that matches a default abbrev replaces it.

An expansion may contain \\n for a newline.

Set this variable through the Customize interface (\\[customize-variable])
or with `setopt':

  (setopt f90-ts-extra-abbrevs
          \\='((\"my\" . \"my custom expansion\")
            (\"00\" . \"!-----------\\n\")
            (\"al\" . \"allocatable\")))

Both validate the value and update the abbrev table and the abbrev
menu.  Plain `setq' does neither, so do not use it for this variable."
  :type '(alist :tag "Extra abbreviations (1 or 2 chars without leading accent)"
                :key-type (string :tag "Abbrev"
                                  :size 2
                                  :valid-regexp "[a-z0-9]\\{1,2\\}")
                :value-type (string :tag "Expansion" :format "  %t: %v"))
  :set #'f90-ts--abbrev-set-extra
  :initialize #'custom-initialize-default
  :group 'f90-ts)


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-custom)

;;; f90-ts-custom.el ends here
