;;; f90-ts-mode.el --- Tree-sitter based Fortran 90 mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein <mscfd@gmx.net>
;; Maintainer: Martin Stein <mscfd@gmx.net>
;; URL: https://github.com/mscfd/emacs-f90-ts-mode
;; Keywords: languages, treesitter, fortran
;; Version: 0.6.1-snapshot
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

;; f90-ts-mode is a major mode for editing Fortran 90/2003 (and newer) source
;; files, based on Emacs's built-in tree-sitter support (requires Emacs 29+)
;;
;; Changelog (recent):
;; [11-2026]
;;   - Support for abbreviations through abbrev-mode and transient menu added.
;;   - Alignment rules for case statements of select case blocks added.
;;   - Alignment rules for trailing comments within continued lines as well as
;;     those following a statement added.
;; [10-2026]
;;   - Extend/reduce variants for mark region operations based on siblings
;;     (first, last, previous, next) added.
;;   - Package f90-ts-nav replaced by improved f90-ts-toc providing a side panel with a
;;     table of content of a f90 buffer, see `MANUAL.md' for details.  This
;;     optional package requires its own use-package statement.
;;   - Alignment rules for improved indentation of variable declarations with
;;     initialisation added.
;;   - Infinite loop caused by `f90-ts-indent-delete-trailing-whitespace' fixed.
;;     This happened if region ended at last line without a trailing newline.
;;   - Right-align-at-comma options for array literals added,
;;     see `f90-ts-indent-array-align'.
;;   - Changes required by updates to the proposed string_literal branch added.
;;   - Support for new keyword "non_recursive" added (requires new grammar).
;;   - Support for new grammar version with a different tree structure in some
;;     type related statements (allocate, select type) added.  Both new and old
;;     grammar CST variants are supported.
;;   - Prefix of ERT test names shortened `f90-ts-mode-test--' (no distinction
;;     between std and extra anymore).
;;   - Transient menu restructured and decomposed.
;;   - Support for hideshow and outline added.
;;
;; [09-2026]
;;   - `f90-ts-mode.el' decomposed into several smaller packages.  Experimental
;;     `f90-ts-nav' (tree in fortran menu and tree view in side panel) has been
;;     made optional and requires a separate use-package to load it,
;;     see `README.md'.
;;   - `f90-ts-indent-delete-trailing-whitespace' added to automatically delete
;;     trailing whitespace after indentation operation.
;;   - Font locking of interface name in deferred procedure declaration fixed.
;;   - Trimming of trailing whitespace characters in thing-end-of-X navigation
;;     added.
;;   - Support for Emacs 29 + tree-sitter 0.20.x added (tested with 29.1, 29.3
;;     and tree-sitter 0.20.8).
;;   - Fontification of error nodes fixed if line limitting is enabled.
;;   - Some issues in comment-region operations fixed (preserve indentation,
;;     preserve trailing whitespace where possible, keep existing alignment
;;     with keep options, do not operate outside of region boundaries, add
;;     missing function `f90-ts-indent-region').
;;   - Indentation after uncommenting lines in comment-region operation on
;;     commented lines of code containing leading ampersand or statement label
;;     fixed.
;;   - Missing option `keep-or-continued-line' added to
;;     `f90-ts--indent-options-alist' for indentation selection options.
;;   - Syntax highlighting, indentation and break/join/fill for string literals
;;     improved.  This requires a proposed (but not yet merged) tree-sitter
;;     language grammar extension.  See README.md for more details.
;;   - Testing with Emacs 31.1 and tree-sitter 0.26 added.
;;
;; Features:
;;   - Almost all statements up to F2023
;;   - Syntax highlighting, including syntactically incorrect code
;;   - Indentation of lines, regions, multiline statements and structure blocks
;;   - Alignment for multiline statements with rotation and other options
;;   - Smart end completion
;;   - Configurable leading ampersand and statement label positions
;;   - Breaking and joining of continued lines
;;   - Fill and rebalance operations for lines or regions (with rightmost
;;     breakpoint selection or interactive break and join session)
;;   - Region selection based on tree-sitter nodes
;;   - (Un)commenting regions with configurable prefixes and indentation rules
;;   - Special comments like doc strings and separators
;;     (syntax highlighting and indentation options)
;;   - Keyword highlighting in comments (like TODO, Remark etc.)
;;   - OpenMP and preprocessor directives
;;   - Coarray keywords and statements
;;   - Imenu and a Fortran menu in the menu bar
;;   - Navigation (defun, things, Xref)
;;   - TOC side panel to show interactive table of content for a f90-ts buffer (optional package)
;;   - Hideshow and outline support (support for external treesit-fold is pending)
;;
;; Features can be found by the fortran menu or a transient popup bound
;; to the key C-c C-f.
;;
;; Installation requires the tree-sitter Fortran grammar, which can be found at
;;   https://github.com/stadelmanma/tree-sitter-fortran
;;
;; Basic setup with use-package:
;;
;;   (use-package f90-ts-mode
;;     :mode ("\\.f90\\'" . f90-ts-mode))
;;
;; See the README and MANUAL at https://github.com/mscfd/emacs-f90-ts-mode
;; for full documentation on options, keybindings, etc.
;;
;; Bugs and features:
;;   https://github.com/mscfd/emacs-f90-ts-mode/issues
;;
;; Notes:
;; - Emacs 31 supports 0.26, and the mode runs in both configurations.
;; - Emacs 30.x must be linked against tree-sitter 0.25.x at runtime.
;; - Emacs 29 support has been tested with treesitter 0.20.8.
;; For details see MANUAL at https://github.com/mscfd/emacs-f90-ts-mode

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'transient)

;; for loading online README and MANUAL
(require 'url)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds (mostly tree-sitter bugs), and some missing
;; functions for Emacs 29 and Emacs 30
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)

;; font locking and indentation rules
(require 'f90-ts-font-lock)
(require 'f90-ts-indent)

;; various operations
(require 'f90-ts-break-join-fill)
(require 'f90-ts-mark-region)
(require 'f90-ts-comment-region)
(require 'f90-ts-xref)
(require 'f90-ts-imenu)
(require 'f90-ts-thing)
(require 'f90-ts-fold)

;;;-----------------------------------------------------------------------------

(defconst f90-ts-mode-version "0.5.0-snapshot"
  "Version of `f90-ts-mode'.")


(defconst f90-ts--github-url
  "https://github.com/mscfd/emacs-f90-ts-mode")


(defconst f90-ts--about-text
  "f90-ts-mode is a major mode for editing Fortran 90/2003 (and newer)
source files, based on Emacs's built-in tree-sitter support
(requires Emacs 29+).

Changelog (recent):

[11-2026]
- Support for abbreviations through abbrev-mode and transient menu added.
- Alignment rules for case statements of select case blocks added.
- Alignment rules for trailing comments within continued lines as well as
  those following a statement added.

[10-2026]
- Extend/reduce variants for mark region operations based on siblings
  (first, last, previous, next) added.
- Package f90-ts-nav replaced by improved f90-ts-toc providing a side panel with a
  table of content of a f90 buffer, see `MANUAL.md' for details.  This
  optional package requires its own use-package statement.
- Alignment rules for improved indentation of variable declarations with
  initialisation added.
- Infinite loop caused by `f90-ts-indent-delete-trailing-whitespace' fixed.
  This happened if region ended at last line without a trailing newline.
- Right-align-at-comma options for array literals added,
  see `f90-ts-indent-array-align'.
- Changes required by updates to the proposed string_literal branch added.
- Support for new keyword \"non_recursive\" added (requires new grammar).
- Support for new grammar version with a different tree structure in some
  type related statements (allocate, select type) added.  Both new and old
  grammar CST variants are supported.
- Prefix of ERT test names shortened `f90-ts-mode-test--' (no distinction
  between std and extra anymore).
- Transient menu restructured and decomposed.
- Support for hideshow and outline added.

[09-2026]
- `f90-ts-mode.el' decomposed into several smaller packages. Experimental
  `f90-ts-nav' (tree in fortran menu and tree view in side panel) has been
  made optional and requires a separate use-package to load it,
  see `README.md'.
- `f90-ts-indent-delete-trailing-whitespace' added to automatically delete
  trailing whitespace after indentation operation.
- Font locking of interface name in deferred procedure declaration fixed.
- Handling of trailing whitespace characters in thing-end-of-X navigation added.
- Support for Emacs 29 + tree-sitter 0.20.x added (tested with 29.1, 29.3 and
  tree-sitter 0.20.8).
- Fontification of error nodes fixed if line limitting is enabled.
- Some issues in comment-region operations fixed (preserve indentation,
  preserve trailing whitespace where possible, keep existing alignment
  with keep options, do not operate outside of region boundaries, add
  missing function `f90-ts-indent-region').
- Indentation after uncommenting lines in comment-region operation on
  commented lines of code containing leading ampersand or statement label fixed.
- Missing option `keep-or-continued-line' added to
  `f90-ts--indent-options-alist' for indentation selection options.
- Improve syntax highlighting, indentation and break/join/fill for string
  literals.  This requires a proposed (but not yet merged) tree-sitter
  language grammar extension.  See README.md for more details.
- Testing with emacs 31.1 and tree-sitter 0.26 added.
")


;;;-----------------------------------------------------------------------------
;; syntax table

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
;; keymap

(defvar-keymap f90-ts-mode-map
  :doc "Keymap for `f90-ts-mode'."
  ;; TAB commands
  "C-<tab>"           #'f90-ts-indent-and-complete-stmt
  "<backtab>"         #'f90-ts-indent-for-tab-command-2 ; S-<tab>
  "C-S-<iso-lefttab>" #'f90-ts-indent-for-tab-command-3 ; Linux
  "C-<backtab>"       #'f90-ts-indent-for-tab-command-3 ; Windows?

  ;; other keybindings inspired by f90-mode
  "C-<return>" #'f90-ts-break-line
  "C-c ;"      #'f90-ts-comment-region-default
  "C-c '"      #'f90-ts-comment-region-custom

  ;; C-c C-f prefix for the transient menu
  "C-c C-f" #'f90-ts-transient
  "C-`"     #'f90-ts-abbrev-menu)



;;;-----------------------------------------------------------------------------
;; abbrev table
;; enabled by (abbrev-mode), or through a transient menu

;; The abbrev definitions are derived from `f90-mode-abbrev-table' in
;; f90.el (GNU Emacs), Copyright (C) 1995-2026 Free Software Foundation, Inc.
;; Modified: three-letter names shortened for the transient menu, duplicate
;; "`pr" fixed, newer Fortran keywords added. See trailing comments below.
(defconst f90-ts--abbrev-definitions
  '(;; From f90-mode.
    ("`al"  . "allocate")
    ("`ab"  . "allocatable")
    ("`ai"  . "abstract interface")
    ("`as"  . "assignment")
    ("`ay"  . "asynchronous")           ; f90-mode: `asy
    ("`ba"  . "backspace")
    ("`bd"  . "block data")
    ("`bl"  . "block")
    ("`c"   . "character")              ; menu key: cc
    ("`cl"  . "close")
    ("`cm"  . "common")
    ("`cx"  . "complex")
    ("`cn"  . "contains")
    ("`cr"  . "critical")
    ("`cy"  . "cycle")
    ("`de"  . "deallocate")
    ("`df"  . "define")
    ("`di"  . "dimension")
    ("`dp"  . "double precision")
    ("`dw"  . "do while")
    ("`el"  . "else")
    ("`ei"  . "else if")                ; f90-mode: `eli
    ("`ew"  . "elsewhere")              ; f90-mode: `elw
    ("`em"  . "elemental")
    ("`e"   . "enumerator")             ; menu key: ee
    ("`eq"  . "equivalence")
    ("`ex"  . "external")
    ("`ey"  . "entry")
    ("`fl"  . "forall")
    ("`fo"  . "format")
    ("`fu"  . "function")
    ("`fa"  . ".false.")
    ("`im"  . "implicit none")
    ("`in"  . "include")
    ("`i"   . "integer")                ; menu key: ii
    ("`it"  . "intent")
    ("`if"  . "interface")
    ("`lo"  . "logical")
    ("`mo"  . "module")
    ("`na"  . "namelist")
    ("`nu"  . "nullify")
    ("`op"  . "optional")
    ("`pa"  . "parameter")
    ("`po"  . "pointer")
    ("`pr"  . "print")
    ("`pi"  . "private")
    ("`pm"  . "program")
    ("`pt"  . "protected")              ; f90-mode: duplicate `pr
    ("`pu"  . "public")
    ("`r"   . "real")                   ; menu key: rr
    ("`rc"  . "recursive")
    ("`rt"  . "return")
    ("`rw"  . "rewind")
    ("`se"  . "select")
    ("`sq"  . "sequence")
    ("`su"  . "subroutine")
    ("`ta"  . "target")
    ("`tr"  . ".true.")
    ("`t"   . "type")                   ; menu key: tt
    ("`vo"  . "volatile")
    ("`wh"  . "where")
    ("`wr"  . "write")
    ;; added: common statements missing in f90-mode (but maybe a bit short)
    ("`en"  . "end")
    ("`us"  . "use")
    ("`sv"  . "save")
    ("`va"  . "value")
    ("`sp"  . "stop")
    ("`xi"  . "exit")
    ("`ca"  . "case")
    ;; added: coarrays and parallel features
    ("`cd"  . "codimension")
    ("`dc"  . "do concurrent")
    ("`es"  . "error stop")
    ("`sa"  . "sync all")
    ("`si"  . "sync images")
    ("`sy"  . "sync memory")
    ("`lk"  . "lock")
    ("`ul"  . "unlock")
    ("`ct"  . "contiguous")
    ;; added: object-oriented features and procedures
    ("`at"  . "abstract")
    ("`cs"  . "class")
    ("`dr"  . "deferred")
    ("`xt"  . "extends")
    ("`fi"  . "final")
    ("`gn"  . "generic")
    ("`pc"  . "procedure")
    ("`mp"  . "module procedure")
    ("`sm"  . "submodule")
    ("`ac"  . "associate")
    ("`st"  . "select type")
    ("`sr"  . "select rank")
    ("`bc"  . "bind(c)")
    ("`kd"  . "kind")
    ("`ma"  . "move_alloc")
    ("`pe"  . "pure")
    ("`ie"  . "impure"))
  "Alist of (ABBREV . EXPANSION) for `f90-ts-mode'.
Single-letter abbrevs (\"`c\", \"`e\", \"`i\", \"`r\", \"`t\") use a doubled
key in the transient menu (cc, ee, ii, rr, tt) so that no menu key is
a prefix of another.  Hence these doubled names are unavailable as
abbrevs of their own.  All other abbrevs must have exactly two
characters after the accent.")


(define-abbrev-table 'f90-ts-mode-abbrev-table
  (mapcar (lambda (e) (list (car e) (cdr e) nil :system t))
          f90-ts--abbrev-definitions)
  "Abbrev table for `f90-ts-mode'.
Derived from the abbrev table of `f90-mode'."
  ;; Accept ` as the first char of an abbrev.  Also allow _ in abbrevs.
  :regexp "\\(?:[^[:word:]_`]\\|^\\)\\(`?[[:word:]_]+\\)[^[:word:]_]*")


(defun f90-ts--abbrev-key (name)
  "Return the transient key for abbrev NAME.
Strip the leading accent.  A single remaining character is doubled (like `i'
becomes `ii'), so that no key is a prefix of another key."
  (let ((key (string-remove-prefix "`" name)))
    (if (= (length key) 1)
        (concat key key)
        key)))


(defun f90-ts--abbrev-make-command (key exp)
  "Define and return a command that insert EXP, bound to menu key KEY.
The command is named `f90-ts--abbrev-insert-KEY'."
  (let ((cmd (intern (format "f90-ts--abbrev-insert-%s" key))))
    (defalias cmd
      (lambda ()
        (interactive)
        (insert exp))
      (format "Insert \"%s\" (abbrev menu key `%s')." exp key))
    cmd))


(defun f90-ts--abbrev-check-keys (entries)
  "Signal an error if the keys of ENTRIES clash.
Two keys clash if they are equal or one is a prefix of the other,
since transient cannot bind both."
  (let ((clashes (cl-loop
                  with keys = (mapcar #'car entries)
                  for (key . others) on keys
                  nconc (cl-loop
                         for other in others
                         when (or (string-prefix-p key other)
                                  (string-prefix-p other key))
                         collect (cons key other)))))
    (cl-assert (null clashes) nil
               "f90-ts abbrev menu: conflicting keys %S" clashes)))


(defconst f90-ts--abbrev-menu-columns 12
  "Maximum number of abbrevs per column in the abbrev menu.")


(defconst f90-ts--abbrev-menu-entries
  (cl-loop
   for (name . exp) in (seq-sort-by (lambda (def) (f90-ts--abbrev-key (car def)))
                                    #'string<
                                    f90-ts--abbrev-definitions)
   for key = (f90-ts--abbrev-key name)
   collect (list key exp (f90-ts--abbrev-make-command key exp)))
  "Transient suffix specs (KEY DESCRIPTION COMMAND), sorted by KEY.
KEY is the abbrev name without the leading accent (doubled for single
letters), DESCRIPTION is the expansion.  Built once at load time from
`f90-ts--abbrev-definitions'.")


;; static data, checking once at load time
(f90-ts--abbrev-check-keys f90-ts--abbrev-menu-entries)


(defun f90-ts--abbrev-menu-groups (_children)
  "Return the column groups of the abbrev menu.
Used as `:setup-children' function of `f90-ts-abbrev-menu'."
  (transient-parse-suffixes
   'f90-ts-abbrev-menu
   (cl-loop
    for chunk in (seq-partition f90-ts--abbrev-menu-entries
                                f90-ts--abbrev-menu-columns)
    collect (vconcat [""] chunk))))


(transient-define-prefix f90-ts-abbrev-menu ()
  "Insert one of the Fortran abbreviation expansions."
  ["Fortran abbreviations (almost the same as in abbrev-mode)"
   :class transient-columns
   :setup-children f90-ts--abbrev-menu-groups])


;;;-----------------------------------------------------------------------------
;; about page and documentation

(defun f90-ts--snapshot-version-p ()
  "Return non-nil if this is a snapshot version."
  (string-suffix-p "-snapshot" f90-ts-mode-version))


(defun f90-ts--github-raw-url ()
  "Return the github url for raw content."
  (string-replace "github.com" "raw.githubusercontent.com" f90-ts--github-url))


(defun f90-ts--git-ref ()
  "Return the git reference used for online documentation.
For snapshot version use the main branch (which might be out of sync with
current version), otherwise use the tag derived from version.
The tag is version string prefixed by \"v\"."
  (if (f90-ts--snapshot-version-p)
      "main"
    (concat "v" f90-ts-mode-version)))


(defun f90-ts--strip-markdown-links ()
  "Replace Markdown links [text](url) by plain text.
This is done as neither internal nor external links currently work
in `markdown-view-mode'."
  (goto-char (point-min))
  (while (re-search-forward
          "\\[\\([^]]+\\)\\](\\([^)]*\\))"
          nil t)
    (replace-match "\\1" t nil)))


(declare-function url-insert-file-contents "url-handlers"
                  (url &optional visit beg end replace))

(defun f90-ts--browse-doc (doc-name)
  "Open the online document DOC-NAME from the github repository.
If `markdown-view-mode' or github variant `gfm-view-mode' from the
package `markdown-mode' are available, then use these."
  (when (f90-ts--snapshot-version-p)
    (message "Opening %s from the current development branch main." doc-name))

  (let ((md-view-mode (cond
                       ((fboundp 'gfm-view-mode)      'gfm-view-mode)
                       ((fboundp 'markdown-view-mode) 'markdown-view-mode))))
    (if md-view-mode
      (let ((url (format "%s/%s/%s"
                         (f90-ts--github-raw-url)
                         (f90-ts--git-ref)
                         doc-name))
            (buffer-name (format "*f90-ts-mode %s*" doc-name)))
        (with-current-buffer (get-buffer-create buffer-name)
          (require 'url)
          (erase-buffer)
          (url-insert-file-contents url)
          (f90-ts--strip-markdown-links)
          ;; use markdown-mode if available
          (funcall md-view-mode)
          (pop-to-buffer (current-buffer))))
      ;; open browser
      (message "Install markdown-mode to view %s directly in a buffer." doc-name)
      (browse-url
       (format "%s/blob/%s/%s"
               f90-ts--github-url
               (f90-ts--git-ref)
               doc-name)))))


(defun f90-ts--browse-readme ()
  "Open the online document \"README.md\" from the github repository."
  (interactive)
  (f90-ts--browse-doc "README.md"))


(defun f90-ts--browse-manual ()
  "Open the online document \"MANUAL.md\" from the github repository."
  (interactive)
  (f90-ts--browse-doc "MANUAL.md"))


(defun f90-ts-mode-about ()
  "Display information about `f90-ts-mode'."
  (interactive)
  (with-help-window "*About f90-ts-mode*"
    (let ((version-line (format "version: f90-ts-mode %s" f90-ts-mode-version)))
      (princ version-line)
      (princ "\n")
      (princ (make-string (string-width version-line) ?-))
      (princ "\n"))
    (princ f90-ts--about-text)
    (princ "\nRepository:\n")
    (princ f90-ts--github-url)

    (insert "\n\nDocumentation")
    (insert
     (if (f90-ts--snapshot-version-p)
         "  (current development branch: main)\n\n"
       (format "  (release %s)\n\n" f90-ts-mode-version)))

    (insert-text-button
     "Open README from github"
     'follow-link t
     'action (lambda (_)
               (f90-ts--browse-doc "README.md")))
    (insert "\n")

    (insert-text-button
     "Open MANUAL from github"
     'follow-link t
     'action (lambda (_)
               (f90-ts--browse-doc "MANUAL.md")))
    (insert "\n")))


;;;-----------------------------------------------------------------------------
;; transient auxiliary

(transient-define-infix f90-ts-transient--indent-list-line ()
  "Transient infix for indent-line alignment variant."
  :class    'transient-lisp-variable
  :variable 'f90-ts-indent-list-line
  :prompt   "Indent list line method: "
  :reader   (lambda (prompt initial _history)
              (let* ((choice (completing-read
                               prompt
                               f90-ts--indent-options-alist
                               nil t nil nil
                               (car (rassq initial f90-ts--indent-options-alist)))))
                (cdr (assoc choice f90-ts--indent-options-alist)))))


(transient-define-infix f90-ts-transient--fill-column ()
  "Transient infix for fill column override."
  :class    'transient-lisp-variable
  :variable 'fill-column
  :prompt   "Fill column: "
  :reader   (lambda (prompt initial _history)
               (read-number prompt initial)))


(transient-define-suffix f90-ts-transient--fill-select-breakpoint-by ()
  "Toggle breakpoint selection between `rightmost' and `interactive'."
  :transient t
  :description (lambda ()
                 (concat "Fill column select by: "
                         (propertize (symbol-name f90-ts-fill-select-breakpoint-by)
                                     'face 'transient-value)))
  (interactive)
  (setq f90-ts-fill-select-breakpoint-by
        (if (eq f90-ts-fill-select-breakpoint-by 'interactive)
            'rightmost
          'interactive)))


;;;-----------------------------------------------------------------------------
;; transient sub-menus

(defun f90-ts--outline-active-p ()
  "Return non-nil if `outline-minor-mode' is active in the current buffer."
  (bound-and-true-p outline-minor-mode))


(transient-define-prefix f90-ts-transient-things ()
  "Navigation for derived types and interfaces in `f90-ts-mode'."
  [["Derived Type"
    ("a" "Beginning"                f90-ts-thing-beginning-of-type)
    ("e" "End"                      f90-ts-thing-end-of-type)
    ("p" "Previous"                 f90-ts-thing-prev-type)
    ("n" "Next"                     f90-ts-thing-next-type)]
   ["Interface"
    ("A" "Beginning"                f90-ts-thing-beginning-of-interface)
    ("E" "End"                      f90-ts-thing-end-of-interface)
    ("P" "Previous"                 f90-ts-thing-prev-interface)
    ("N" "Next"                     f90-ts-thing-next-interface)]])


(transient-define-prefix f90-ts-transient-hideshow ()
  "Hideshow commands for `f90-ts-mode'."
  [["Hideshow"
    ("c" "Toggle block"             hs-toggle-hiding)
    ("h" "Hide block"               hs-hide-block)
    ("s" "Show block"               hs-show-block)
    ("l" "Hide level"               hs-hide-level)
    ("H" "Hide all"                 hs-hide-all)
    ("S" "Show all"                 hs-show-all)
    ("C" "Cycle block"              hs-cycle
     :if (lambda () (fboundp 'hs-cycle)))]])


(transient-define-prefix f90-ts-transient-outline ()
  "Outline commands for `f90-ts-mode'."
  [["Show"
    ("e" "Entry"                    outline-show-entry)
    ("s" "Subtree"                  outline-show-subtree)
    ("a" "All"                      outline-show-all)
    ("k" "Branches"                 outline-show-branches)
    ("i" "Children"                 outline-show-children)]
   ["Hide"
    ("c" "Entry"                    outline-hide-entry)
    ("d" "Subtree"                  outline-hide-subtree)
    ("q" "Sublevels"                outline-hide-sublevels)
    ("t" "Body"                     outline-hide-body)
    ("l" "Leaves"                   outline-hide-leaves)]
   ["Move & cycle"
    ("n" "Next heading"             outline-next-visible-heading)
    ("p" "Previous heading"         outline-previous-visible-heading)
    ("f" "Next same level"          outline-forward-same-level)
    ("b" "Previous same level"      outline-backward-same-level)
    ("u" "Up heading"               outline-up-heading)
    ("TAB" "Cycle subtree"          outline-cycle
     :if (lambda () (fboundp 'outline-cycle)))]])


(transient-define-prefix f90-ts-transient-misc ()
  "Miscellaneous commands for `f90-ts-mode'."
  [["About & Doc"
    ("a" "About"                    f90-ts-mode-about)
    ("r" "README"                   f90-ts--browse-readme)
    ("m" "MANUAL"                   f90-ts--browse-manual)]])


;;;-----------------------------------------------------------------------------
;; transient main menu

(transient-define-prefix f90-ts-transient ()
  "F90 Tree-sitter Mode."
  ;; Modify
  [["Indentation, break & join"
    ("L"     "Indent list line:"          f90-ts-transient--indent-list-line)
    ("TAB"   "Indent & complete line"     f90-ts-indent-and-complete-line)
    ("s"     "Indent & complete stmt"     f90-ts-indent-and-complete-stmt)
    ("I"     "Indent & complete region"   f90-ts-indent-and-complete-region)
    ("C-TAB" "Indent line"                f90-ts-indent-line)
    ("C-I"   "Indent region"              f90-ts-indent-region)
    ("b"     "Break line"                 f90-ts-break-line)
    ("j"     "Join with previous line"    f90-ts-join-line-prev)
    ("J"     "Join with next line"        f90-ts-join-line-next)
    ("C-s"   "Shift line break"           f90-ts-shift-line-break)]
   ["Mark and (un)comment region"
    ("r"   "Enlarge"                    f90-ts-mark-region-enlarge)
    ("0"   "Shrink to first child"      f90-ts-mark-region-shrink-child-first)
    ("9"   "Shrink to last child"       f90-ts-mark-region-shrink-child-last)
    ("X"   "Exchange point and mark"    exchange-point-and-mark)
    ("c"   "Comment region (default)"   f90-ts-comment-region-default)
    ("C"   "Comment region (custom)"    f90-ts-comment-region-custom)]
   ["Mark sibling region"
    ("{"   "First sibling"              f90-ts-mark-region-first-sibling)
    ("["   "Previous sibling"           f90-ts-mark-region-prev-sibling)
    ("]"   "Next sibling"               f90-ts-mark-region-next-sibling)
    ("}"   "Last sibling"               f90-ts-mark-region-last-sibling)
    ("M-{" "Extend/reduce to first"     f90-ts-mark-region-extend-first-sibling)
    ("M-[" "Extend/reduce to previous"  f90-ts-mark-region-extend-prev-sibling)
    ("M-]" "Extend/reduce to next"      f90-ts-mark-region-extend-next-sibling)
    ("M-}" "Extend/reduce to last"      f90-ts-mark-region-extend-last-sibling)
    ("X"   "Exchange point and mark"    exchange-point-and-mark)]]

  ;; Navigate and sub-menus
  [["Fill/Rebalance"
    ("C-f" "Fill column:"               f90-ts-transient--fill-column)
    ;; description text for "C-b" is in description slot
    ;; of `f90-ts-transient--fill-select-breakpoint-by'
    ("C-b"                              f90-ts-transient--fill-select-breakpoint-by)
    ("f"   "Fill region/buffer"         f90-ts-fill-region)
    ("M-f" "Fill at line"               f90-ts-fill-at-line)
    ("M-j" "Fill with prev line"        f90-ts-fill-prev-line)
    ("M-J" "Fill with next line"        f90-ts-fill-next-line)]
   ["Procedure"
    ("a"   "Beginning"                  f90-ts-thing-beginning-of-procedure)
    ("e"   "End"                        f90-ts-thing-end-of-procedure)
    ("p"   "Previous"                   f90-ts-thing-prev-procedure)
    ("n"   "Next"                       f90-ts-thing-next-procedure)]
   ["Xref"
    ("."   "Find definition"            xref-find-definitions)
    (","   "Find references"            xref-find-references)
    ("/"   "Find apropos"               xref-find-apropos)
    ("<"   "Go back"                    xref-go-back)
    (">"   "Go forward"                 xref-go-forward)]
   ["More"
    ("B" "Open TOC side panel"          f90-ts-toc-buffer-open
     :if (lambda () (featurep 'f90-ts-toc)))
    ("F" "Focus TOC side panel"         f90-ts-toc-buffer-focus
     :if (lambda () (featurep 'f90-ts-toc)))
    ("t"   "Types & interfaces..."      f90-ts-transient-things)
    ("@"   "Hideshow..."                f90-ts-transient-hideshow
     :if (lambda () (bound-and-true-p hs-minor-mode)))
    ("$"   "Outline..."                 f90-ts-transient-outline
     :if f90-ts--outline-active-p)
    ("m"   "Misc & help..."             f90-ts-transient-misc)]])


;;;-----------------------------------------------------------------------------
;; menu definition

(easy-menu-define f90-ts-mode-menu f90-ts-mode-map
  "Menu for `f90-ts-mode'."
  '("Fortran"
    ("Indent"
     ;; `indent-for-tab-command' invokes `f90-ts-indent-and-complete-line',
     ;; but we use `indent-for-tab-command' to have keybinding shown properly
     ["Indent line"                 indent-for-tab-command             :active t]
     ["Indent line (alt 2)"         f90-ts-indent-for-tab-command-2    :active t]
     ["Indent line (alt 3)"         f90-ts-indent-for-tab-command-3    :active t]
     ["Indent & complete statement" f90-ts-indent-and-complete-stmt    :active t]
     ["Indent & complete region"    f90-ts-indent-and-complete-region  :active (region-active-p)]
     "---"
     ["Complete end statements in region" f90-ts-complete-smart-end-region :active t])
    ("Break & Join lines"
     ["Break line"          f90-ts-break-line      :active t]
     ["Join with prev line" f90-ts-join-line-prev  :active t]
     ["Join with next line" f90-ts-join-line-next  :active t])
    ("Comment"
     ["Comment/uncomment region (default prefix)" f90-ts-comment-region-default :active (region-active-p)]
     ["Comment/uncomment region (custom prefix)"  f90-ts-comment-region-custom  :active (region-active-p)])
    ("Mark region"
     ["Enlarge"               f90-ts-mark-region-enlarge            :active t]
     ["Shrink to first child" f90-ts-mark-region-shrink-child-first :active (region-active-p)]
     ["Shrink to last child"  f90-ts-mark-region-shrink-child-last  :active (region-active-p)]
     ["First sibling"         f90-ts-mark-region-first-sibling      :active (region-active-p)]
     ["Previous sibling"      f90-ts-mark-region-prev-sibling       :active (region-active-p)]
     ["Next sibling"          f90-ts-mark-region-next-sibling       :active (region-active-p)]
     ["Last sibling"          f90-ts-mark-region-last-sibling       :active (region-active-p)]
     "---"
     ["Extend/reduce to first sibling"    f90-ts-mark-region-extend-first-sibling :active (region-active-p)]
     ["Extend/reduce to previous sibling" f90-ts-mark-region-extend-prev-sibling  :active (region-active-p)]
     ["Extend/reduce to next sibling"     f90-ts-mark-region-extend-next-sibling  :active (region-active-p)]
     ["Extend/reduce to last sibling"     f90-ts-mark-region-extend-last-sibling  :active (region-active-p)])
    "---"
    ("Defun & Thing"
     ["Beginning of defun" beginning-of-defun :active t]
     ["End of defun"       end-of-defun       :active t]
     ["Mark defun"         mark-defun         :active t]
     "---"
     ["Narrow to defun"    narrow-to-defun    :active t]
     ["Widen"              widen              :active (buffer-narrowed-p)]
     "---"
     ["Next procedure"          f90-ts-thing-next-procedure          :active t]
     ["Prev procedure"          f90-ts-thing-prev-procedure          :active t]
     ["Beginning of procedure"  f90-ts-thing-beginning-of-procedure  :active t]
     ["End of procedure"        f90-ts-thing-end-of-procedure        :active t]
     "---"
     ["Next type"               f90-ts-thing-next-type               :active t]
     ["Prev type"               f90-ts-thing-prev-type               :active t]
     ["Beginning of type"       f90-ts-thing-beginning-of-type       :active t]
     ["End of type"             f90-ts-thing-end-of-type             :active t]
     "---"
     ["Next interface"          f90-ts-thing-next-interface          :active t]
     ["Prev interface"          f90-ts-thing-prev-interface          :active t]
     ["Beginning of interface"  f90-ts-thing-beginning-of-interface  :active t]
     ["End of interface"        f90-ts-thing-end-of-interface        :active t])
    ("Xref"
     ["Find definition"  xref-find-definitions :active t]
     ["Find references"  xref-find-references  :active t]
     ["Find apropos"     xref-find-apropos     :active t]
     "---"
     ["Go back"          xref-go-back          :active t]
     ["Go forward"       xref-go-forward       :active t])
    "---"
    ["Open TOC side panel"  f90-ts-toc-buffer-open   :visible (featurep 'f90-ts-toc) :active t]
    ["Focus TOC side panel" f90-ts-toc-buffer-focus  :visible (featurep 'f90-ts-toc) :active t]
    "---"
    ["Customize f90-ts"  (customize-group 'f90-ts) :active t]
    ["About f90-ts-mode" f90-ts-mode-about t]
    ["README (GitHub)"   f90-ts--browse-readme t]
    ["MANUAL (GitHub)"   f90-ts--browse-manual t]))


;;;-----------------------------------------------------------------------------

;;;###autoload
(define-derived-mode f90-ts-mode prog-mode "F90[TS]"
  "Major mode for editing Fortran 90+ files, using tree-sitter library."
  :group 'f90-ts
  :syntax-table f90-ts-mode-syntax-table
  :abbrev-table f90-ts-mode-abbrev-table

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

  ;; set font-lock feature list
  (setq-local treesit-font-lock-feature-list
              '((comment preproc error)                          ; level 1
                (builtin keyword string type)                    ; level 2
                (constant number escape-sequence)                ; level 3
                (function variable operator bracket delimiter))) ; level 4

  ;; use the pre-defined font-lock rules variable
  (setq-local treesit-font-lock-settings (f90-ts-font-lock-rules))

  ;; use the pre-defined indentation rules variable
  (setq-local treesit-simple-indent-rules (f90-ts-indent-rules))

  ;; set Imenu
  (setq-local imenu-create-index-function #'f90-ts-simple-imenu)

  ;; set Defun stuff
  (setq-local treesit-defun-type-regexp f90-ts--thing-defun-regexp-pred)
  (setq-local treesit-defun-name-function #'f90-ts--defun-name)
  (setq-local treesit-defun-tactic 'nested)  ; or 'top-level?
  (setq-local treesit-thing-settings f90-ts--thing-settings)

  ;; this setup function must be called after setting variables above
  (treesit-major-mode-setup)

  ;; set indentation functions (both add smart end completion before
  ;; indentation, so no hook available); this must be done after
  ;; calling treesit-major-mode-setup
  (setq-local indent-line-function #'f90-ts-indent-and-complete-line)
  (setq-local indent-region-function #'f90-ts-indent-and-complete-region)

  (add-hook 'xref-backend-functions #'f90-ts-xref-backend nil t)

  ;; hideshow and outline support (hs-minor-mode, outline-minor-mode)
  (f90-ts-fold-setup)

  ;;(setq-local treesit--font-lock-verbose t)
  ;;(setq-local treesit--indent-verbose t)

  ;; provide mode name
  (setq-local mode-name "F90-TS"))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-mode)

;;; f90-ts-mode.el ends here
