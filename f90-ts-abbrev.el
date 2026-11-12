;;; f90-ts-abbrev.el --- Abbrevs and abbrev menu for f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein <mscfd@gmx.net>
;; Maintainer: Martin Stein <mscfd@gmx.net>
;; URL: https://github.com/mscfd/emacs-f90-ts-mode
;; Keywords: languages, treesitter, fortran

;; This file is part of f90-ts-mode, but NOT part of GNU Emacs.

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

;; Abbrev table of `f90-ts-mode' (enabled by `abbrev-mode') and the
;; transient abbrev menu `f90-ts-abbrev-menu'.
;;
;; The system abbrevs in `f90-ts-mode-abbrev-table' are always derived
;; from `f90-ts--abbrev-definitions' and `f90-ts-extra-abbrevs'.  They are
;; (re)installed:
;; (1) Once when this file is loaded (so reloading the file after changes
;;     of the defaults updates the table).
;; (2) Whenever `f90-ts-extra-abbrevs' is set through its setter
;;     `f90-ts--abbrev-set-extra' in `f90-ts-custom.el' (validate, store,
;;     reinstall) which uses `f90-ts--abbrev-validate' and
;;     `f90-ts--abbrev-refresh' from here.
;; Each installation first removes all system abbrevs, so abbrevs dropped
;; from `f90-ts-extra-abbrevs' do not linger.  Abbrevs defined by the user
;; (not marked as system abbrevs) are never touched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'abbrev)
(require 'transient)

;; defcustom `f90-ts-extra-abbrevs'
(require 'f90-ts-custom)

;;;-----------------------------------------------------------------------------

;; abbrev table

;; The abbrev definitions are derived from `f90-mode-abbrev-table' in
;; f90.el (GNU Emacs), Copyright (C) 1995-2026 Free Software Foundation, Inc.
;; Modified: three-letter names shortened for the transient menu, duplicate
;; "`pr" fixed, newer Fortran keywords added.  See trailing comments below.

(defconst f90-ts--abbrev-definitions
  '(;; From f90-mode.
    ("`al" . "allocate")
    ("`ab" . "allocatable")
    ("`ai" . "abstract interface")
    ("`as" . "assignment")
    ("`ay" . "asynchronous")       ; f90-mode: `asy
    ("`ba" . "backspace")
    ("`bd" . "block data")
    ("`bl" . "block")
    ("`c"  . "character")          ; menu key: cc
    ("`cl" . "close")
    ("`cm" . "common")
    ("`cx" . "complex")
    ("`cn" . "contains")
    ("`cr" . "critical")
    ("`cy" . "cycle")
    ("`de" . "deallocate")
    ("`df" . "define")
    ("`di" . "dimension")
    ("`dp" . "double precision")
    ("`dw" . "do while")
    ("`el" . "else")
    ("`ei" . "else if")            ; f90-mode: `eli
    ("`ew" . "elsewhere")          ; f90-mode: `elw
    ("`em" . "elemental")
    ("`en" . "enumerator")         ; f90-mode: `e
    ("`eq" . "equivalence")
    ("`ex" . "external")
    ("`ey" . "entry")
    ("`fl" . "forall")
    ("`fo" . "format")
    ("`fu" . "function")
    ("`fa" . ".false.")
    ("`im" . "implicit none")
    ("`in" . "include")
    ("`i"  . "integer")            ; menu key: ii
    ("`it" . "intent")
    ("`if" . "interface")
    ("`lo" . "logical")
    ("`mo" . "module")
    ("`na" . "namelist")
    ("`nu" . "nullify")
    ("`op" . "optional")
    ("`pa" . "parameter")
    ("`po" . "pointer")
    ("`pr" . "print")
    ("`pi" . "private")
    ("`pm" . "program")
    ("`pt" . "protected")          ; f90-mode: duplicate `pr
    ("`pu" . "public")
    ("`r"  . "real")               ; menu key: rr
    ("`rc" . "recursive")
    ("`rt" . "return")
    ("`rw" . "rewind")
    ("`se" . "select")
    ("`sq" . "sequence")
    ("`su" . "subroutine")
    ("`ta" . "target")
    ("`tr" . ".true.")
    ("`t"  . "type")               ; menu key: tt
    ("`vo" . "volatile")
    ("`wh" . "where")
    ("`wr" . "write")
    ;; added: common statements missing in f90-mode (but maybe a bit short)
    ("`sv" . "save")
    ("`va" . "value")
    ("`sp" . "stop")
    ("`xi" . "exit")
    ("`ca" . "case")
    ;; added: coarrays and parallel features
    ("`cd" . "codimension")
    ("`dc" . "do concurrent")
    ("`es" . "error stop")
    ("`sa" . "sync all")
    ("`si" . "sync images")
    ("`sy" . "sync memory")
    ("`lk" . "lock")
    ("`ul" . "unlock")
    ("`ct" . "contiguous")
    ;; added: object-oriented features and procedures
    ("`at" . "abstract")
    ("`cs" . "class")
    ("`dr" . "deferred")
    ("`xt" . "extends")
    ("`fi" . "final")
    ("`gn" . "generic")
    ("`pc" . "procedure")
    ("`mp" . "module procedure")
    ("`sm" . "submodule")
    ("`ac" . "associate")
    ("`sc" . "select case")
    ("`st" . "select type")
    ("`sr" . "select rank")
    ("`bc" . "bind(c)")
    ("`kd" . "kind")
    ("`ma" . "move_alloc")
    ("`pe" . "pure")
    ("`ie" . "impure"))
  "Alist of (ABBREV . EXPANSION) for `f90-ts-mode'.
Single-letter abbrevs (\"`c\", \"`i\", \"`r\", \"`t\") use a doubled
key in the transient menu (cc, ee, ii, rr, tt) so that no menu key is
a prefix of another.  Hence these doubled names are unavailable as
abbrevs of their own.  All other abbrevs must have exactly two
characters after the accent.")


(define-abbrev-table 'f90-ts-mode-abbrev-table nil
  "Abbrev table for `f90-ts-mode'.
Derived from the abbrev table of `f90-mode'.  Populated by
`f90-ts--abbrev-refresh' from `f90-ts--abbrev-definitions' and
`f90-ts-extra-abbrevs'."
  ;; an abbrev is an optional ` followed by word constituents.  `_' is a
  ;; symbol (not word) constituent and deliberately not part of abbrev
  ;; names, which are validated as one or two [a-z0-9] characters.
  :regexp "\\(?:[^[:word:]`]\\|^\\)\\(`?[[:word:]]+\\)[^[:word:]]*")


;;;-----------------------------------------------------------------------------
;; keys, commands and validation

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
The command is named `f90-ts--abbrev-insert-KEY'.  It is redefined on every
call, so it never keeps an outdated expansion."
  (let ((cmd (intern (format "f90-ts--abbrev-insert-%s" key))))
    (defalias cmd
      (lambda ()
        (interactive)
        (insert exp))
      (format "Insert \"%s\" (abbrev menu key `%s')." exp key))
    cmd))


(defun f90-ts--abbrev-check-keys (entries)
  "Signal a `user-error' if the keys of ENTRIES clash.
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
    (when clashes
      (user-error "Abbrev table in f90-ts-mode: conflicting keys %S" clashes))))


(defun f90-ts--abbrev-check-extra (abbrevs)
  "Signal a `user-error' if ABBREVS is not a valid extra abbrev alist."
  (cl-loop
   for abl in abbrevs
   do (unless (and (consp abl)
                   (stringp (car abl))
                   (stringp (cdr abl))
                   (string-match-p "\\`[a-z0-9]\\{1,2\\}\\'" (car abl)))
        (user-error "Abbrev table in f90-ts-mode: abbrev %S needs (\"xy\" . \"expansion\") with \
one or two characters [a-z0-9] and without the leading accent"
                    abl))))


(defun f90-ts--abbrev-unescape (str)
  "Translate the escape \\n in STR into a proper newline character."
  (replace-regexp-in-string "\\\\n" "\n" str t t))


(defun f90-ts--abbrev-effective-definitions (extra)
  "Return default abbrevs, overridden and extended by EXTRA.
EXTRA is an alist like `f90-ts-extra-abbrevs'.  The mandatory leading accent
is added to the extra abbrevs here, and their escapes are translated by
`f90-ts--abbrev-unescape'.  If a name occurs more than once, the last
definition wins."
  (let ((all (append f90-ts--abbrev-definitions
                     (cl-loop
                      for (name . exp) in extra
                      collect (cons (concat "`" name)
                                    (f90-ts--abbrev-unescape exp))))))
    (cl-loop
     for (def . rest) on all
     unless (assoc (car def) rest #'string=)
     collect def)))


;;;-----------------------------------------------------------------------------
;; transient abbrev menu

(defconst f90-ts--abbrev-menu-columns 12
  "Maximum number of abbrevs per column in the abbrev menu.")


(defconst f90-ts--abbrev-menu-max-width 20
  "Maximum number of expansion characters shown in the abbrev menu.
Longer expansions are cut off and followed by \"...\".")


(defconst f90-ts--abbrev-menu-newline "\\n"
  "String shown in the abbrev menu in place of a newline in an expansion.
Use \"\\\\n\" if your font lacks the return symbol.")


(defun f90-ts--abbrev-menu-label (exp)
  "Return EXP shortened for display in the abbrev menu.
Newlines are shown as `f90-ts--abbrev-menu-newline'."
  (let ((shown (replace-regexp-in-string
                "\n" f90-ts--abbrev-menu-newline exp t t)))
    (if (> (length shown) f90-ts--abbrev-menu-max-width)
        (concat (substring shown 0 f90-ts--abbrev-menu-max-width) "...")
      shown)))


(defun f90-ts--abbrev-menu-entries ()
  "Return transient suffix specs (KEY DESCRIPTION COMMAND), sorted by KEY.
KEY is the abbrev name without the leading accent (doubled for single
letters), DESCRIPTION is the expansion, shortened by
`f90-ts--abbrev-menu-label'.  Signal a `user-error' on key clashes."
  (let ((entries
         (cl-loop
          for (name . exp) in (f90-ts--abbrev-effective-definitions
                               f90-ts-extra-abbrevs)
          for key = (f90-ts--abbrev-key name)
          collect (list key
                        (f90-ts--abbrev-menu-label exp)
                        (f90-ts--abbrev-make-command key exp)))))
    (setq entries (seq-sort-by #'car #'string< entries))
    (f90-ts--abbrev-check-keys entries)
    entries))


(defun f90-ts--abbrev-menu-groups (_children)
  "Return the column groups of the abbrev menu.
Used as `:setup-children' function of `f90-ts-abbrev-menu'.  Called each
time the menu is invoked, hence always reflects `f90-ts-extra-abbrevs'."
  (transient-parse-suffixes
   'f90-ts-abbrev-menu
   (cl-loop
    for chunk in (seq-partition (f90-ts--abbrev-menu-entries)
                                f90-ts--abbrev-menu-columns)
    collect (vconcat [""] chunk))))


(transient-define-prefix f90-ts-abbrev-menu ()
  "Insert one of the Fortran abbreviation expansions."
  ["Fortran abbreviations (almost the same as in abbrev-mode)"
   :class transient-columns
   :setup-children f90-ts--abbrev-menu-groups])


;;;-----------------------------------------------------------------------------
;; installation into the abbrev table

(defun f90-ts--abbrev-purge ()
  "Remove all system abbrevs from `f90-ts-mode-abbrev-table'.
Abbrevs defined by the user are kept.  Without this step, abbrevs removed
from `f90-ts-extra-abbrevs' or from the defaults would stay in the table."
  (let ((table f90-ts-mode-abbrev-table)
        stale)
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (symbol-value sym)
                           (abbrev-get sym :system))
                  (push (symbol-name sym) stale)))
              table)
    (dolist (name stale)
      (unintern name table))
    ;; keep the abbrev machinery informed about the change
    (abbrev-table-put table :abbrev-table-modiff
                      (1+ (or (abbrev-table-get table :abbrev-table-modiff) 0)))))


(defun f90-ts--abbrev-install (definitions)
  "Replace all system abbrevs by DEFINITIONS, an alist (ABBREV . EXPANSION)."
  (f90-ts--abbrev-purge)
  (cl-loop
   for (name . exp) in definitions
   do (define-abbrev f90-ts-mode-abbrev-table name exp nil :system t)))


(defun f90-ts--abbrev-validate (extra)
  "Signal a `user-error' if EXTRA is not usable as `f90-ts-extra-abbrevs'.
Check the format of EXTRA and the menu keys of the resulting effective
abbrevs.  This has no side effects, so it can run before the variable is set."
  (f90-ts--abbrev-check-extra extra)
  (f90-ts--abbrev-check-keys
   (cl-loop
    for (name . _exp) in (f90-ts--abbrev-effective-definitions extra)
    collect (list (f90-ts--abbrev-key name)))))


(defun f90-ts--abbrev-refresh ()
  "Validate and (re)install all abbrevs into `f90-ts-mode-abbrev-table'.
Validation happens first, so a bad configuration leaves the table untouched."
  (f90-ts--abbrev-validate f90-ts-extra-abbrevs)
  (f90-ts--abbrev-install
   (f90-ts--abbrev-effective-definitions f90-ts-extra-abbrevs)))


(defun f90-ts-add-custom-abbrevs (abbrevs)
  "Add ABBREVS, a list of (ABBREV . EXPANSION), to `f90-ts-extra-abbrevs'.
ABBREV is given without the leading accent."
  (setopt f90-ts-extra-abbrevs (append f90-ts-extra-abbrevs abbrevs)))


;; Install at load time.  This runs on every load of this file, also when
;; reloading after an upgrade, so new or changed defaults are never missed.
;; An invalid `f90-ts-extra-abbrevs' must not leave the table empty.
(condition-case err
    (f90-ts--abbrev-refresh)
  (user-error
   (f90-ts--abbrev-install f90-ts--abbrev-definitions)
   (display-warning 'f90-ts
                    (format "%s; only default abbrevs installed"
                            (error-message-string err)))))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-abbrev)

;;; f90-ts-abbrev.el ends here
