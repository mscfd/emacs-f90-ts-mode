;;; abbrev-table.el --- Test collection for the abbrev table of f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Resource file with a test collection for `f90-ts-abbrev', not meant to be
;; loaded directly.  It is registered in \"f90-ts-mode-test.el\" through
;; `f90-ts-mode-test--collection-register' and runs as a single ert test,
;; with one subtest per `f90-ts-mode-test-defsub' form.
;;
;; This subtest collection check the abbrev table, the abbrev menu and the
;; validation of `f90-ts-extra-abbrevs'.  Abbrev expansion while typing into
;; a buffer is tested in \"abbrev_expand.erts\".

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'f90-ts-mode-test)


(defun f90-ts-mode-test--abbrev-count ()
  "Return the number of system abbrevs in `f90-ts-mode-abbrev-table'."
  (let ((n 0))
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (symbol-value sym)
                           (abbrev-get sym :system))
                  (cl-incf n)))
              f90-ts-mode-abbrev-table)
    n))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--defaults ()
  "All default abbrevs are installed, and nothing else."
  (f90-ts-mode-test--with-extra-abbrevs nil
    (should (cl-loop
             for (name . exp) in f90-ts--abbrev-definitions
             always (equal (abbrev-expansion
                            name f90-ts-mode-abbrev-table)
                           exp)))
    (should (= (f90-ts-mode-test--abbrev-count)
               (length f90-ts--abbrev-definitions)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--add-and-override ()
  "Extra abbrevs are added, and replace defaults of the same name."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine") ("al" . "allocatable"))
    (should (equal (abbrev-expansion "`my" f90-ts-mode-abbrev-table) "mine"))
    (should (equal (abbrev-expansion "`al" f90-ts-mode-abbrev-table)
                   "allocatable"))
    (should (equal (abbrev-expansion "`de" f90-ts-mode-abbrev-table)
                   "deallocate"))
    (should (= (f90-ts-mode-test--abbrev-count)
               (1+ (length f90-ts--abbrev-definitions))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--remove-is-not-stale ()
  "Removing extras removes their abbrevs and restores overridden defaults."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine") ("al" . "allocatable"))
    (setopt f90-ts-extra-abbrevs nil)
    (should-not (abbrev-expansion "`my" f90-ts-mode-abbrev-table))
    (should (equal (abbrev-expansion "`al" f90-ts-mode-abbrev-table)
                   "allocate"))
    (should (= (f90-ts-mode-test--abbrev-count)
               (length f90-ts--abbrev-definitions)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--replace-extras ()
  "Replacing one extra by another leaves only the new one."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
    (setopt f90-ts-extra-abbrevs '(("yo" . "yours")))
    (should-not (abbrev-expansion "`my" f90-ts-mode-abbrev-table))
    (should (equal (abbrev-expansion "`yo" f90-ts-mode-abbrev-table) "yours"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--last-wins-and-newline ()
  "The last of several equal names wins, and \\n becomes a newline."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "first")
                                          ("my" . "a\\nb")
                                          ("my" . "second\\nline"))
    (should (equal (abbrev-expansion "`my" f90-ts-mode-abbrev-table)
                   "second\nline"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--add-custom-abbrevs ()
  "`f90-ts-add-custom-abbrevs' appends and installs."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
    (f90-ts-add-custom-abbrevs '(("yo" . "yours")))
    (should (equal (abbrev-expansion "`my" f90-ts-mode-abbrev-table) "mine"))
    (should (equal (abbrev-expansion "`yo" f90-ts-mode-abbrev-table) "yours"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--refresh-idempotent ()
  "Refreshing repeatedly does not accumulate or lose abbrevs."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
    (let ((n (f90-ts-mode-test--abbrev-count)))
      (f90-ts--abbrev-refresh)
      (f90-ts--abbrev-refresh)
      (should (= n (f90-ts-mode-test--abbrev-count))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--user-abbrevs-survive ()
  "Abbrevs defined by the user are not touched by refreshing."
  (unwind-protect
      (progn
        (define-abbrev f90-ts-mode-abbrev-table "`usr" "user_defined")
        (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
          (setopt f90-ts-extra-abbrevs nil)
          (should (equal (abbrev-expansion "`usr" f90-ts-mode-abbrev-table)
                         "user_defined"))))
    (unintern "`usr" f90-ts-mode-abbrev-table)))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--invalid-values ()
  "Invalid values are rejected by the setter of `f90-ts-extra-abbrevs'.
`setopt' only warns about a value of the wrong type and then calls the setter,
so the setter must reject such values as well.  It is called directly here,
which also keeps those type warnings out of the test output."
  (let ((setter (get 'f90-ts-extra-abbrevs 'custom-set)))
    (should setter)
    (cl-loop
     for bad in '((("TOO" . "x"))   ; upper case
                  (("abc" . "x"))   ; too long
                  (("a-" . "x"))    ; wrong character
                  (("" . "x"))      ; too short
                  (("`my" . "x"))   ; accent is added automatically
                  (("my" . 42))     ; expansion not a string
                  ("my")            ; not an alist
                  (("ii" . "x")))   ; menu key clash with default "`i"
     do (ert-info ((format "%S" bad) :prefix "Value: ")
          (should-error (funcall setter 'f90-ts-extra-abbrevs bad)
                        :type 'user-error)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--invalid-value-changes-nothing ()
  "A rejected value changes neither the variable nor the abbrev table."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
    (should-error (setopt f90-ts-extra-abbrevs '(("yo" . "yours") ("TOO" . "x")))
                  :type 'user-error)
    (should (equal f90-ts-extra-abbrevs '(("my" . "mine"))))
    (should (equal (abbrev-expansion "`my" f90-ts-mode-abbrev-table) "mine"))
    (should-not (abbrev-expansion "`yo" f90-ts-mode-abbrev-table))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--menu-entries ()
  "The menu is derived from the current effective abbrevs."
  (f90-ts-mode-test--with-extra-abbrevs '(("my" . "mine"))
    (let ((entries (f90-ts--abbrev-menu-entries)))
      (should (assoc "my" entries))
      (should (assoc "ii" entries))      ; "`i" has a doubled menu key
      (should-not (assoc "i" entries))
      (should (= (length entries) (1+ (length f90-ts--abbrev-definitions)))))
    (setopt f90-ts-extra-abbrevs nil)
    (should-not (assoc "my" (f90-ts--abbrev-menu-entries)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--menu-commands-not-stale ()
  "Menu commands insert the current expansion, also after a change."
  (f90-ts-mode-test--with-extra-abbrevs nil
    (with-temp-buffer
      (funcall (nth 2 (assoc "al" (f90-ts--abbrev-menu-entries))))
      (should (equal (buffer-string) "allocate"))))
  (f90-ts-mode-test--with-extra-abbrevs '(("al" . "allocatable"))
    (with-temp-buffer
      (funcall (nth 2 (assoc "al" (f90-ts--abbrev-menu-entries))))
      (should (equal (buffer-string) "allocatable")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--customize-set-variable ()
  "`customize-set-variable' goes through the setter as well."
  (f90-ts-mode-test--with-extra-abbrevs nil
    (customize-set-variable 'f90-ts-extra-abbrevs '(("my" . "mine")))
    (should (equal (abbrev-expansion "`my" f90-ts-mode-abbrev-table) "mine"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--abbrev--load-with-invalid-extras ()
  "Loading with invalid extras warns and installs the default abbrevs only."
  (let ((file (locate-library "f90-ts-abbrev"))
        (warnings nil))
    (should file)
    (unwind-protect
        (let ((f90-ts-extra-abbrevs '(("TOO" . "x"))))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type msg &rest _) (push msg warnings))))
            (load file nil t))
          (should warnings)
          (should (equal (abbrev-expansion "`al" f90-ts-mode-abbrev-table)
                         "allocate"))
          (should (= (f90-ts-mode-test--abbrev-count)
                     (length f90-ts--abbrev-definitions))))
      ;; back to the real configuration
      (f90-ts--abbrev-refresh))))


(provide 'abbrev-table)

;;; abbrev-table.el ends here
