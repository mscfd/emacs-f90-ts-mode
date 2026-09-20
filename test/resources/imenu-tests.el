;;; imenu-tests.el --- Test collection for the imenu index of f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Resource file with subtests for `f90-ts-simple-imenu', not meant to be
;; loaded directly.  It is registered in \"f90-ts-mode-test.el\" through
;; `f90-ts-mode-test--collection-register' and runs as a single ert test, with
;; one subtest per `f90-ts-mode-test-defsub' form below.
;;
;; The subtests test the implementation in \"f90-ts-imenu.el\" and the queries
;; in `f90-ts--nav-queries', which are shared with the toc buffer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'imenu)
(require 'f90-ts-mode)
(require 'f90-ts-imenu)
(require 'f90-ts-mode-test)


;;;-----------------------------------------------------------------------------
;; auxiliary stuff

(defmacro f90-ts-mode-test--imenu-with-source (text &rest body)
  "Run BODY in a temporary `f90-ts-mode' buffer holding TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (f90-ts-mode)
     (goto-char (point-min))
     ,@body))


(defun f90-ts-mode-test--imenu-groups ()
  "Return the Imenu index of the current buffer as ((LABEL NAME ...) ...).
The groups are sorted by label, the names within a group keep their order.
Positions are dropped, see `f90-ts-mode-test--imenu-entries' for them."
  (sort (cl-loop
         for (label . items) in (funcall imenu-create-index-function)
         collect (cons label (mapcar #'car items)))
        (lambda (a b) (string< (car a) (car b)))))


(defun f90-ts-mode-test--imenu-entries ()
  "Return all entries of the Imenu index as a flat list (LABEL NAME . MARKER)."
  (cl-loop
   for (label . items) in (funcall imenu-create-index-function)
   append (cl-loop for (name . marker) in items
                   collect (cons label (cons name marker)))))


(defun f90-ts-mode-test--imenu-names (label)
  "Return the names of the group LABEL of the current buffer, or nil."
  (cdr (assoc label (f90-ts-mode-test--imenu-groups))))


(defun f90-ts-mode-test--imenu-narrow-to (begin-regexp end-regexp)
  "Narrow the buffer from BEGIN-REGEXP to the end of END-REGEXP."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (re-search-forward begin-regexp)
    (let ((beg (match-beginning 0)))
      (re-search-forward end-regexp)
      (narrow-to-region beg (match-end 0)))))


(defun f90-ts-mode-test--imenu-nodes (query)
  "Return the nodes captured by QUERY in the current buffer, in buffer order."
  (mapcar #'cdr
          (treesit-query-capture (treesit-buffer-root-node)
                                 query)))


(defconst f90-ts-mode-test--imenu-text
  "module m
  integer :: i
  type t
    integer :: comp
  contains
    procedure :: p1
  end type
  type, extends(t) :: u
  end type
  type, public :: w
  end type w
  interface gen
    module procedure sub1
  end interface gen
  interface
    module subroutine fs()
    end subroutine
  end interface
  abstract interface
    subroutine ab()
    end subroutine
  end interface
  interface operator(+)
    module procedure add
  end interface
  interface assignment(=)
    module procedure asg
  end interface
contains
  module procedure mp
  end procedure
  function f(x) result(r)
  end function
  pure integer function g()
  end function
  subroutine s
  contains
    subroutine inner
    end subroutine
  end subroutine
end module
submodule (m) sm
contains
end submodule
program p
end program
"
  "Source text with all kinds of entries, as well as some which are not listed.
Not listed are the type-bound procedure p1, the procedure statements in the
interfaces, the unnamed and the abstract interface.")


(defconst f90-ts-mode-test--imenu-text-groups
  '(("derived type" "t" "u" "w")
    ("function" "f" "g")
    ("interface" "gen" "operator(+)" "assignment(=)")
    ("module" "m")
    ("module procedure" "mp")
    ("program" "p")
    ("submodule" "sm")
    ("subroutine" "fs" "ab" "s" "inner")
    ("variable" "i" "comp"))
  "Groups of the index of `f90-ts-mode-test--imenu-text', sorted by label.")


;;;-----------------------------------------------------------------------------
;; index structure

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--mode-integration ()
  "The mode installs `f90-ts-simple-imenu', and Imenu accepts the index.
Tests the setup in `f90-ts-mode' and that the format of the index is the one
expected by `imenu--make-index-alist': an alist of labels with name-marker
lists."
  (f90-ts-mode-test--imenu-with-source f90-ts-mode-test--imenu-text
    (should (eq imenu-create-index-function
                #'f90-ts-simple-imenu))
    (let ((alist (imenu--make-index-alist t)))
      (should (equal (mapcar #'car (cdr (assoc "module" alist)))
                     '("m")))
      (should (equal (mapcar #'car (cdr (assoc "subroutine" alist)))
                     '("fs" "ab" "s" "inner"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--empty-buffers ()
  "Buffers without any entry give an empty index.
Tests that `f90-ts-simple-imenu' handles an empty capture list."
  (f90-ts-mode-test--imenu-with-source ""
    (should-not (funcall imenu-create-index-function)))
  (f90-ts-mode-test--imenu-with-source "! only a comment\n"
    (should-not (funcall imenu-create-index-function))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--all-kinds ()
  "The index of the big text has all groups, names in buffer order.
Tests the whole pipeline of `f90-ts-simple-imenu': combined query, label
mapping, and grouping with the order of names inside a group."
  (f90-ts-mode-test--imenu-with-source f90-ts-mode-test--imenu-text
    (should (equal (f90-ts-mode-test--imenu-groups)
                   f90-ts-mode-test--imenu-text-groups))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--text-covers-all-kinds ()
  "The big text has an entry for each kind of `f90-ts--nav-queries'.
Tests that the labels of the index are exactly the labels of the queries.
This makes sure that a kind added later without a test here is noticed."
  (f90-ts-mode-test--imenu-with-source f90-ts-mode-test--imenu-text
    (let ((groups (f90-ts-mode-test--imenu-groups))
          (labels (mapcar (lambda (entry) (plist-get (cdr entry) :label))
                          f90-ts--nav-queries)))
      (cl-loop
       for label in labels
       do (ert-info (label :prefix "label: ")
            (should (assoc label groups))))
      (cl-loop
       for group in groups
       do (should (member (car group) labels))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--source-case-is-kept ()
  "Names are listed as written, whatever the case of the source.
Tests that the names are taken from the buffer text of the captured nodes."
  (f90-ts-mode-test--imenu-with-source
      "MODULE Mod_A
  INTEGER :: Ab
CONTAINS
  SUBROUTINE Sub_X
  END SUBROUTINE
END MODULE
"
    (should (equal (f90-ts-mode-test--imenu-groups)
                   '(("module" "Mod_A")
                     ("subroutine" "Sub_X")
                     ("variable" "Ab"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--same-name-twice ()
  "Entries with the same name are all kept.
Tests that the grouping in `f90-ts--imenu-group-items' does not merge or
drop items with equal names."
  (f90-ts-mode-test--imenu-with-source
      "module m\nend module\nmodule m\nend module\n"
    (should (equal (f90-ts-mode-test--imenu-names "module")
                   '("m" "m")))))


;;;-----------------------------------------------------------------------------
;; queries: derived types, interfaces, procedures

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--derived-types ()
  "Types are listed with and without attributes and end names.
Tests the query for `derived_type_definition'."
  (f90-ts-mode-test--imenu-with-source
      "module m
  type t
  end type
  type, extends(t) :: u
  end type
  type, public, abstract :: v
  end type v
  type :: w
  end type
end module
"
    (should (equal (f90-ts-mode-test--imenu-names "derived type")
                   '("t" "u" "v" "w")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--type-bound-procedures-not-listed ()
  "Type-bound procedures, generics and finalizers are no entries.
Tests that the query for `derived_type_definition' captures only the
type name, and nothing from the type body but the components."
  (f90-ts-mode-test--imenu-with-source
      "module m
  type t
    integer :: comp
  contains
    procedure :: p1
    procedure, pass(self) :: p2 => impl2
    generic :: gen => p1, p2
    final :: fin
  end type
end module
"
    (should (equal (f90-ts-mode-test--imenu-groups)
                   '(("derived type" "t")
                     ("module" "m")
                     ("variable" "comp"))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--interfaces ()
  "Named, operator and assignment interfaces are listed, others are not.
Tests the query for `interface'.  Unnamed and abstract interfaces have no
name, so they do not produce an entry, but the procedures declared inside
are listed.  Procedure statements are not listed."
  (f90-ts-mode-test--imenu-with-source
      "module m
  interface
    module subroutine fs()
    end subroutine
  end interface
  abstract interface
    subroutine ab()
    end subroutine
  end interface
  interface gen
    module procedure p1, p2
  end interface gen
  interface operator(.add.)
    module procedure add
  end interface
  interface assignment(=)
    module procedure asg
  end interface
end module
"
    (should (equal (f90-ts-mode-test--imenu-names "interface")
                   '("gen" "operator(.add.)" "assignment(=)")))
    (should (equal (f90-ts-mode-test--imenu-names "subroutine")
                   '("fs" "ab")))
    (should-not (f90-ts-mode-test--imenu-names "module procedure"))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--procedures ()
  "Functions with prefixes and result clauses, subroutines, module procedures.
Tests the queries for `function', `subroutine', `module_procedure' and
`submodule'."
  (f90-ts-mode-test--imenu-with-source
      "submodule (parent) sm
contains
  module subroutine foo()
  end subroutine
  module function bar() result(r)
    integer :: r
  end function
  module procedure baz
  end procedure
  recursive subroutine rec()
  end subroutine
  pure elemental real function el(x)
    real, intent(in) :: x
  end function
end submodule
"
    (should (equal (f90-ts-mode-test--imenu-groups)
                   '(("function" "bar" "el")
                     ("module procedure" "baz")
                     ("submodule" "sm")
                     ("subroutine" "foo" "rec")
                     ("variable" "r" "x"))))))


;;;-----------------------------------------------------------------------------
;; queries: variables

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--variable-declarators ()
  "All forms of declarators are listed, in all kinds of statements.
Tests the query for `variable_declaration' with its alternatives: plain,
initialised, sized and pointer declarators."
  (f90-ts-mode-test--imenu-with-source
      "program p
  integer :: i = 1, j(10), k
  real, dimension(3) :: x
  real, pointer :: q => null()
  character(len=*), parameter :: str = 'a'
  type(t) :: y
  class(t), allocatable :: z
end program
"
    (should (equal (f90-ts-mode-test--imenu-names "variable")
                   '("i" "j" "k" "x" "q" "str" "y" "z"))))
  ;; several statements, also on one line, and comments
  (f90-ts-mode-test--imenu-with-source
      "subroutine s
  integer :: x  ! trailing comment
  integer :: a, b
  integer :: c; integer :: d
  ! integer :: commented_out
end subroutine
"
    (should (equal (f90-ts-mode-test--imenu-names "variable")
                   '("x" "a" "b" "c" "d")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--variables-in-all-scopes ()
  "Module variables, components, interface bodies, locals and block locals.
Tests that the query for `variable_declaration' is not restricted to a
parent node."
  (f90-ts-mode-test--imenu-with-source
      "module m
  integer :: mv
  type t
    integer :: comp
  end type
  interface
    subroutine ext(n)
      integer, intent(in) :: n
    end subroutine
  end interface
contains
  subroutine s
    integer :: loc
    block
      integer :: tmp
    end block
  end subroutine
end module
"
    (should (equal (f90-ts-mode-test--imenu-names "variable")
                   '("mv" "comp" "n" "loc" "tmp")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--dummy-arguments-not-listed ()
  "Dummy arguments are only listed through their declarations.
Tests that the queries for `subroutine' and `function' capture only the name,
not the argument list."
  (f90-ts-mode-test--imenu-with-source
      "subroutine s(a, b, c)
  integer :: a
  real :: b
end subroutine
"
    (should (equal (f90-ts-mode-test--imenu-names "variable") '("a" "b")))
    (should (equal (f90-ts-mode-test--imenu-names "subroutine") '("s")))))


;;;-----------------------------------------------------------------------------
;; markers, narrowing, edits, broken source

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--markers-point-at-names ()
  "Every entry is a marker in the current buffer, placed at its name.
Tests that proper markers are generated in `f90-ts--imenu-captures-to-items'."
  (f90-ts-mode-test--imenu-with-source f90-ts-mode-test--imenu-text
    (let ((entries (f90-ts-mode-test--imenu-entries))
          (buffer (current-buffer)))
      (should (= (length entries)
                 (apply #'+ (mapcar (lambda (group) (length (cdr group)))
                                    f90-ts-mode-test--imenu-text-groups))))
      (cl-loop
       for (label name . marker) in entries
       do (ert-info ((format "%s %s" label name) :prefix "entry: ")
            (should (markerp marker))
            (should (eq (marker-buffer marker) buffer))
            (goto-char marker)
            (should (looking-at (regexp-quote name))))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--narrowed-buffer ()
  "A narrowed buffer lists only what is inside the restriction.
Tests that `f90-ts-simple-imenu' queries the root node of the visible region,
and that it leaves restriction and point as they are."
  (f90-ts-mode-test--imenu-with-source
      "module m
  integer :: a
contains
  subroutine s
    integer :: b
  end subroutine
  subroutine t
    integer :: c
  end subroutine
end module
"
    (f90-ts-mode-test--imenu-narrow-to "subroutine s" "end subroutine")
    (goto-char (1+ (point-min)))
    (let ((restriction (cons (point-min) (point-max)))
          (point-before (point)))
      (should (equal (f90-ts-mode-test--imenu-groups)
                     '(("subroutine" "s") ("variable" "b"))))
      (should (equal restriction (cons (point-min) (point-max))))
      (should (= point-before (point))))
    ;; widening brings everything back
    (widen)
    (should (equal (f90-ts-mode-test--imenu-names "subroutine") '("s" "t")))
    (should (equal (f90-ts-mode-test--imenu-names "variable") '("a" "b" "c")))
    (should (equal (f90-ts-mode-test--imenu-names "module") '("m")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--follows-edits ()
  "The index is built anew from the buffer on every call.
Tests that `f90-ts-simple-imenu' does not cache."
  (f90-ts-mode-test--imenu-with-source
      "module m\ncontains\n  subroutine s\n  end subroutine\nend module\n"
    (should (equal (f90-ts-mode-test--imenu-names "subroutine") '("s")))
    (re-search-forward "^end module")
    (beginning-of-line)
    (let ((beg (point)))
      (insert "  subroutine t\n  end subroutine\n")
      (should (equal (f90-ts-mode-test--imenu-names "subroutine")
                     '("s" "t")))
      (delete-region beg (point)))
    (should (equal (f90-ts-mode-test--imenu-names "subroutine") '("s")))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--broken-source ()
  "A source with syntax errors gives an index without signaling.
Tests the robustness of `f90-ts-simple-imenu' for partial trees.  What is
listed for broken structures is up to the grammar, but the complete
structures around them are still found."
  (f90-ts-mode-test--imenu-with-source
      "subroutine good1
end subroutine
subroutine broken(
  integer :: x
subroutine good2
end subroutine
"
    (let ((index (funcall imenu-create-index-function)))
      (should (listp index))
      (should (member "good1"
                      (mapcar #'car (cdr (assoc "subroutine" index))))))))


;;;-----------------------------------------------------------------------------
;; helper functions of f90-ts-imenu.el

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--group-items ()
  "`f90-ts--imenu-group-items' groups by label and keeps the order.
The order of the groups themselves is not checked."
  (should-not (f90-ts--imenu-group-items nil))
  (let ((groups (f90-ts--imenu-group-items
                 '(("a" "x" 1) ("b" "y" 2) ("a" "z" 3) ("a" "w" 4)))))
    (should (= (length groups) 2))
    (should (equal (cdr (assoc "a" groups)) '(("x" . 1) ("z" . 3) ("w" . 4))))
    (should (equal (cdr (assoc "b" groups)) '(("y" . 2))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--captures-to-items ()
  "Captures become (LABEL NAME MARKER) items, unknown captures are dropped.
Tests `f90-ts--imenu-captures-to-items', the mapping of a capture symbol
to a label through `f90-ts--nav-capture-key-alist'."
  (f90-ts-mode-test--imenu-with-source
      "module m\n  integer :: a\nend module\n"
    (let* ((root (treesit-buffer-root-node))
           (captures (treesit-query-capture root f90-ts--imenu-query-compiled))
           (items (f90-ts--imenu-captures-to-items captures)))
      (should (equal (mapcar #'car items) '("module" "variable")))
      (should (equal (mapcar #'cadr items) '("m" "a")))
      ;; a capture without entry in the nav queries is ignored
      (should (= (length (f90-ts--imenu-captures-to-items
                          (cons (cons 'no_such_capture (cdr (car captures)))
                                captures)))
                 (length items)))
      (should-not (f90-ts--imenu-captures-to-items nil)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--name-pos-fn ()
  "`f90-ts--imenu-name-pos-fn' returns the (NAME . POS) pairs of a node.
It uses the query of the node type, so nodes without a query, and nodes
where the query captures nothing, give nil."
  (f90-ts-mode-test--imenu-with-source
      "module m
  integer :: a, b(3)
  interface gen
  end interface
  interface
  end interface
contains
  subroutine s
  end subroutine
end module
"
    (let ((module (car (f90-ts-mode-test--imenu-nodes '((module) @n))))
          (decl   (car (f90-ts-mode-test--imenu-nodes
                        '((variable_declaration) @n))))
          (ifaces (f90-ts-mode-test--imenu-nodes '((interface) @n)))
          (sub    (car (f90-ts-mode-test--imenu-nodes '((subroutine) @n)))))
      (should (equal (f90-ts--imenu-name-pos-fn module)
                     (list (cons "m" (save-excursion
                                       (re-search-forward "module \\(m\\)" nil t)
                                       (match-beginning 1))))))
      ;; one pair per declarator, in order
      (should (equal (mapcar #'car (f90-ts--imenu-name-pos-fn decl))
                     '("a" "b")))
      (should (apply #'< (mapcar #'cdr (f90-ts--imenu-name-pos-fn decl))))
      (should (equal (mapcar #'car (f90-ts--imenu-name-pos-fn sub)) '("s")))
      ;; named interface has a name, unnamed one has none
      (should (equal (mapcar #'car (f90-ts--imenu-name-pos-fn (car ifaces)))
                     '("gen")))
      (should-not (f90-ts--imenu-name-pos-fn (cadr ifaces)))
      ;; a node type without query
      (should-not (f90-ts--imenu-name-pos-fn (treesit-buffer-root-node))))))


;;;-----------------------------------------------------------------------------
;; nav queries shared with the toc buffer

(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--nav-queries-wellformed ()
  "Every nav query has label, capture and query, and uses its own capture.
Tests the entries of `f90-ts--nav-queries', and that each query compiles on
its own, which shows which kind is broken if the combined query is not."
  (should f90-ts--nav-queries)
  (cl-loop
   for (key . plist) in f90-ts--nav-queries
   do (ert-info (key :prefix "kind: ")
        (should (stringp key))
        (should (stringp (plist-get plist :label)))
        (should (plist-get plist :capture))
        (should (symbolp (plist-get plist :capture)))
        (should (stringp (plist-get plist :query)))
        (should (string-match-p
                 (concat "@"
                         (regexp-quote (symbol-name (plist-get plist :capture)))
                         "\\_>")
                 (plist-get plist :query)))
        (should (treesit-query-compile 'fortran (plist-get plist :query))))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--nav-queries-unique ()
  "Keys, labels and captures are unique.
The labels must be unique as the index is grouped by label, and the captures
must be as they identify the kind of an entry."
  (cl-flet ((unique-p (list)
              (= (length list) (length (delete-dups (copy-sequence list))))))
    (should (unique-p (mapcar #'car f90-ts--nav-queries)))
    (should (unique-p (mapcar (lambda (e) (plist-get (cdr e) :label))
                              f90-ts--nav-queries)))
    (should (unique-p (mapcar (lambda (e) (plist-get (cdr e) :capture))
                              f90-ts--nav-queries)))))


(f90-ts-mode-test-defsub f90-ts-mode-test--imenu--combined-query-captures ()
  "The combined query only produces captures which belong to a kind.
Tests that no query has an additional capture, which would be dropped
silently in `f90-ts--imenu-captures-to-items'."
  (f90-ts-mode-test--imenu-with-source f90-ts-mode-test--imenu-text
    (let ((captures (treesit-query-capture (treesit-buffer-root-node)
                                           f90-ts--imenu-query-compiled)))
      (should captures)
      (cl-loop
       for (symbol . _node) in captures
       do (should (assq symbol f90-ts--nav-capture-key-alist))))))


;;;-----------------------------------------------------------------------------

(provide 'imenu-tests)

;;; imenu-tests.el ends here
