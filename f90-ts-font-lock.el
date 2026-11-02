;;; f90-ts-font-lock.el --- Font-lock rules for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide font-lock support for `f90-ts-mode'.  The core is a set of
;; rules for the tree-sitter font locking engine.  There are also some
;; custom fontification functions for comment and ERROR nodes.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary functions
(require 'f90-ts-auxiliary)


;;;-----------------------------------------------------------------------------
;;; Font-locking: auxiliary

(defun f90-ts--fontify-comment (node override start end &rest _)
  "Fontify NODE assumed to be a comment.
Check whether NODE satisfies a special comment rule, and if it does,
use the face provided by the first matching rule.
If no rule matches, use `font-lock-comment-face'.

Keyword matches from `f90-ts-comment-keyword-regexp' are additionally
highlighted with `font-lock-warning-face' on top of the base face.

Argument OVERRIDE is passed to `treesit-fontify-with-override' for the comment
rule but not for matched keywords, which are enforced by override=t.

Restrict fontification to the region between START and END."
  (cl-assert (f90-ts--node-type-p node "comment")
             nil "fontify-comment: comment node expected")
  (let* ((rule (f90-ts--comment-matching-rule node))
         (face (or (and rule (plist-get rule :face))
                   'font-lock-comment-face))
         (comment-start (treesit-node-start node))
         (comment-end (treesit-node-end node)))

    ;; apply base face to the whole comment node
    (treesit-fontify-with-override comment-start comment-end
                                   face override
                                   start end)

    ;; overlay keyword matches on top
    (when f90-ts-comment-keyword-regexp
      (save-excursion
        (goto-char comment-start)
        (cl-loop while (re-search-forward f90-ts-comment-keyword-regexp comment-end t)
                 do (treesit-fontify-with-override
                       (match-beginning 0) (match-end 0)
                       'font-lock-warning-face t
                       start end))))))


(defun f90-ts--fontify-error (node override start end &rest _)
  "Add error fontification for span of NODE if enabled and applicable.
If `f90-ts-font-lock-error-show' is non-nil, and NODE of type \"ERROR\" has
no other error node has descendant, then the function appends
`f90-ts-font-lock-error-face' to existing font-lock face.
Often an error results in several \"ERROR\" nodes in the chain towards the root.
Only the smallest error nodes should be marked.  Moreover, the span is trimmed
to exclude leading and trailing blanks, which are sometimes part of ERROR nodes.
Restrict fontification to the region between START and END, using OVERRIDE."
  (when (and f90-ts-font-lock-error-show
             (let ((sparse-tree (treesit-induce-sparse-tree node "^ERROR$")))
               ;; if the sparse-tree has only one node (node itself),
               ;; it has the shape "(nil (node))"
               (and (= (length (cdr sparse-tree)) 1)
                    (null (cdr (cadr sparse-tree))))))

    (cl-destructuring-bind (node-start . node-end) (f90-ts--node-span-trimmed node)
      (let ((end-err (if (eq f90-ts-font-lock-error-show 'all)
                         node-end
                       (cl-assert (and (integerp f90-ts-font-lock-error-show)
                                       (> f90-ts-font-lock-error-show 0))
                                  nil
                                  "invalid value f90-ts-font-lock-error-show: %s"
                                  f90-ts-font-lock-error-show)
                       ;; go f90-ts-font-lock-error-show minus one line forward and
                       ;; then trim to last character of that line
                       (min node-end
                            (save-excursion
                              (goto-char node-start)
                              (end-of-line f90-ts-font-lock-error-show)
                              (skip-chars-backward " \t")
                              (point))))))
        (treesit-fontify-with-override node-start end-err
                                       'f90-ts-font-lock-error-face
                                       override start end)))))


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
   `((call_expression
      (identifier) @font-lock-builtin-face
      (:pred f90-ts--builtin-function-p @font-lock-builtin-face))
     (subroutine_call
      "call"
      subroutine: (identifier) @font-lock-builtin-face
      (:pred f90-ts--builtin-function-p @font-lock-builtin-face)))))


(defun f90-ts--font-lock-rules-keyword ()
  "Font-lock rules for Fortran keywords."
  (let ((keywords
         (vconcat
          ["program" "module" "submodule"
           "function" "subroutine" "procedure" "interface"
           "bind" "result" "end" "call"
           "public" "private" "protected" "contains"
           "use" "include" "only" "import"
           "implicit" "none" "external" "intrinsic" "non_intrinsic"
           "pure" "impure" "elemental" "recursive"
           "type" "class" "is" "typeof" "classof"
           "if" "then" "else" "elseif" "endif"
           "do" "while"
           "cycle" "exit" "error" "stop" "return" "entry"
           "associate" "block" "critical"
           "enum" "enumeration" "enumerator"
           "where" "elsewhere" "forall" "concurrent"
           "select" "case" "rank" "default"
           "shared" "local" "local_init" "reduce"
           "extends" "abstract"
           "pass" "nopass" "deferred" "non_overridable"
           "operator" "assignment" "generic" "final"
           "allocatable"
           "intent" "in" "out" "inout" "optional"
           "parameter" "save" "target" "pointer" "value"
           "dimension" "codimension" "contiguous" "volatile" "asynchronous"
           "open" "close" "print" "read" "write" "format"
           "inquire" "wait" "backspace" "endfile" "rewind" "flush"
           "nullify" "allocate" "deallocate"
           "sync" "all" "images" "memory"
           "form" "team" "change"
           "lock" "unlock"
           "fail" "image"]
          ;; keywords which only exist in some grammar variants
          (when (f90-ts--grammar-keyword-p "non_recursive")
            ["non_recursive"]))))

    (treesit-font-lock-rules
     :language 'fortran
     :feature 'keyword
     ;; match type qualifiers/attributes
     ;; match this before keyword matching, as this highlights the whole
     ;; attribute (like "dimension(1:N)" instead of just the keyword);
     ;; TODO: do we want to match the whole attribute uniformly, or should
     ;; we use separate faces like operator face for "+" in "dimension(1:M+N)"?
     '((type_qualifier) @font-lock-keyword-face)

     :language 'fortran
     :feature 'keyword
     '(;; match keywords in select case statements
       ;; (which are not covered by simple keywords below)
       (case_statement
        "case"    @font-lock-keyword-face
        (default) @font-lock-keyword-face)
       ;; match keywords in select type statements
       ;; (which are not covered by simple keywords below)
       (type_statement
        (default) @font-lock-keyword-face)
       ;; match keywords in select rank statements
       ;; (which are not covered by simple keywords below)
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
     ;; match keywords exposed by the grammar, but last to allow more
     ;; specific rules to overwrite simple keyword highlightling (see
     ;; above at type_qualifier rule);
     ;; note that this matches anonymous nodes representing the keyword,
     ;; not the keyword text itself, and these nodes are always stored lower-case
     ;; (hence no need to match case insensitive as is necessary with builtins)
     `((,keywords @font-lock-keyword-face)))))


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
   (if (f90-ts--grammar-string-literal-decomposed-p)
       '((preproc_include
          "#include"                             @font-lock-preprocessor-face
          path: (string_literal (string_content) @font-lock-string-face)))
     '((preproc_include
      "#include"                                 @font-lock-preprocessor-face
      path: (string_literal)                     @font-lock-string-face)))

   :language 'fortran
   :feature 'preproc
   '((preproc_def
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

     (import_statement
      "import"
      (identifier)               @font-lock-type-face)

     (end_type_statement
      (name)                     @font-lock-type-face))

   :language 'fortran
   :feature 'type
   (if (f90-ts--grammar-derived-type-name-p)
       '((allocate_statement
          type: (derived_type
                 name: (type_name) @font-lock-type-face))
         (type_statement
          type: (derived_type
                 name: (type_name) @font-lock-type-face)))
     '((allocate_statement
        type: (identifier) @font-lock-type-face)
       (type_statement
        type: (identifier) @font-lock-type-face)))))


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
                   ((identifier)   @font-lock-function-name-face)
                   ((derived_type_member_expression
                    [(identifier)
                     (derived_type_member_expression)]
                    "%"
                    (type_member)  @font-lock-function-name-face))
                   ])

     ((procedure_interface)               @font-lock-function-name-face)
     ;; within derived type declarations (before contains), match only
     ;; procedure declaration
     (variable_declaration
      type: (procedure)
      ;; is this always a pointer to a procedure?
      declarator: (identifier)            @font-lock-function-name-face)
     (procedure_statement
      (procedure_kind)
      "("
      (procedure_interface)               @font-lock-function-name-face)
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
   '(((identifier) @f90-ts-font-lock-special-var-face
      (:pred f90-ts--node-special-var-p @f90-ts-font-lock-special-var-face)))))


(defun f90-ts--font-lock-rules-string ()
  "Font-lock rules for strings."
  (if (f90-ts--grammar-string-literal-decomposed-p)
      (treesit-font-lock-rules
       :language 'fortran
       :feature 'string
       '(((string_content) @font-lock-string-face))

       :language 'fortran
       :feature 'escape-sequence
       '(("\\" @f90-ts-font-lock-escape-face)))

    (treesit-font-lock-rules
     :language 'fortran
     :feature 'string
     '(((string_literal) @font-lock-string-face)))))


(defun f90-ts--font-lock-rules-value ()
  "Font-lock rules for numbers, booleans, etc. (except strings)."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'number
   '(((number_literal) @font-lock-number-face)
     ((complex_literal) @font-lock-number-face))

   :language 'fortran
   :feature 'constant
   '(((boolean_literal) @font-lock-constant-face)
     ((null_literal) @font-lock-constant-face)
     ((statement_label) @font-lock-constant-face))))


(defun f90-ts--font-lock-rules-delimiter ()
  "Font-lock rules for brackets and delimiters."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'bracket
   '(["(" ")" "[" "]" "(/" "/)"] @f90-ts-font-lock-bracket-face)

   :language 'fortran
   :feature 'delimiter
   `(["," ":" ";" "::" "=>" "&"
      ,@(when (f90-ts--grammar-string-literal-decomposed-p) '("\""))]
     @f90-ts-font-lock-delimiter-face)))


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


(defun f90-ts--font-lock-rules-error ()
  "Font-lock rule for error nodes.
Append a customizable property like italic or underline to hightlight an
error region, which tree-sitter was not able to parse.
This rule should be processed last, so that the error property can be
prepended to overwrite any properties of existing font lock face."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'error
   :override 'prepend
   '(;; if enabled append some error face properties to existing faces
     ((ERROR) @f90-ts--fontify-error))))


(defun f90-ts-font-lock-rules ()
  "Return list of font-lock rules.
Internally, some rules are selected depending on grammar properties, like
changes in how string_literal is parsed and whether it is decomposed."
  (append
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
   (f90-ts--font-lock-rules-string)
   (f90-ts--font-lock-rules-value)
   (f90-ts--font-lock-rules-delimiter)
   (f90-ts--font-lock-rules-error)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-font-lock)

;;; f90-ts-font-lock.el ends here

