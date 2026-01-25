;;; -*- lexical-binding: t; -*-
;;; f90-ts-mode.el --- Tree-sitter major mode for Fortran

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein
;; Version: 0.1
;; Keywords: languages, treesitter, fortran
;; Package-Name: f90-ts-mode

;; Provides syntax highlighting and structural navigation for Fortran 90+
;; files using the Tree-sitter parsing engine.

;;; Commentary:
;;
;; This mode is a tree-sitter based alternative to the classic
;; `f90-mode'. It incorporates some logic and code snippets adapted
;; from the original f90.el by Glenn Morris and others.

;; INSTALLATION:
;; for a simple setup:
;; 1. Save this file as ~/.emacs.d/lisp/f90-ts-mode.el
;; 2. Add (add-to-list 'load-path "/home/you/.emacs.d/lisp")
;;    and (require 'f90-ts-mode) or use use-package
;; 3. Ensure your treesitter grammar is available, for example:
;;    (add-to-list 'treesit-language-source-alist
;;                 '(fortran "/home/you/treesitter/f90"))
;;    Then run: M-x treesit-install-language-grammar RET fortran RET


(require 'treesit)

;;------------------------------------------------------------------------------

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
  "Extra indentation applied to most blocks like function and
subroutine bodies, control statements (do, if, associate ...)."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)

(defcustom f90-ts-indent-continued 5
  "Extra indentation applied to continued lines."
  :type  'integer
  :safe  'integerp
  :group 'f90-ts-indent)


(defconst f90-ts-indent-lists-options
  '(radio (const :tag "Keep if aligned or align to first element" keep-or-first)
          (const :tag "Always align with first element" always-first)
          (const :tag "Indent as for continued lines" continued-line)
          (const :tag "Rotate elements" rotate))
  "Options for indentation of list like structures on continued lines.")


(defcustom f90-ts-indent-lists-region 'keep-or-first
  "Options for how to indent list like structures on continued lines,
used as default setting, in particular if indent-region is invoked."
  :type f90-ts-indent-lists-options
  :group 'f90-ts)


(defcustom f90-ts-indent-lists-line 'rotate
  "Options for how to indent list like structures on continued lines,
used for indentation of a single line. Used in function
f90-ts-indent-for-tab-command, which can be bound to a key like <tab>."
  :type f90-ts-indent-lists-options
  :group 'f90-ts)


;;------------------------------------------------------------------------------

(defcustom f90-ts-smart-end 'blink
  "Copy from original f90 prog mode. If set to blink, then jump to the
opening clause of a smart end completion, no-blink does completion without
jumping and nil turns of smart end completion."
  :type  '(choice (const blink) (const no-blink) (const nil))
  :safe  (lambda (value) (memq value '(blink no-blink nil)))
  :group 'f90-ts)


;;------------------------------------------------------------------------------

(defface f90-ts-font-lock-intrinsic-face
  '((t :foreground "DarkOrchid4"
       :weight semi-bold))
  "Face for custom font-lock highlighting."
  :group 'f90-ts)


(defface f90-ts-font-lock-delimiter-face
  '((t :foreground "Sienna4"
       :weight medium))
  "Face for custom font-lock highlighting."
  :group 'f90-ts)

(defface f90-ts-font-lock-bracket-face
  '((t :foreground "BlueViolet"
       :weight bold))
  "Face for custom font-lock highlighting."
  :group 'f90-ts)


(defface f90-ts-font-lock-operator-face
  '((t :foreground "Brown3"
       :weight bold))
  "Face for custom font-lock highlighting."
  :group 'f90-ts)


(defface f90-ts-font-lock-openmp-face
  '((t :foreground "turquoise4"
       :weight medium))
  "Face for openmp statements."
  :group 'f90-ts)


(defface f90-ts-font-lock-special-var-face
  '((t :foreground "blue4"
       :weight semi-bold))
  "Face for special variables like self or this."
  :group 'f90-ts)


(defface f90-ts-font-lock-special-comment-face
  '((t :foreground "Sienna4"
       :weight bold))
  "Face for openmp statements."
  :group 'f90-ts)


(defface f90-ts-font-lock-test1-face
  '((t :foreground "Red"
       :background "Yellow"))
  "Face for testing."
  :group 'f90-ts)


(defface f90-ts-font-lock-test2-face
  '((t :foreground "Blue"
       :background "Yellow"))
  "Face for testing."
  :group 'f90-ts)


;;------------------------------------------------------------------------------
;; keymap and syntax table

(defvar f90-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'f90-ts-indent-line-or-region)
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
                                        ; "my_flag.and.other_flag", appearing as one big symbol,
                                        ; likewise it could be difficult with floating point numbers?

    table)
  "Syntax table for `f90-ts-mode'.")


;;------------------------------------------------------------------------------
;; Font-locking

(defun f90-ts--font-lock-rules-openmp ()
  "Font-lock rules for openmp statements, which are currently stored as
comments in the tree. Must be parsed before plain comments."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'comment
   '(;; capture comments starting with !$, which are openmp statements
     ((comment) @f90-ts-font-lock-openmp-face
      (:pred f90-ts-openmp-node-p @f90-ts-font-lock-openmp-face))
     )))


(defun f90-ts--font-lock-rules-comment ()
  "Font-lock rules for comments."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'comment
   '(
     ((comment) @f90-ts-font-lock-special-comment-face
      (:pred f90-ts-special-comment-node-p @f90-ts-font-lock-special-comment-face))
     ((comment) @font-lock-comment-face)
     )))


(defun f90-ts--font-lock-rules-keyword ()
  "Font-lock rules for Fortran keywords."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'keyword
   ;; match type qualifiers/attributes
   '((type_qualifier) @font-lock-keyword-face)

   :language 'fortran
   :feature 'keyword
   ;; match keywords in select case statements
   ;; (which are not covered by simple keywords below
   '((case_statement
      "case"    @font-lock-keyword-face
      (default) @font-lock-keyword-face))

   :language 'fortran
   :feature 'keyword
   ;; match keywords in select type statements
   ;; (which are not covered by simple keywords below
   '((type_statement
      (default) @font-lock-keyword-face))

   :language 'fortran
   :feature 'keyword
   ;; match keywords exposed by the grammar
   '((["program" "module" "submodule"
      "function" "subroutine" "procedure" "result"
      "end" "call"
      "if" "then" "else" "elseif" "endif"
      "do" "while"
      "cycle" "exit"
      "type" "class" "is" "typeof" "classof"
      "extends" "abstract"
      "pass" "nopass" "deferred"
      "operator" "assignment" "generic" "final"
      "select" "case" "default"
      "use" "only" "implicit" "none"
      "interface" "contains" "return"
      "public" "private" "protected"
      "allocate" "deallocate" "allocatable"
      "intent" "in" "out" "inout"
      "parameter" "save" "target" "pointer" "optional"
      "pure" "impure" "elemental" "recursive"
      "dimension" "contiguous" "volatile"
      "associate" "block" "critical"
      "where" "forall" "concurrent"] @font-lock-keyword-face)
     (["print" "write" "read"] @f90-ts-font-lock-intrinsic-face))
   ))


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
      "#else"              @font-lock-preprocessor-face)
     )))


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
      (name)                  @font-lock-function-name-face)
     )))


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
             (identifier)        @font-lock-type-face)))
     (end_type_statement
      (name)                     @font-lock-type-face)
     )

   :language 'fortran
   :feature 'type
   ;; special declarations (e.g. within allocate statements)
   ;; TODO: should the grammar use type_name instead of identifier as done elsewhere?
   '(((allocate_statement
      type: (identifier)        @font-lock-type-face))
     (type_statement
      type: (identifier)        @font-lock-type-face)
   )))


(defun f90-ts--font-lock-rules-function ()
  "Font-lock rules for functions and subroutines."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((subroutine_statement
      name: (name)                 @font-lock-function-name-face)
     (function_statement
      name: (name)                 @font-lock-function-name-face)
     (function_result
      (identifier)                 @default)
     (subroutine_call
      subroutine: (identifier)     @font-lock-function-name-face)
     (call_expression (identifier) @font-lock-function-name-face)

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
      declarator: (method_name)          @font-lock-function-name-face)
     )))


(defun f90-ts--font-lock-rules-interface ()
  "Font-lock rules for interface blocks."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'function
   '((interface_statement
      "interface"
      (name)                      @font-lock-function-name-face)
     (procedure_statement
      declarator: (method_name)   @font-lock-function-name-face)
     )))


(defun f90-ts--font-lock-rules-end ()
  "Font-lock rules for end statement of functions, subroutines,
associates and others."
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

     (end_interface_statement
      "end"
      "interface"
      (name)       @font-lock-function-name-face)
     )))


(defun f90-ts--font-lock-rules-variable ()
  "Font-lock rules for variables."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'variable
   '(
     ((identifier) @f90-ts-font-lock-special-var-face
      (:pred f90-ts-special-var-p @f90-ts-font-lock-special-var-face))
   )))


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
     ((null_literal) @font-lock-constant-face))
   ))


(defun f90-ts--font-lock-rules-delimiter ()
  "Font-lock rules for brackets and delimiters."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'bracket
   '(["(" ")" "[" "]" "(/" "/)"] @f90-ts-font-lock-bracket-face)
   
   :language 'fortran
   :feature 'delimiter
   '(["," ":" ";" "::" "=>" "&"] @f90-ts-font-lock-delimiter-face)
   ))


(defun f90-ts--font-lock-rules-operator ()
  "Font-lock rules for operators."
  (treesit-font-lock-rules
   :language 'fortran
   :feature 'operator
   '((logical_expression operator: _  @f90-ts-font-lock-operator-face)
     (logical_expression "\\.not\\."  @f90-ts-font-lock-operator-face)
     (math_expression    operator: _  @f90-ts-font-lock-operator-face)
     ("="                             @f90-ts-font-lock-operator-face)
     ("%"                             @f90-ts-font-lock-operator-face)
   )))


(defvar f90-ts-font-lock-rules
  (list
   (f90-ts--font-lock-rules-openmp)
   (f90-ts--font-lock-rules-comment)
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


;;------------------------------------------------------------------------------
;; Indentation

;;++++++++++++++
;; matchers

(defun f90-ts--continued-line-is ()
  "Check whether we are after some continued line."
  (lambda (node parent bol &rest _)
    (when-let ((prev-stmt (f90-ts--previous-stmt-first node parent)))
      (f90-ts--after-stmt-line1-p prev-stmt (point)))))


(defun f90-ts--openmp-comment-is ()
  "Matcher that checks whether node is an openmp comment."
  (lambda (node parent bol &rest _)
    (and (f90-ts--node-type-p node "comment")
         (f90-ts-openmp-node-p node))))


(defun f90-ts--special-comment-is ()
  "Matcher that checks whether node is a special comment."
  (lambda (node parent bol &rest _)
    (and (f90-ts--node-type-p node "comment")
         (f90-ts-special-comment-node-p node))))


(defun f90-ts--comment-is ()
  "Matcher that checks whether node and previous node are comments."
  (lambda (node parent bol &rest _)
    (when (f90-ts--node-type-p node "comment")
      (let ((prev-sib (treesit-node-prev-sibling node)))
        (and (f90-ts--node-type-p prev-sib "comment")
             (f90-ts-special-comment-node-p node))))))


(defun n-p-ps (type-n type-p type-ps)
  "Matcher that checks types of node, parent and previous statement."
  (lambda (node parent bol &rest _)
    (let ((prev-stmt (f90-ts--previous-stmt-keyword node parent)))
      (let ((result (and (f90-ts--node-type-p node type-n)
                         (f90-ts--node-type-p parent type-p)
                         (f90-ts--node-type-p prev-stmt type-ps))))
        (when result
          (f90-ts-log :indent "match: type-n type-p type-ps = %s, %s, %s" type-n type-p type-ps))
        result))))


(defun p-ps-pss (type-p type-ps type-pss)
  "Matcher that checks types of parent, previous statement and next
sibling of previous statement.
This matcher with the previous statement nodes is mainly used for
incomplete statements with error nodes and empty lines, where node
is nil and parent an ERROR."
  (lambda (node parent bol &rest _)
    (let* ((prev-stmt (f90-ts--previous-stmt-keyword node parent))
           (ps-sib (and prev-stmt (treesit-node-next-sibling prev-stmt))))
      (let ((result (and (f90-ts--node-type-p parent type-p)
                         (f90-ts--node-type-p prev-stmt type-ps)
                         (f90-ts--node-type-p ps-sib type-pss))))
        (when result
          (f90-ts-log :indent "match: type-p type-ps type-pss= %s, %s, %s" type-p type-ps type-pss))
        result))))

;; not used currently
(defun n-p-nps-pps (type-n type-p type-nps type-pps)
  "Matcher that checks types of node, parent and previous node sibling and previous parent sibling."
  (lambda (node parent bol &rest _)
    (let ((npsib (and node (treesit-node-prev-sibling node)))
          (ppsib (and parent (treesit-node-prev-sibling parent))))

      (let ((result (and (f90-ts--node-type-p node type-n)
                         (f90-ts--node-type-p parent type-p)
                         (f90-ts--node-type-p npsib type-nps)
                         (f90-ts--node-type-p ppsib type-pps))))
        (when result
          (f90-ts-log :indent "match: type-n type-p type-nps, type-pps = %s, %s, %s, %s" type-n type-p type-nps type-pps))
        result))))


(defun n-p-ch-psib (type-n type-p type-ch type-psib)
  "Matcher that checks types of node, parent, first child of node
and previous sibling of node (actually last child of parent previous
to position, which also works for node=nil)."
  (lambda (node parent bol &rest _)
    (let* ((child0 (and node (treesit-node-child node 0 t)))
           (prev-sib (and parent
                          (f90-ts--previous-sibling parent))))
      ;;(f90-ts-log :indent "n-p-ch-psib-matcher: node=%s" (and node (treesit-node-type node)))
      ;;(f90-ts-log :indent "n-p-ch-psib-matcher: parent=%s" (and parent (treesit-node-type parent)))
      ;;(f90-ts-log :indent "n-p-ch-psib-matcher: child0=%s" (and child0 (treesit-node-type child0)))
      ;;(f90-ts-log :indent "n-p-ch-psib-matcher: prevsib=%s" (and prev-sib (treesit-node-type prev-sib)))
      (and (f90-ts--node-type-p node type-n)
           (f90-ts--node-type-p parent type-p)
           (f90-ts--node-type-p child0 type-ch)
           (f90-ts--node-type-p prev-sib type-psib)))))


;;++++++++++++++
;; anchors

(defun previous-stmt-anchor (node parent bol)
  "Anchor at previous statements indentation."
  (if-let ((prev-stmt (f90-ts--previous-stmt-first node parent)))
      (f90-ts--indent-pos-at-node prev-stmt)
    bol))


(defun catch-all-anchor (node parent bol)
  "Anchor function to provide a fallback anchor in the catch-all case. This
applies if some rule is missing, but also if we just want to indent
with the previous relevant line."
  ;; node is nil if on an empty line, thus we should use parent to
  ;; find a meaningful previous "sibling" of node
  (let* ((prev-sib (and parent
                        (f90-ts--previous-sibling parent)))
         (prev-stmt (f90-ts--previous-stmt-first node parent))
         (node-sel (or prev-stmt prev-sib parent node)))
    (if node-sel
        (f90-ts--indent-pos-at-node node-sel)
      bol)))


;;++++++++++++++
;; offset functions: general

;; We cannot directly write (defun f90-ts--minus-offset (offset) ...)
;; and use it in indent rules with the desired offset value like
;; f90-ts-indent-block, as this bakes the current value into the rule,
;; and makes it immune to later changes
(defun f90-ts--minus-block-offset (_node _parent _bol &rest _)
  "Returns offset -f90-ts-indent-block uncondionally."
  (- f90-ts-indent-block))


(defun f90-ts--indent-toplevel-offset (node parent _bol)
  "Indent internal_procedure: 0 when inside module, otherwise use f90-ts-indent-contain."
  ;(f90-ts-log :indent "%d  %d  %d" bol (treesit-node-start parent) (treesit-node-start node))
  (let* ((grandparent (treesit-node-parent parent))
         (ggparent    (and grandparent (treesit-node-parent grandparent))))
    (f90-ts-log :indent "toplevel-offset: grandparent type = %s" (and grandparent (treesit-node-type grandparent)))
    (f90-ts-log :indent "toplevel-offset: ggparent type = %s" (and ggparent (treesit-node-type ggparent)))
    ;; before 'contains' statement, grandparent is translation_unit,
    ;; after 'contains' it is module or program
    (if (or (and grandparent (member (treesit-node-type grandparent)
                                     '("module" "program" "translation_unit")))
            (and grandparent ggparent
                 (string= (treesit-node-type grandparent) "ERROR")
                 (string= (treesit-node-type ggparent) "translation_unit")))
        f90-ts-indent-toplevel
      f90-ts-indent-contain)))


;;++++++++++++++
;; offset functions: lists on continued lines
;; handling alignment options

;; indent region variant is used internally, hence is overridden if
;; tab version is required")
(defvar-local f90-ts--align-continued-variant-tab nil
  "Current variant for indentation, if nil use region variant,
otherwise use tab variant.")


(defun f90-ts-indent-line-or-region ()
  "Indent line with tab variant, or region with region variant if
region is active."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (unwind-protect
        (progn
          (setq f90-ts--align-continued-variant-tab t)
          (indent-for-tab-command))
      (setq f90-ts--align-continued-variant-tab nil))))


;;++++++++++++++
;; offset functions: lists on continued lines
;; rotation logic
(defun f90-ts--align-continued-location (node)
  "Determine node dependent position related values. These are current
column, line number and symbol type at point (for deciding where to
align). If NODE is nil, then no relevant node is at point and point
position is used instead. This is important for handling empty line
in particular."
  (if node
      (list (f90-ts--node-column node)
            (f90-ts--node-line node)
            ;; decide which kind of (named or anonymous) nodes to filter
            ;; (argument, parentheses, comma, ampersend, etc)
            (f90-ts--align-node-symbol node))
    (list (f90-ts--column-number-at-pos (point))
          (line-number-at-pos)
          nil)))


(defun f90-ts--align-continued-node-col (node sym)
  "Compute column of NODE. If NODE is opening parenthesis and SYM is not
a parenthesis, then we add 1, so that the column for alignment is one right
to the opening parenthesis."
  (let ((col (f90-ts--node-column node)))
    (if (and (not (eq sym 'parenthesis))
             (string= (treesit-node-text node) "("))
        (1+ col)
      col)))


(defun f90-ts--align-continued-expand-assoc (nodes)
  "Replace association nodes in list NODES by their first two children,
which are field 'name' and '=>'. There is no handling of the selector
part currently."
  (seq-mapcat
   (lambda (node)
     (if (string= "association" (treesit-node-type node))
         (seq-take (treesit-node-children node) 2)
       (list node)))
   nodes))


(defun f90-ts--align-continued-expand-logical-expression (node)
  "Return relevant children of a nested logical expression tree.
An expression like A .and. B .and. C is roughly represented as
logical_expression(logical_expression(A .and. B), .and. C).
The routine returns the five children A, .and., B, and., C.
It does not descend into parenthesized_expressions."
  (if (string= (treesit-node-type node) "logical_expression")
      (mapcan #'f90-ts--align-continued-expand-logical-expression
              (treesit-node-children node))
    (list node)))


(defun f90-ts--align-continued-children (parent)
  "Determine relevant childrens of parent, depending on type of node of parent.
Depending on context, we might need to drop children of PARENT or use grandparent."
  (f90-ts-log :indent "cont-children parent type: %s" (treesit-node-type parent))

  (cond
   ((string= (treesit-node-type parent) "parameters")
    ;; subroutine or function arguments drop the first child,
    ;; which is opening parenthesis
    (when-let ((children (treesit-node-children parent)))
      (seq-drop children 1)))

   ((string= (treesit-node-type parent) "association_list")
    ;; expand it, as the it contains a list of nodes, whose children are required
    (when-let ((children (treesit-node-children parent)))
      (f90-ts--align-continued-expand-assoc children)))

   ((string= (treesit-node-type parent) "association")
    ;; one step up, then this is association_list, this happens on a continued line:
    ;; (name &
    ;;   => selector)
    (when-let* ((grandparent (treesit-node-parent parent))
                (aunts-uncles (treesit-node-children grandparent)))
      (f90-ts--align-continued-expand-assoc aunts-uncles)))

   ((or (string= (treesit-node-type parent) "binding_list")
        (string= (treesit-node-type parent) "final_statement"))
    (when-let ((children (treesit-node-children parent)))
      ;; drop the (binding_name ...) part, and the => binding symbol
      (seq-drop children 2)))

   ((string= (treesit-node-type parent) "variable_declaration")
    (when-let ((children (treesit-node-children parent)))
      ;; drop everything before the first declarator field (first declared variable)
      (seq-drop-while
       (lambda (child)
         (not (string= "declarator" (treesit-node-field-name child))))
       children)))

   ((string= (treesit-node-type parent) "logical_expression")
    ;; expand logical expression
    ;; example not yet handled: do while (cond1 .and. cond2 &
    ;;                                          .and. cond3)
    ;; here: (cond1 .and. cond2) are implicitly parenthesised
    ;; solution walk up to find root node of logical_expression type,
    ;; then expand logical_expression (but not parenthesised_expression) recursively
    (when-let ((root (treesit-parent-while
                      parent
                      (lambda (n) (string= "logical_expression" (treesit-node-type n))))))
      (f90-ts-inspect-node :indent root "root")
      (let ((children (f90-ts--align-continued-expand-logical-expression root)))
        (f90-ts-log :indent "cont-logexpr: %s" children)
        children)))

   (t
    ;; for cases where nothing needs to be dropped (or just unidentified yet)
    (when-let ((children (and parent (treesit-node-children parent))))
      children))))


(defun f90-ts--align-continued-tokens-prev (children cur-line node-sym)
  "From a list of CHILDREN select relevant nodes prior to given
line CUR-LINE and satisfying some predicates like prior to CUR-LINE
and of same symbol type NODE-SYM."
  ;; if node-sym is not known take almost all kind of nodes, except for continuation symbol
  (let ((pred-almost (lambda (n) (not (eq (f90-ts--align-node-symbol n) 'ampersand))))
        (pred-node-sym (lambda (n) (eq (f90-ts--align-node-symbol n) node-sym))))
    ;; filter nodes by predicate, and if symbol based selection is empty,
    ;; fall back to almost all symbol selection (all except ampersand)
    ;(f90-ts-log :indent "ap: %s" (mapcar #'f90-ts--align-node-symbol children))

    (let ((args-prev-almost (f90-ts--nodes-on-prev-lines children cur-line pred-almost))
          (args-prev-sym (and node-sym
                              (f90-ts--nodes-on-prev-lines children cur-line pred-node-sym))))
      ;(f90-ts-log :indent "apsym: %s" (mapcar #'f90-ts--align-node-symbol args-prev-sym))
      ;(f90-ts-log :indent "apall: %s" (mapcar #'f90-ts--align-node-symbol args-prev-almost))
      (or args-prev-sym args-prev-almost))))


(defun f90-ts--align-continued-select (tokens cur-col cur-line node-sym variant)
  "From TOKENS filter relevant tokens by previous line and symbol type NODE-SYM predicates.
Then determine relevant column position and select column depending on CUR-COL.
This offset is to be used with an column-0 anchor, and hence is a column number."
  ;;(f90-ts-log :indent "contarg1: %d, %s" cur-line node-sym)
  ;;(f90-ts-log :indent "contarg2: %s" tokens)
  (let* ((tokens-prev (f90-ts--align-continued-tokens-prev tokens cur-line node-sym))
         (tokens-col-unsorted (seq-map
                               (lambda (n)
                                 (f90-ts--align-continued-node-col n node-sym))
                               tokens-prev))
         (tokens-col (seq-uniq (seq-sort #'< tokens-col-unsorted)))
         (is-aligned (member cur-col tokens-col)))
    ;;(f90-ts-log :indent "contarg3: %s" tokens-prev)
    ;;(f90-ts-log :indent "contarg4: %s" tokens-col)
    ;;(f90-ts-log :indent "contarg5: %d" cur-col)
    (cond
     ((and tokens-col
           (or (eq variant 'always-first)
               (not is-aligned)))
      ;; we have relevant tokens, and either always-first or not aligned
      (car tokens-col))

     ((and is-aligned
           (eq variant 'keep-or-first))
      ;; aligned, keep current column
      cur-col)

     ((and is-aligned
           (eq variant 'rotate))
      ;; aligned, rotate (go to next column or wrap around)
      (let* ((next-col
              (seq-find (lambda (arg-col) (< cur-col arg-col))
                        tokens-col)))
        ;; if there is a next-col, take it, otherwise get column of first
        ;; argument on previous line
        (or next-col (car tokens-col))))

     (t
      ;; no previous arguments, do something else
      nil)
     )))


(defun f90-ts--align-continued-list-cont (node parent bol)
  "Offset with continued-line option. Return prev-stmt offset
plus indentation offset for continued lines."
  (if-let ((prev-stmt (f90-ts--previous-stmt-first node parent)))
      ;; we start with offset-0, so we need to add column of prev-stmt
      (+ f90-ts-indent-continued
         (f90-ts--node-column prev-stmt))
    bol))


(defun f90-ts--align-continued-list-offset (node parent bol)
  "For lists of tokens like arguments, indent continued Fortran
arguments under argument of previous lines, rotating through
positions. If anonymous node like parenthesis, comma etc, then do
the same, but rotate through columns with symbols of same kind on
previous argument lines.
This offset function is to be used with an column-0 anchor."
  (let ((variant (if f90-ts--align-continued-variant-tab
                     f90-ts-indent-lists-line
                   f90-ts-indent-lists-region)))
    (f90-ts-log :indent "continued list variant is: %s" variant)
    (if (eq variant 'continued-line)
        (f90-ts--align-continued-list-cont node parent bol)

      (seq-let (cur-col cur-line node-sym) (f90-ts--align-continued-location node)
        (let* ((children (and parent (f90-ts--align-continued-children parent)))
               (node-sym-noamp (unless (eq node-sym 'ampersand) node-sym)))
          ;; a line starting with an ampersand is not allowed in standard fortran,
          ;; thus we do not want to align ampersand;
          ;; if lines starts with an ampersand, then this is probably still missing
          ;; some text about to typed, so we align assuming it to be an empty line
          (or (f90-ts--align-continued-select children cur-col cur-line node-sym-noamp variant)
              ;; failed, most likely as there are on prior list items, as point is not on the
              ;; line of the start of the list, use end position of
              ;; parent node (argument_list or whatever) plus 1
              ;; (for example end position corresponds to opening parenthesis)
              (1+ (f90-ts--node-column parent)))))
      )))


(defun f90-ts--align-continued-assoc-error (node parent bol)
  "The same as f90-ts--align-continued-list-offset, but for incomplete
associate lists, where PARENT is an ERROR node.
First map PARENT and call original function."
  ;; the matcher ensures that prev-stmt and ps-sib have type "associate" and
  ;; "association_list", the parent of prev-stmt must have type
  ;; associate_statement, and this one should contain a child association_list
  (let* ((prev-stmt (f90-ts--previous-stmt-first node parent))
         (ps-sib (treesit-node-next-sibling prev-stmt)))
    (f90-ts--align-continued-list-offset node ps-sib bol)))


(defun f90-ts--align-node-symbol (node)
  "Return a symbol representing anonymous punctuation or operator NODE.
If NODE is nil return nil."
  (when node
    ;;(f90-ts-log :indent "symbol node: type=%s, field=%s, text=%s" (treesit-node-type node) (treesit-node-field-name node) (treesit-node-text node))
    (cond
     ((string= (treesit-node-type node) "ERROR")
      'error)

     ((string= (treesit-node-field-name node) "operator")
      'operator)

     ((string= (treesit-node-type node) "&")
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
          ;; default: argument for anything else (this is probably a named node
          ;; of type identifier, number_literal, call_expression etc.
          (_    'named)))))))


;;++++++++++++++
;; debug stuff

(defun fail-info-is (msg)
  "Matcher that always fails, but prints a separator line and
additionally some node and parent info if MSG=first."
  (lambda (node parent bol &rest _)
    (when (string= msg "first")
        (f90-ts-log :indent "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" msg))
    (f90-ts-log :indent "---------info %s--------------" msg)
    (when (or (string= msg "first") (string= msg "catch all"))
      (let* ((grandparent (and parent (treesit-node-parent parent)))
             (prev-stmt (f90-ts--previous-stmt-keyword node parent))
             (pssib (and prev-stmt (treesit-node-next-sibling prev-stmt)))
             ;;(npsib (and node (treesit-node-prev-sibling node)))
             ;;(ppsib (and parent (treesit-node-prev-sibling parent)))
             (child0 (and node (treesit-node-child node 0 t)))
             (sibling0 (and parent (treesit-node-child parent 0 t)))
             (prev-sib (and parent (f90-ts--previous-sibling parent)))
             )
        (f90-ts-log :indent "position: point=%d, bol=%d, lbp=%d, line=%d"
                    (point) bol (line-beginning-position) (line-number-at-pos))
        (let ((ttttt (format "types n-p-gp-ps-ch = %s, %s, %s, %s, %s"
                             (and node (treesit-node-type node))
                             (and parent (treesit-node-type parent))
                             (and grandparent (treesit-node-type grandparent))
                             (and prev-stmt (treesit-node-type prev-stmt))
                             (and child0 (treesit-node-type child0)))))
          (f90-ts-log :indent (propertize ttttt 'face '(:foreground "brown3"))))
        (f90-ts-inspect-node :indent node "info[node]")
        (f90-ts-inspect-node :indent parent "info[parent]")
        (f90-ts-inspect-node :indent grandparent "info[grandparent]")
        (f90-ts-inspect-node :indent prev-stmt "info[prevstmt]")
        (f90-ts-inspect-node :indent pssib "info[pssib]")
        (f90-ts-inspect-node :indent child0 "info[child0]")
        (f90-ts-inspect-node :indent sibling0 "info[firstsib]")
        (f90-ts-inspect-node :indent prev-sib "info[prevsib]")
        ;;(f90-ts-inspect-node :indent npsib "info[npsib]")
        ;;(f90-ts-inspect-node :indent ppsib "info[ppsib]")
        ))
    nil))


(defun f90-ts-indent-rules-info (msg)
  "Used to create a debug indentation rule, which never matches but prints some
debug info. Used as ',@(f90-ts-indent-rules-info \"msg\"')"
  `(;; for testing purposes
    ((fail-info-is ,msg) parent 0)))


;;++++++++++++++
;; simple indentation rules

(defvar f90-ts-indent-rules-test-first
  `(;; for testing purposes
    ,@(f90-ts-indent-rules-info "first")
    )
  "Indentation rules executed first and intended for testing purposes.")


(defvar f90-ts-indent-rules-openmp
  `(;; indent a sequence of openmp statements, these are comments starting
    ;; with !$, so this needs to be done before comments are processed
    ,@(f90-ts-indent-rules-info "openmp")
    ((f90-ts--openmp-comment-is) column-0 0)
    )
  "Indentation rules for openmp. Currently openmp are comment nodes, which start
with !$ or !$omp")


(defvar f90-ts-indent-rules-comments
  `(;; indent a sequence of comments with respect to previous comment
    ,@(f90-ts-indent-rules-info "comments")
    ((f90-ts--special-comment-is) parent 0)
    ((f90-ts--comment-is) prev-sibling 0)
    )
  "Indentation rules for comments (excluding openmp statements).")


(defvar f90-ts-indent-rules-lists
  `(;; we compute absolute column position, using parents column as anchor is not useful for lists
    ,@(f90-ts-indent-rules-info "lists")
    ((parent-is "association_list")                     column-0 f90-ts--align-continued-list-offset)  ;; associate statement
    ((p-ps-pss  "ERROR" "associate" "association_list") column-0 f90-ts--align-continued-assoc-error) ;; unclosed associate statement

    ;; functions and subroutines
    ((parent-is   "argument_list")                 column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps nil  "parameters" "subroutine")       column-0 f90-ts--align-continued-list-offset) ;; arguments of subroutine
    ((n-p-ps nil  "parameters" "function")         column-0 f90-ts--align-continued-list-offset) ;; arguments of function

    ;; logical expressions
    ;;((n-p-ps nil  "parenthesized_expression" "do") column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps nil  "logical_expression"       "do") column-0 f90-ts--align-continued-list-offset)
    ;;((n-p-ps nil  "parenthesized_expression" "if") column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps nil  "logical_expression"       "if") column-0 f90-ts--align-continued-list-offset)

    ;; binding and method lists
    ((parent-is   "binding_list")    column-0 f90-ts--align-continued-list-offset) ;; generic statement in DT decl
    ((parent-is   "final_statement") column-0 f90-ts--align-continued-list-offset) ;; final statement in DT decl

    ;; variable declarations
    ((parent-is   "variable_declaration") column-0 f90-ts--align-continued-list-offset) ;; standard variable declaration
    )
  "Indentation rules for lists on continued lines with alignment on previous list items.
For example: argument lists, association lists, (logical) expressions with alignment at operators, etc.")


(defvar f90-ts-indent-rules-continued
  `(;; handle continued lines which are not yet caught, this also happens for
    ;; unclosed argument lists, where rules-lists cannot be matched
    ,@(f90-ts-indent-rules-info "continued")
    ((f90-ts--continued-line-is) previous-stmt-anchor f90-ts-indent-continued)
    )
  "Indentation rules for continued lines. Argument lists and similar continued lines must have been dealt with before.")


(defvar f90-ts-indent-rules-internal-proc
  `(;; contains statements in modules, programs, subroutines or functions,
    ;; no indentation for contains
    ,@(f90-ts-indent-rules-info "internal_proc")
    ((node-is "internal_procedures") parent 0)
    ((parent-is "internal_procedures") parent f90-ts--indent-toplevel-offset)
    ((n-p-gp nil "ERROR" "internal_procedures") parent f90-ts--indent-toplevel-offset)
    )
  "Indentation rules for internal_proc node, which occurs in conjunction with contain statements.")


(defvar f90-ts-indent-rules-prog-mod
  `(;; program or module interface part (before contains) and end statement
    ,@(f90-ts-indent-rules-info "program module")

    ;; in all cases: first match node with end_xyz_statement, and then only
    ;; whether parent is xyz, as parent is xyz in both cases

    ((node-is "end_program_statement") parent 0)
    ((parent-is  "program")            parent f90-ts--indent-toplevel-offset)
    ((n-p-ps nil "ERROR" "program")    parent f90-ts--indent-toplevel-offset)

    ;; parent-is uses regexp matching, thus use "^module" to avoid that it
    ;; matches "submodule"
    ((node-is "end_module_statement") parent 0)
    ((parent-is  "^module")           parent f90-ts--indent-toplevel-offset)
    ((n-p-ps nil "ERROR" "module")    parent f90-ts--indent-toplevel-offset)

    ((node-is "end_submodule_statement") parent 0)
    ((parent-is  "submodule")            parent f90-ts--indent-toplevel-offset)
    ((n-p-ps nil "ERROR" "submodule")    parent f90-ts--indent-toplevel-offset)
    )
  "Indentation rules for program and module nodes.")


(defvar f90-ts-indent-rules-function
  `(;; functions and subroutine bodies
    ,@(f90-ts-indent-rules-info "function")
    ((node-is "end_subroutine_statement") parent 0)
    ((node-is "end_function_statement") parent 0)
    ((parent-is "subroutine") parent f90-ts-indent-block)
    ((parent-is "function") parent f90-ts-indent-block)
    ((n-p-ps nil nil "subroutine") parent f90-ts-indent-block)
    ((n-p-ps nil nil "function") parent f90-ts-indent-block)
    )
  "Indentation rules for functions and subroutines.")


(defvar f90-ts-indent-rules-interface
  `(;; (abstract) interface bodies
    ,@(f90-ts-indent-rules-info "interface")
    ((node-is "end_interface_statement") parent 0)
    ((parent-is "interface") parent f90-ts-indent-block)
    ((n-p-ps nil "ERROR" "interface") parent f90-ts-indent-block)
    )
  "Indentation rules for interface blocks.")


(defvar f90-ts-indent-rules-derived-type
  `(;; type definitions
    ,@(f90-ts-indent-rules-info "derived type")
    ((n-p-gp      "end_type_statement"      "derived_type_definition" nil)                      parent 0)
    ((n-p-ch-psib "derived_type_procedures" "derived_type_definition" "contains_statement" nil) parent 0)
    ((n-p-ch-psib "ERROR"                   "derived_type_definition" "contains_statement" nil) parent 0)
    ((parent-is "derived_type_procedures") parent f90-ts-indent-block)
    ((parent-is "derived_type_definition") parent f90-ts-indent-block)
    )
  "Indentation rules for derived-type statements.")


(defvar f90-ts-indent-rules-if
  `(;; if-then-else statements
    ,@(f90-ts-indent-rules-info "if then else")
    ;; this must be first, as its parent is an if-statement
    ;; but node=nil/something and parent=if_statement is possible (some line after if)
    ((n-p-gp "end_if_statement" "if_statement"  nil)      parent 0)
    ((n-p-gp "elseif_clause"    "if_statement"  nil)      parent 0)
    ((n-p-gp "else_clause"      "if_statement"  nil)      parent 0)
    ((n-p-ps nil                "if_statement"  "if")     parent f90-ts-indent-block)
    ((n-p-ps nil                "if_statement"  "elseif") parent f90-ts-indent-block) ; line after elseif
    ((n-p-ps nil                "if_statement"  "else")   parent f90-ts-indent-block) ; line after else, with empty else block
    ((n-p-ps nil                "else_clause"   "else")   parent f90-ts-indent-block) ; line after else, with non-empty else block
    ((n-p-ps nil                "elseif_clause" "elseif") parent f90-ts-indent-block)

    ((n-p-ps "elseif_clause"    "ERROR" "if") previous-stmt-anchor 0)
    ((n-p-ps "elseif"           "ERROR" "if") previous-stmt-anchor 0)
    ((n-p-gp "elseif_clause"    "ERROR" nil)  previous-stmt-anchor f90-ts--minus-block-offset) ;; at elseif line, incomplete

    ((n-p-ps "else_clause"      "ERROR" "if")     previous-stmt-anchor 0)
    ((n-p-ps "else_clause"      "ERROR" "elseif") previous-stmt-anchor 0)
    ((n-p-gp "else_clause"      "ERROR" nil)      previous-stmt-anchor f90-ts--minus-block-offset) ;; at elseif line, incomplete
    ((n-p-ps "else"             "ERROR" "if")     previous-stmt-anchor 0)
    ((n-p-gp "else"             "ERROR" nil)      previous-stmt-anchor f90-ts--minus-block-offset)

    ((n-p-ps nil                "ERROR" "if")     previous-stmt-anchor f90-ts-indent-block) ;; empty line after if
    ((n-p-ps nil                "ERROR" "elseif") previous-stmt-anchor f90-ts-indent-block) ;; empty line after elseif
    ((n-p-ps nil                "ERROR" "else")   previous-stmt-anchor f90-ts-indent-block) ;; empty line after else
    )
  "Indentation rules for if-then-else statements.")


(defvar f90-ts-indent-rules-control
  `(;; control statements
    ,@(f90-ts-indent-rules-info "control")
    ((n-p-gp    "end_do_loop" "do_loop" nil)  parent 0)
    ((n-p-ps    nil           "do_loop"           "do") parent f90-ts-indent-block)  ;; proper do block (with or without while)
    ((n-p-ps    nil           "ERROR"             "do") previous-stmt-anchor f90-ts-indent-block)

    ((n-p-gp "end_block_construct_statement" "block_construct" nil)     parent 0)
    ((n-p-ps nil                             "block_construct" "block") parent f90-ts-indent-block)
    ((n-p-ps nil                             "ERROR"           "block") previous-stmt-anchor f90-ts-indent-block)

    ((n-p-gp "end_associate_statement" "associate_statement" nil)         parent 0)
    ((n-p-ps "association"             "associate_statement" nil)         column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps ")"                       "associate_statement" nil)         column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps "=>"                      "association"         nil)         column-0 f90-ts--align-continued-list-offset)
    ((n-p-ps nil                       "association"         "associate") parent f90-ts-indent-block)
    ((n-p-ps nil                       "associate_statement" "associate") parent f90-ts-indent-block)
    )
  "Indentation rules for control statements like do loops, associate and block statements.")


(defvar f90-ts-indent-rules-select
  `(;; control statements
    ,@(f90-ts-indent-rules-info "select type statement")
    ((n-p-gp "end_select_statement" "select_type_statement" nil)              parent 0)
    ((n-p-ps "type_statement"       "select_type_statement" nil)              parent 0)
    ((n-p-ps nil                    "select_type_statement" "type")           parent f90-ts-indent-block)
    ((n-p-ps nil                    "select_type_statement" "class")          parent f90-ts-indent-block)
    ((n-p-ps nil                    "type_statement"        "type")           parent f90-ts-indent-block)
    ((n-p-ps nil                    "type_statement"        "class")          parent f90-ts-indent-block)
    ((n-p-gp     "ERROR"            "select_type_statement" nil)              parent f90-ts-indent-block)
    ((n-p-gp nil                    "ERROR"                 "type_statement") grand-parent f90-ts-indent-block)
    ((parent-is                     "select_type_statement")                  parent 0)

    ,@(f90-ts-indent-rules-info "select case statement")
    ((n-p-gp "end_select_statement" "select_case_statement" nil)              parent 0)
    ((n-p-ps "case_statement"       "select_case_statement" nil)              parent 0)
    ((n-p-ps nil                    "select_case_statement" "case")           parent f90-ts-indent-block)
    ((n-p-ps nil                    "case_statement"        "case")           parent f90-ts-indent-block)
    ((n-p-gp "ERROR"                "select_case_statement" nil)              parent f90-ts-indent-block)
    ((n-p-gp nil                    "ERROR"                 "case_statement") grand-parent f90-ts-indent-block)
    ((parent-is                     "select_case_statement")                  parent 0)
    )
  "Indentation rules for select statements (case and type).")


(defvar f90-ts-indent-rules-catch-all
  `(;; final catch-all rule, with a fallback anchor which also prints
    ;; some diagnostics to allow adding further rules
    ,@(f90-ts-indent-rules-info "catch remaining")
    ((n-p-ch-psib nil "translation_unit" nil "subroutine_statement") parent f90-ts-indent-block)
    ((n-p-ch-psib nil "ERROR"            nil "subroutine_statement") parent f90-ts-indent-block)
    ((n-p-ch-psib nil "translation_unit" nil "function_statement") parent f90-ts-indent-block)
    ((n-p-ch-psib nil "ERROR"            nil "function_statement") parent f90-ts-indent-block)
    ,@(f90-ts-indent-rules-info "catch all")
    (catch-all catch-all-anchor 0)
    )
  "Final indentation rule to handle default case and catch anything else.")


(defvar f90-ts-indent-rules
  `((fortran
     ,@f90-ts-indent-rules-test-first
     ,@f90-ts-indent-rules-openmp
     ,@f90-ts-indent-rules-comments
     ,@f90-ts-indent-rules-lists
     ,@f90-ts-indent-rules-continued
     ,@f90-ts-indent-rules-internal-proc
     ,@f90-ts-indent-rules-prog-mod
     ,@f90-ts-indent-rules-function
     ,@f90-ts-indent-rules-interface
     ,@f90-ts-indent-rules-derived-type
     ,@f90-ts-indent-rules-if
     ,@f90-ts-indent-rules-control
     ,@f90-ts-indent-rules-select
     ,@f90-ts-indent-rules-catch-all
     ))
  "List of all indentation rules in its proper sequence.")


;;------------------------------------------------------------------------------
;; Smart end completion

(defconst f90-ts--complete-end-structs
  '("end_program_statement"
    "end_module_statement"
    "end_submodule_statement"
    ;"end_block_data_statement"
    "end_subroutine_statement"
    "end_function_statement"
    ;"end_module_procedure_statement"
    "end_type_statement"
    "end_interface_statement"
    ;"end_do_label_loop_statement"
    "end_do_loop_statement"
    "end_if_statement"
    ;"end_where_statement"
    ;"end_forall_statement"
    "end_select_statement"
    "end_block_construct_statement"
    "end_associate_statement"
    ;"end_enum_statement"
    ;"end_enumeration_statement"
    ;"end_coarray_team_statement"
    ;"end_coarray_critical_statement"
    )
    "List of type name used to represent end struct statements for
smart end completion. Statements not yet supported are commented out.")


(defun f90-ts--complete-replace-if-changed (start end completion)
  "Replace text in region START...END with COMPLETION, but only if different."
  (let ((original (buffer-substring-no-properties start end)))
    (if (string= original completion)
        (goto-char end)
      (progn
        (delete-region start end)
        (goto-char start)
        (insert completion)
        ))))


(defun f90-ts--complete-smart-end-name (node)
  "Extract the name from NODE. Depending on NODE type, extraction is
different, as subtrees are built differently."
  (when-let* ((query (pcase (treesit-node-type node)
                       ("program"                  "(program (program_statement (_) * (name) @name))")
                       ("module"                   "(module (module_statement (_) * (name) @name))")
                       ("submodule"                "(submodule (submodule_statement (_) * (name) @name))")
                       ("subroutine"               "(subroutine (subroutine_statement name: (_) @name))")
                       ("function"                 "(function (function_statement name: (_) @name))")
                       ("interface"                "(interface (interface_statement (_) * (name) @name))")
                       ("derived_type_definition"  "(derived_type_statement (_) * (type_name) @name)")
                       (_                          nil)
                       ))
              (capture (treesit-query-capture node query)))
    (f90-ts-log :complete "captured name node: %s, %s" capture (cdr (car capture)))
    (cdr (car capture))))


(defun f90-ts--complete-smart-end-compose (node construct-type)
  "Create an 'end CONSTRUCT-TYPE name' completion from NODE.
CONSTRUCT-TYPE is a string like 'subroutine', 'function', 'module', etc."
  (if-let ((name-node (f90-ts--complete-smart-end-name node)))
      (let ((name (treesit-node-text name-node t)))
        (format "end %s %s" construct-type name))
    ;; Fallback if no name found
    (format "end %s" construct-type)))


(defun f90-ts--complete-smart-end-map (node)
  "Map start type of start NODE to completion string."
  (let ((type (treesit-node-type node)))
    (f90-ts-log :complete "smart-end-map type of node: %s" type)
    (pcase type
      ("program"                 (f90-ts--complete-smart-end-compose node "program"))
      ("module"                  (f90-ts--complete-smart-end-compose node "module"))
      ("submodule"               (f90-ts--complete-smart-end-compose node "submodule"))
      ("subroutine"              (f90-ts--complete-smart-end-compose node "subroutine"))
      ("function"                (f90-ts--complete-smart-end-compose node "function"))
      ("derived_type_definition" (f90-ts--complete-smart-end-compose node "type"))
      ("interface"               (f90-ts--complete-smart-end-compose node "interface"))

      ;; TODO: just simple completion, no label or name extraction so far
      ("if_statement"            "end if")
      ("do_loop"                 "end do")
      ("associate_statement"     "end associate")
      ("block_construct"         "end block")
      ("select_case_statement"   "end select")
      ("select_type_statement"   "end select")

      ;; unrecognised, this is not the start of a structured block
      (_                         nil)
      )))


(defun f90-ts--complete-smart-end-show (node-stmt)
  "Show the completion either by jumping to it or printing into the
message buffer, depending on position and whether 'blink is set."
  (let ((top-of-window (window-start))
        (start-block (treesit-node-start node-stmt)))
    (save-excursion
      (goto-char start-block)
      (if (or (eq f90-ts-smart-end 'no-blink)
              (< start-block top-of-window))
          (message "matches %s: %s"
                   (what-line)
                   (buffer-substring
                    (line-beginning-position)
                    (line-end-position)))
        (sit-for blink-matching-delay)))
    ))


(defun f90-ts--complete-smart-end-region (node)
  "Find the end character to be replaced by end completion for node.
We want to remove trailing white space characters, but keep comments
or commands following an semicolon `;`. Moreover, incompletely typed
text like in `end subr_out` with point at `_` should be handled as well."
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


(defun f90-ts--complete-smart-end-node (node &optional fshow)
  "Check whether NODE represents an end struct statement and try find
a completion for it like structure type and name.
Example: compelete `end` closing a subroutine block by `end subroutine mysub`"
  ;; the region check ensures that end statements on continued lines are left alone
  (let ((type (treesit-node-type node))
        (start (treesit-node-start node))
        (end (f90-ts--complete-smart-end-region node))
        (text (treesit-node-text node)))
    ;; make sure that we are looking at and end statement, the parser might add
    ;; and end_xyz_statement node in error recovery mode (e.g. at end of file)
    (when (and (= (line-number-at-pos start) (line-number-at-pos end))
               (and text (string-match-p "^end" text))
               (member type f90-ts--complete-end-structs))
      (f90-ts-log :complete "smart end: text = %s, type = %s" (treesit-node-text node t) type)
      (f90-ts-log :complete "smart end: start = %s, end = %d" start end)
      (when-let* ((node-stmt (treesit-node-parent node))
                  (completion (f90-ts--complete-smart-end-map node-stmt)))
          (f90-ts-log :complete "smart end: node type=%s, stmt type=%s" (treesit-node-type node) (treesit-node-type node-stmt))
          (f90-ts-log :complete "smart end: node start=%s, end=%s" node-stmt node)
          (f90-ts-log :complete "completion string: %S" completion)

          (f90-ts--complete-replace-if-changed start end completion)

          (when fshow
            ;; show the completion, do this even if nothing has been changed,
            ;; as this is useful to shortly show the start of the block
            (funcall fshow node-stmt))
          ))))


;; The idea for smart end completion is taken from the classic f90-mode.
(defun f90-ts--complete-smart-tab ()
  "Provide context-aware completion using tree-sitter after indentation by tab.
Currently it handles end statements."
  (when f90-ts-smart-end
    (f90-ts-log :complete "smart tab point: %d" (point))
    (when-let* ((node-indent (f90-ts--node-at-indent-pos (point)))
                (start (treesit-node-start node-indent))
                (node (treesit-parent-while
                       node-indent
                       (lambda (n) (= start (treesit-node-start n)))))
                (end (treesit-node-end node)))
      (f90-ts-log :complete "node-indent: %s" node-indent)
      (f90-ts-log :complete "complete at node: type=%s, start=%d, end=%d"
                  (treesit-node-type node)
                  (treesit-node-start node)
                  (treesit-node-end node))
      (f90-ts--complete-smart-end-node
       node
       #'f90-ts--complete-smart-end-show))))


;;------------------------------------------------------------------------------
;; Indentation and smart end completion

(defun f90-ts--indent-and-complete ()
  (interactive)
  (f90-ts-log :indent "INDENT ============================")
  (treesit-indent)
  (f90-ts-log :complete "DONE ==========================")
  (f90-ts-log :complete "COMPLETE ==========================")
  (f90-ts--complete-smart-tab)
  (f90-ts-log :complete "DONE ==========================")
  )


(defun f90-ts-complete-smart-end-region (start end)
  "Execute smart end completion in region, using treesitter nodes
representing end constructs."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((root (treesit-buffer-root-node))
         (end-stmts (f90-ts--search-subtree
                     root
                     (lambda (n) (member (treesit-node-type n) f90-ts--complete-end-structs))
                     start end
                     t t)))
    ;; process in reverse order (from last to first, search-subtree returns
    ;; in reversed order (due to last argument t), this way node positions do
    ;; not become stale after completion of end statements
    (cl-loop
     for node in end-stmts
     do (f90-ts--complete-smart-end-node node)
     )))

(defun f90-ts-indent-and-complete-region (start end)
  "Indent region and execute smart end completion in specified region,
based on the treesitter tree overlapping that region."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (treesit-indent-region start end)
  (f90-ts-complete-smart-end-region start end))


;;------------------------------------------------------------------------------
;; Break lines and add continuation symbol

(defun f90-ts--break-line-insert-amp-at-end ()
  "If not yet present, insert ampersand at end of line."
  (or (eq (char-before) ?&)
      (insert " &")))


;; Portions of the following code are adapted from `f90.el`,
;; which is part of GNU Emacs.
(defun f90-ts-break-line ()
  "Break line at point, insert continuation marker where necessary and indent."
  (interactive "*")
  (cond
   ((f90-ts-in-string-p)
    (insert "&\n&"))

   ((f90-ts-in-openmp-p)
    ;; looks like a comment, but starting with special "!$" sequence
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
    (delete-horizontal-space)
    (insert "\n"))

   (t
    (f90-ts--break-line-insert-amp-at-end)
    (delete-horizontal-space)
    (newline 1)
    ;;(if f90-beginning-ampersand (insert "&"))
    ))

  (indent-according-to-mode))



;;------------------------------------------------------------------------------
;; Comment region using some prefix

(defcustom f90-ts-comment-region-prefix "!!$"
  "Comment prefix."
  :type 'string
  :group 'f90-ts)


(defcustom f90-ts-extra-comment-prefixes '("!$omp" "!$acc" "!!!" "!>" "!<")
  "List of additional comment prefixes for interactive selection."
  :type '(repeat string)
  :group 'f90-ts)



;; The following code are adapted from `f90.el`, which is part of GNU Emacs.
(defun f90-ts-comment-region-with-prefix (beg-region end-region prefix)
  "Comment/uncomment every line in the region using comment prefix.
Insert debug comment prefix at the start of every line
in the region, or, if already present, remove it."
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
  "Comment/uncomment every line in the region using default !!$ prefix."
  (interactive "*r")
  (f90-comment-region-with-prefix beg-region end-region f90-comment-region-prefix))


(defun f90-ts-comment-region-custom (beg-region end-region prefix)
  "Comment/uncomment every line in the region using custom PREFIX.
If called interactively, prompt for a prefix from `f90-ts-extra-comment-prefixes`
and `f90-comment-region-prefix`."
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (region-beginning))
    (region-end)
    (completing-read "choose comment prefix: "
                          (cons f90-comment-region-prefix f90-ts-extra-comment-prefixes)
                          nil t nil nil f90-comment-region-prefix)))
  (f90-comment-region-with-prefix beg-region end-region prefix))


;;------------------------------------------------------------------------------
;; auxiliary predicates

(defcustom f90-ts-special-var-regexp "\\_<\\(self\\|this\\)\\_>"
  "Regular expression for matching names of special variables like
self or this. Used for applying a special font lock face."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-prefix-regexp "!\\(?:!*\\|[<>]\\)\\s-*"
  "Regular expression for matching and capturing comment starts (excluding openmp).
For example \"![<>]?\" optionally adds symbols < and > used by documentation tools.
Also add trailing whitespace characters to preserve indentation within comments.
This is used for applying the same comment starter in comment section, see
`f90-ts-break-line`."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defcustom f90-ts-openmp-prefix-regexp "!\\$\\(?:omp\\)?\\s-+"
  "Regular expression for matching comment starts (excluding openmp).
For example \"![<>]?\" optionally adds symbols < and > used by documentation tools.
Also add trailing whitespace characters to preserve indentation within comments."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defcustom f90-ts-special-comment-regexp ""
  "Regular expression for matching special comments (e.g. for structuring code).
Used for applying a special font lock face."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defun f90-ts--comment-prefix (node)
  "Extract the starting character sequence from NODE, assumed to be of type comment.
Include any special symbol characters "
  ;; first match openmp as comment prefix would just take the initial ! and ignoring
  ;; following $omp part in openmp statements
  (let ((rx-comment (concat "^\\(?:" f90-ts-openmp-prefix-regexp "\\)\\|\\(?:" f90-ts-comment-prefix-regexp "\\)")))
    (when (string-match rx-comment (treesit-node-text node))
      (f90-ts-log :auxiliary "matched comment prefix: <%s>" (match-string 0 (treesit-node-text node)))
      (match-string 0 (treesit-node-text node)))))


(defun f90-ts-special-var-p (node)
  "Check if NODE is an identifier and matches the special variable regexp.
Note that the parse uses identifier not just for variables, but for types etc."
  (when (string= (treesit-node-type node) "identifier")
    ;; we do not prepend or append symbol start or end assertions, as it should also
    ;; work with more general regexps (like highlight all variables with a certain prefix)
    ;;(string-match f90-ts-special-var-regexp (treesit-node-text node))))
    (string-match "self" (treesit-node-text node))))


(defun f90-ts-openmp-node-p (node)
  "Check if NODE is a comment node and has the openmp comment prefix."
  (when (string= (treesit-node-type node) "comment")
    (string-match (concat "^" f90-ts-openmp-prefix-regexp) (treesit-node-text node))))


(defun f90-ts-special-comment-node-p (node)
  "Check if NODE is a comment node and satisfies the special comment regexp."
  (when (and (not (string-empty-p f90-ts-special-comment-regexp))
             (string= (treesit-node-type node) "comment"))
    (string-match (concat "^" f90-ts-special-comment-regexp) (treesit-node-text node))))


(defun f90-ts-in-string-p ()
  "Non-nil if point is inside a string."
  (when-let ((node (treesit-node-at (point))))
    (when (string= (treesit-node-type node) "string_literal")
      (let ((start (treesit-node-start node))
            (end (treesit-node-end node))
            (pos (point)))
        ;; start and end position are the quotation characters
        (and (< start pos) (< pos end))))))


(defun f90-ts-in-openmp-p ()
  "Non-nil if point is inside an openmp statement, which starts with !$ and
looks like a comment. The grammar does not parse openmp currently."
  (when-let ((node (treesit-node-at (point))))
    (when (f90-ts-openmp-node-p node)
      (f90-ts-log :auxiliary "in-openmp: %s" (treesit-node-type node))
      (f90-ts-log :auxiliary "in-openmp: %d, %d, %d" (treesit-node-start node) (treesit-node-end node) (point))
      (let ((start (treesit-node-start node))
            (pos (point)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts-in-comment-p ()
  "Non-nil if point is after start of comment. This excludes openmp statements,
which look like comments and are currently not parsed by the treesitter grammar."
  (when-let ((node (treesit-node-at (point))))
    (when (and (string= (treesit-node-type node) "comment")
               (not (f90-ts-openmp-node-p node)))
      (f90-ts-log :auxiliary "in-comment: %s" (treesit-node-type node))
      (f90-ts-log :auxiliary "in-comment: %d, %d, %d" (treesit-node-start node) (treesit-node-end node) (point))
      (let ((start (treesit-node-start node))
            (pos (point)))
        ;; start position is the comment symbol itself
        (and (< start pos))))))


(defun f90-ts-bol-to-point-blank-p ()
  "Return non-nil if only blank characters exist between BOL and point.
Note that in fortran, a continuation symbol shall not be used on blank lines."
  (looking-back "^\\s-*" (line-beginning-position)))


(defun f90-ts--node-type-p (node type)
  "If type is nil, return non-nil and ignore node.
Otherwise return non-nil if node is non-nil and is of type TYPE."
  (if type
      (and node (string= (treesit-node-type node) type))
    t))


(defun f90-ts--node-not-comment-p (node)
  "Return true if NODE is not of type comment."
  (let ((type (treesit-node-type node)))
    (not (string= type "comment"))))


(defun f90-ts--node-not-comment-or-error-p (node)
  "Return true if NODE is not of type comment or error. This is used to
find relevant nodes."
  (let ((type (treesit-node-type node)))
    (not (member type (list "comment" "ERROR")))))


(defun f90-ts-node-overlap-region-p (node start end)
  "Return true if NODE overlaps with region START END."
  (and (< (treesit-node-start node) end)
       (> (treesit-node-end node)   start)))


;;------------------------------------------------------------------------------
;; auxiliary walk and query functions

(defun f90-ts--search-subtree (root pred &optional start end prune reversed)
  "Collect nodes within subtree of ROOT (not necessarily the treesitter
root) for which PRED returns non-nil.
If START and END are non-nil, only visit nodes overlapping that region.
If PRUNE is non-nil, do not descend into children of nodes that
satisfy PRED.
If REVERSED is true, return in reversed order."
  (f90-ts-log :auxiliary "root %s" root)
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
  "For a node, find the most relevant (grand...)parent node starting at
the same position. This is done by ascend to parent nodes until start
position becomes different or an ERROR or translation_unit node is
encountered."
  ;; ascend as long as parent starts at the same position and is not an ERROR node
  (cl-loop
   for current = node then parent
   for parent = (treesit-node-parent current)
   while (and parent
              (= (treesit-node-start parent)
                 (treesit-node-start current))
              (not (or (string= (treesit-node-type parent) "ERROR")
                       (string= (treesit-node-type parent) "translation_unit"))))
   finally return current))


(defun f90-ts--previous-stmt-keyword (node parent)
  "Return the statement leaf node (usually some keyword like if,
elseif, do, etc.) for `f90-ts-prev-stmt-first`. In case of a block label
The first leaf node is the label, not the keyword. For use as anchor,
the label is required. For use as matcher, we need the keyword.
Keyword nodes become relevant for incomplete code with ERROR nodes."
  ;; if the statement starts with a block label, then first is unnamed
  ;; node label, and its parent is block_label_start_expression. Its
  ;; next sibling is a keyword like if or do
  (let* ((first (f90-ts--previous-stmt-first node parent))
         (fparent (and first (treesit-node-parent first))))
    (if (string= (treesit-node-type fparent) "block_label_start_expression")
	    (treesit-node-next-sibling fparent)
      first)))

(defun f90-ts--previous-stmt-first (node parent)
  "Start at node and walk up the tree until a previous sibling can be found.
Then walk down previous sibling to further narrow it down among its children.
Finally return the leaf node at the start of the line with this previous node.
Ignore nodes which do not satisfy the predicate f90-ts--node-not-comment-or-error-p
during ascend or descend (for example comment nodes)."
 (let ((cur-line (line-number-at-pos))
       (predicate #'f90-ts--node-not-comment-or-error-p)
       (n (or node parent))
       prev-sib)  ; previous sibling for children scans (ascending and descending)

   ;;(f90-ts-inspect-node :auxiliary n "prev-stmt-n")
   ;; ascend until a previous sibling was found
   (while (and n (not prev-sib))
     (setq prev-sib (f90-ts--before-child n cur-line predicate))
     ;;(f90-ts-inspect-node :auxiliary prev-sib "prevsib1")
     (setq n (treesit-node-parent n)))

   (let ((prev-descend prev-sib))
     ;; descend the previous sibling to find sub nodes which are still previous to current line
     (while (and prev-sib (> (treesit-node-child-count prev-sib) 0))
       (setq prev-sib (f90-ts--before-child prev-descend cur-line predicate))
       ;;(f90-ts-inspect-node :auxiliary prev-sib "prevsib2")
       (setq prev-descend (or prev-sib prev-descend)))

     ;; return the first leaf on the line where prev-descend is placed
     ;; (take continuation lines into account and go to beginning of statement)
     (and prev-descend (f90-ts--first-node-of-stmt prev-descend))
     )))


(defun f90-ts--previous-sibling (parent)
  "Previous sibling based on position of point. Especially for empty
line, it often happens that node=nil, but parent is some relevant node,
whose children, which are kind of siblings to nil-node-position, can be
used to determine things like indentation.
For the first sibling itself, we do not exclude ERROR nodes, then the
ERROR node is descended (usually just one step) to find a node with relevant
structure type, like subroutine_statement or similar."
  (let* ((cur-line (line-number-at-pos))
         (predicate #'f90-ts--node-not-comment-p)
         (psib (f90-ts--before-child parent cur-line predicate)))
    ;; if psib=nil, just return nil
    ;; if psib=ERROR node, descend and try to find some non-error node
    (cl-loop
     with current = psib
     while (and current
                (string= (treesit-node-type current) "ERROR")
                (treesit-node-child current 0 t))
     do (setq current (treesit-node-child current 0 t))
     finally return current)))


(defun f90-ts--before-child (node line predicate)
  "Return child of NODE, which is on a previous line and satisfies
PREDICATE. Take the last of all children satisfying this condition."
  (when-let* ((children (treesit-node-children node))
              (children-prev (f90-ts--nodes-on-prev-lines children line predicate)))
    (car (last children-prev))))


(defun f90-ts--first-node-on-line (pos)
  "Return the first node on the line at POS."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)

    (let* ((node-indent (treesit-node-at (point)))
           (node-amp1 (and (< 1 (point)) (treesit-node-at (1- (point)))))
           (node-amp2 (and (bolp) node-amp1 (treesit-node-next-sibling node-amp1))))
      ;;(f90-ts-inspect-node :auxiliary node-amp1 "na1")
      ;;(f90-ts-inspect-node :auxiliary node-amp2 "na2")
      ;;(f90-ts-inspect-node :auxiliary node-indent "nai")

      ;; this is a bit tricky: for a continuation line, there are two unnamed nodes "&"
      ;; one at the back-to-indentation position, and one on the previous line,
      ;; but treesit-node-at does not return the ampersand at back-to-indentation, but
      ;; the next leaf node with same start position;
      ;; using (1- (point)) gives the proper ampersand, except if (point) is at
      ;; beginning of line position, in this case:
      ;;   if previous line closes with an ampersand, then amp2 contains that one
      ;;   if previous line is empty, then amp1 contains the ampersand at beginning of line
      ;;   if previous line is a comment (no ampersand required), then amp1 has the comment
      ;;      and amp2 has the desired ampersand
      (cond
       ((and node-amp2 (string= (treesit-node-type node-amp2) "&"))
        node-amp2)
       ((and node-amp1 (string= (treesit-node-type node-amp1) "&"))
        node-amp1)
       (t
        node-indent)))))


(defun f90-ts--last-node-on-line (pos)
  "Go to end of line of POS and obtain the node at this end of line position."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (treesit-node-at (point))))


(defun f90-ts--first-node-of-stmt (node)
  "Return the first node of the statement at which NODE is placed.
Use f90-ts--first-node-on-line, check for continuation symbol and
if present, further go back skipping comments until beginning of
statement is found."
  (let* ((start (treesit-node-start node))
         (head (f90-ts--first-node-on-line start)))
    ;;(f90-ts-inspect-node :indent head "start")
    ;;(f90-ts-inspect-node :indent head "head ")
    (while (string= (treesit-node-type head) "&")
      ;; go to corresponding terminating '&'
      ;; (the head of line ampersand might be virtual, not a real symbol)
      (let ((prev (treesit-node-prev-sibling head)))
        (while (and prev (f90-ts--node-type-p prev "comment"))
          (setq prev (treesit-node-prev-sibling prev)))
        ;; jump to the start of that line, and go on if necessary
        (setq head (f90-ts--first-node-on-line (treesit-node-start prev)))))
    head))


(defun f90-ts--nodes-on-prev-lines (nodes cur-line predicate)
  "Return all NODES on any previous line.
For empty NODES, return an empty list."
  (seq-filter
   (lambda (node)
     (and (funcall predicate node)
          (< (f90-ts--node-line node)
             cur-line)))
   nodes))


(defun f90-ts--after-stmt-line1-p (node pos)
  "Check whether position POS is right after the line of where NODE is
located, being part of a statement possibly spread over several lines.
Empty lines are automatically skipped as those are not present in the tree."
  ;; strategy: get last node on the same line as NODE, check whether it is &,
  ;; goto next node, which is & on next line and compare with line number at pos;
  ;; note that if "&" is at end of line, then there is always a second "&"
  ;; at beginning of the next non-empty/non-comment line or at EOF.
  ;; Hence (treesit-next-sibling last) below can always be executed.

  ;;(f90-ts-inspect-node :auxiliary node "astmt1-node")
  (let* ((cur-line (line-number-at-pos pos))
         (last (f90-ts--last-node-on-line (treesit-node-start node)))
         (nsib (when (and last (string= (treesit-node-type last) "&"))
                 (treesit-node-next-sibling last))))
    ;;(f90-ts-inspect-node :auxiliary last "astmt1-last")
    ;;(f90-ts-inspect-node :auxiliary nsib "astmt1-nsib")
    (and nsib
         (not (< (f90-ts--node-line nsib) cur-line)))))


(defun f90-ts--indent-pos-at-node (node)
  "Determine indentation position of line where start of NODE is located."
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
  "Determine line number of start position of node."
  (line-number-at-pos (treesit-node-start node)))


(defun f90-ts--node-column (node)
  "Determine column number of start position of node."
  (f90-ts--column-number-at-pos (treesit-node-start node)))


(defun f90-ts--column-number-at-pos (pos)
  "Compute column by position."
  (save-excursion
    (goto-char pos)
    (current-column)))


;;------------------------------------------------------------------------------

(define-derived-mode f90-ts-mode prog-mode "F90[TS]"
  "Major mode for editing Fortran 90+ files, using tree-sitter library."
  :group 'f90-ts
  (interactive)
  ;; check if treesit has a ready parser for 'fortran
  (unless (treesit-available-p)
    (error "Tree-sitter support is not available"))

  ;; create parser or report error
  (if (not (treesit-ready-p 'fortran))
      (message "Tree-sitter parser for 'fortran not ready. Run `M-x treesit-install-language-grammar RET fortran RET' or ensure treesit-language-source-alist points to a built grammar.")
    (treesit-parser-create 'fortran))

  ;; font-lock feature list controls what features are enabled for highlighting
  (setq-local treesit-font-lock-feature-list
              '((comment preproc)               ; level 1
                (keyword string type)           ; level 2
                (constant number)               ; level 3
                (function variable operator)    ; level 4
                (bracket delimiter)))           ; level 5

  ;; use the pre-defined font-lock rules variable
  (setq-local treesit-font-lock-settings
              (apply #'append f90-ts-font-lock-rules))

  ;; font-lock level
  (setq-local treesit-font-lock-level 5)

  ;; use the pre-defined indentation rules variable
  (setq-local treesit-simple-indent-rules f90-ts-indent-rules)

  ;; basic setup helper provided by emacs for tree-sitter powered modes
  (when (fboundp 'treesit-major-mode-setup)
    (treesit-major-mode-setup))

  ;; set indentation function (if necessary, some custom function can be used
  ;(setq-local indent-line-function #'treesit-indent-line)
  (setq-local indent-line-function #'f90-ts--indent-and-complete)

  ;; provide a simple mode name in the modeline
  (setq-local mode-name "F90-TS"))


;;(add-to-list 'auto-mode-alist '("\\.f90\\'" . f90-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.f95\\'" . f90-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.f03\\'" . f90-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.f08\\'" . f90-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-ts-mode))


;;------------------------------------------------------------------------------
;; log buffer

(defconst f90-ts-log-categories-all
  '(:info :debug :indent :fontlock :complete :auxiliary)
  "List of all log categories supported by the log function.
Mostly used to discover use of f90-ts-log without a missing or mistyped
category argument.")

(defcustom f90-ts-log-categories
  '()
  "List of enabled log categories for f90-tree-sitter.

Each entry enables a class of info or debug messages in `f90-ts-log'."
  ;; generated entries from the list are like (const :tag "debug" :debug)
  :type `(set
          ,@(mapcar (lambda (cat)
                      `(const :tag ,(substring (symbol-name cat) 1) ,cat))
                    f90-ts-log-categories-all))
  :group 'f90-tree-sitter)


(defconst f90-ts-log-buffer "*f90-ts-log*"
  "Buffer name used for f90 tree-sitter logging.")


(defun f90-ts--log-get-buffer ()
  "Return the log buffer and if it does not yet exists, create a new one."
  (or (get-buffer f90-ts-log-buffer)
      (with-current-buffer (get-buffer-create f90-ts-log-buffer)
        (f90-ts-log-mode)
        (current-buffer))))


(defun f90-ts--log-insert (category fmt &rest args)
  "Insert a message given by FMT and ARGS into the log buffer."
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
        (goto-char (point-max))
        ))

    ;; scroll window if buffer is displayed
    (when-let ((win (get-buffer-window buf t))) ; visible in any frame
      (with-selected-window win
        (end-of-buffer)))))


(defun f90-ts-log (category fmt &rest args)
  "Log a message in CATEGORY if that category is enabled.
CATEGORY is a keyword identifying the log category (e.g. :info or :indent).
FMT + ARGS are passed to `format' and like message arguments."
  (cond
   ((memq category f90-ts-log-categories)
    ;; use apply to properly forwards optional list of arguments args
    (apply #'f90-ts--log-insert category fmt args))
   ((not (memq category f90-ts-log-categories-all))
    (f90-ts--log-insert :info "ERROR: unknown category <%s>" category))
   ))


(defun f90-ts-log-clear ()
  "Clear the f90-ts log buffer."
  (interactive)
  (when (get-buffer f90-ts-log-buffer)
    (with-current-buffer f90-ts-log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))


(defun f90-ts-log-show ()
  "Show the f90-ts log buffer in the current frame."
  (interactive)
  (let ((buf (get-buffer "*f90-ts-log*")))
    (if buf
        (with-selected-frame (selected-frame)
          (switch-to-buffer buf))
      (message "no *f90-ts-log* buffer present"))))


(defmacro f90-ts--redirect-message (category &rest body)
  "Evaluate BODY with all calls to `message` redirected to f90-ts-log-buffer.
This is done to allow treesit inspect routines to output into the log buffer.
Note that for the call to f90-ts--log-insert, messages are redirected to
original function to avoid infinite loops in case f90-ts--log-insert or some
helper function invokes message."
  (let ((original-msg (make-symbol "original-message")))
    `(let ((,original-msg (symbol-function 'message)))
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args)
                    ;; Temporarily restore original message
                    (cl-letf (((symbol-function 'message) ,original-msg))
                      (apply #'f90-ts-log category fmt args))
                    nil)))
         ,@body))))


(defvar f90-ts-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c k") #'f90-ts-log-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `f90-ts-log-mode'.")


(define-derived-mode f90-ts-log-mode special-mode "F90-TS-Log"
  "Major mode for the f90-tree-sitter log buffer."
  (setq-local truncate-lines t)      ; keep logs on one line per entry
  (setq-local buffer-read-only t)    ; ensure it stays read-only
  (buffer-disable-undo)              ; improve performace, no undo required
)


;;------------------------------------------------------------------------------
;; debug stuff

(defun f90-ts-debug-font-lock ()
  "Show the compiled font-lock settings."
  (interactive)
  (pp treesit-font-lock-settings))


(defun f90-ts--inspect--show ()
  "Show the smallest tree-sitter node(s) at point in the echo area."
  (when (and (bound-and-true-p treesit-parser-list)
             (treesit-ready-p 'fortran))
    (let* ((node (treesit-node-at (point)))
           (types (when node (treesit-node-type node))))
      (when types
        (message "[TS] node: %s  range: %s" types (cons (treesit-node-start node) (treesit-node-end node)))))))


(defun f90-ts-inspect-node-at-point (category)
  "Show the tree-sitter node at point as a one-liner in the log buffer."
  (interactive (list :debug))
  (f90-ts--redirect-message category
   (let* ((node (treesit-node-at (point)))
          (parent (treesit-node-parent node)))
     (if (not node)
         (message "no Tree-sitter node at point")
       (let* ((type  (treesit-node-type node))
              (start (treesit-node-start node))
              (end   (treesit-node-end node))
              (len   (- end start)))
         (treesit-inspect-node-at-point)
         (message "parent at point: type= %s  - name= %s -  start=%d  end=%d  len=%d"
                  type treesit--inspect-name start end len))))))


(defun f90-ts-inspect-node (category node info)
  "Show inspect info of treesitter NODE as a one-liner in the log buffer.
Prefix the line with 'inspect<INFO>'."
  (f90-ts--redirect-message category
   (if node
       (let ((pos (treesit-node-start node)))
         (save-excursion
           (goto-char pos)
           (let* ((type  (treesit-node-type node))
                  (start (treesit-node-start node))
                  (end   (treesit-node-end node))
                  (len   (- end start))
                  (line  (line-number-at-pos)))
             (f90-ts-treesit-inspect-node node)
             (message "inspect<%s>: type= %s  -  name= %s - start=%d  end=%d  len=%d  line=%d"
                      info type treesit--inspect-name start end len line))))
     (message "inspect<%s>: nil" info))))


(defun f90-ts-treesit-inspect-node (node-inspect)
  "Copy of treesit-inspect-node-at-point, but highlight provided
NODE-INSPECT and use its start position as point."
  ;; NODE-LIST contains all the node that starts at point.
  (let* ((node-start (treesit-node-start node-inspect))
         (node-list
          (cl-loop for node = (treesit-node-at node-start)
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
    ;; Escape the percent character for mode-line. (Bug#65540)
    (setq treesit--inspect-name (string-replace "%" "%%" name))
    (force-mode-line-update)))


;;------------------------------------------------------------------------------

(provide 'f90-ts-mode)
