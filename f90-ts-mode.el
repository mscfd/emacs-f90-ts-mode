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


;; same as in legacy f90 mode
(defcustom f90-ts-beginning-ampersand nil
  "Non-nil gives automatic insertion of `&' at start of continuation line."
  :type  'boolean
  :safe  'booleanp
  :group 'f90-ts)


;;------------------------------------------------------------------------------

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
  "Face for special comments."
  :group 'f90-ts)


;;------------------------------------------------------------------------------
;; keymap and syntax table

(defvar f90-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<tab>") #'f90-ts-indent-and-complete-stmt)
    (define-key map (kbd "A-<backspace>") #'f90-ts-join-line-prev)
    (define-key map (kbd "A-<delete>") #'f90-ts-join-line-next)
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


;;------------------------------------------------------------------------------
;; auxiliary predicates

(defcustom f90-ts-special-var-regexp "\\_<\\(self\\|this\\)\\_>"
  "Regular expression for matching names of special variables like
self or this. Used for applying a special font lock face."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defcustom f90-ts-comment-prefix-regexp "!\\S-*\\s-+"
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
Used for applying a special font lock face and alignment with parent node."
  :type 'regexp
  :safe #'stringp
  :group 'f90-ts)


(defun f90-ts--node-type-p (node type)
  "If TYPE is nil, return true and ignore NODE.
If NODE is nil and TYPE is non-nil, return nil.
If TYPE is a string, return true if NODE is non-nil and is of type TYPE.
If TYPE is a list of strings, return true if NODE is non-nil its type is
among the elements of TYPE."
  (or (not type)
      (and node
           (let ((type-n (treesit-node-type node)))
             (if (stringp type)
                 (string= type-n type)
               (member type-n type))))))


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
  (when (f90-ts--node-type-p node "identifier")
    ;; we do not prepend or append symbol start or end assertions, as it should also
    ;; work with more general regexps (like highlight all variables with a certain prefix)
    ;;(string-match f90-ts-special-var-regexp (treesit-node-text node))))
    (string-match "self" (treesit-node-text node))))


(defun f90-ts-openmp-node-p (node)
  "Check if NODE is a comment node and has the openmp comment prefix."
  (when (f90-ts--node-type-p node "comment")
    (string-match (concat "^" f90-ts-openmp-prefix-regexp) (treesit-node-text node))))

(defun f90-ts-preproc-node-p (node)
  "Check if NODE is a preprocessor node and has the '#' prefix."
    (let ((type (treesit-node-type node)))
      (or (string-match "^preproc_" type)
          (string-prefix-p "#" type))))

(defun f90-ts-special-comment-node-p (node)
  "Check if NODE is a comment node and satisfies the special comment regexp."
  (when (and (not (string-empty-p f90-ts-special-comment-regexp))
             (f90-ts--node-type-p node "comment"))
    (string-match (concat "^" f90-ts-special-comment-regexp)
                  (treesit-node-text node))))


;; the regexp engine is lacking a case insensitive switch, so we need to
;; lowercase the identifier by hand
(defun f90-ts-builtin-p (node)
  "Return true if NODE represents an builtin function.
The function assumes that NODE is an identifier and only checks the
text of the node."
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
               "random_number" "random_seed" "range" "real" "repeat" "reshape"
               "rrspacing" "scale" "scan" "selected_int_kind"
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
    (when (and (f90-ts--node-type-p node "comment")
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


(defun f90-ts--node-is-ampersand-p (node)
  "Check whether node is continuation symbol &."
  (f90-ts--node-type-p node "&"))


(defun f90-ts--node-not-comment-p (node)
  "Return true if NODE is not of type comment."
  (not (f90-ts--node-type-p node "comment")))


(defun f90-ts--node-not-comment-or-error-p (node)
  "Return true if NODE is not of type comment or error. This is used to
find relevant nodes."
  (not (f90-ts--node-type-p node '("comment" "ERROR"))))


(defun f90-ts-node-overlap-region-p (node start end)
  "Return true if NODE overlaps with region START END."
  (and (< (treesit-node-start node) end)
       (> (treesit-node-end node)   start)))


;;------------------------------------------------------------------------------
;; auxiliary walk and query functions


(defun f90-ts--node-start-or-point (node)
  "Return start of node or current point.
During indentation, node is first node on the line, but on empty lines,
node is nil and we need to fallback to point."
  (or (and node (treesit-node-start node))
      (point)))


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
              (not (f90-ts--node-type-p parent
                                        '("ERROR" "translation_unit"))))
   finally return current))


(defun f90-ts--previous-stmt-keyword-by-first (first)
  "Auxiliary function for `f90-ts--previous-stmt-keyword` or when
first statement is known."
  ;; block structure statements like if, do, associate etc. can start
  ;; with a label, if a (optional) label is present, skip it and extract
  ;; the keyword node;
  ;; in general, the AST looks like:
  ;; (do_loop
  ;;  (block_label_start_expression
  ;;   (label)
  ;;   :)
  ;;  (do_statement do
  ;;  ...))
  ;; we want to extract the unnamed node "do" of the statement following
  ;; the block_label_start expression;
  ;; unfortunately, if the label name is a reserved keyword, then (label)
  ;; becomes (label name) and FIRST is anonymous node "name"
  (let* ((nlabel (or (and (f90-ts--node-type-p first "label")
                          first)
                     (and first
                          (treesit-node-parent first))))
         (fparent (and nlabel (treesit-node-parent nlabel))))
    (if (f90-ts--node-type-p fparent "block_label_start_expression")
	    (let ((fp-next (treesit-node-next-sibling fparent)))
          (cl-loop
           for n = fp-next then child
           for child = (treesit-node-child n 0)
           while child
           finally return n))
      ;; not a label expression, just return first
      first)))


(defun f90-ts--previous-stmt-keyword (node parent)
  "Return the statement leaf node (usually some keyword like if,
elseif, do, etc.) for `f90-ts-prev-stmt-first`. In case of a block label
The first leaf node is the label, not the keyword. For use as anchor,
the label is required. For use as matcher, we need the keyword.
Keyword nodes become relevant for incomplete code with ERROR nodes."
  ;; if the statement starts with a block label, then first is unnamed
  ;; node label, and its parent is block_label_start_expression. Its
  ;; next sibling is a keyword like if or do
  (let ((first (f90-ts--previous-stmt-first node parent)))
    (f90-ts--previous-stmt-keyword-by-first first)))


(defun f90-ts--previous-stmt-first (node parent)
  "Return most previous statement.
First search for any reasonable (leaf) node, which is before current
line, and then go to start of line. If on a continued line, follow it
to the start of the statement.
If current point is within a continued line, then previous leaf node
belongs to the continued line as well, and previous-stmt return the
node at the start of the continued line.

In order to find the most previous leaf node start at node and ascend
the tree until a previous sibling on a previous line can be found. Just
taking a sibling of node is not possible, as node might be nil (empty
line), or node is part of an expression tree with deep nesting or
similar. We really want to go to previous line with a proper node on
it. Once we have a proper node, descend the previous sibling to further
narrow it down among its children.
Finally return the leaf node at the start of the line. Follow continued
lines to first line of continued statement.

Ignore nodes which do not satisfy the predicate
`f90-ts--node-not-comment-or-error-p` during ascend or descend
(for example comment nodes)."
  (let ((cur-line (f90-ts--line-number-at-node-or-pos node))
        (predicate #'f90-ts--node-not-comment-or-error-p)
        (n (or node parent))
        prev-sib)  ; previous sibling for children scans (ascending and descending)
    ;;(f90-ts-inspect-node :auxiliary node "prev-stmt-node")
    ;;(f90-ts-inspect-node :auxiliary parent "prev-stmt-parent")
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
     for current = psib then child
     for child = (and current (treesit-node-child current 0 t))
     while (and child
                (f90-ts--node-type-p current "ERROR"))
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
      ;;(f90-ts-log :indent "first node: %d, %s" (point) node-indent)
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
       ((f90-ts--node-type-p node-amp2 "&")
        node-amp2)
       ((f90-ts--node-type-p node-amp1 "&")
        node-amp1)
       (t
        node-indent)))))


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
  "In continued lines, continuation symbol ampersands appears in
sequences like &, (comment)*, &. This routines checks whether node
FIRST is any of those ampersand or comment nodes, and returns the first
ampersand node of this sequence. FIRST is assumed to be the first node
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


(defun f90-ts--pos-is-continued-p (pos)
  "Check whether line at POS belongs to a continued statement, but is
not the first line of such a statement.
Either at start line there is an &. Or it is a comment, and going
backwards by prev-sibling after a sequence of comments (and only
comments), we find an &."
  ;; if line is empty, first gives the next node on some subsequent
  ;; line; if the line is after some continuation ampersand
  ;; then next node is either a comment or the second ampersand,
  ;; from which we can go backwards
  (let* ((first (f90-ts--first-node-on-line pos)))
    (f90-ts--find-first-ampersand first)))


(defun f90-ts--line-continued-at-end-p (last pos)
  "Check whether line at POS is continued. It usually ends in &, but
might be followed by a comment. We first check that there is a next line
after current line.
LAST is expected to be the last node on the line. Instead of obtaining
LAST itself using POS, it is expected as an argument (the only user of
this function requires LAST for further work)."
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


(defun f90-ts--pos-within-continued-stmt-p (pos)
  "Check whether POS is on some line of a continued statement.
This needs to check forward or backward, as first and last line
must also match."
  (or (f90-ts--pos-is-continued-p pos)
      (when-let ((last (f90-ts--last-node-on-line pos)))
        (f90-ts--line-continued-at-end-p last pos))))


(defun f90-ts--first-node-of-stmt (node)
  "Return the first node of the statement at which NODE is placed.
Use f90-ts--first-node-on-line, check for continuation symbol and
if present, further go back, skipping comments and empty line until
beginning of statement is found."
  ;;(f90-ts-inspect-node :indent node "node")
  (cl-loop
   for namp = node then next-namp
   for first = (progn
                 ;;(f90-ts-inspect-node :indent node "node")
                 ;;(f90-ts-inspect-node :indent namp "namp")
                 (f90-ts--first-node-on-line
                  (treesit-node-start namp)))
   for next-namp = (f90-ts--find-first-ampersand first)
   while next-namp
   finally return first))


(defun f90-ts--nodes-on-prev-lines (nodes cur-line &optional predicate)
  "Return all NODES on any previous line.
For empty NODES, return an empty list.
If PREDICATE is provided, additionally filter by predicate."
  (seq-filter
   (lambda (node)
     (and (or (not predicate)
              (funcall predicate node))
          (< (f90-ts--node-line node)
             cur-line)))
   nodes))


(defun f90-ts--after-stmt-line1-p (node pos)
  "Check whether position POS is on the next line after the line where
NODE is located (NODE assumed to being part of a statement possibly
spread over several lines). Empty lines are automatically skipped as
those are not present in the tree."
  ;; strategy: get last node on the same line as NODE, check whether it is &,
  ;; goto next node, which is & on next line and compare with line number at pos;
  ;; note that if "&" is at end of line, then there is always a second "&"
  ;; at beginning of the next non-empty/non-comment line or at EOF,
  ;; hence (treesit-next-sibling last) below can always be executed.
  ;;(f90-ts-inspect-node :auxiliary node "astmt1-node")
  ;;(f90-ts-log :auxiliary "astmt: pos = %d, node start = %d" pos (treesit-node-start node))
  (when-let* ((cur-line (line-number-at-pos pos))
              (pos-node (treesit-node-start node))
              (last (f90-ts--last-node-on-line pos-node)))
    ;;(f90-ts-inspect-node :auxiliary last "astmt1-last")
    (when (f90-ts--line-continued-at-end-p last pos-node)
      ;;(f90-ts-log :auxiliary "astmt: on continued lines")
      (let ((nsib (treesit-node-next-sibling last)))
        ;;(f90-ts-inspect-node :auxiliary nsib "astmt1-nsib")
        ;;(f90-ts-log :auxiliary "astmt: last start = %d, nsib start = %d"
        ;;            (treesit-node-start last) (treesit-node-start nsib))
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


(defun f90-ts--node-start-or-pos (node)
  "Return start position of node or position of point if node is nil.
In indent-region batch processing, position by (point) is not valid,
but node is always some node. On the other hand, indentation of a single
line has node=nil on empty lines, in which case, we can and should
return (point)."
  (or (and node (treesit-node-start node))
      (point)))


(defun f90-ts--line-number-at-node-or-pos (node)
  "If NODE is non-nil, return line number at which start position is
located, otherwise return line number of current point position."
  (or (and node (f90-ts--node-line node))
      (line-number-at-pos)))


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
      "where" "forall" "concurrent"] @font-lock-keyword-face))
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


;;------------------------------------------------------------------------------
;; Indentation

;;++++++++++++++
;; matchers

(defun f90-ts--continued-line-first-is ()
  "Check whether we are first continued line of a continued statement.
This line is indented relative to the statements first line.
All other line use indentation of previous line."
  (lambda (node parent bol &rest _)
    (when-let ((prev-stmt (f90-ts--previous-stmt-first node parent))
               (pos (f90-ts--node-start-or-pos node)))
      (f90-ts--after-stmt-line1-p prev-stmt pos)
      )))


;; this is also true for the first continued line, if this needs
;; extra handling, then the matcher for the first line should be called first
(defun f90-ts--continued-line-some-is ()
  "Check whether we are on some continued line of a continued statement."
  (lambda (node parent bol &rest _)
    (cond
     (node
      (let ((pos (treesit-node-start node)))
        (f90-ts--pos-is-continued-p pos)))

     (parent
      ;; node=nil but parent is a proper node, then we are probably on an empty line
      (when-let ((psib (f90-ts--previous-sibling parent)))
        ;; previous-sibling already excludes comment node, hence we can directly
        ;; check whether there is an ampersand
        (f90-ts--node-type-p psib "&"))))))


(defun f90-ts--openmp-comment-is ()
  "Matcher that checks whether node is an openmp comment."
  (lambda (node parent bol &rest _)
    (and (f90-ts--node-type-p node "comment")
         (f90-ts-openmp-node-p node))))

(defun f90-ts--preproc-node-is ()
  "Matcher that checks whether node is a fortran preprocessor node."
  (lambda (node parent bol &rest _)
    (and node (f90-ts-preproc-node-p node))))


(defun f90-ts--preproc-at-toplevel-is ()
  "Matcher that checks if parent is a preprocessor node and its first
non-preprocessor ancestor is a toplevel node (program, module, etc.)."
  ;; Content inside preprocessor:
  ;; - Search up the tree for the first "Ancestor" that is NOT a
  ;;   preprocessor node.
  ;; - If that ancestor is a module or program -> toplevel indent
  (lambda (_n parent &rest _)
     (and parent
          (f90-ts-preproc-node-p parent)
          (let ((ancestor (treesit-parent-until
                           parent
                           (lambda (n)
                             (not (f90-ts-preproc-node-p n))))))
            (and ancestor
                 (string-match-p "module\\|program" (treesit-node-type ancestor)))))))


(defun f90-ts--special-comment-is ()
  "Matcher that checks whether node is a special comment.
These are aligned to their parents."
  (lambda (node parent bol &rest _)
    (and (f90-ts--node-type-p node "comment")
         (f90-ts-special-comment-node-p node))))


(defun f90-ts--comment-region-is ()
  "Matcher that checks whether node and previous node are comments
and are of same type (both special or both other).
Use first node of previous line (skipping empty lines) to avoid
trailing comments."
  (lambda (node parent bol &rest _)
    (when (f90-ts--node-type-p node "comment")
      (when-let* ((prev-sib (treesit-node-prev-sibling node))
                  (prev-line (f90-ts--first-node-on-line
                              (treesit-node-start prev-sib))))
        (f90-ts-inspect-node :indent node "node")
        (f90-ts-inspect-node :indent prev-sib "prev-sib")
        (f90-ts-inspect-node :indent prev-line "prev-line")
        (and (f90-ts--node-type-p prev-line "comment")
             (eq (not (f90-ts-special-comment-node-p node))
                 (not (f90-ts-special-comment-node-p prev-line))))
        ))))


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


(defun previous-line-anchor (node parent bol)
  "Anchor at previous line with a statement on it. Used for continued
lines, where the previous sibling or parent is not the right anchor."
  (let* ((cur-line (f90-ts--line-number-at-node-or-pos node))
         ;; TODO: for the moment, do not exclude comments or anything else,
         ;; just get indentation at previous relevant line, it might only
         ;; contain a comment
         (predicate (lambda (n) t))
         (psib (f90-ts--before-child parent cur-line predicate)))
    ;;(f90-ts-inspect-node :indent psib "prev-line-anchor-psib")
    (if psib
        (f90-ts--indent-pos-at-node psib)
      bol)))


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
  "Indent stuff right below top level nodes: f90-ts-indent-toplevel when
inside module, submodule or program, otherwise use f90-ts-indent-contain."
  ;(f90-ts-log :indent "%d  %d  %d" bol (treesit-node-start parent) (treesit-node-start node))
  (let* ((grandparent (treesit-node-parent parent))
         (ggparent    (and grandparent (treesit-node-parent grandparent))))
    (f90-ts-log :indent "toplevel-offset: grandparent type = %s" (and grandparent (treesit-node-type grandparent)))
    (f90-ts-log :indent "toplevel-offset: ggparent type = %s" (and ggparent (treesit-node-type ggparent)))
    ;; before 'contains' statement, grandparent is translation_unit,
    ;; after 'contains' it is module or program
    (if (or (f90-ts--node-type-p grandparent '("module" "submodule" "program" "translation_unit"))
            (and (f90-ts--node-type-p grandparent "ERROR")
                 (f90-ts--node-type-p ggparent "translation_unit")))
        f90-ts-indent-toplevel
      f90-ts-indent-contain)))


;;++++++++++++++
;; anchor functions: lists on continued lines
;; rotation logic

(defun f90-ts--align-node-symbol (node)
  "Return a symbol for NODE for grouping nodes. Relevant nodes
for alignment are mostly selected among nodes with same symbol.
If NODE is nil return nil."
  (when node
    (cond
     ((f90-ts--node-type-p node "ERROR")
      'error)

     ((f90-ts--node-type-p node "comment")
      'comment)

     ((string= (treesit-node-field-name node) "operator")
      'operator)

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
          ;; default: argument for anything else (this is probably a named node
          ;; of type identifier, number_literal, call_expression etc.
          (_    'named)))))))


(defun f90-ts--align-continued-location (node)
  "Determine node dependent position related values. These are current
column, line number and symbol type at point (for deciding
where to align). If NODE is nil, then no relevant node is at point and
point position is used instead. This is important for handling empty
line."
  (if node
      (list (f90-ts--node-column node)
            (f90-ts--node-line node)
            ;; decide which kind of (named or anonymous) nodes to filter
            ;; (argument, parentheses, comma, ampersend, etc)
            (f90-ts--align-node-symbol node))
    (list (f90-ts--column-number-at-pos (point))
          (line-number-at-pos)
          nil)
    ))


;;++++++++++++++
;; list context: association_list

(defun f90-ts--align-continued-expand-assoc (nodes)
  "Replace association nodes in list NODES by their first two children,
which are field 'name' and '=>'. There is no handling of the selector
part currently. Skip continuation symbols, which might be between
'name' and arrow '=>'."
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


(defun f90-ts--align-continued-assocation-items (list-context _node)
  "Determine relevant childrens of LIST-CONTEXT = 'association_list'."
  (cl-assert (f90-ts--node-type-p list-context "association_list")
             nil "expected list context: association_list, got '%s'" list-context)
  ;; expand it, as the it contains a list of nodes, whose children are required
  (when-let ((children (treesit-node-children list-context)))
    ;;(f90-ts-log :indent "assoc_list children: %s" children)
    (f90-ts--align-continued-expand-assoc children)))


;;++++++++++++++
;; list context: logical_expression

(defun f90-ts--align-continued-expand-log-expr (node)
  "Recursively compute relevant children of a nested logical expression
tree. An expression like A .and. B .and. C is roughly represented as
logical_expression(logical_expression(A .and. B), .and. C).
The routine returns the five children A, .and., B, and., C.
It does not descend into parenthesized_expressions."
  (if (f90-ts--node-type-p node "logical_expression")
      (mapcan #'f90-ts--align-continued-expand-log-expr
              (treesit-node-children node))
    (list node)))


(defun f90-ts--align-continued-log-expr-items (list-context _node)
  "Determine relevant childrens of LIST-CONTEXT = 'logical_expression'."
  (cl-assert (f90-ts--node-type-p list-context "logical_expression")
             nil "expected list context: logical_expression, got '%s'" list-context)
  ;; expand logical expression
  ;; example: do while (cond1 .and. cond2 &
  ;;                          .and. cond3)
  ;; here: (cond1 .and. cond2) are implicitly parenthesised
  ;; solution walk up to find root node of logical_expression type,
  ;; then expand logical_expression (but not parenthesised_expression) recursively
  (when-let ((root (treesit-parent-while
                    list-context
                    (lambda (n) (f90-ts--node-type-p n "logical_expression")))))
    (f90-ts-inspect-node :indent root "root")
    (let ((children (f90-ts--align-continued-expand-log-expr root)))
      ;; (f90-ts-log :indent "cont-logexpr: %s" children)
      children)))


;;++++++++++++++
;; list context: parameters

(defun f90-ts--align-continued-parameters-items (list-context _node)
  "Determine relevant childrens of LIST-CONTEXT = 'parameters'."
  (cl-assert (f90-ts--node-type-p list-context "parameters")
             nil "expected list context: parameters, got '%s'" list-context)
  ;; this also includes the opening and closing parenthesis
  (treesit-node-children list-context))


;;++++++++++++++
;; list context: binding_list, final_statement

(defun f90-ts--align-continued-binding-items (list-context _node)
  "Determine relevant childrens of LIST-CONTEXT = 'binding_list'
or 'final_statement'. Both occur in the contains part of a derived
type definition."
  (cl-assert (or (f90-ts--node-type-p list-context "binding_list")
                 (f90-ts--node-type-p list-context "final_statement"))
             nil "expected list context: binding_list or final_statement, got '%s'" list-context)
  (when-let ((children (treesit-node-children list-context)))
    ;; drop the (binding_name ...) part, and the => binding symbol
    (seq-drop children 2)))


;;++++++++++++++
;; list context: variable_declarations

(defun f90-ts--align-continued-var-decl-items (list-context node)
  "Determine relevant childrens of LIST-CONTEXT = 'variable_declaration'."
  (cl-assert (f90-ts--node-type-p list-context "variable_declaration")
             nil "expected list context: variable_declaration, got '%s'" list-context)
  (when-let ((children (treesit-node-children list-context)))
    (let* ((pos (f90-ts--node-start-or-point node))
           (attr-children (seq-filter (lambda (n)
                                        (string= (treesit-node-field-name n) "attribute"))
                                      children))
           (decl-children (seq-filter (lambda (n)
                                        (string= (treesit-node-field-name n) "declarator"))
                                      children))

           ;;(attr-end (seq-max (seq-map (lambda (child) (treesit-node-end child))
           ;;                            attr-children)))
           (decl-start (seq-min (seq-map (lambda (child) (treesit-node-start child))
                                         decl-children)))
           )
      (f90-ts-log :indent "cont var-decl: pos=%d, decl-start=%d" pos decl-start)
      (if (< pos decl-start)
          attr-children
        decl-children))))


;;++++++++++++++
;; list context: arguments

(defun f90-ts--align-continued-arguments-items (list-context _node)
  "Determine relevant childrens of LIST-CONTEXT = 'argument_list'."
  (cl-assert (f90-ts--node-type-p list-context "argument_list")
             nil "expected list context: argument_list, got '%s'" list-context)
  (treesit-node-children list-context))


;;++++++++++++++
;; additional anchor for aligned lists:

(defun f90-ts--align-continued-default-anchor (_list-context items-filtered _node-sym prev-stmt-1)
  "Return a list of default positions (anchors) depending on
LIST-CONTEXT, NODE-SYM and whether ITEMS-FILTERED has any nodes.
The default list consists of one single value which is standard
continued line indentation."
  (unless items-filtered
    (list
     (list (treesit-node-start prev-stmt-1) f90-ts-indent-continued)
     )))


(defun f90-ts--align-continued-tuple-anchor (list-context items-filtered node-sym prev-stmt-1)
  "Return a list of default anchor positions depending on context and
whether ITEMS-FILTERED has any nodes.
For argument lists (call sub(...)) and parameters (subroutine sub (...)),
a default and fallback position is to align one column position to the
right of the initial opening parenthesis."
  ;; for argument_list, there should always be the opening parenthesis
  (cl-assert (or (f90-ts--node-type-p list-context "argument_list")
                 (f90-ts--node-type-p list-context "parameters"))
             nil "expected list context: argument_list or parameters, got '%s'" list-context)
  (seq-filter #'identity
              (list
               ;; always offer one column to the right of opening
               ;; parenthesis position, except for closing parenthesis
               (unless (eq node-sym 'parenthesis)
                 (list (treesit-node-start list-context) 1))
               ;; offer default continued line indentation if items-filtered is empty
               (unless items-filtered
                 (list (treesit-node-start prev-stmt-1) f90-ts-indent-continued))
               )))


;;++++++++++++++
;; list context: items and columns

;; TODO: if no suitable symbol is found, use first item position and
;; default continued indent position, example:
;; associate(some_very_long_name &
;;                => x)
;; suggest anchor with offset 0 and 5 for "=>" relative to
;; first position of some_very_long_name
(defun f90-ts--align-continued-items-filter (items node-sym)
  "From a list ITEMS of node items select relevant nodes satisfying
some predicates like being of compatible symbol type to NODE-SYM."
  ;; if node-sym is not known take almost all kind of nodes, except for continuation symbol
  (let ((pred-almost (lambda (n) (not (member (f90-ts--align-node-symbol n)
                                              '(parenthesis ampersand)))))
        (pred-node-sym (lambda (n) (eq (f90-ts--align-node-symbol n) node-sym)))
        )
    ;; filter nodes by predicate, and if symbol based selection is empty,
    ;; fall back to almost-all symbol selection (all except ampersand)
    (let ((items-almost (seq-filter pred-almost items))
          (items-sym (and node-sym
                          (seq-filter pred-node-sym items)))
          )
      ;(f90-ts-log :indent "items-sym: %s" (mapcar #'f90-ts--align-node-symbol items-sym))
      ;(f90-ts-log :indent "items-aall: %s" (mapcar #'f90-ts--align-node-symbol items-almost))
      (or items-sym items-almost)
      )))


(defun f90-ts--align-continued-col-pos (items)
  "Map a list of ITEMS of nodes or otherwise obtained pair
(buffer positions offset), to a list of triples of
column number, buffer position and offset."
  (seq-map
   (lambda (n)
     (if (treesit-node-p n)
         (list
          ;; some node, compute column number and buffer position, offset=0
          (f90-ts--node-column n)
          (treesit-node-start n)
          0)
       ;; a pair (buffer position, offset) add column
       ;; compute column at position and then add offset
       ;; (do not compute column at position + offset, this might not be valid)
       (cons
        (+ (f90-ts--column-number-at-pos (car n)) (cadr n))
        n)))
   items))


(defun f90-ts--align-continued-cp-sort (cp-alist)
  "Make list CP-LIST of (column position offset) triples unique by
column position. For several entries with same column number,
take the element with the largest buffer position."
  ;;(f90-ts-log :indent "cp-alist: %s" cp-alist)
  ;;(f90-ts-log :indent "cp-groups: %s" (seq-group-by #'car cp-alist))
  (let ((cp-alist-unique
         (seq-map (lambda (group)
                    ;; elements produced by seq-group-by are:
                    ;; group = (col (col pos1 offset1) (col pos2 offset2) ...)
                    ;; ((cdr group) gets rid of initial col)
                    (cl-reduce (lambda (cp1 cp2)
                                 (if (> (cadr cp1) (cadr cp2)) cp1 cp2))
                               (cdr group)))
                  (seq-group-by #'car cp-alist))))
    ;;(f90-ts-log :indent "cp-unique: %s " cp-alist-unique)
    (seq-sort (lambda (a b) (< (car a) (car b))) cp-alist-unique)))


(defun f90-ts--align-continued-select (items cur-col variant)
  "From a list ITEMS of nodes or other determined column positions,
determine relevant column positions and select column depending on
CUR-COL, which has current column.
The selected column is return as (anchor offset)."
  (let* ((col-pos-unsorted (f90-ts--align-continued-col-pos items))
         (col-pos (f90-ts--align-continued-cp-sort col-pos-unsorted))
         (aligned-at (seq-find (lambda (cp) (= cur-col (car cp)))
                               col-pos)))
    (f90-ts-log :indent "cont columns-positions unsorted: %s" col-pos-unsorted)
    (f90-ts-log :indent "cont columns-positions: %s" col-pos)
    (cond
     ((and col-pos
           (or (eq variant 'always-first)
               (not aligned-at)))
      (f90-ts-log :indent "cont take first: col-pos = %s" (car col-pos))
      ;; we have relevant items, and either always-first or not aligned
      ;; retrieve buffer position for first entry
      (cdar col-pos))

     ((and aligned-at
           (eq variant 'keep-or-first))
      (f90-ts-log :indent "cont aligned and keep/first: aligned-at = %s" aligned-at)
      ;; aligned, keep current column, but use proper element from col-pos
      ;; as anchor, otherwise indent-region does not take indentation of anchor
      ;; position into account
      (cdr aligned-at))

     ((and aligned-at
           (eq variant 'rotate))
      ;; aligned, rotate (go to next column or wrap around)
      ;; recall that col-pos is sorted by columns
      (let* ((next-cp
              (seq-find (lambda (cp) (< cur-col (car cp)))
                        col-pos)))
        (f90-ts-log :indent "cont aligned and rotate: next-pos = %s" next-cp)
        ;; if there is a next-col, take it, otherwise get column of first
        ;; argument on previous line
        (or (cdr next-cp) (cdar col-pos))))

     (t
      ;; no previous arguments, do something else
      ;; actually this should not happen, the :get-other function
      ;; should always return some fallback position
      ;; (like prev-stmt-1+default indent for continued lines)
      (cl-assert t nil "no relevant columns found")
      nil)
     )))


(defun f90-ts--align-continued-list-anchor (variant node list-context prev-stmt-1)
  "Determine items on continued lines in a list-like context and use
their buffer positions for alignmet. If anonymous node like parenthesis,
comma etc, then do the same, but rotate through items with symbols
of same kind on previous argument lines."
  (seq-let (cur-col cur-line node-sym) (f90-ts--align-continued-location node)
    (f90-ts-log :indent "cont location: %s, %s" node (f90-ts--align-node-symbol node))
    (f90-ts-log :indent "cont location: col=%s, line=%d, sym=%s" cur-col cur-line node-sym)
    (let* ((get-items (f90-ts--get-list-context-prop :get-items list-context))
           (get-other (f90-ts--get-list-context-prop :get-other list-context))
           (items-context (funcall get-items list-context node))
           (items-prev (seq-filter
                        (lambda (node) (< (f90-ts--node-line node)
                                          cur-line))
                        items-context))
           (items-filtered (f90-ts--align-continued-items-filter
                            items-prev
                            node-sym))
           (anchors-other (funcall get-other list-context items-filtered node-sym prev-stmt-1))
           )

      (f90-ts-log :indent "cont items context: %s" items-context)
      (f90-ts-log :indent "cont items prev: %s" items-prev)
      (f90-ts-log :indent "cont items filtered: %s" items-filtered)
      (f90-ts-log :indent "cont anchor other: %s" anchors-other)

      (f90-ts--align-continued-select (append anchors-other
                                              items-filtered)
                                      cur-col
                                      variant))))


;;++++++++++++++
;; anchor functions: continued lines
;; determine whether list or standard case

(defvar-local f90-ts--continued-line-offset-cache nil
  "This is necessary to separate anchor and offset function for
continued lines. After determining an anchor node myanchor on a
previous line, we cannot return (treesit-node-start myanchor) plus
offset. This buffer position can be on the current line, and then
indentation fails. So we cache the offset and return position of
myanchor.
This variable stores computed offset as (bol . offset). The bol is
only used in an assertion to ensure that offset is not stale.

Once the cached value is used, it is also removed. Thus if a new
offset is stored, the cache is expected to be nil.
")


(defun f90-ts--continued-line-offset-cache-set (bol offset)
  "Store BOL and OFFSET in cache."
  ;;(cl-assert (not f90-ts--continued-line-offset-cache)
  ;;           "offset cache is not nil (continued line)")
  (unless f90-ts--continued-line-offset-cache
    (f90-ts-log :indent "continued line: offset cache is not nil, cache=%s"
                f90-ts--continued-line-offset-cache))
  (setq f90-ts--continued-line-offset-cache (cons bol offset)))


(defun f90-ts--continued-line-offset (_node _parent bol)
  "Return cached offset. Use BOL to assert that cached offset is valid."
  (let ((bol-cached (car-safe f90-ts--continued-line-offset-cache))
        (offset-cached (cdr-safe f90-ts--continued-line-offset-cache))
        )
    ;;(cl-assert (and bol-cached
    ;;                (eq bol bol-cached))
    ;;           nil
    ;;           "stale offset found (continued line), bol expected %s, got %s" bol-cached bol)

    (if (not bol-cached)
        (f90-ts-log :indent "continued line: no cached offset")
      (unless (eq bol bol-cached)
        (f90-ts-log :indent "continued line: stale offset, bol=%s, cached=%s"
                    bol f90-ts--continued-line-offset-cache)))

    ;; clear cache
    (setq f90-ts--continued-line-offset-cache nil)
    (or offset-cached
        0)))


;; get-items: function to determine node items in the list context
;;            relevant for alignment
;; get-other: other columns for alignment (default values and fallback values)
(defconst f90-ts--list-context-types
  '(("argument_list"        . (:get-items f90-ts--align-continued-arguments-items
                               :get-other f90-ts--align-continued-tuple-anchor))
    ("parameters"           . (:get-items f90-ts--align-continued-parameters-items
                               :get-other f90-ts--align-continued-tuple-anchor))
    ("logical_expression"   . (:get-items f90-ts--align-continued-log-expr-items
                               :get-other f90-ts--align-continued-default-anchor))
    ("binding_list"         . (:get-items f90-ts--align-continued-binding-items
                               :get-other f90-ts--align-continued-default-anchor))
    ("final_statement"      . (:get-items f90-ts--align-continued-binding-items
                               :get-other f90-ts--align-continued-default-anchor))
    ("variable_declaration" . (:get-items f90-ts--align-continued-var-decl-items
                               :get-other f90-ts--align-continued-default-anchor))
    ("association_list"     . (:get-items f90-ts--align-continued-assocation-items
                               :get-other f90-ts--align-continued-default-anchor))
    )
  "List of tree-sitter node types presenting some kind of list context
which is suitable for alignment indentation.
And the associated function to extract relevant children for alignemnt.")


(defun f90-ts--get-list-context-prop (pkey list-context)
  "Lookup LIST-CONTEXT and return the property value for PKEY
from the list-context-type alist."
  ;;(f90-ts-log :indent "context-prop: context=%s " (treesit-node-type list-context))
  (let ((properties (alist-get (treesit-node-type list-context)
                               f90-ts--list-context-types
                                nil
                                nil
                                #'string=))
        )
    ;;(f90-ts-log :indent "context-prop: prop=%s " properties)
    (plist-get properties pkey)))


(defun f90-ts--align-continued-list-context (pos parent ps-key)
  "In case some list alignment option is active, determine whether we
are within a list type context and the relevant context node. Often
this is PARENT, but sometimes a related node like grandparent, etc.
The smallest such context, starting on a previous line is returned.
Return value nil signals that this is not a list context."
  (let* ((line (line-number-at-pos pos))
         (stmt-root (treesit-node-on (treesit-node-start ps-key)
                                     pos))
         )
    (when-let*
        ((stmt-max
          (treesit-search-subtree
           stmt-root
           (lambda (n) (and (< (f90-ts--node-line n) line)
                            (<= line (line-number-at-pos (treesit-node-end n)))
                            (assoc (treesit-node-type n)
                                   f90-ts--list-context-types)))))
         (stmt-min ; stmt-max serves as implicit upper bound
          (treesit-parent-until
           parent
           (lambda (n) (and (< (f90-ts--node-line n) line)
                            (assoc (treesit-node-type n)
                                   f90-ts--list-context-types)))
           t ; include parent in search
           ))
         )

      (f90-ts-log :indent "continued root: pos=%d, line=%s, root=%s" pos line stmt-root)
      (f90-ts-log :indent "continued root: stmt-min=%s" stmt-min)
      (f90-ts-log :indent "continued root: stmt-max=%s" stmt-max)

      stmt-min)))


(defun f90-ts--continued-line-anchor (node parent bol &rest _)
  "For a continued line, determine the kind of continuation and
return a suitable anchor.
If the current position is within a list like context, try to align
list items (like arguments in a procedure call). Otherwise determine
standard indentation for continued lines.

Exact behaviour is determined by custom variables
`f90-ts-indent-lists-line` and `f90-ts-indent-lists-region`.

Note that due to the matcher, we already know that we are on a
continued line of a continued statement. The statement line itself
is not catched by the continued line matcher."
  (let* ((variant (if f90-ts--align-continued-variant-tab
                      f90-ts-indent-lists-line
                    f90-ts-indent-lists-region))
         (prev-stmt-1 (f90-ts--previous-stmt-first node parent))
         (default-anchor-offset (if prev-stmt-1
                                    (list (treesit-node-start prev-stmt-1)
                                          f90-ts-indent-continued)
                                  (list bol 0))))

    (f90-ts-log :indent "continued line: variant=%s" variant)
    (f90-ts-log :indent "continued line: prev-stmt-1=%s" prev-stmt-1)
    (f90-ts-log :indent "continued line: default=%s, bol=%s" default-anchor-offset bol)
    (f90-ts-log :indent "continued line: offset cache=%s" f90-ts--continued-line-offset-cache)

    (let ((anchor-offset
           (if (or (eq variant 'continued-line)
                   (not prev-stmt-1))
               default-anchor-offset
             (let* ((ps-key (f90-ts--previous-stmt-keyword-by-first prev-stmt-1))
                    (pos (f90-ts--node-start-or-point node))
                    (list-context (f90-ts--align-continued-list-context pos parent ps-key))
                    )
               (f90-ts-log :indent "continued line: list-context=%s" list-context)
               (f90-ts-log :indent "continued region: %s" (treesit-node-on (treesit-node-start prev-stmt-1)
                                                                           (treesit-node-end parent)))
               (if list-context
                   (f90-ts--align-continued-list-anchor variant
                                                        node
                                                        list-context
                                                        prev-stmt-1)
                 ;; default continued line indentation
                 default-anchor-offset)))))
      (f90-ts-log :indent "cont anchor final: %s" anchor-offset)
      ;; cache offset for offset function, return anchor
      (f90-ts--continued-line-offset-cache-set bol (cadr anchor-offset))
      (car anchor-offset)
      )))


;;++++++++++++++
;; debug stuff

(defun fail-info-is (msg)
  "Matcher that always fails, but prints a separator line and
additionally some node and parent info if MSG=first."
  (lambda (node parent bol &rest _)
    (f90-ts-log :indent "---------info %s--------------" msg)
    (when (or (string= msg "first") (string= msg "catch all"))
      (let* ((grandparent (and parent (treesit-node-parent parent)))
             (prev-stmt (f90-ts--previous-stmt-keyword node parent))
             (pssib (and prev-stmt (treesit-node-next-sibling prev-stmt)))
             ;;(npsib (and node (treesit-node-prev-sibling node)))
             ;;(ppsib (and parent (treesit-node-prev-sibling parent)))
             (child0 (and node (treesit-node-child node 0 t)))
             (sibling0 (and parent (treesit-node-child parent 0 t))) ;; first named sibling
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
  `(;; comments are ignored if checking previous statements, first comment
    ;; in a sequence of comments can be aligned like a normal statement,
    ;; but if a comment follows another comment, then we need an extra rule
    ;; to align to previous comment
    ,@(f90-ts-indent-rules-info "comments")
    ;; indent a sequence of comments of same kind (special or other)
    ;; with respect to previous comment
    ((f90-ts--comment-region-is) prev-sibling 0)
    ;; indent special comments like their parent nodes
    ;; this check is after the region check, hence previous sibling
    ;; is not a comment of same kind
    ((f90-ts--special-comment-is) parent 0)
    )
  "Indentation rules for comments (excluding openmp statements).")

(defvar f90-ts-indent-rules-preproc
  `(;; indent preprocessor directive.
    ,@(f90-ts-indent-rules-info "preprocessor directive")
    ;; Directive itself: no indent
    ((f90-ts--preproc-node-is) column-0 0)
    ;; Directive with preproc parent at toplevel
    ((f90-ts--preproc-at-toplevel-is) grand-parent f90-ts-indent-toplevel)
    ;; Other
    ((n-p-gp nil "preproc_.*" nil) grand-parent f90-ts-indent-block))
  "Indentation rules for preprocessor directives.")

(defvar f90-ts-indent-rules-continued
  `(;; handle continued lines
    ,@(f90-ts-indent-rules-info "continued")
    ;; special case, first node is closing parenthesis means this is a continued line
    ((n-p-gp ")" "parenthesized_expression" nil) parent 0)
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
    ((f90-ts--continued-line-some-is) f90-ts--continued-line-anchor f90-ts--continued-line-offset)
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
    ((node-is    "end_subroutine_statement") parent 0)
    ((node-is    "end_function_statement")   parent 0)
    ((parent-is  "subroutine")               parent f90-ts-indent-block)
    ((parent-is  "function")                 parent f90-ts-indent-block)
    ((n-p-ps nil nil "subroutine")           parent f90-ts-indent-block)
    ((n-p-ps nil nil "function")             parent f90-ts-indent-block)
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
    ((n-p-ps nil                "if_statement"  "if")     parent f90-ts-indent-block) ; line right after if
    ((n-p-ps nil                "if_statement"  "elseif") parent f90-ts-indent-block) ; line right after elseif
    ((n-p-ps nil                "if_statement"  "else")   parent f90-ts-indent-block) ; line right after else, with empty else block
    ((n-p-ps nil                "else_clause"   "else")   parent f90-ts-indent-block) ; line after else, with non-empty else block
    ((n-p-ps nil                "elseif_clause" "elseif") parent f90-ts-indent-block)

    ((n-p-ps "elseif_clause"    "ERROR" "if") previous-stmt-anchor 0)
    ((n-p-ps "elseif"           "ERROR" "if") previous-stmt-anchor 0)
    ((n-p-gp "elseif_clause"    "ERROR" nil)  previous-stmt-anchor f90-ts--minus-block-offset) ;; at elseif line, incomplete

    ((n-p-ps "else_clause"      "ERROR" "if")     previous-stmt-anchor 0)
    ((n-p-ps "else_clause"      "ERROR" "elseif") previous-stmt-anchor 0)
    ((n-p-gp "else_clause"      "ERROR" nil)      previous-stmt-anchor f90-ts--minus-block-offset) ;; at else line, incomplete
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
    ((n-p-ps    nil           "do_loop" "do") parent f90-ts-indent-block)  ;; proper do block (with or without while)
    ((n-p-ps    nil           "ERROR"   "do") previous-stmt-anchor f90-ts-indent-block)

    ((n-p-gp "end_block_construct_statement" "block_construct" nil)     parent 0)
    ((n-p-ps nil                             "block_construct" "block") parent f90-ts-indent-block)
    ((n-p-ps nil                             "ERROR"           "block") previous-stmt-anchor f90-ts-indent-block)

    ((n-p-gp "end_associate_statement" "associate_statement" nil)         parent 0)
    ((n-p-ps nil                       "association"         "associate") parent f90-ts-indent-block)
    ((n-p-ps nil                       "associate_statement" "associate") parent f90-ts-indent-block)
    ((n-p-ps nil                       "ERROR"               "associate") previous-stmt-anchor f90-ts-indent-block)
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
     ,@f90-ts-indent-rules-preproc
     ,@f90-ts-indent-rules-openmp
     ,@f90-ts-indent-rules-comments
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


(defun f90-ts--complete-replace-if-changed (beg end completion)
  "Replace text in region BEG...END with COMPLETION, but only if
different. Return true if something was changed."
  (let* ((original (buffer-substring-no-properties beg end))
         (is-equal (string= original completion)))
    (if is-equal
        (goto-char end)
      (progn
        (delete-region beg end)
        (goto-char beg)
        (insert completion)
        ))
    (not is-equal)))



(defconst f90-ts--complete-smart-end-query
  `(("program"                 . "(program (program_statement \"program\" @construct (_) * (name) @name))")
    ("module"                  . "(module (module_statement \"module\" @construct (_) * (name) @name))")
    ("submodule"               . "(submodule (submodule_statement \"submodule\" @construct (_) * (name) @name))")
    ("subroutine"              . "(subroutine (subroutine_statement \"subroutine\" @construct name: (_) @name))")
    ("function"                . "(function (function_statement \"function\" @construct name: (_) @name))")
    ("interface"               . ,(concat "(interface (interface_statement (abstract_specifier)?"
                                          "\"interface\" @construct"
                                          "[((name) @name)"
                                          " ((operator) @name)"
                                          " ((assignment) @name)]?"
                                          "))"
                                          ))
    ("derived_type_definition" . "(derived_type_definition (derived_type_statement \"type\" @construct (_) * (type_name) @name))")
    ("if_statement"            . "(if_statement (block_label_start_expression (_) @name)? \"if\" @construct)")
    ("do_loop"                 . "(do_loop (block_label_start_expression (_) @name)? (do_statement \"do\" @construct))")
    ("associate_statement"     . "(associate_statement (block_label_start_expression (_) @name)? \"associate\" @construct)")
    ("block_construct"         . "(block_construct (block_label_start_expression (_) @name)? \"block\" @construct)")
    ("select_case_statement"   . "(select_case_statement (block_label_start_expression (_) @name)? \"select\" @construct)")
    ("select_type_statement"   . "(select_type_statement (block_label_start_expression (_) @name)? \"select\" @construct)")
    )
  "Treesitter queries to extract relevant nodes for smart end completion.")


(defun f90-ts--complete-smart-end-extract (node capture)
  "Extract relevant nodes from CAPTURES. Somehow anchoring at NODE does
not work. First search for root=NODE. Then extract subsequent matches,
some of which are optional and possibly not present or in different
order."
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
  "Extract construct type and name from NODE. Depending on NODE type,
extraction is different, as subtrees are built differently.
The construct type is fixed, but we want to query the lower/upper case
to match usage in opening and end statement."
  (when-let* ((query (alist-get (treesit-node-type node)
                                f90-ts--complete-smart-end-query
                                nil
                                nil
                                #'string=))
              (query-root (concat query " @root"))
              (capture-all (treesit-query-capture node query-root))
              (capture     (f90-ts--complete-smart-end-extract node capture-all))
              )
    ;; @root is added to also get the root node of the captured subtree,
    ;; capture result is an alist (('root, root), ('construct, construct) ('name, name),
    ;; ('root, root), ('construct, construct) ('name, name), ...),
    ;; where root and name are the captured nodes,
    ;; we need to make sure that root=node, which might not be the case in nested block structure
    ;; (where the inner loop, if etc. also matches),
    ;; this could be done with the :anchor pattern, but it is rejected... syntax not valid?
    ;;(f90-ts-log :complete "smart end name captured: cap-all=%s" capture-all)
    (f90-ts-log :complete "smart end name captured: cap=%s" capture)
    (let* ((construct-node (alist-get 'construct capture))
           (name-node (alist-get 'name capture)))
      (f90-ts-log :complete "smart end name captured: construct=%s name=%s" construct-node name-node)
      (cl-assert construct-node
                 nil
                 "complete-smart-end: no construct node found")
      (list
       (and construct-node (treesit-node-text construct-node t))
       (and name-node (treesit-node-text name-node t)))
      )))


(defun f90-ts--complete-smart-end-compose (node)
  "Create an 'end construct name' completion from NODE, where construct
is a string like 'subroutine', 'function', 'module', etc.
If no suitable query for recovering the construct name exists, then
the construct is either not yet supported or not the start of a
structured block statement. In this case, return nil."
  (f90-ts-log :complete "smart end: type of node = %s" (and node (treesit-node-type node)))
  (when-let ((construct-name (f90-ts--complete-smart-end-name node)))
    (cl-assert construct-name
               nil
               "complete-smart-end: structure query failed")
    (let* ((construct (car construct-name))
           (name (cadr construct-name))
           (c0 (substring construct 0 1))
           (end
            (cond
             ((string= construct (upcase construct))
              "END")
             ((string= c0 (upcase c0))
              "End")
             (t
              "end"))))
      (if name
          (format "%s %s %s" end construct name)
        ;; fallback if no name found
        (format "%s %s" end construct)))))


(defun f90-ts--complete-smart-end-show (node-block)
  "Show the completion either by jumping to it or printing into the
message buffer, depending on position and whether 'blink is set."
  (let ((top-of-window (window-start))
        (start-block (treesit-node-start node-block)))
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


(defun f90-ts--complete-smart-end-node (node)
  "Check whether NODE represents an end struct statement and try find
a completion for it like structure type and name.
Example: complete `end` closing a subroutine block by
`end subroutine mysub`.
After the changes, NODE will become stale. The function returns the new
statement node representing the whole block. This is used for the blink
option or statement indentation."
  ;; the region check ensures that end statements on continued lines are left alone
  (let ((type (treesit-node-type node))
        (beg (treesit-node-start node))
        (end (f90-ts--complete-smart-end-region node))
        (text (treesit-node-text node)))
    ;; make sure that we are looking at and end statement, the parser might add
    ;; and end_xyz_statement node in error recovery mode (e.g. at end of file)
    (when (and (= (line-number-at-pos beg) (line-number-at-pos end))
               (and text (string-match-p "^end" text))
               (member type f90-ts--complete-end-structs))
      (f90-ts-log :complete "smart end: text = %s, type = %s" (treesit-node-text node t) type)
      (f90-ts-log :complete "smart end: beg = %s, end = %d" beg end)
      (let ((node-block (treesit-node-parent node))
            node-block-new)
        (when-let ((completion (f90-ts--complete-smart-end-compose node-block)))
          (f90-ts-log :complete "smart end: node type=%s, stmt type=%s"
                      (treesit-node-type node)
                      (treesit-node-type node-block))
          (f90-ts-log :complete "smart end: node beg=%s, node end=%s" node-block node)
          (f90-ts-log :complete "completion string: %S" completion)

          (let (beg-marker
                end-marker)
            (unwind-protect
                (progn
                  ;; beg marker should move with inserted/deleted text
                  ;; to stay at start of node
                  (setq beg-marker (copy-marker (treesit-node-start node-block) t))
                  (setq end-marker (copy-marker (treesit-node-end node-block) t))
                  ;; returns true if text was changed
                  (f90-ts--complete-replace-if-changed beg end completion)
                  (when (treesit-node-check node-block 'outdated)
                    (setq node-block-new (treesit-node-on (marker-position beg-marker)
                                                          (marker-position end-marker))))
                  )
              (when beg-marker (set-marker beg-marker nil))
              (when end-marker (set-marker end-marker nil))))
          )
        ;; if nothing has changed new node is nil, return the original one
        (or node-block-new node-block)))))


(defun f90-ts--complete-smart-end-indent (node-block)
  "Indent block represented by NODE-BLOCK. This can be optionally
invoked after smart end completion to indent the whole block.
If indentation changes something, the tree is updated and node-block
becomes stale. In this case, the function return the node-block-new,
otherwise nil."
  (let (beg-marker
        end-marker
        node-block-new)
    (unwind-protect
        (progn
          ;; use region variant for indentation (no rotation of list
          ;; items on continued lines)
          (setq f90-ts--align-continued-variant-tab nil)
          (setq beg-marker (copy-marker (treesit-node-start node-block) t))
          (setq end-marker (copy-marker (treesit-node-end node-block) t))
          (treesit-indent-region beg-marker end-marker)
          (when (treesit-node-check node-block 'outdated)
            (setq node-block-new (treesit-node-on (marker-position beg-marker)
                                                  (marker-position end-marker))))
      ;; revert to tab variant
      (setq f90-ts--align-continued-variant-tab t)
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil))))
    node-block-new))


;; The idea for smart end completion is taken from the classic f90-mode.
(defun f90-ts--complete-smart-tab (indent-block)
  "Provide context-aware completion using tree-sitter after indentation by tab.
Currently it handles end statements.

If INDENT-BLOCK is true, then indent the whole block with indent-region."
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
      (when (= (line-number-at-pos)
               (f90-ts--node-line node))
        (when-let ((node-block (f90-ts--complete-smart-end-node node)))
          (f90-ts-log :complete "smart end actions: node-block=%s" node-block)
          (when indent-block
            ;; update node-block, if indent changes anything
            (setq node-block (or (f90-ts--complete-smart-end-indent node-block)
                                 node-block)))
          (f90-ts--complete-smart-end-show node-block))))))


;;------------------------------------------------------------------------------
;; Indentation: auxiliary functions for continued lines

(defun f90-ts--indent-stmt-first (first)
  "Indent first line with FIRST being first node on that line, the first
node of the statement. Compute and return the applied offset."
  (f90-ts-log :indent "indent statement0: pos=%d, first=%s" (point) first)
  (save-excursion
    (goto-char (treesit-node-start first))
    ;; indent first line of statement and compute applied offset
    (beginning-of-line)
    (let ((before-indent (current-indentation)))
      (treesit-indent)
      (- (current-indentation) before-indent))))


(defun f90-ts--indent-stmt-rest (start offset)
  "Indent remaining lines after first line of a multi line statement.
First line can be found at position START.
The same OFFSET computed for the first line is applied to all lines.
Instead of walking lines, the loop goes from last node on line to
next node, which must be on next relevant line. This automatically
skips empty lines."
  (save-excursion
    (goto-char start)
    (cl-loop
     for last = (f90-ts--last-node-on-line (point))
     for next = (and last (treesit-node-next-sibling last))
     while (or (f90-ts--node-type-p last "comment")
               (f90-ts--line-continued-at-end-p last (line-end-position)))
     do (progn
          (goto-char (treesit-node-start next))
          (f90-ts-log :indent "indent statement1: %d, last=%s, next=%s" (point) last next)
          (indent-line-to
           (max 0 (+ (current-indentation) offset))))
     )))


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
    (f90-ts-log :indent "indent statement region: beg=%d, end=%d" beg-reg end-reg)
    ;; possibly slow if continued lines are very long (but safer)
    (let ((old-text (buffer-substring-no-properties beg-reg end-reg)))
      (treesit-indent-region beg-reg end-reg)
      (string= old-text (buffer-substring-no-properties beg-reg end-reg)))

    ;; this is fast, but is indent-region implemented in such a way that tick
    ;; increases only if indentation on some line really has changed?
    ;; maybe this variant is not worth the risk
    ;;(let ((old-tick (buffer-chars-modified-tick)))
    ;;  (treesit-indent-region beg-reg end-reg)
    ;;  (= old-tick (buffer-chars-modified-tick)))))
    ))


;;------------------------------------------------------------------------------
;; Indentation and smart end completion

;; line indentation for <tab>, C-<tab> etc.
;;  * f90-ts-indent-and-complete-stmt
;;  * f90-ts-indent-and-complete-line
;;  * f90-ts-indent-line
;;
;; region indentation:
;;  * f90-ts-indent-and-complete-region

(defvar-local f90-ts--align-continued-variant-tab nil
  "Current variant for indentation, if nil use region variant,
otherwise use tab variant.")


(defun f90-ts--indent-and-complete-line-aux (indent-block)
  "Auxiliary wrapper for indent-and-complete-line function. It takes
an additional argument INDENT-BLOCK, which indents a whole block if
point is at line containing its end statement."
  (unwind-protect
      (progn
        (setq f90-ts--align-continued-variant-tab t)
        (f90-ts-log :indent "INDENT ============================")
        (treesit-indent)
        (f90-ts-log :complete "DONE ==========================")
        (f90-ts-log :complete "COMPLETE ==========================")
        (f90-ts--complete-smart-tab indent-block)
        (f90-ts-log :complete "DONE =========================="))
    (setq f90-ts--align-continued-variant-tab nil)
    ))


(defun f90-ts-indent-and-complete-line ()
  "Default function for indent and smart complete of end lines, bound
to <tab. More advanced indentations of involved continued lines and
block structures is done by `f90-ts--indent-and-complete-stmt`"
  (interactive)
  (f90-ts--indent-and-complete-line-aux nil))


(defun f90-ts-indent-line ()
  "Default function for indentation of a single line. Smart end
completion or other extra stuff is not executed."
  (interactive)
  (unwind-protect
      (progn
        (setq f90-ts--align-continued-variant-tab t)
        (f90-ts-log :indent "INDENT ============================")
        (treesit-indent)
        (f90-ts-log :complete "DONE ==========================")
    (setq f90-ts--align-continued-variant-tab nil)
    )))


(defun f90-ts-indent-and-complete-stmt ()
  "In general, this just calls `f90-ts--indent-and-complete-line-aux`
with true for indent-block.
However, if within a continued line region, determine first line of
statement and indent region from this first line up to and including
current line. If indentation of current line has not changed, then
indent the current line by invoking `f90-ts--indent-and-complete` to
apply rules like rotation of list context items.
Otherwise default indent with line choice."
  (interactive)
  (cond
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (f90-ts-complete-smart-end-region beg end)))

   ((not (f90-ts--pos-within-continued-stmt-p (point)))
    ;; just do indent-and-complete plus block indentation if applicable
    (f90-ts--indent-and-complete-line-aux t))

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
        (back-to-indentation))
      ))))


;; used by f90-ts-indent-and-complete-region
(defun f90-ts-complete-smart-end-region (beg end)
  "Execute smart end completion in region from BEG to END,
using treesitter nodes representing end constructs."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((beg-pos (if (markerp beg) (marker-position beg) beg))
        (end-pos (if (markerp end) (marker-position end) end)))
    (let* ((root (treesit-buffer-root-node))
           (end-stmts (f90-ts--search-subtree
                       root
                       (lambda (n) (member (treesit-node-type n) f90-ts--complete-end-structs))
                       beg-pos end-pos
                       t t)))
      ;; process in reverse order (from last to first, search-subtree returns
      ;; in reversed order (due to last argument t), this way node positions do
      ;; not become stale after completion of end statements
      (cl-loop
       for node in end-stmts
       do (f90-ts--complete-smart-end-node node)
       ))))


(defun f90-ts-indent-and-complete-region (beg end)
  "Indent region and execute smart end completion in specified region,
based on the treesitter tree overlapping that region."
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
          (treesit-indent-region beg-marker end-marker)
          (f90-ts-complete-smart-end-region beg-marker end-marker))
      (when beg-marker (set-marker beg-marker nil))
      (when end-marker (set-marker end-marker nil)))))


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
    ;; this results in an empty line, no ampersand
    (delete-horizontal-space)
    (insert "\n"))

   (t
    (delete-horizontal-space)
    (f90-ts--break-line-insert-amp-at-end)
    (newline 1)
    (if f90-ts-beginning-ampersand (insert "&"))
    ))

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
  "Join previous line with the current one, if it is part of a continued
statement. This is (partially) a counterpart to `f90-ts-break-line`.
If previous line has comments (at end, next line etc.) joining is not
done."
  (interactive)
  (let* ((first-node (f90-ts--first-node-on-line (point)))
         (amp-node (when (and first-node
                              (f90-ts--node-type-p first-node "&"))
                     first-node))
         (prev-node (and amp-node (treesit-node-prev-sibling amp-node))))
    ;;(f90-ts-inspect-node :info last-node "last")
    ;;(f90-ts-inspect-node :info amp-node "amp")
    ;;(f90-ts-inspect-node :info next-node "next")
    (cl-assert (f90-ts--node-type-p prev-node '("&" "comment"))
               nil "internal error: prev node is not an ampersand or comment?")
    (if (and amp-node
             prev-node
             (f90-ts--node-type-p prev-node "&"))
        (let ((beg (treesit-node-start prev-node))
              (end (treesit-node-end amp-node)))
          (delete-region beg end)
          (goto-char beg)
          (fixup-whitespace))
      (message "join failed: not a simple continued line"))))


(defun f90-ts-join-line-next ()
  "Join current line with the next one, if it is part of a continued
statement. This is (partially) a counterpart to `f90-ts-break-line`.
If continued line has comments (at end, next line etc.) joining is not
done."
  (interactive)
  (let* ((last-node (f90-ts--last-node-on-line (point)))
         (amp-node (when (and last-node
                              (f90-ts--node-type-p last-node "&"))
                     last-node))
         (next-node (and amp-node (treesit-node-next-sibling amp-node))))
    ;;(f90-ts-inspect-node :info last-node "last")
    ;;(f90-ts-inspect-node :info amp-node "amp")
    ;;(f90-ts-inspect-node :info next-node "next")
    (cl-assert (f90-ts--node-type-p next-node '("&" "comment"))
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

(define-derived-mode f90-ts-mode prog-mode "F90[TS]"
  "Major mode for editing Fortran 90+ files, using tree-sitter library."
  :group 'f90-ts
  (interactive)
  ;; check if treesit has a ready parser for 'fortran
  (unless (treesit-available-p)
    (error "Tree-sitter support is not available"))

  ;; create parser or report error
  (if (not (treesit-ready-p 'fortran))
      (message
       (concat "Tree-sitter parser for 'fortran not ready. "
               "Run `M-x treesit-install-language-grammar RET fortran RET' "
               "or ensure treesit-language-source-alist points to a built grammar."))
    (treesit-parser-create 'fortran))

  ;; font-lock feature list controls what features are enabled for highlighting
  (setq-local treesit-font-lock-feature-list
              '((comment preproc)               ; level 1
                (builtin keyword string type)   ; level 2
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

  ;; set indentation functions (both add smart end completion)
  (setq-local indent-line-function #'f90-ts-indent-and-complete-line)
  (setq-local indent-region-function #'f90-ts-indent-and-complete-region)

  ;; provide a simple mode name in the modeline
  (setq-local mode-name "F90-TS"))


;;------------------------------------------------------------------------------
;; debug: log buffer

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
;; debug: node inspection and other stuff

(defun f90-ts-debug-font-lock ()
  "Show the compiled font-lock settings."
  (interactive)
  (pp treesit-font-lock-settings))


(defun f90-ts-inspect-node (category node info)
  "Show inspect info of treesitter NODE as a one-liner in the log buffer.
Prefix the line with 'inspect<INFO>'."
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


(defun f90-ts-treesit-inspect-node (node-inspect)
  "Copy of treesit-inspect-node-at-point, but highlight provided
NODE-INSPECT and use its start position as point.
Return the constructed name."
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


;;------------------------------------------------------------------------------

(provide 'f90-ts-mode)
