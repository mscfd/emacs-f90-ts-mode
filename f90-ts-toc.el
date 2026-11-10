;;; f90-ts-toc.el --- Table of content tree for f90-ts-mode buffers -*- lexical-binding: t; -*-

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

;; Table of content buffer based on an induced sparse tree.  This shows the
;; main structures in a buffer as a table of content tree, and allows some
;; navigation and operations on the structure.
;;
;; The panel follows the selected window: a single idle timer switches the
;; panel to the current f90-ts buffer, refreshes it after edits, and moves
;; point in the panel to the entry belonging to point in the source.
;; Several keys in the panel are bound.  To find and discover them, ? can
;; be pressed to open a transient menu, where all key and there operations
;; are listed.  Each key works without pressing ? first
;; (see `f90-ts-toc-transient').
;; Mouse: click previews, double-click or middle-click jumps, right-click opens
;; a menu.
;;
;; While the source buffer is narrowed, all entries stay visible, but the ones
;; outside of the narrowed region are dimmed, see `f90-ts-toc-dimmed-face'.
;; The entries inside are for example a module with its variables, subroutines
;; and types.  Narrowing to one of them narrows further.  Walking in the panel
;; works too: moving point (or clicking) to a dimmed line widens the source
;; buffer and narrows it to the structure of that line, which is the entry
;; itself or, for a variable, the structure it is declared in.  Narrowing
;; on a variable does the same.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; hl-line is used for highlighting lines in the toc buffer
(require 'hl-line)

;; pulse is used to flash the previewed entry
(require 'pulse)

;; transient menu of the panel
(require 'transient)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff, in particular `f90-ts--nav-queries'
(require 'f90-ts-auxiliary)

;; mark and comment region, used for the node operations in the panel
(require 'f90-ts-mark-region)
(require 'f90-ts-comment-region)

;; main mode to hook into f90-ts-mode-menu
(require 'f90-ts-mode)


;;;-----------------------------------------------------------------------------

(defgroup f90-ts-toc nil
  "TOC buffer (table of content) options used by `f90-ts-toc'.
This is part of `f90-ts-mode'."
  :prefix "f90-ts-"
  :group  'f90-ts)


(defface f90-ts-toc-procedure-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for subroutine, function, and interface entries in toc buffer."
  :group 'f90-ts-toc)


(defface f90-ts-toc-type-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for type entries in toc buffer."
  :group 'f90-ts-toc)


(defface f90-ts-toc-module-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for module, submodule, program entries in toc buffer."
  :group 'f90-ts-toc)


(defface f90-ts-toc-variable-face
  '((t :inherit default))
  "Face for variable entries in toc buffer."
  :group 'f90-ts-toc)


(defface f90-ts-toc-separator-face
  '((t :inherit shadow))
  "Face for the `contains' separator in toc buffer."
  :group 'f90-ts-toc)


(defface f90-ts-toc-dimmed-face
  '((t :inherit shadow))
  "Face for entries outside the narrowed region of the source buffer.
It is applied on top of the faces of the entries by overlays."
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-contains-label "---contains---"
  "Label of the separator shown in the toc buffer for `contains'.
It separates the entries before and after the `contains' keyword of a module,
submodule, program, procedure or type, if there are entries after it.
If nil, no separator is shown."
  :type '(choice (const :tag "No separator" nil) string)
  :safe (lambda (x) (or (null x) (stringp x)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-buffer-auto-sync t
  "If non-nil the point in the toc buffer follows point in the source buffer."
  :type 'boolean
  :safe  #'booleanp
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-buffer-idle-delay 0.5
  "Seconds of idle time before the toc buffer follows the selected window.
This covers switching to another source buffer, refreshing after edits and
syncing point.  A change takes effect when the panel is (re)opened."
  :type 'number
  :safe (lambda (x) (and (numberp x) (> x 0)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-buffer-side 'left
  "Side of the frame where the toc buffer is displayed.
Toggle interactively with `f90-ts-toc-buffer-toggle-side', which also moves a
visible panel.  Changing this option only takes effect when the panel is
opened again."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :safe (lambda (x) (memq x '(left right)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-name-shortening 'truncate
  "How names that do not fit into the toc buffer are shown.
With `truncate', the window truncates the line at its edge.  With `left' or
`right', the name is shortened from that side and the cut is marked with
`f90-ts-toc-ellipsis', so that the interesting end stays visible, for example
\"…me_which_does_not_fit\" (left) or \"very_long_name_which_…\" (right).
The full name is shown as tooltip.
Cycle interactively with `f90-ts-toc-buffer-cycle-shortening'."
  :type '(choice (const :tag "Truncate line at window edge" truncate)
                 (const :tag "Shorten name from the left" left)
                 (const :tag "Shorten name from the right" right))
  :safe (lambda (x) (memq x '(truncate left right)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-ellipsis "…"
  "String marking the cut of a shortened name in the toc buffer.
This should be a single character.  It is only used if
`f90-ts-toc-name-shortening' is `left' or `right'.  Choose a plain character
like `*' if your font lacks the Unicode ones."
  :type '(choice (const :tag "… horizontal ellipsis" "…")
                 (const :tag "⋯ midline horizontal ellipsis" "⋯")
                 (const :tag "‥ two dot leader" "‥")
                 (const :tag "· middle dot" "·")
                 (const :tag "* asterisk" "*")
                 (const :tag "+ plus" "+")
                 (const :tag "~ tilde" "~")
                 (const :tag "> greater than" ">")
                 (string :tag "Other (one character)"))
  :safe (lambda (x) (and (stringp x) (= (length x) 1)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-buffer-width 40
  "Width of toc buffer."
  :type 'integer
  :safe (lambda (x) (and (integerp x) (> x 0)))
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-show-variables t
  "If non-nil, show variable declarations in the toc buffer.
Toggle interactively with `f90-ts-toc-buffer-toggle-variables'."
  :type 'boolean
  :safe #'booleanp
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-show-interface-procedures t
  "If non-nil, show interface blocks in the toc buffer.
This applies to all interface blocks (generic, general without a name,
and abstract) together with their contents, like procedure statements and
interface bodies.
Toggle interactively with `f90-ts-toc-buffer-toggle-interface-procedures'."
  :type 'boolean
  :safe #'booleanp
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-include-doc-comments t
  "If non-nil, comments before a structure belong to its entry.
This aims at documentation comments with special comment prefix, such as \"!>\"
recognised by doxygen or ford.
It applies to entries like subroutines, functions, types, and interfaces, but
not to variables.  If the previous sibling of the structure in the syntax tree
is a comment, it is a doc comment, even if separated from the structure by an
empty line.  Further comments before it only count if there is no empty line
between them.  Point in such a comment line is then synced to the entry of the
structure, and narrowing the source buffer to the entry also includes the doc
comments.  Marking with and without doc comments is always available.
Toggle interactively with `f90-ts-toc-buffer-toggle-doc-comments'."
  :type 'boolean
  :safe #'booleanp
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-abbreviate-labels t
  "If non-nil, use the short labels of `f90-ts-toc-short-labels'.
This reduces the width needed by the toc buffer.
Toggle interactively with `f90-ts-toc-buffer-toggle-abbreviations'."
  :type 'boolean
  :safe #'booleanp
  :group 'f90-ts-toc)


(defcustom f90-ts-toc-short-labels
  '(("program"                 . "prog")
    ("module"                  . "mod")
    ("submodule"               . "smod")
    ("subroutine"              . "sub")
    ("function"                . "fun")
    ("module_procedure"        . "proc")
    ("procedure_statement"     . "proc")
    ("derived_type_definition" . "type")
    ("interface"               . "ifc")
    ("variable_declaration"    . "var"))
  "Alist mapping entry kinds to short labels in the toc buffer.
Entries of a kind without an element here use their long label.
Only used if `f90-ts-toc-abbreviate-labels' is non-nil."
  :type '(alist :key-type string :value-type string)
  :safe (lambda (x)
          (and (listp x)
               (cl-every (lambda (c)
                           (and (consp c) (stringp (car c)) (stringp (cdr c))))
                         x)))
  :group 'f90-ts-toc)


;;;-----------------------------------------------------------------------------

(defconst f90-ts--toc-faces
  '(("program"                  :face f90-ts-toc-module-face)
    ("module"                   :face f90-ts-toc-module-face)
    ("submodule"                :face f90-ts-toc-module-face)
    ("subroutine"               :face f90-ts-toc-procedure-face)
    ("function"                 :face f90-ts-toc-procedure-face)
    ("module_procedure"         :face f90-ts-toc-procedure-face)
    ("derived_type_definition"  :face f90-ts-toc-type-face)
    ("interface"                :face f90-ts-toc-procedure-face)
    ("procedure_statement"      :face f90-ts-toc-procedure-face)
    ("variable_declaration"     :face f90-ts-toc-variable-face))
  "Face lookup by type key.
This complements `f90-ts--toc-queries'.")


(defconst f90-ts--toc-extra-queries
  '(("procedure_statement"
     :label   "procedure"
     :capture name_iface_proc
     :parent  ("interface")
     :query   "(procedure_statement (procedure_kind) declarator: (method_name) @name_iface_proc)"
     :leaf    t))
  "Query specifications for the toc buffer in addition to `f90-ts--nav-queries'.
Format as in `f90-ts--nav-queries'.  The additional property :parent is a
list of node types.  If present, an entry is only created if the parent of its
struct node is of one of these types.  For example, `procedure_statement' also
occurs for type-bound procedures, which are not listed.")


(defconst f90-ts--toc-anonymous-kinds '("interface")
  "Kinds whose structure is listed even if no name was captured.
This applies to interface blocks without a generic name.")


(defun f90-ts--toc-queries ()
  "Return `f90-ts--nav-queries' followed by `f90-ts--toc-extra-queries'."
  (append f90-ts--nav-queries f90-ts--toc-extra-queries))


(defun f90-ts--toc-capture-keys ()
  "Return an alist mapping capture symbols to keys for all toc queries."
  (cl-loop
   for (key . plist) in (f90-ts--toc-queries)
   collect (cons (plist-get plist :capture) key)))


(defun f90-ts--toc-entry-label (key name)
  "Return the display label string for KEY and NAME.
KEY is a symbol from `f90-ts--toc-queries'.  NAME is the identifier or name
of the entry.
The base label is looked up via :label in the query plist.  If NAME is
non-nil and non-empty it is appended as \"LABEL: NAME\"."
  (let* ((plist (alist-get key (f90-ts--toc-queries) nil nil #'string=))
         (label-base (plist-get plist :label)))
    (if (and name (not (string-empty-p name)))
        (format "%s: %s" label-base name)
      label-base)))


(defun f90-ts--toc-entry-face (key)
  "Return the face for KEY stored in `f90-ts--toc-faces'.
If no :face property is present, return `f90-ts-toc-variable-face' as default."
  (or (plist-get (alist-get key f90-ts--toc-faces nil nil #'string=) :face)
      'f90-ts-toc-variable-face))


(defun f90-ts--toc-entry-short-label (key name)
  "Return the label for KEY and NAME as used in the toc buffer.
This is the short label from `f90-ts-toc-short-labels' if
`f90-ts-toc-abbreviate-labels' is non-nil and KEY has one, otherwise the same
as `f90-ts--toc-entry-label'."
  (let ((short (and f90-ts-toc-abbreviate-labels
                    (alist-get key f90-ts-toc-short-labels nil nil #'string=))))
    (cond
     ((null short)
      (f90-ts--toc-entry-label key name))
     ((and name
           (not (string-empty-p name)))
      (format "%s: %s" short name))
     (t short))))


;;;-----------------------------------------------------------------------------
;;; TOC tree builder (for use with the toc buffer)
;;; (this mirrors imenu, but it is recursively constructed)

(defvar-local f90-ts--toc-tree-cache-root nil
  "The treesit root node at the time of the last toc tree build.
Used to verify via `treesit-node-eq' whether the cache is still valid.")


(defvar-local f90-ts--toc-tree-cache nil
  "The last generated sparse toc tree.")


(defvar f90-ts--toc-tree-query-compiled nil
  "Pre-compiled tree-sitter query for menu generation with struct captures.
Compiled on first use by `f90-ts--toc-tree-query'.")


(defun f90-ts--toc-tree-query ()
  "Return the compiled query for menu generation, compile it if necessary."
  (or f90-ts--toc-tree-query-compiled
      (setq f90-ts--toc-tree-query-compiled
            (treesit-query-compile
             'fortran
             (mapconcat
              (lambda (entry)
                (let ((query (plist-get (cdr entry) :query)))
                  ;; wrap each original query with @struct on the outer form
                  ;; Example:
                  ;;   (subroutine (...) @name_subroutine)
                  ;; becomes:
                  ;;   ((subroutine (...) @name_subroutine) @struct)
                  (concat "(" query " @struct)")))
              (f90-ts--toc-queries)
              "\n")))))


(defun f90-ts--toc-tree-combine-captures (captures)
  "Pair consecutive (struct name struct name ...) in CAPTURES.
Return a list of triples (name-type struct-node name-node) where name-type
is capture symbol of name and nodes are captured node of struct and name.

In general the captures have exactly one name for a struct.  But in some cases,
it might be possible that there is zero, one or more name captures in the list.
Return a triple for each (struct name_i) pair with name_i being one of the name
nodes after struct.  For a struct without name, return the triple
\(nil struct-node nil), see `f90-ts--toc-anonymous-kinds'."
  (let ((groups (mapcar #'nreverse
                        (nreverse (cl-reduce
                                   (lambda (acc cap)
                                     (if (eq (car cap) 'struct)
                                         (push (list cap) acc) ; build new struct sublist
                                       (setcar acc (cons cap (car acc)))
                                       acc))
                                   captures
                                   :initial-value nil)))))

    (cl-mapcan (lambda (group)
                 (let ((struct-node (cdar group))
                       (names (cdr group)))
                   (if names
                       (cl-loop for (cap-sym . name-node) in names
                                collect (list cap-sym struct-node name-node))
                     (list (list nil struct-node nil)))))
               groups)))


(defun f90-ts--toc-tree-node-hash-fn (node)
  "Hash function for NODE using its start position."
  (treesit-node-start node))


(define-hash-table-test 'f90-ts--toc-tree-node-hash-test
  #'treesit-node-eq
  #'f90-ts--toc-tree-node-hash-fn)


(defun f90-ts--toc-contains-pos (struct-node node-table)
  "Return the position of the keyword separating procedures in STRUCT-NODE.
This is the start of the \"contains\" statement of STRUCT-NODE, or nil.
The statement is either a child of STRUCT-NODE or a child of a wrapper node
which is a child of STRUCT-NODE (like the list of internal procedures).
Children which are entries in NODE-TABLE themselves are not searched, so that a
nested procedure or type does not contribute its own `contains'."
  (cl-flet ((contains-p (n)
              (f90-ts--node-type-p n "contains_statement")))
    (cl-loop
     for child in (treesit-node-children struct-node)
     thereis (cond
              ((contains-p child)
               (treesit-node-start child))
              ((gethash child node-table)
               nil)
              (t
               (cl-loop for grandchild in (treesit-node-children child)
                        when (contains-p grandchild)
                        return (treesit-node-start grandchild)))))))


(defun f90-ts--toc-anonymous-name (struct-node)
  "Return a name for STRUCT-NODE, which has no captured name, or nil.
Currently, this is \"abstract\" for an abstract interface."
  (let ((statement (treesit-node-child struct-node 0)))
    (and statement
         (cl-some (lambda (child)
                    (f90-ts--node-type-p child "abstract_specifier"))
                  (treesit-node-children statement))
         "abstract")))


(defun f90-ts--toc-entity-key (cap-sym struct-node capture-keys)
  "Return the query key for an entity, or nil if it should not be listed.
CAP-SYM is the capture symbol of the name, or nil if the struct has no name.
STRUCT-NODE is the struct node.  CAPTURE-KEYS maps capture symbols to keys.
A struct without name is only listed for a type in
`f90-ts--toc-anonymous-kinds'.  The :parent property of the query is taken
into account."
  (let ((key (if cap-sym
                 (alist-get cap-sym capture-keys)
               (car (member (treesit-node-type struct-node)
                            f90-ts--toc-anonymous-kinds)))))
    (when cap-sym
      (cl-assert key nil "key is nil for capture-symbol = %s, struct = %s"
                 cap-sym struct-node))
    (and key
         (f90-ts--node-type-p
          (treesit-node-parent struct-node)
          (plist-get (alist-get key (f90-ts--toc-queries) nil nil #'string=)
                     :parent))
         key)))


(defun f90-ts--toc-prev-sibling (node)
  "Return the previous sibling of NODE, or nil.
If NODE is the first child of a parent which starts at the same position, use
the previous sibling of the parent instead.  Tree-sitter may attach comments
before the first entry of a wrapper node (like a list of procedures) to the
wrapper's parent."
  (cl-loop
   for cur = node then parent
   for parent = (treesit-node-parent cur)
   for prev = (treesit-node-prev-sibling cur)
   when prev
   return prev
   while (and parent
              (= (treesit-node-start parent)
                 (treesit-node-start cur)))))


(defun f90-ts--toc-comment-starts-line-p (node)
  "Return non-nil if comment NODE is the first thing on its line.
This excludes trailing comments of a statement."
  (let ((pos (treesit-node-start node)))
    (= pos (f90-ts--pos-first-nonspace pos))))


(defun f90-ts--toc-comments-adjacent-p (first second)
  "Return non-nil if comment nodes FIRST and SECOND are on adjacent lines.
FIRST is the earlier node.  There must be no empty line in between."
  (save-excursion
    (goto-char (treesit-node-end first))
    (skip-chars-backward " \t\n")
    (forward-line 1)
    (= (point)
       (progn (goto-char (treesit-node-start second))
              (line-beginning-position)))))


(defun f90-ts--toc-doc-start (node)
  "Return the start of the doc comments before NODE, or nil.
The doc comments are determined by the previous siblings of NODE, see
`f90-ts--toc-prev-sibling'.  If the previous sibling is a comment, it is a doc
comment, even if there is an empty line between it and NODE.  Further comments
before it are doc comments as well, but only if there is no empty line between
any two of them.  Trailing comments of a statement are not doc comments.
Return the start of the first (topmost) doc comment.  The restriction of the
buffer is ignored."
  (save-excursion
    (save-restriction
      (widen)
      (let (doc last)
        (cl-loop
         for cur = (f90-ts--toc-prev-sibling node)
         then (f90-ts--toc-prev-sibling cur)
         while (and cur
                    (f90-ts--node-type-p cur "comment")
                    (f90-ts--toc-comment-starts-line-p cur)
                    (or (null last)
                        (f90-ts--toc-comments-adjacent-p cur last)))
         do (setq doc  (treesit-node-start cur)
                  last cur))
        doc))))


(defun f90-ts--toc-tree-node-table (root)
  "Capture all menu nodes under ROOT in one pass using name-struct pairing.
Return a hash table mapping each struct node to a list of entries
\(KEY NAME POS CONTAINS-POS SPAN DOC-BEG), where KEY is a type name from
`f90-ts--toc-queries' or `f90-ts--toc-extra-queries', CONTAINS-POS is the
position of the `contains' keyword of the struct, or nil if there is none,
SPAN is the trimmed span (BEG . END) of the struct node, and DOC-BEG is the
start of the doc comments before the struct, see `f90-ts--toc-doc-start', or
nil.  Only structures have DOC-BEG, not the kinds in `f90-ts--toc-line-kinds'."
  (let* ((captures (treesit-query-capture root (f90-ts--toc-tree-query)))
         (entities (f90-ts--toc-tree-combine-captures captures))
         (capture-keys (f90-ts--toc-capture-keys))
         (table (make-hash-table :test 'f90-ts--toc-tree-node-hash-test)))
    (cl-loop
     for (cap-sym struct-node name-node) in entities
     for key = (f90-ts--toc-entity-key cap-sym struct-node capture-keys)
     when key
     do (cl-pushnew
         (list key
               (if name-node
                   (treesit-node-text name-node t)
                 (f90-ts--toc-anonymous-name struct-node))
               (treesit-node-start (or name-node struct-node))
               nil
               (save-restriction
                 (widen)
                 (f90-ts--node-span-trimmed struct-node))
               (unless (f90-ts--toc-line-kind-p key)
                 (f90-ts--toc-doc-start struct-node)))
         (gethash struct-node table)
         :test #'equal))
    (maphash (lambda (struct-node entries)
               (let ((contains (f90-ts--toc-contains-pos struct-node table)))
                 (puthash struct-node
                          (mapcar (lambda (entry)
                                    (if contains
                                        (let ((entry (copy-sequence entry)))
                                          (setf (nth 3 entry) contains)
                                          entry)
                                      entry))
                                  (nreverse entries))
                          table)))
             table)
    table))


(defun f90-ts--toc-tree-predicate (node-table)
  "Return a predicate that accepts nodes present in NODE-TABLE."
  (lambda (node)
    (gethash node node-table)))


(defun f90-ts--toc-tree-process-fn (node-table)
  "Return a PROCESS-FN for `treesit-induce-sparse-node' for the toc tree.
The returned process function replaces a raw node with its entry list
from NODE-TABLE."
  (lambda (node)
    (gethash node node-table)))


(defun f90-ts--toc-tree-build ()
  "Build the sparse toc tree for the current buffer.
If the cached tree is still valid, return this.
Otherwise build the tree and cache it along with its root node in
`f90-ts--toc-tree-cache' and `f90-ts--toc-tree-cache-root'.

A sparse node in the tree is a pair (ENTRIES . CHILDREN),
where ENTRIES is a list of entries (KEY NAME POS CONTAINS-POS SPAN DOC-BEG)
as produced by
`f90-ts--toc-tree-node-table'.  CHILDREN is a list of sparse nodes
which are children of the sparse node.
If a sparse node has children, then ENTRIES should contain at most one triple.
For example, the root node usually is not interesting itself but has a list of
children, so ENTRIES=nil but CHILDREN is non-empty."
  ;; always build the tree for the whole buffer, also if it is narrowed.  The
  ;; parser follows the restriction, and widening while nodes of the narrowed
  ;; tree are in use makes them outdated
  (save-restriction
    (widen)
    (let ((root (treesit-buffer-root-node)))
      (if (and f90-ts--toc-tree-cache-root
               f90-ts--toc-tree-cache
               (treesit-node-eq root f90-ts--toc-tree-cache-root)
               (not (treesit-node-check f90-ts--toc-tree-cache-root 'outdated)))
          f90-ts--toc-tree-cache
        (let* ((node-table  (f90-ts--toc-tree-node-table root))
               (sparse-tree (treesit-induce-sparse-tree
                             root
                             (f90-ts--toc-tree-predicate node-table)
                             (f90-ts--toc-tree-process-fn node-table))))
          (setq f90-ts--toc-tree-cache-root root)
          (setq f90-ts--toc-tree-cache      sparse-tree)
          sparse-tree)))))


;;;-----------------------------------------------------------------------------
;;; toc buffer: based on toc tree

;; There is exactly one toc buffer and one global idle timer.
;; The timer follows the selected window: whenever Emacs is idle and the
;; selected window shows an f90-ts buffer, the toc buffer is switched to that
;; buffer, refreshed if the buffer was modified and synced to point.
;; Polling is used to discover changes or point movement in the source buffer.
;; The timer is cancelled when the toc buffer is killed.

(defvar f90-ts--toc-buffer-name "*F90-TS TOC*"
  "Buffer name for the f90-ts toc buffer.")


(defvar-local f90-ts--toc-buffer-source nil
  "The Fortran source buffer this toc panel was spawned from.")


(defvar-local f90-ts--toc-buffer-source-tick nil
  "`buffer-chars-modified-tick' of the source buffer at the last render.")


(defvar-local f90-ts--toc-buffer-restriction nil
  "Restriction (BEG . END) of the source buffer used for the last confinement.
Nil if the source buffer was not narrowed.")


(defvar-local f90-ts--toc-buffer-confine nil
  "Positions (BEG . END) of the toc buffer lines inside the narrowed region.
These are the lines of the entries inside the narrowed region of the source
buffer, the other lines are dimmed.  Nil if the source buffer is not narrowed.
See `f90-ts--toc-buffer-apply-confine' and `f90-ts--toc-buffer-walk'.")


(defvar-local f90-ts--toc-buffer-last-sync-pos nil
  "Position in the source buffer at the last automatic sync.
This is used to avoid excessive syncing.")


(defvar f90-ts--toc-timer nil
  "The idle timer driving switch, refresh and sync of the toc buffer.
It only exists while the toc buffer is live, see `f90-ts--toc-start'
and `f90-ts--toc-stop'.")


(defvar f90-ts-toc-mode-menu)


(defvar-keymap f90-ts-toc-mode-map
  :doc "Keymap for `f90-ts-toc-mode'."
  "RET"              #'f90-ts-toc-buffer-jump
  "SPC"              #'f90-ts-toc-buffer-preview
  "n"                #'next-line
  "p"                #'previous-line
  "g"                #'f90-ts-toc-buffer-refresh
  "m"                #'f90-ts-toc-buffer-mark-node
  "M"                #'f90-ts-toc-buffer-mark-node-with-doc
  "c"                #'f90-ts-toc-buffer-comment-node
  "C"                #'f90-ts-toc-buffer-comment-node-custom
  "w"                #'f90-ts-toc-buffer-narrow-node
  "v"                #'f90-ts-toc-buffer-toggle-variables
  "i"                #'f90-ts-toc-buffer-toggle-interface-procedures
  "d"                #'f90-ts-toc-buffer-toggle-doc-comments
  "a"                #'f90-ts-toc-buffer-toggle-abbreviations
  "s"                #'f90-ts-toc-buffer-toggle-side
  "t"                #'f90-ts-toc-buffer-cycle-shortening
  "q"                #'f90-ts-toc-buffer-quit
  "?"                #'f90-ts-toc-transient
  "C-g"              #'f90-ts-toc-buffer-quit
  "<mouse-1>"        #'f90-ts-toc-buffer-mouse-preview
  "<double-mouse-1>" #'f90-ts-toc-buffer-mouse-jump
  "<mouse-2>"        #'f90-ts-toc-buffer-mouse-jump
  "<mouse-3>"        #'f90-ts-toc-buffer-context-menu)


;;;-----------------------------------------------------------------------------
;;; toc buffer: map sparse tree to toc buffer tree

(cl-defstruct f90-ts-toc-buffer-node
  "A node in the toc buffer tree.
KIND is the type key (\"subroutine\", \"module\", etc.), which serves as a key.
LABEL is the display string, HELP the (long) tooltip string.
MARKER points into the source buffer.
SPAN is the trimmed span (BEG . END) of the node in the source buffer, or nil.
It is valid as long as the source buffer is not modified.
DOC-BEG is the start of the doc comments before the node, or nil.
NAME is the name part of LABEL, which may be shortened, or nil.
CHILDREN is a (possibly empty) list of child nodes."
  kind label marker face children help name span doc-beg)


(defun f90-ts--toc-kind-hidden-p (kind)
  "Return non-nil if entries of KIND are hidden by the display options."
  (and kind
       (or (and (not f90-ts-toc-show-variables)
                (string= kind "variable_declaration"))
           (and (not f90-ts-toc-show-interface-procedures)
                (cl-member kind '("interface" "procedure_statement")
                       :test #'string=)))))


(defconst f90-ts--toc-line-kinds '("variable_declaration" "procedure_statement")
  "Kinds of leaf entries which are only associated with point on their own line.
Several of them may be on the same line.")


(defun f90-ts--toc-line-kind-p (kind)
  "Return non-nil if KIND is in `f90-ts--toc-line-kinds'."
  (and kind
       (cl-member kind f90-ts--toc-line-kinds :test #'string=)
       t))


(defun f90-ts--toc-buffer-node-from-entry (entry &optional children)
  "Build a `f90-ts-toc-buffer-node' from a (KEY NAME POS) ENTRY and CHILDREN.
The marker is created in the current buffer, which must be the source buffer."
  (let* ((key  (nth 0 entry))
         (name (nth 1 entry))
         (pos  (nth 2 entry)))
    (make-f90-ts-toc-buffer-node
     :kind     key
     :label    (f90-ts--toc-entry-short-label key name)
     :name     name
     :help     (f90-ts--toc-entry-label key name)
     :marker   (set-marker (make-marker) pos)
     :span     (nth 4 entry)
     :doc-beg  (nth 5 entry)
     :face     (f90-ts--toc-entry-face key)
     :children (or children '()))))


(defun f90-ts--toc-buffer-add-contains (entry child-nodes)
  "Return CHILD-NODES of ENTRY, with a separator inserted at the keyword.
The separator is placed before the first child located after the `contains'
keyword of ENTRY.  Without `contains', without a child after it, or if
`f90-ts-toc-contains-label' is nil, return CHILD-NODES unchanged.
The marker is created in the current buffer, which must be the source buffer."
  (let* ((contains (nth 3 entry))
         (index    (and contains f90-ts-toc-contains-label
                        (cl-position-if
                         (lambda (node)
                           (> (marker-position (f90-ts-toc-buffer-node-marker node))
                              contains))
                         child-nodes))))
    (if (null index)
        child-nodes
      (append (cl-subseq child-nodes 0 index)
              (list (make-f90-ts-toc-buffer-node
                     :kind     "contains"
                     :label    f90-ts-toc-contains-label
                     :help     "contains"
                     :marker   (set-marker (make-marker) contains)
                     :face     'f90-ts-toc-separator-face
                     :children '()))
              (nthcdr index child-nodes)))))


(defun f90-ts--toc-buffer-from-sparse-tree (sparse-node)
  "Convert SPARSE-NODE into a list of toc buffer nodes.
SPARSE-NODE is from `f90-ts--toc-tree-build' and originates in
`f90-ts--toc-tree-node-table'.  It is a pair (ENTRIES . CHILDREN),
where each ENTRY in the list of ENTRIES has the form
\(KEY NAME POS CONTAINS-POS SPAN DOC-BEG).
CHILDREN are further sparse nodes of the same shape.

Return a flat list of `f90-ts-toc-buffer-node' structs."
  (let* ((entries  (car sparse-node))
         (children (cdr sparse-node)))
    (cond
     ;; root sentinel: no entries, just walk the forest
     ((null entries)
      (cl-mapcan #'f90-ts--toc-buffer-from-sparse-tree children))

     ;; leaf node: one toc buffer node per entry, no children
     ((null children)
      (mapcar #'f90-ts--toc-buffer-node-from-entry entries))

     ;; branch: build the parent node(s) with converted children
     (t
      (let ((child-nodes (cl-mapcan #'f90-ts--toc-buffer-from-sparse-tree children)))
        ;; there should only be exactly one entry for a non-leaf node,
        ;; be defensive: only the first gets the children
        (cons (f90-ts--toc-buffer-node-from-entry
               (car entries)
               (f90-ts--toc-buffer-add-contains (car entries) child-nodes))
              (mapcar #'f90-ts--toc-buffer-node-from-entry (cdr entries))))))))


;;;-----------------------------------------------------------------------------
;;; toc buffer: rendering

(defvar f90-ts--toc-render-width nil
  "Width available for lines while rendering the toc buffer, or nil.")


(defvar-local f90-ts--toc-buffer-rendered-width nil
  "Width available for lines at the last render of the toc buffer.")


(defun f90-ts--toc-tail-to-width (string width)
  "Return the longest tail of STRING with display width at most WIDTH."
  (cl-loop for start from 0
           when (<= (string-width (substring string start)) width)
           return (substring string start)))


(defun f90-ts--toc-shorten-name (name width)
  "Return NAME shortened to display width WIDTH.
Shorten from the side given by `f90-ts-toc-name-shortening' and mark the cut
with `f90-ts-toc-ellipsis'.  Return NAME unchanged if it fits, if shortening
is `truncate', or if WIDTH is too small to show anything besides the marker."
  (let* ((ellipsis (or f90-ts-toc-ellipsis ""))
         (avail    (- width (string-width ellipsis))))
    (cond
     ((eq f90-ts-toc-name-shortening 'truncate) name)
     ((<= (string-width name) width)            name)
     ((< avail 1)                               name)
     ((eq f90-ts-toc-name-shortening 'left)
      (concat ellipsis (f90-ts--toc-tail-to-width name avail)))
     (t
      (concat (truncate-string-to-width name avail) ellipsis)))))


(defun f90-ts--toc-fit-label (label name width)
  "Return LABEL with its trailing NAME shortened to fit into WIDTH columns.
The prefix of LABEL in front of NAME is kept.  Return LABEL unchanged if WIDTH
or NAME is nil, or if LABEL does not end with NAME."
  (if (and width name
           (not (string-empty-p name))
           (string-suffix-p name label))
      (let ((prefix (string-remove-suffix name label)))
        (concat prefix
                (f90-ts--toc-shorten-name name
                                          (- width (string-width prefix)))))
    label))


(defun f90-ts--toc-buffer-text-width (win)
  "Return the number of columns available for lines in window WIN.
If WIN is nil use `f90-ts-toc-buffer-width'."
  (if win
      (window-body-width win)
    f90-ts-toc-buffer-width))


(defun f90-ts--toc-width-stale-p (toc-buf win)
  "Return non-nil if TOC-BUF must be rendered again to fit window WIN.
That is the case if names are shortened and the window width has changed
since the last render."
  (and (not (eq f90-ts-toc-name-shortening 'truncate))
       (not (eql (window-body-width win)
                 (buffer-local-value 'f90-ts--toc-buffer-rendered-width
                                     toc-buf)))))


(defun f90-ts--toc-buffer-render-nodes (nodes depth)
  "Insert NODES at DEPTH into the toc buffer and recurse into children.
Variable entries are skipped unless `f90-ts-toc-show-variables' is non-nil,
and interface blocks (generic, general, abstract) including their children
unless `f90-ts-toc-show-interface-procedures' is non-nil."
  (cl-loop
   for node in nodes
   for kind = (f90-ts-toc-buffer-node-kind node)
   unless (f90-ts--toc-kind-hidden-p kind)
   do (progn
        (insert
         (propertize
          (concat (make-string (* depth 2) ?\s)
                  (propertize
                   (f90-ts--toc-fit-label
                    (f90-ts-toc-buffer-node-label node)
                    (f90-ts-toc-buffer-node-name node)
                    (and f90-ts--toc-render-width
                         (- f90-ts--toc-render-width (* depth 2))))
                   'face (f90-ts-toc-buffer-node-face node)))
          'f90-ts-toc-marker (f90-ts-toc-buffer-node-marker node)
          'f90-ts-toc-kind   kind
          'f90-ts-toc-depth  depth
          'f90-ts-toc-span   (f90-ts-toc-buffer-node-span node)
          'f90-ts-toc-doc    (f90-ts-toc-buffer-node-doc-beg node)
          'mouse-face        'highlight
          'help-echo         (f90-ts-toc-buffer-node-help node))
         "\n")
        (f90-ts--toc-buffer-render-nodes
         (f90-ts-toc-buffer-node-children node) (1+ depth)))))


(defun f90-ts--toc-buffer-render (tree-nodes &optional keep-view)
  "Render TREE-NODES into the current toc buffer.
If KEEP-VIEW is non-nil, restore the line of point and the first visible line
afterwards, so that re-rendering the same source does not scroll the panel."
  (let* ((inhibit-read-only t)
         (win        (get-buffer-window (current-buffer) 'visible))
         (point-line (and keep-view
                          (line-number-at-pos (if win (window-point win) (point)))))
         (start-line (and keep-view win
                          (line-number-at-pos (window-start win))))
         (f90-ts--toc-render-width (f90-ts--toc-buffer-text-width win)))
    (setq f90-ts--toc-buffer-rendered-width f90-ts--toc-render-width)
    (erase-buffer)
    (f90-ts--toc-buffer-render-nodes tree-nodes 0)
    (goto-char (point-min))
    (when point-line
      (forward-line (1- point-line)))
    (when win
      (set-window-point win (point))
      (when start-line
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (set-window-start win (point)))))))


(defun f90-ts--toc-source-restriction (src)
  "Return the restriction (BEG . END) of the buffer SRC, or nil if not narrowed."
  (and (buffer-live-p src)
       (with-current-buffer src
         (and (buffer-narrowed-p)
              (cons (point-min) (point-max))))))


(defun f90-ts--toc-buffer-confine-bounds (restriction)
  "Return (BEG . END) of the toc buffer lines with entries inside RESTRICTION.
RESTRICTION is a pair of positions in the source buffer.  BEG is the start of
the first such line, END the end of the last one.
If there are no such lines, return nil."
  (pcase-let ((`(,lo . ,hi) restriction))
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       for pos = (point) then (progn
                                (forward-line 1)
                                (point))
       for marker = (get-text-property pos 'f90-ts-toc-marker)
       for mpos   = (and marker (marker-position marker))
       while (or (not (eobp))
                 (= pos (point)))
       when (and mpos
                 (<= lo mpos hi))
       minimize pos into beg
       and maximize (line-end-position) into end
       finally return (when beg
                        (cons beg end))))))


(defun f90-ts--toc-buffer-clamp (pos)
  "Return POS moved into `f90-ts--toc-buffer-confine', if there is one."
  (if f90-ts--toc-buffer-confine
      (max (car f90-ts--toc-buffer-confine)
           (min pos (cdr f90-ts--toc-buffer-confine)))
    pos))


(defun f90-ts--toc-buffer-update-dimming ()
  "Dim the lines of the current toc buffer outside `f90-ts--toc-buffer-confine'.
Remove all dimming if there is no confinement."
  (remove-overlays (point-min) (point-max) 'f90-ts-toc-dim t)
  (when f90-ts--toc-buffer-confine
    (cl-loop
     for (beg . end) in (list (cons (point-min)
                                    (car f90-ts--toc-buffer-confine))
                              (cons (cdr f90-ts--toc-buffer-confine)
                                    (point-max)))
     when (< beg end)
     do (let ((ov (make-overlay beg end)))
          (overlay-put ov 'face 'f90-ts-toc-dimmed-face)
          (overlay-put ov 'f90-ts-toc-dim t)))))


(defun f90-ts--toc-buffer-apply-confine ()
  "Dim the lines outside of the narrowed source region and keep point inside.
Must be called with the toc buffer as current buffer.  If the source buffer is
narrowed, the lines of the entries inside the narrowed region (for example a
module, its variables, subroutines and types) are the confined ones, see
`f90-ts--toc-buffer-confine'.  All lines stay visible, but the other lines are
dimmed, and point is moved onto a confined line if it is not on one.  Moving
point out of the confined lines afterwards is walking, see
`f90-ts--toc-buffer-walk'.  Nothing is confined if the source buffer is not
narrowed, or if the narrowed region contains no entry."
  (let* ((restriction (f90-ts--toc-source-restriction f90-ts--toc-buffer-source))
         (win         (get-buffer-window (current-buffer) 'visible)))
    (setq f90-ts--toc-buffer-restriction restriction
          f90-ts--toc-buffer-confine
          (and restriction (f90-ts--toc-buffer-confine-bounds restriction)))
    (f90-ts--toc-buffer-update-dimming)
    (when f90-ts--toc-buffer-confine
      (goto-char (f90-ts--toc-buffer-clamp (point)))
      (when win
        (set-window-point win (f90-ts--toc-buffer-clamp (window-point win)))))))


(defun f90-ts--toc-buffer-clear ()
  "Empty the toc buffer, which must be current, after its source was killed."
  (cl-assert (eq (current-buffer)
                 (get-buffer f90-ts--toc-buffer-name))
             t "current buffer must be the f90-ts toc buffer")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq f90-ts--toc-buffer-source         nil
        f90-ts--toc-buffer-source-tick    nil
        f90-ts--toc-buffer-restriction    nil
        f90-ts--toc-buffer-confine        nil
        f90-ts--toc-buffer-last-sync-pos  nil)
  (f90-ts--toc-buffer-update-dimming))


(defun f90-ts--toc-buffer-update (src)
  "Rebuild the toc buffer from the f90-ts source buffer SRC and return it.
This only fills the buffer.  It neither displays it nor moves point."
  (let* ((tree-nodes (with-current-buffer src
                       (save-restriction
                         (widen)
                         (f90-ts--toc-buffer-from-sparse-tree
                          (f90-ts--toc-tree-build)))))
         (tick       (buffer-chars-modified-tick src))
         (toc-buf    (get-buffer-create f90-ts--toc-buffer-name)))
    (with-current-buffer toc-buf
      ;; only on first creation, otherwise defvar-local variables are deleted
      (unless (derived-mode-p 'f90-ts-toc-mode)
        (f90-ts-toc-mode))
      (let ((same-source (eq src f90-ts--toc-buffer-source)))
        (setq f90-ts--toc-buffer-source      src
              f90-ts--toc-buffer-source-tick tick)
        (f90-ts--toc-buffer-render tree-nodes same-source)
        (f90-ts--toc-buffer-apply-confine)))
    toc-buf))


;;;-----------------------------------------------------------------------------
;;; toc buffer: interaction

(defun f90-ts--toc-buffer-marker-at-point ()
  "Return the marker stored on the current line, or nil."
  (get-text-property (line-beginning-position) 'f90-ts-toc-marker))


(defun f90-ts--toc-buffer-entry-marker ()
  "Return the marker of the entry on the current line, or nil if there is none.
Signal a `user-error' if the source buffer was killed."
  (when-let* ((marker (f90-ts--toc-buffer-marker-at-point)))
    (unless (and (buffer-live-p (marker-buffer marker))
                 (buffer-live-p f90-ts--toc-buffer-source))
      (user-error "Source buffer no longer live"))
    marker))


(defun f90-ts--toc-buffer-source-window (buffer)
  "Return a window displaying the source BUFFER.
If BUFFER is shown in several windows, take the one selected most recently.
If BUFFER is not visible, display it in some window other than the toc panel."
  (or (car (sort (get-buffer-window-list buffer nil 'visible)
                 (lambda (w1 w2)
                   (> (window-use-time w1) (window-use-time w2)))))
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-use-some-window
                         display-buffer-pop-up-window)
                        (inhibit-same-window . t)))))


(defun f90-ts-toc-buffer-jump ()
  "Jump to the entry on the current line in the source buffer."
  (interactive)
  (when-let* ((marker (f90-ts--toc-buffer-entry-marker)))
    (select-window (f90-ts--toc-buffer-source-window (marker-buffer marker)))
    (push-mark nil t)
    (goto-char marker)
    (recenter)))


(defun f90-ts-toc-buffer-preview ()
  "Preview the entry at point without leaving the toc buffer."
  (interactive)
  (when-let* ((marker (f90-ts--toc-buffer-entry-marker)))
    (with-selected-window (f90-ts--toc-buffer-source-window (marker-buffer marker))
      (goto-char marker)
      (recenter)
      (pulse-momentary-highlight-one-line (point)))))


(defun f90-ts--toc-buffer-entry-span ()
  "Return the trimmed span (BEG . END) of the entry on the current line.
The span refers to the source buffer.  Signal a `user-error' if the line has
no node, or if the source buffer was modified since the toc buffer was rendered,
since then the span might be wrong."
  (let ((span (get-text-property (line-beginning-position) 'f90-ts-toc-span)))
    (unless (and span (f90-ts--toc-buffer-entry-marker))
      (user-error "No node on this line"))
    (unless (eql (buffer-chars-modified-tick f90-ts--toc-buffer-source)
                 f90-ts--toc-buffer-source-tick)
      (user-error "Source buffer was modified, refresh the panel first"))
    span))


(defun f90-ts--toc-buffer-entry-extent (with-doc)
  "Return the span (BEG . END) of the entry at point in the source buffer.
If WITH-DOC is non-nil and the entry has doc comments before it,
the span starts at the first of these comments.  Otherwise it is the trimmed
span of the node.  Signal a `user-error' as `f90-ts--toc-buffer-entry-span'."
  (let ((span (f90-ts--toc-buffer-entry-span))
        (doc  (get-text-property (line-beginning-position) 'f90-ts-toc-doc)))
    (if (and with-doc doc)
        (cons doc (cdr span))
      span)))


(defun f90-ts--toc-buffer-check-accessible (extent)
  "Signal a `user-error' if EXTENT = (BEG . END) is not accessible.
That is the case if the source buffer is narrowed and EXTENT is not completely
inside the restriction.  Operations on such a region would silently clamp to
the restriction and act on the wrong text."
  (with-current-buffer f90-ts--toc-buffer-source
    (unless (and (<= (point-min) (car extent))
                 (<= (cdr extent) (point-max)))
      (user-error
       "Node is outside of the narrowed region of the source buffer"))))


(defun f90-ts--toc-buffer-mark-extent (extent)
  "Mark or unmark the source buffer region EXTENT = (BEG . END).
If EXTENT is already the active region in the source buffer, deactivate it.
Otherwise mark it and select the source window, since the region is only
visible there."
  (f90-ts--toc-buffer-check-accessible extent)
  (let* ((src f90-ts--toc-buffer-source)
         (win (f90-ts--toc-buffer-source-window src)))
    (if (with-current-buffer src
          (and (use-region-p)
               (equal (f90-ts--region-trimmed) extent)))
        (with-current-buffer src
          (deactivate-mark))
      (select-window win)
      (f90-ts--mark-region (car extent) (cdr extent) f90-ts-mark-region-order))))


(defun f90-ts-toc-buffer-mark-node ()
  "Mark or unmark the node of the entry at point in the source buffer.
The region is the trimmed span of the node, without comment lines before it.
If this region is already active in the source buffer, deactivate it instead.
Marking selects the source window, since the region is only visible there.
See `f90-ts-toc-buffer-mark-node-with-doc' for the variant including comments."
  (interactive)
  (f90-ts--toc-buffer-mark-extent (f90-ts--toc-buffer-entry-extent nil)))


(defun f90-ts-toc-buffer-mark-node-with-doc ()
  "Mark or unmark the node of the entry at point including its doc comments.
This is like `f90-ts-toc-buffer-mark-node', but the region starts at the
doc comments before the node, if there are any.  This does not depend
on `f90-ts-toc-include-doc-comments'."
  (interactive)
  (f90-ts--toc-buffer-mark-extent (f90-ts--toc-buffer-entry-extent t)))


(defun f90-ts--toc-buffer-structure-line-p ()
  "Return non-nil if the toc buffer line at point show a structure.
Only structures such as modules, subroutines and types can be narrowed to.
Variables, interface procedures and the separator before the contained
procedures are no structures."
  (and (get-text-property (line-beginning-position) 'f90-ts-toc-span)
       (not (f90-ts--toc-line-kind-p
             (get-text-property (line-beginning-position) 'f90-ts-toc-kind)))))


(defun f90-ts--toc-buffer-structure-line ()
  "Return the start of the toc buffer line of the structure at point.
That is the line at point if it shows a structure, see
`f90-ts--toc-buffer-structure-line-p'.  Otherwise it is the closest line above
with a smaller depth showing a structure, which is for example the subroutine
a variable is declared in."
  (save-excursion
    (beginning-of-line)
    (if (f90-ts--toc-buffer-structure-line-p)
        (point)
      (let ((depth (get-text-property (point) 'f90-ts-toc-depth))
            (found nil))
        (while (and depth
                    (not found)
                    (zerop (forward-line -1)))
          (let ((above (get-text-property (point) 'f90-ts-toc-depth)))
            (when (and above (< above depth)
                       (f90-ts--toc-buffer-structure-line-p))
              (setq found (point)))))
        found))))


(defun f90-ts--toc-buffer-narrow-source (beg end)
  "Widen the source buffer, narrow it to BEG..END and show it from BEG.
The source window is the one selected last, see
`f90-ts--toc-buffer-source-window'."
  (let ((src f90-ts--toc-buffer-source))
    (with-selected-window (f90-ts--toc-buffer-source-window src)
      (widen)
      (narrow-to-region beg end)
      (goto-char beg)
      (recenter 0))
    ;; A window which is not selected is not necessarily redisplayed
    ;; after the restriction changed.
    (force-window-update src)))


(defun f90-ts--toc-buffer-walk ()
  "Narrow the source buffer to the structure of the line at point, if necessary.
This lets point walk through the panel while the source buffer is narrowed.
Nothing happens if the source buffer is not narrowed or if the line is inside
the narrowed region, that is, not dimmed.  Otherwise the source buffer is
widened and narrowed to the structure of the line, see
`f90-ts--toc-buffer-structure-line', and the dimming follows.  Point stays on
its line.  This is a `post-command-hook' function of the toc buffer, so errors
are reported as messages."
  (when (and f90-ts--toc-buffer-confine
             (not (<= (car f90-ts--toc-buffer-confine)
                      (point)
                      (cdr f90-ts--toc-buffer-confine))))
    (condition-case err
        (when-let* ((line (f90-ts--toc-buffer-structure-line))
                    (extent (save-excursion
                              (goto-char line)
                              (f90-ts--toc-buffer-entry-extent
                               f90-ts-toc-include-doc-comments))))
          (f90-ts--toc-buffer-narrow-source (car extent) (cdr extent))
          (f90-ts--toc-buffer-apply-confine)
          ;; the source point moved, but point in the panel must stay where
          ;; it is, so the background sync has nothing to do
          (setq f90-ts--toc-buffer-last-sync-pos
                (with-current-buffer f90-ts--toc-buffer-source (point))))
      (user-error
       (message "%s" (error-message-string err))))))


(defun f90-ts-toc-buffer-narrow-node ()
  "Narrow the source buffer to the node of the entry at point, or widen it.
Narrow to the trimmed span of the node and put point at its start.  If
`f90-ts-toc-include-doc-comments' is non-nil, the doc comments before
the node are included.  If the source buffer is already narrowed to exactly
this span, widen it instead.  On a variable (or any other entry which is no
structure) use the structure it belongs to, see
`f90-ts--toc-buffer-structure-line'.  Narrowing to a structure inside the
narrowed region narrows further.  While the source buffer is narrowed, the
entries outside the narrowed region are dimmed and walking to one of them
narrows the source buffer to its structure, see `f90-ts--toc-buffer-walk'."
  (interactive)
  (let* ((line   (or (f90-ts--toc-buffer-structure-line)
                     (user-error "No node on this line")))
         (extent (save-excursion
                   (goto-char line)
                   (f90-ts--toc-buffer-entry-extent
                    f90-ts-toc-include-doc-comments)))
         (beg    (car extent))
         (end    (cdr extent))
         (src    f90-ts--toc-buffer-source))
    (if (with-current-buffer src
          (and (buffer-narrowed-p)
               (= (point-min) beg)
               (= (point-max) end)))
        (progn
          (with-current-buffer src
            (widen))
          ;; widening in a window which is not selected does not necessarily
          ;; redisplay it, the source window would look narrowed until selected
          (force-window-update src))
      (f90-ts--toc-buffer-narrow-source beg end))
    ;; dim the entries outside of the narrowed region
    (f90-ts--toc-buffer-apply-confine)
    (f90-ts--toc-buffer-sync-now)))


(defun f90-ts--toc-buffer-comment-node-with (prefix)
  "Comment or uncomment the node of the entry at point using PREFIX.
If PREFIX is nil, use `f90-ts-comment-region-default', otherwise
`f90-ts-comment-region-with-prefix' with PREFIX.  The panel keeps the focus,
and point in the source buffer does not move.  This is the common part of
`f90-ts-toc-buffer-comment-node' and `f90-ts-toc-buffer-comment-node-custom'."
  (let* ((span (f90-ts--toc-buffer-entry-span))
         (src  f90-ts--toc-buffer-source))
    (f90-ts--toc-buffer-check-accessible span)
    (with-selected-window (f90-ts--toc-buffer-source-window src)
      (barf-if-buffer-read-only)
      (save-excursion
        (if prefix
            (f90-ts-comment-region-with-prefix (car span) (cdr span) prefix)
          (f90-ts-comment-region-default (car span) (cdr span)))))))


(defun f90-ts-toc-buffer-comment-node ()
  "Comment or uncomment the node of the entry at point in the source buffer.
Use the trimmed span of the node and `f90-ts-comment-region-default', which
comments or uncomments each line of the span.  The panel keeps the focus,
and point in the source buffer does not move.
See `f90-ts-toc-buffer-comment-node-custom' to choose another prefix."
  (interactive)
  (f90-ts--toc-buffer-comment-node-with nil))


(defun f90-ts--toc-buffer-read-comment-prefix ()
  "Read a comment prefix from the candidates of `f90-ts-comment-region-custom'.
These are `f90-ts-extra-comment-prefixes' and `f90-ts-comment-region-prefix'.
Signal a `user-error' if the entry at point cannot be commented, before the
user is asked."
  (f90-ts--toc-buffer-check-accessible (f90-ts--toc-buffer-entry-span))
  (completing-read "choose comment prefix: "
                   (append f90-ts-extra-comment-prefixes
                           (list f90-ts-comment-region-prefix))
                   nil t nil nil (car f90-ts-extra-comment-prefixes)))


(defun f90-ts-toc-buffer-comment-node-custom (prefix)
  "Comment or uncomment the node of the entry at point using comment PREFIX.
Like `f90-ts-toc-buffer-comment-node', but the prefix is not the default one.
If called interactively, prompt for a prefix from
`f90-ts-extra-comment-prefixes' and `f90-ts-comment-region-prefix', as
`f90-ts-comment-region-custom' does.
The panel keeps the focus, and point in the source buffer does not move."
  (interactive (list (f90-ts--toc-buffer-read-comment-prefix)))
  (f90-ts--toc-buffer-comment-node-with prefix))


(defun f90-ts-toc-buffer-mouse-preview (event)
  "Move point to the entry clicked in EVENT and preview it."
  (interactive "e")
  (mouse-set-point event)
  (f90-ts--toc-buffer-walk)
  (f90-ts-toc-buffer-preview))


(defun f90-ts-toc-buffer-mouse-jump (event)
  "Move point to the entry clicked in EVENT and jump to it."
  (interactive "e")
  (mouse-set-point event)
  (f90-ts--toc-buffer-walk)
  (f90-ts-toc-buffer-jump))


(defun f90-ts-toc-buffer-context-menu (event)
  "Pop up the toc buffer menu at the entry clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (f90-ts--toc-buffer-walk)
  (popup-menu f90-ts-toc-mode-menu))


;;;-----------------------------------------------------------------------------
;;; toc buffer: sync of point

(defun f90-ts--toc-buffer-associated-line (pos)
  "Return the start of the toc buffer line associated with source position POS.
Must be called with the toc buffer as current buffer.  Return nil if the toc
buffer has no entries.

For non-variable entries the associated entry is the closest one whose
marker position is on or before the line of POS.  Entries of the kinds in
`f90-ts--toc-line-kinds' (variables, interface procedures) only count if they
are on the same source line as POS; among those, the entry is selected if POS
is >= the marker position, except for the first one on the line which also
captures positions before it on that line.
If `f90-ts-toc-include-doc-comments' is non-nil, the doc comments
before a structure belong to its entry, so a position there is associated with
this entry."
  (let* ((bounds (with-current-buffer f90-ts--toc-buffer-source
                   (save-excursion
                     (goto-char pos)
                     (cons (line-beginning-position) (line-end-position)))))
         (bol (car bounds))
         (eol (cdr bounds)))
    (save-excursion
      (goto-char (point-min))
      (or (cl-loop
           with found = nil
           with var-seen = nil
           for line-pos = (line-beginning-position)
           for marker = (get-text-property line-pos 'f90-ts-toc-marker)
           for mpos = (and marker (marker-position marker))
           for doc = (and f90-ts-toc-include-doc-comments
                          (get-text-property line-pos 'f90-ts-toc-doc))
           ;; doc comments are part of the entry, so it starts before its name
           for epos = (and mpos (or doc mpos))
           until (or (eobp) (and epos (> epos eol)))
           do (when mpos
                (if (not (f90-ts--toc-line-kind-p
                          (get-text-property line-pos 'f90-ts-toc-kind)))
                    (setq found line-pos)
                  (when (and (>= mpos bol)
                             (or (not var-seen) (<= mpos pos)))
                    (setq found    line-pos
                          var-seen t))))
           do (forward-line 1)
           finally return found)
          ;; point is before the first entry: use the first entry
          (and (get-text-property (point-min) 'f90-ts-toc-marker)
               (point-min))))))


(defun f90-ts--toc-buffer-sync-now (&optional pos)
  "Move point in the toc buffer to the entry associated with POS.
POS is a position in the source buffer, default is point there.
Do nothing if the toc buffer is not visible."
  (when-let* ((toc-buf (get-buffer f90-ts--toc-buffer-name))
              (toc-win (get-buffer-window toc-buf 'visible))
              (src     (buffer-local-value 'f90-ts--toc-buffer-source toc-buf))
              ((buffer-live-p src)))
    (let ((pos (or pos (with-current-buffer src (point)))))
      (with-current-buffer toc-buf
        (when-let* ((line-pos (f90-ts--toc-buffer-associated-line pos)))
          (setq line-pos (f90-ts--toc-buffer-clamp line-pos))
          (goto-char line-pos)
          (set-window-point toc-win line-pos)
          ;; only scroll if necessary, do not recenter on every sync
          (unless (pos-visible-in-window-p line-pos toc-win)
            (with-selected-window toc-win
              (recenter)))
          (hl-line-highlight))))))


(defun f90-ts--toc-buffer-rerender ()
  "Render the toc buffer again.
For example, this is invoked after a display option has changed.
Must be called with the toc buffer as current buffer.  Keep the entry
at point if it still exists."
  (when (buffer-live-p f90-ts--toc-buffer-source)
    (let* ((marker (f90-ts--toc-buffer-marker-at-point))
           (pos    (and marker (marker-position marker))))
      (f90-ts--toc-buffer-update f90-ts--toc-buffer-source)
      (f90-ts--toc-buffer-sync-now pos))))


(defun f90-ts-toc-buffer-refresh ()
  "Rebuild the toc buffer from the source buffer."
  (interactive)
  (let ((src (or f90-ts--toc-buffer-source
                 (user-error "Not linked to a F90-TS source buffer"))))
    (unless (buffer-live-p src)
      (user-error "Linked source buffer no longer live"))
    ;; invalidate the shared cache
    (with-current-buffer src
      (setq f90-ts--toc-tree-cache-root nil))
    (f90-ts--toc-buffer-rerender)))


(defun f90-ts-toc-buffer-toggle-variables ()
  "Toggle display of variable entries in the toc buffer."
  (interactive)
  (setq f90-ts-toc-show-variables (not f90-ts-toc-show-variables))
  (f90-ts--toc-buffer-rerender)
  (message "F90-TS-TOC: variables %s"
           (if f90-ts-toc-show-variables "shown" "hidden")))


(defun f90-ts-toc-buffer-toggle-interface-procedures ()
  "Toggle display of interface blocks in the toc buffer.
This applies to all interface blocks: generic, general (without a name), and
abstract ones, together with their contents."
  (interactive)
  (setq f90-ts-toc-show-interface-procedures
        (not f90-ts-toc-show-interface-procedures))
  (f90-ts--toc-buffer-rerender)
  (message "F90-TS-TOC: interfaces %s"
           (if f90-ts-toc-show-interface-procedures "shown" "hidden")))


(defun f90-ts-toc-buffer-toggle-doc-comments ()
  "Toggle whether doc comments before a structure belong to its entry.
See `f90-ts-toc-include-doc-comments'."
  (interactive)
  (setq f90-ts-toc-include-doc-comments
        (not f90-ts-toc-include-doc-comments))
  (f90-ts--toc-buffer-sync-now)
  (message "F90-TS-TOC: doc comments %s"
           (if f90-ts-toc-include-doc-comments "included" "not included")))


(defun f90-ts-toc-buffer-toggle-side ()
  "Toggle the toc panel between the left and the right side of the frame.
A visible panel is moved immediately.  If it was selected, it stays selected."
  (interactive)
  (setq f90-ts-toc-buffer-side
        (if (eq f90-ts-toc-buffer-side 'right) 'left 'right))
  (when-let* ((toc-buf (get-buffer f90-ts--toc-buffer-name))
              (win     (get-buffer-window toc-buf 'visible)))
    (let ((selected (eq win (selected-window))))
      (delete-window win)
      (let ((new-win (f90-ts--toc-buffer-show toc-buf)))
        (f90-ts--toc-buffer-sync-now)
        (when (and selected new-win)
          (select-window new-win)))))
  (message "F90-TS-TOC: panel on the %s side" f90-ts-toc-buffer-side))


(defconst f90-ts--toc-shortening-modes '(truncate left right)
  "Values of `f90-ts-toc-name-shortening' in the order of cycling.")


(defun f90-ts-toc-buffer-set-shortening (mode)
  "Set how long names are shown in the toc buffer to MODE.
MODE is one of `truncate', `left' and `right', see
`f90-ts-toc-name-shortening'."
  (interactive
   (list (intern (completing-read
                  "Shorten long names from (truncate, left, right): "
                  (mapcar #'symbol-name f90-ts--toc-shortening-modes)
                  nil t))))
  (unless (memq mode f90-ts--toc-shortening-modes)
    (user-error "Invalid mode %s" mode))
  (setq f90-ts-toc-name-shortening mode)
  (when-let* ((toc-buf (get-buffer f90-ts--toc-buffer-name)))
    (with-current-buffer toc-buf
      (f90-ts--toc-buffer-rerender)))
  (message "F90-TS-TOC: long names %s"
           (pcase mode
             ('truncate "truncated by the window")
             ('left     "shortened from the left")
             ('right    "shortened from the right"))))


(defun f90-ts-toc-buffer-cycle-shortening ()
  "Cycle how long names are shown: truncated, shortened left or right."
  (interactive)
  (f90-ts-toc-buffer-set-shortening
   (or (cadr (memq f90-ts-toc-name-shortening f90-ts--toc-shortening-modes))
       (car f90-ts--toc-shortening-modes))))


(defun f90-ts-toc-buffer-toggle-abbreviations ()
  "Toggle abbreviated labels in the toc buffer."
  (interactive)
  (setq f90-ts-toc-abbreviate-labels (not f90-ts-toc-abbreviate-labels))
  (f90-ts--toc-buffer-rerender)
  (message "F90-TS-TOC: abbreviated labels %s"
           (if f90-ts-toc-abbreviate-labels "on" "off")))


;;;-----------------------------------------------------------------------------
;;; toc buffer: transient menu

(defun f90-ts--toc-transient-state (label state)
  "Return LABEL followed by the indicator STATE, for a suffix description.
STATE is a boolean, shown as on or off, or a string shown as it is."
  (concat label " "
          (if (stringp state)
              (propertize state 'face 'transient-value)
            (propertize (if state "[on]" "[off]")
                        'face (if state
                                  'transient-value
                                'transient-inactive-value)))))


;; The keys are the ones of `f90-ts-toc-mode-map'.  Commands which only change
;; what is shown in the panel keep the menu open (:transient t), commands
;; which leave the panel or act on the source buffer close it.
(transient-define-prefix f90-ts-toc-transient ()
  "Transient menu for the F90-TS TOC side panel.
The keys are the same as in `f90-ts-toc-mode-map'."
  [["Entry"
    ("RET" "Jump to entry"    f90-ts-toc-buffer-jump)
    ("SPC" "Preview entry"    f90-ts-toc-buffer-preview :transient t)
    ("n"   "Next line"        next-line :transient t)
    ("p"   "Previous line"    previous-line :transient t)]
   ["Node"
    ("m"   "Mark/unmark"      f90-ts-toc-buffer-mark-node)
    ("M"   "Mark/unmark with doc comments"
     f90-ts-toc-buffer-mark-node-with-doc)
    ("c"   "Comment/uncomment" f90-ts-toc-buffer-comment-node)
    ("C"   "Comment/uncomment (custom prefix)"
     f90-ts-toc-buffer-comment-node-custom)
    ("w"   "Narrow/widen"     f90-ts-toc-buffer-narrow-node)]]
  [["Display"
    ("v" (lambda ()
           (f90-ts--toc-transient-state "Variables" f90-ts-toc-show-variables))
     f90-ts-toc-buffer-toggle-variables :transient t)
    ("i" (lambda ()
           (f90-ts--toc-transient-state
            "Interfaces" f90-ts-toc-show-interface-procedures))
     f90-ts-toc-buffer-toggle-interface-procedures :transient t)
    ("d" (lambda ()
           (f90-ts--toc-transient-state
            "Preceding doc comments" f90-ts-toc-include-doc-comments))
     f90-ts-toc-buffer-toggle-doc-comments :transient t)
    ("a" (lambda ()
           (f90-ts--toc-transient-state
            "Abbreviated labels" f90-ts-toc-abbreviate-labels))
     f90-ts-toc-buffer-toggle-abbreviations :transient t)
    ("t" (lambda ()
           (f90-ts--toc-transient-state
            "Shorten long names"
            (format "[%s]" f90-ts-toc-name-shortening)))
     f90-ts-toc-buffer-cycle-shortening :transient t)]
   ["Panel"
    ;; moving the panel deletes and recreates its window: close the menu
    ("s" (lambda ()
           (f90-ts--toc-transient-state
            "Side" (format "[%s]" f90-ts-toc-buffer-side)))
     f90-ts-toc-buffer-toggle-side)
    ("g"   "Refresh"          f90-ts-toc-buffer-refresh :transient t)
    ("q"   "Quit panel"       f90-ts-toc-buffer-quit)]])


;;;-----------------------------------------------------------------------------
;;; toc buffer: background timer

(defun f90-ts--toc-stop ()
  "Cancel the idle timer of the toc buffer."
  (when (timerp f90-ts--toc-timer)
    (cancel-timer f90-ts--toc-timer))
  (setq f90-ts--toc-timer nil))


(defun f90-ts--toc-start ()
  "Start or restart the idle timer of the toc buffer.
It drives switching, refreshing and syncing of the toc buffer."
  (f90-ts--toc-stop)
  (setq f90-ts--toc-timer
        (run-with-idle-timer f90-ts-toc-buffer-idle-delay t
                             #'f90-ts--toc-tick)))


(defun f90-ts--toc-update-visible (toc-buf)
  "Switch, refresh and sync the visible TOC-BUF as needed."
  (let ((buf (window-buffer (selected-window))))
    ;; the source buffer was killed: do not keep dead entries around
    (unless (buffer-live-p (buffer-local-value 'f90-ts--toc-buffer-source toc-buf))
      (with-current-buffer toc-buf
        (f90-ts--toc-buffer-clear)))
    ;; the panel was resized: shortened names must fit again
    (when-let* ((win (get-buffer-window toc-buf 'visible))
                ((f90-ts--toc-width-stale-p toc-buf win)))
      (with-current-buffer toc-buf
        (f90-ts--toc-buffer-rerender)))
    ;; Follow the selected window, but only if it shows a f90-ts buffer.
    ;; Everything else (toc panel itself, minibuffer, helm, other modes)
    ;; leaves the panel as it is.
    (when (and (not (eq buf toc-buf))
               (with-current-buffer buf (derived-mode-p 'f90-ts-mode)))
      (let* ((pos   (with-current-buffer buf (point)))
             (stale (or (not (eq buf (buffer-local-value
                                      'f90-ts--toc-buffer-source toc-buf)))
                        (not (eql (buffer-chars-modified-tick buf)
                                  (buffer-local-value
                                   'f90-ts--toc-buffer-source-tick toc-buf))))))
        (when stale
          (f90-ts--toc-buffer-update buf))
        ;; the source was narrowed or widened (either from panel or externally)
        (when (not (equal (f90-ts--toc-source-restriction buf)
                          (buffer-local-value 'f90-ts--toc-buffer-restriction
                                              toc-buf)))
          (with-current-buffer toc-buf
            (f90-ts--toc-buffer-apply-confine))
          (setq stale t))
        (when (and f90-ts-toc-buffer-auto-sync
                   (or stale
                       (not (eql pos (buffer-local-value
                                      'f90-ts--toc-buffer-last-sync-pos toc-buf)))))
          (f90-ts--toc-buffer-sync-now pos)
          (with-current-buffer toc-buf
            (setq f90-ts--toc-buffer-last-sync-pos pos)))))))


(defun f90-ts--toc-tick ()
  "Idle timer function of the toc buffer.
Nothing is done unless the toc buffer is visible, so this never pops the panel
up on its own.  The timer cancels itself once the toc buffer is gone."
  (condition-case-unless-debug err
      (let ((toc-buf (get-buffer f90-ts--toc-buffer-name)))
        (if (not (buffer-live-p toc-buf))
            (f90-ts--toc-stop)
          (when (get-buffer-window toc-buf 'visible)
            (f90-ts--toc-update-visible toc-buf))))
    (error
     (f90-ts--toc-stop)
     (message "f90-ts-toc: background update stopped: %s"
              (error-message-string err)))))


;;;-----------------------------------------------------------------------------
;;; toc buffer: mode and entry points

(define-derived-mode f90-ts-toc-mode special-mode "F90-TS-TOC"
  "Major mode for the F90-TS TOC side panel."
  (setq truncate-lines t)
  ;; keep the highlight in the panel while it is not the selected window
  (setq-local hl-line-sticky-flag t)
  (hl-line-mode 1)
  ;; no background activity without a toc buffer
  (add-hook 'kill-buffer-hook #'f90-ts--toc-stop nil t)
  ;; walking: moving point to a dimmed line narrows to its structure
  (add-hook 'post-command-hook #'f90-ts--toc-buffer-walk nil t))


(defun f90-ts--toc-buffer-show (toc-buf)
  "Display TOC-BUF in a side window unless it is visible already.
The side is given by `f90-ts-toc-buffer-side'.  Return the window."
  (or (get-buffer-window toc-buf 'visible)
      (display-buffer
       toc-buf
       `(display-buffer-in-side-window
         (side . ,f90-ts-toc-buffer-side)
         (window-width . ,f90-ts-toc-buffer-width)
         (preserve-size . (t . nil))))))


;;;###autoload
(defun f90-ts-toc-buffer-open ()
  "Open (or refresh) the F90-TS TOC side panel for the current buffer."
  (interactive)
  (unless (derived-mode-p 'f90-ts-mode)
    (user-error "Not a F90-TS source buffer, toc buffer not available"))
  (let ((toc-buf (f90-ts--toc-buffer-update (current-buffer))))
    (when-let* ((win (f90-ts--toc-buffer-show toc-buf))
                ((f90-ts--toc-width-stale-p toc-buf win)))
      ;; the window width is only known now
      (with-current-buffer toc-buf
        (f90-ts--toc-buffer-rerender)))
    (f90-ts--toc-start)
    (f90-ts--toc-buffer-sync-now)))


;;;###autoload
(defun f90-ts-toc-buffer-focus ()
  "Focus the TOC side panel, opening it first if necessary."
  (interactive)
  (unless (get-buffer-window f90-ts--toc-buffer-name 'visible)
    (f90-ts-toc-buffer-open))
  (when-let* ((toc-win (get-buffer-window f90-ts--toc-buffer-name 'visible)))
    (select-window toc-win)))


(defun f90-ts-toc-buffer-quit ()
  "Close the toc panel, kill its buffer and stop all background activity."
  (interactive)
  (when-let* ((toc-buf (get-buffer f90-ts--toc-buffer-name)))
    (if-let* ((win (get-buffer-window toc-buf 'visible)))
        (quit-window t win)
      (kill-buffer toc-buf)))
  (f90-ts--toc-stop))


(easy-menu-define f90-ts-toc-mode-menu f90-ts-toc-mode-map
  "Menu for the F90-TS TOC side panel."
  '("f90-ts-toc"
    ["Jump to entry"     f90-ts-toc-buffer-jump    :active t]
    ["Preview entry"     f90-ts-toc-buffer-preview :active t]
    ["Mark/unmark node"  f90-ts-toc-buffer-mark-node :active t]
    ["Mark/unmark node with doc comments"
     f90-ts-toc-buffer-mark-node-with-doc :active t]
    ["Comment/uncomment node" f90-ts-toc-buffer-comment-node :active t]
    ["Comment/uncomment node (custom prefix)"
     f90-ts-toc-buffer-comment-node-custom :active t]
    ["Narrow/widen to node" f90-ts-toc-buffer-narrow-node :active t]
    "---"
    ["Show variables"    f90-ts-toc-buffer-toggle-variables
     :style toggle :selected f90-ts-toc-show-variables]
    ["Show interfaces" f90-ts-toc-buffer-toggle-interface-procedures
     :style toggle :selected f90-ts-toc-show-interface-procedures]
    ["Include doc comments" f90-ts-toc-buffer-toggle-doc-comments
     :style toggle :selected f90-ts-toc-include-doc-comments]
    ["Abbreviate labels" f90-ts-toc-buffer-toggle-abbreviations
     :style toggle :selected f90-ts-toc-abbreviate-labels]
    ("Shorten long names"
     ["Truncate at window edge" (f90-ts-toc-buffer-set-shortening 'truncate)
      :style radio :selected (eq f90-ts-toc-name-shortening 'truncate)]
     ["Shorten from the left"   (f90-ts-toc-buffer-set-shortening 'left)
      :style radio :selected (eq f90-ts-toc-name-shortening 'left)]
     ["Shorten from the right"  (f90-ts-toc-buffer-set-shortening 'right)
      :style radio :selected (eq f90-ts-toc-name-shortening 'right)]
     "---"
     ["Cycle" f90-ts-toc-buffer-cycle-shortening])
    ["Panel on right side" f90-ts-toc-buffer-toggle-side
     :style toggle :selected (eq f90-ts-toc-buffer-side 'right)]
    "---"
    ["Refresh"           f90-ts-toc-buffer-refresh :active t]
    "---"
    ["Quit"              f90-ts-toc-buffer-quit    :active t]))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-toc)

;;; f90-ts-toc.el ends here
