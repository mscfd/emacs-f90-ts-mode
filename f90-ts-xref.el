;;; f90-ts-xref.el --- Basic xref implementation for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide a basic xref implementation for `f90-ts-mode'.  The implementation
;; does not filter semantically and finds many references not in scope at
;; point.  It is not intended to replace a proper xref tool like eglot/lsp.
;; It immediately defers to these tools if those are used.

;; Provide a basic xref backend for `f90-ts-mode'.  It works on the
;; current buffer only.  Definitions, references and apropos are found by
;; tree-sitter queries over the buffer syntax tree, hence nothing in other
;; files or modules is found.  Matching is by name (case-insensitive) without
;; any consideration of scope.  References may include unrelated symbols.
;;
;; This is not meant to replace a proper xref tool.  The backend is not
;; selected if eglot, lsp-mode or a tags table is used.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'xref)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)

;;;-----------------------------------------------------------------------------
;;; Xref backend

(defconst f90-ts--xref-def-query
  "(program (program_statement (name) @def))
   (module (module_statement (name) @def))
   (submodule (submodule_statement (name) @def))
   (subroutine (subroutine_statement name: (name) @def))
   (function (function_statement name: (name) @def))
   (module_procedure_statement name: (name) @def)
   (interface_statement (name) @def)
   (interface_statement (operator (operator_name) @def))
   (interface_statement (assignment \"=\" @def))
   (derived_type_definition (derived_type_statement (type_name) @def))
   (preproc_def name: (identifier) @def)
   (preproc_function_def name: (identifier) @def)"
  "Query for xref for finding definitions of symbol captured as @def.

Each clause matches a syntactic construct that introduces a named
entity (e.g. subroutine, function, module, derived type, or macro)
and binds the relevant node to the capture name @def.

The resulting captures are consumed by
`f90-ts--xref-find-definitions' to locate definition positions.")

(defconst f90-ts--xref-ref-query
  "[(identifier)
    (name)
    (type_name)
    (type_member)
    (method_name)
    (operator_name)
    (user_defined_operator)
    (procedure_interface)
   ] @ref"
  "Query for xref for finding references of symbols captured by @ref.

The query is very general and might find incorrect references.  There is
also no consideration for the scope of a symbol.")

(defun f90-ts--xref-find-definitions (identifier)
  "Find all definitions of IDENTIFIER in the current buffer.

IDENTIFIER is a string representing the symbol to search for.
The search is case-insensitive, reflecting Fortran's
case-insensitive semantics.

This function:
1. Executes `f90-ts--xref-def-query' against the current buffer's tree-sitter
   syntax tree.
2. Filters captured nodes whose text matches IDENTIFIER.
3. Converts matching nodes into xref-item objects.

Return a list of xref-item instances suitable for consumption by
`xref-backend-definitions'."
  (let* ((id-lowcase (downcase identifier))
         (scope (treesit-buffer-root-node))
         (id-nodes (treesit-query-capture scope
                                          f90-ts--xref-def-query
                                          nil nil t)))
    (cl-loop
     for node in id-nodes
     when (string= (downcase (treesit-node-text node t)) id-lowcase)
     collect (f90-ts--xref-make-item node))))


(defun f90-ts--xref-find-references (identifier)
  "Find all references to IDENTIFIER in the current buffer.

IDENTIFIER is a string naming the symbol to search for.
The search is case-insensitive.

This function performs a broad tree-sitter query that captures all
identifier nodes, then filters them by textual equality with
IDENTIFIER.

Note that this approach is purely syntactic and does not account
for scope, shadowing, or semantic resolution.

Return a list of xref-item instances suitable for
`xref-backend-references'."
  (let* ((id-lowcase (downcase identifier))
         (scope (treesit-buffer-root-node))
         (id-nodes (treesit-query-capture scope
                                          f90-ts--xref-ref-query
                                          nil nil t)))
    (cl-loop
     for node in id-nodes
     when (string= (downcase (treesit-node-text node t)) id-lowcase)
     collect (f90-ts--xref-make-item node))))


(defun f90-ts--xref-make-item (node)
  "Construct an xref-item from a tree-sitter NODE.

NODE is expected to represent an identifier or definition site.
The function extracts:

- A human-readable display string consisting of the full line containing NODE.
- A buffer location corresponding to the start of NODE.

The resulting xref-item can be used by xref commands to display and navigate
search results."
  (let ((start (treesit-node-start node)))
    (xref-make
     (save-excursion
       (goto-char start)
       (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
     (xref-make-buffer-location (current-buffer) start))))


(cl-defmethod xref-backend-identifier-at-point ((_backend (eql f90-ts)))
  "Return the identifier at point for the f90-ts backend.

This method is invoked by xref to determine the default symbol
under the cursor when performing operations like
`xref-find-definitions'.

It uses `thing-at-point' with the symbol type and returns the
result as a string, or nil if no symbol is found."
  (thing-at-point 'symbol t))


(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql f90-ts)))
  "Return a completion table of identifiers in the current buffer.

This method provides candidates for identifier completion in xref commands.
It collects all identifier nodes from the tree-sitter syntax tree and returns
their textual representations.
Currently only named nodes with type \"identifier\" are captured.

Duplicates are not explicitly removed, so callers may wish to handle
deduplication if necessary."
  (let ((id-nodes (treesit-query-capture
                   (treesit-buffer-root-node)
                   '((identifier) @id)
                   nil nil t)))
    (cl-loop
     for node in id-nodes
     collect (treesit-node-text node t))))


(cl-defmethod xref-backend-definitions ((_backend (eql f90-ts)) identifier)
  "Return definition locations for IDENTIFIER using the f90-ts backend.

Uses `f90-ts--xref-find-definitions', which performs a buffer-local search for
matching definition nodes."
  (f90-ts--xref-find-definitions identifier))


(cl-defmethod xref-backend-references ((_backend (eql f90-ts)) identifier)
  "Return reference locations for IDENTIFIER using the f90-ts backend.

Uses `f90-ts--xref-find-references', which performs a syntactic search over
identifier nodes in the current buffer."
  (f90-ts--xref-find-references identifier))


(cl-defmethod xref-backend-apropos ((_backend (eql f90-ts)) pattern)
  "Return all identifiers matching regexp PATTERN in the current buffer.

This implementation performs a tree-sitter query over all identifier
nodes and returns those whose textual representation matches PATTERN.

The match is case-insensitive, following Fortran conventions."
  (let ((case-fold-search t))
    (cl-loop
     for node
     in (treesit-query-capture (treesit-buffer-root-node)
                               '((identifier) @id)
                               nil nil t)
     for text = (buffer-substring-no-properties
                 (treesit-node-start node)
                 (treesit-node-end node))
     when (string-match-p pattern text)
     collect (f90-ts--xref-make-item node))))


(defun f90-ts-xref-backend ()
  "Return the symbol identifying the f90-ts xref backend.
Return f90-ts backend unless a higher-level backend is active.

This function is intended to be added to
`xref-backend-functions'.  When invoked by xref's backend
discovery mechanism, it signals that the current buffer should
use the f90-ts backend by returning the symbol f90-ts.

Because this function is typically installed buffer-locally, it
activates the backend only for buffers using `f90-ts-mode'."
  (cond
   ;; prefer LSP and eglot if active
   ((bound-and-true-p lsp-mode) nil)
   ((bound-and-true-p eglot--managed-mode) nil)

   ;; prefer etags if TAGS table exists
   ((and tags-file-name (file-exists-p tags-file-name)) nil)

   ;; otherwise use the backend provided here
   (t 'f90-ts)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-xref)

;;; f90-ts-xref.el ends here
