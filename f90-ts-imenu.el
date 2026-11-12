;;; f90-ts-imenu.el --- Imenu implementation for f90-ts-mode -*- lexical-binding: t; -*-

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

;; Provide imenu for `f90-ts-mode'.  It captures groups listed in
;; `f90-ts--nav-queries' from `f90-ts-auxiliary'.

;;; Code:

(require 'cl-lib)
(require 'treesit)

;; defface and defcustom stuff
(require 'f90-ts-custom)

;; provide workarounds and missing functions
(require 'f90-ts-workaround)

;; auxiliary stuff
(require 'f90-ts-auxiliary)


;;;-----------------------------------------------------------------------------

(defconst f90-ts--imenu-query-compiled
  (let ((query-string
         (mapconcat
          (lambda (entry)
            (plist-get (cdr entry) :query))
          f90-ts--nav-queries
          "\n")))
    (treesit-query-compile 'fortran query-string))
  "Pre-compiled global query for a one-pass scan to build imenu.")


(defun f90-ts--imenu-spec-for-type (type)
  "Return the plist for TYPE from `f90-ts--nav-queries'."
  (alist-get type f90-ts--nav-queries nil nil #'string=))


(defun f90-ts--imenu-name-pos-fn (node)
  "Return list of (NAME . POSITION) for NODE using its associated imenu query.
This extracts the name by taking the first captured node from the query,
regardless of the capture name symbol used."
  (let* ((type  (treesit-node-type node))
         (spec  (f90-ts--imenu-spec-for-type type))
         (query (plist-get spec :query))
         (caps  (and query (treesit-query-capture node query))))
    (cl-loop
     for (_ . node) in caps
     collect (cons (treesit-node-text node t)
                   (treesit-node-start node)))))


(defun f90-ts--imenu-group-items (items)
  "Group flat ITEMS list into ((LABEL (NAME . MARKER) ...) ...) for Imenu.
Each element of ITEMS is (LABEL NAME . MARKER)."
  (cl-loop
   for (label . group) in (seq-group-by #'car items)
   collect (cons label
                 (mapcar (lambda (item)
                           (cons (cadr item) (caddr item)))
                         group))))


(defun f90-ts--imenu-captures-to-items (captures)
  "Convert raw CAPTURES from `treesit-query-capture' to a flat list of items.
Items are triples (LABEL NAME MARKER), where LABEL is derived from the
capture symbol via `f90-ts--nav-queries'."
  (cl-loop
   for (cap-sym . node) in captures
   for key   = (alist-get cap-sym f90-ts--nav-capture-key-alist)
   for label = (and key (plist-get (alist-get key f90-ts--nav-queries) :label))
   when label
   collect (list label
                 (treesit-node-text node t)
                 (set-marker (make-marker)
                             (treesit-node-start node)))))


(defun f90-ts-simple-imenu ()
  "Return an Imenu index for the current buffer using a single query pass.
Using `treesit-simple-imenu' is far more expensive computationally."
  (let* ((root     (treesit-buffer-root-node))
         (captures (treesit-query-capture root f90-ts--imenu-query-compiled))
         (items    (f90-ts--imenu-captures-to-items captures)))
    (f90-ts--imenu-group-items items)))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-imenu)

;;; f90-ts-imenu.el ends here
