;;; f90-ts-log.el --- Debug logging for f90-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Martin Stein

;; Author: Martin Stein
;; Maintainer: Martin Stein
;; URL: https://github.com/mscfd/emacs-f90-ts-mode
;; Keywords: languages, treesitter, fortran
;; Version: 0.1.2pre
;; Package-Requires: ((emacs "30.1"))

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
;; This package is intended to help with development and debugging of
;; package f90-ts-mode.  Stubs of f90-ts-log-msg, f90-ts-log-inspect-node
;; and f90-ts-log-indent-print-state are provided in f90-ts-mode.el.  These are
;; overwritten by the real functions by loading the logging module.

(require 'f90-ts-mode)

;;; Code:

;;;-----------------------------------------------------------------------------
;; log buffer

(defconst f90-ts-log-buffer "*f90-ts-log*"
  "Buffer name used for f90 tree-sitter logging.")


(defun f90-ts-log---get-buffer ()
  "Return the log buffer and if it does not yet exists, create a new one."
  (or (get-buffer f90-ts-log-buffer)
      (with-current-buffer (get-buffer-create f90-ts-log-buffer)
        (f90-ts-log-mode)
        (current-buffer))))


(defun f90-ts-log-msg (category fmt &rest args)
  "Insert a message into the dedicated *f90-ts-log* log buffer.
The message is computed from FMT and ARGS (using `format') and prefixed
by CATEGORY and a time stamp."
  (let ((buf (f90-ts-log---get-buffer)))
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
        (goto-char (point-max))))

    ;; scroll window if buffer is displayed
    (when-let ((win (get-buffer-window buf t))) ; visible in any frame
      (with-selected-window win
        (goto-char (point-max))))))


(defun f90-ts-log-clear ()
  "Clear the f90-ts log buffer."
  (interactive)
  (when (get-buffer f90-ts-log-buffer)
    (with-current-buffer f90-ts-log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))


;;;###autoload
(defun f90-ts-log-show ()
  "Show the f90-ts log buffer in the current frame."
  (interactive)
  (let ((buf (f90-ts-log---get-buffer)))
    (with-selected-frame (selected-frame)
      (switch-to-buffer buf))))


(defvar f90-ts-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") #'f90-ts-log-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `f90-ts-log-mode'.")


(define-derived-mode f90-ts-log-mode special-mode "F90-TS-Log"
  "Major mode for the f90-ts log buffer."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (buffer-disable-undo))


;;;-----------------------------------------------------------------------------

(defun f90-ts-log-line (category msg &optional pos)
  "Write the line at POS or point to the log buffer.
Use CATEGORY and MSG to prefix the log message."
  (let ((p (or pos (point))))
  (save-excursion
    (goto-char p)
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (f90-ts-log-msg category "%s: line at <pos:%d,line:%d> = %S"
                      msg p (line-number-at-pos p) line)))))


;;;-----------------------------------------------------------------------------
;; node inspection

(defun f90-ts-log--treesit-inspect-node (node-inspect)
  "Construct a descriptive node name for NODE-INSPECT.
This is mostly a copy of `treesit-inspect-node-at-point', but this variation
highlights provided NODE-INSPECT and uses its start position as point."
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


(defun f90-ts-log-inspect-node (category node info)
  "Show inspect info of treesitter NODE as a one-liner in the log buffer.
Prefix the line with CATEGORY and `inspect<info>' using INFO."
  (if node
      (let* ((type  (treesit-node-type node))
             (start (treesit-node-start node))
             (end   (treesit-node-end node))
             (len   (- end start))
             (line  (f90-ts--node-line node))
             (inspect-name  (f90-ts-log--treesit-inspect-node node)))
        (f90-ts-log-msg category "inspect<%s>: type= %s  -  name= %s - start=%d  end=%d  len=%d  line=%d"
                    info type inspect-name start end len line))
    (f90-ts-log-msg category "inspect<%s>: nil" info)))


;;;-----------------------------------------------------------------------------

;;; Logging function for indentation cache `f90-ts--indent-cache'
(defun f90-ts-log--indent-cache-print ()
  "Print out current cache state of indentation cache `f90-ts--indent-cache'."
  (if (null f90-ts--indent-cache)
      (f90-ts-log-msg :indent "cache: nil")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-cached-node)        "node@cache")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-cached-parent)      "parent@cache")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-child0)             "child0@cache")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-prev-sib-by-parent) "psibp@cache")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-prev-stmt-first)    "pstmt-1@cache")
    (f90-ts-log-inspect-node :indent (f90-ts--indent-prev-stmt-keyword)  "pstmt-k@cache")
    (f90-ts-log-msg :indent "anchor@cache = %s" (f90-ts--indent-cached-anchor))
    (f90-ts-log-msg :indent "offset@cache = %s" (f90-ts--indent-cached-offset))))


;;; Logging rule for treesit-simple-indent. It always fails.
(defun f90-ts-log-indent-print-state (msg)
  "Always fail as indentation matcher, but print a separator line.
Additionally if MSG=\"start\" or \"catch all\" print detailed data of position
and nodes for debugging purposes into the exclusive log buffer."
  (lambda (node parent bol &rest _)
    (f90-ts-log-msg :indent "---------info %s--------------" msg)
    (when (or (string= msg "start") (string= msg "catch all"))
      (let* ((grandparent (and parent (treesit-node-parent parent)))
             (psibp (f90-ts--indent-prev-sib-by-parent))
             (pstmt-k (f90-ts--indent-prev-stmt-keyword))
             (child0 (f90-ts--indent-child0)))

        (f90-ts-log-msg :indent "position: point=%d, bol=%d, lbp=%d, line=%d"
                    (point) bol (line-beginning-position) (line-number-at-pos))
        (let ((tttttt (format "types n-p-gp-psibp-pstmtk-ch = %s, %s, %s, %s, %s, %s"
                              (and node (treesit-node-type node))
                              (and parent (treesit-node-type parent))
                              (and grandparent (treesit-node-type grandparent))
                              (and psibp (treesit-node-type psibp))
                              (and pstmt-k (treesit-node-type pstmt-k))
                              (and child0 (treesit-node-type child0)))))
          (f90-ts-log-msg :indent (propertize tttttt 'face '(:foreground "brown2")))
          (f90-ts-log--indent-cache-print))))
    nil))


;;;-----------------------------------------------------------------------------

(provide 'f90-ts-log)

;;; f90-ts-log.el ends here
