# f90-ts-mode — Tree-sitter based Fortran 90 mode for Emacs

**f90-ts-mode** is a major mode for editing **Fortran 90 / Fortran 2003** (and newer)
based on Emacs’s built-in **Tree-sitter** support (requires Emacs 29+).

The mode is under **development**, features partially implemented:
- Font locking
- Indentation
- Smart end completion
- Line break with automatic continuation

Currently it relies on the upstream treesitter grammar fork
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git)

The mode currently has quite extensive debug logging in a separate `*f90-ts-log*` buffer.


## Features
(with inspiration from the legacy f90 mode in emacs)

- Syntax highlighting
- Indentation
- Smart end completion 
- Break lines with automatic continuation
- openmp


## Installation

First get the patched treesitter fortran grammar at [mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git),
generate the grammar (`tree-sitter generate`) and install the grammar in emacs.

Put f90-ts-mode.el into a directory where emacs can find it, and link to the tree-sitter-fortran repository.

```elisp
(add-to-list 'load-path
             "/home/user/path/to/f90/ts/mode")
(add-to-list 'treesit-language-source-alist
             '(fortran "/home/user/path/to/tree-sitter-fortran"))
```


This is a quite extensive way to load and setup f90-ts-mode, intended for development purposes:
```elisp
(defun f90-ts-toggle-mode ()
  "Toggle between `f90-mode' (legacy) and `f90-ts-mode'."
  (interactive)
  (if (eq major-mode 'f90-ts-mode)
      (if (fboundp 'f90-mode)
          (f90-mode)
        (message "Legacy `f90-mode' not available"))
    (f90-ts-mode)))


(use-package f90-ts-mode
  ;; :ensure nil tells use-package NOT to try installing this from MELPA/ELPA.
  :ensure nil
  
  ;; commented: use legacy f90-mode during development by default
  ;; :mode can be used to automatically enable the mode for Fortran files
  ;:mode "\\.f90\\'"

  ;; instead: specify functions that, when called, should trigger the loading of f90-ts-mode.el
  :commands (f90-ts-toggle-mode f90-ts-mode)

  :config
  (message "f90-ts-mode loaded")

  ;; replaces the currently active frame with the log buffer, should work in any buffer
  (global-set-key (kbd "A-h l") #'f90-ts-log-show)

  (define-key f90-ts-mode-map (kbd "A-h m") #'treesit-explore-mode)
  (define-key f90-ts-mode-map (kbd "A-h P") #'treesit-inspect-node-at-point)
  (define-key f90-ts-mode-map (kbd "A-h p") #'f90-ts-inspect-node-at-point)
  (define-key f90-ts-mode-map (kbd "A-h f") #'describe-face)

  (define-key f90-ts-mode-map (kbd "A-h A-j")
    (lambda ()
      (interactive)
      (progn
        ;; restart f90-ts-mode
        (f90-ts-toggle-mode)
        (f90-ts-toggle-mode)
        (f90-ts-log :info "toggled f90-ts-mode off and on again")))))


(global-set-key (kbd "A-h j") #'f90-ts-toggle-mode)
(global-set-key (kbd "A-h k") #'f90-ts-mode)
```
