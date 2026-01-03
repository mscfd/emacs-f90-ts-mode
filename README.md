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


**NOTE**: Use treesitter version 0.25.x, as emacs (including 30.2) has problems with the 0.26 branch.
For example, queries are not translated correctly.

Check that the following versions are correct:

(`M-:` = `eval-expression`)
* with `M-:` `(treesit-library-abi-version)` should be 15
* with `M-:` `(treesit-language-abi-version 'fortran)` should be 15
* `ldd bin_path_to_emacs/emacs | grep libtree-sitter` should show `libtree-sitter.so.0.25`


## Features
(with inspiration from the legacy f90 mode in emacs)

- Syntax highlighting
- Indentation
- Smart end completion
- Break lines with automatic continuation and comment starters for comment lines
- openmp


### Syntax highlight

Additionally to the usual faces, there are some extra custom faces:

| Face Name                               | Used For                                   |
|-----------------------------------------|--------------------------------------------|
| `f90-ts-font-lock-intrinsic-face`       | intrinsic procedures and functions         |
| `f90-ts-font-lock-delimiter-face`       | delimiters such as commas and separators   |
| `f90-ts-font-lock-bracket-face`         | brackets and parentheses                   |
| `f90-ts-font-lock-operator-face`        | operators and assignments                  |
| `f90-ts-font-lock-openmp-face`          | openmp directives                          |
| `f90-ts-font-lock-special-var-face`     | special variables (e.g. `self`, `this`)    |
| `f90-ts-font-lock-special-comment-face` | special comments (e.g. `FIXME`, `REMARK`)  |

Special variables are recognised by regexp matching with customizable variable `f90-ts-special-var-regexp`.
Special comments are recognised by regexp matching with customizable variable `f90-ts-special-comment-regexp`.


*Note*: use interactive elisp function describe-face to find out which face is applied and to customize it if necessary.


### Indentation

Indentation is still missing quite a number of statements. But it covers most commonly used statements, including error
cases which frequently happen during typing. Lines within incomplete and unfinished blocks are mostly correctly inlined,
but sometimes, the treesitter AST is just not usable.

Customizable variables for indentations are:

| Variable                         | Description                                                                 |
|----------------------------------|-----------------------------------------------------------------------------|
| `f90-ts-indent-toplevel`         | extra indentation applied to contain sections at the top level              |
| `f90-ts-indent-contain`          | extra indentation applied to nested contain sections                        |
| `f90-ts-indent-block`            | extra indentation applied to most blocks, such as functions, subroutines, and control statements (`do`, `if`, `associate`, etc.) |
| `f90-ts-indent-continued`        | extra indentation applied to continued lines                                |

*Remark*: f90-ts-indent-toplevel is used to reduce the indentation of anything which is right below the program
or (sub)module level. This is done as this indentation level usually does not improve readability, as almost
everything except for very few lines (like module, contains and end line) is indented.

Currently implemented rules are:

| Rule Set              | Description                                                                              |
|-----------------------|------------------------------------------------------------------------------------------|
| openmp                | openmp directives stored as comments starting with `!$` or `!$omp`                       |
| comments              | regular and special comments                                                             |
| lists                 | continued lists such as argument lists, association lists, bindings, and expressions     |
| continued lines       | generic continued lines not matched by list rules, indented by `f90-ts-indent-continued` |
| internal procedures   | `contains` sections and internal procedures in programs, modules and procedures          |
| program / module      | program, module, and submodule bodies                                                    |
| functions             | function and subroutine bodies, including `end function` / `end subroutine`              |
| interfaces            | (abstract) interface blocks                                                              |
| derived types         | derived-type definitions                                                                 |
| if / then / else      | `if`, `elseif`, and `else` constructs                                                    |
| control statements    | `do`, `block`, and `associate` constructs                                                |
| select statements     | `select case` and `select type` statements                                               |
| catch-all             | final fallback rule for unmatched cases                                                  |


#### Continued lists

For list structures (argument lists, variable declarations, logical expressions in if and do while etc.) spread
over several continued lines, four options exists, which can be set for default indentation (in particular indent-region)
via `f90-ts-indent-lists-region` and single line indentation via `f90-ts-indent-lists-line`.
Options are `continued-line`, `keep-or-first`, `always-first` and `rotate`.


#### Continued lines

To improve usability of continued lists alignment, the list alignment is only applied, if the first line of the
statement is aligned. Otherwise, the whole block is moved by the offset applied to the first line of the statement.


### Smart end completion

The legacy mode provided smart end completion coupled to indentation, bound to key TAB. This is replicated by f90-ts-mode
using the treesitter generated AST. Control statements with names are not yet supported.


### Break lines

Inspired by the legacy f90 mode as well. Break a line and add continuation symbols. If the current line is a comment,
then extract the comment starter (like '!>', '!<' or similar), including indentation offset after the starter and
insert it into the new line to continue the comment.
The comment starter is found by regexp `f90-ts-comment-prefix-regexp`, which can be customized if necessary.

This can be bound to a key by
```
(define-key f90-ts-mode-map (kbd "<C-return>") #'f90-ts-break-line)
```


### Comment region

This is also a feature from the legacy f90 mode. The selected region is (un)commented with a default or with a selectable prefix.
```
(define-key f90-ts-mode-map (kbd "C-c ;") #'f90-ts-comment-region-default)
(define-key f90-ts-mode-map (kbd "C-c '") #'f90-ts-comment-region-custom)
```

Default prefix `f90-ts-comment-region-prefix` and extra prefixes `f90-ts-extra-comment-prefixes` can be customized as desired.
Standard prefixes like `!$omp`, `!>` and some others are predefined.



## Installation

First get the master branch of the patched treesitter fortran grammar at
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git).

Generate the grammar and install the grammar in emacs.


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

  ;; :defer t ensures the file is only loaded when the mode is called
  ;:defer t

  ;; commented: use legacy f90-mode during development by default
  ;; :mode can be used to automatically enable the mode for Fortran files
  :mode "\\.f90\\'"

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

(global-set-key (kbd "A-h j") #'f90-ts-toggle-mode)
(global-set-key (kbd "A-h k") #'f90-ts-mode)

(defun f90-ts-tests-run ()
  "Reload and run all f90-ts-mode tests."
  (interactive)
  ;; Run tests and display results
  (ert "^f90-ts-mode-"))

(with-eval-after-load 'f90-ts-mode
  (require 'f90-ts-mode-tests nil t))

(global-set-key (kbd "A-h t") #'f90-ts-tests-run)
(global-set-key (kbd "A-h u") #'f90-ts-mode-tests-update)
(global-set-key (kbd "A-h s") #'f90-ts-mode-test-single)
```
