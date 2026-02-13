# f90-ts-mode — Tree-sitter based Fortran 90 mode for Emacs

**f90-ts-mode** is a major mode for editing **Fortran 90 / Fortran 2003** (and newer)
based on Emacs’s built-in **Tree-sitter** support (requires Emacs 29+).

The mode is under **development**, features might only be partially implemented.
It also has quite extensive debug logging in a separate `*f90-ts-log*` buffer, which
can be controlled by custom variable `f90-ts-log-categories`.


Currently it relies on the upstream treesitter grammar fork
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git)



**NOTE**: The fortran grammar should be compiled with treesitter version 0.25.x, as emacs (including 30.2) does not yet support the 0.26 branch.
For example, queries are not translated as expected by the 0.26 branch.

The following can be used to check whether versions are correct:

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
- Join continued lines
- Comment regions
- Mark regions based on tree-sitter nodes
- Handling openmp and preprocessor directives


### Syntax highlight

Additionally to the usual faces, there are some extra custom faces:

| Face Name                                 | Description                                        |
|-------------------------------------------|----------------------------------------------------|
| `f90-ts-font-lock-intrinsic-face`         | intrinsic procedures and functions                 |
| `f90-ts-font-lock-delimiter-face`         | delimiters such as commas and separators           |
| `f90-ts-font-lock-bracket-face`           | brackets and parentheses                           |
| `f90-ts-font-lock-operator-face`          | operators and assignments                          |
| `f90-ts-font-lock-openmp-face`            | openmp directives                                  |
| `f90-ts-font-lock-special-var-face`       | special variables (e.g. `self`, `this`)            |
| `f90-ts-font-lock-separator-comment-face` | separator comments (e.g. `---------`, `arguments`) |

Special variables are recognised by regexp matching with customizable variable `f90-ts-special-var-regexp`.
Separator comments are recognised by regexp matching with customizable variable `f90-ts-separator-comment-regexp`.


*Note*: Executing `M-x describe-face` can be used to find out which face is applied and to customize it if necessary.


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
| preproc               | indentation of and at preprocessor directives                                            |
| comments              | for sequences of regular and separator comments                                          |
| continued lines       | multi-line statements, column alignment for lists (like variable declarations, arguments, etc.) |
| internal procedures   | `contains` sections and internal procedures in programs, modules and procedures          |
| program / module      | program, module, and submodule bodies                                                    |
| functions             | function and subroutine bodies, including `end function` / `end subroutine`              |
| interfaces            | (abstract) interface blocks                                                              |
| derived types         | derived-type definitions                                                                 |
| if / then / else      | `if`, `elseif`, and `else` constructs                                                    |
| control statements    | `do`, `block`, and `associate` constructs                                                |
| select statements     | `select case` and `select type` statements                                               |
| catch-all             | final fallback rule for unmatched cases                                                  |


#### Indentation of multiline statement

Indentation of multiline statements is complex. Indentation for region works a bit differently than
indentation of a single line. The reason is that indentation of a single line can rotate through
eligible column given by similar items on previous lines:
```
call sub_with_many_arguments(argx, another, one_more, &
                                   another2, one_more2, &
                             argy, just_this, &
                             argz)
```
Four options are currently implemented: `continued-line`, `keep-or-first`, `always-first` and `rotate`
Behaviour of indentation of a region and of a line are controlled by `f90-ts-indent-lists-region`
and `f90-ts-indent-lists-line`, respecitvely.

TODO:
* implement further list like structures, refine existing once
* add fifth option: `keep-or-next`
* use same option for region and line indentation, and provide other options like rotate or always first
by separate functions bound to keys like `C-<tab>`, `M-<tab>` and `A-<tab>` etc.
* handle leading ampersand (related to `f90-ts-beginning-ampersand` for line breaks)


#### Separator comments

Separator comments are recognised by regexp `f90-ts-separator-comment-regexp`.
Highlight is done with `f90-ts-font-lock-separator-comment-face`.
Additionally these special comments are also aligned differently.
Normal comments are indented like statements. Separator comments are aligned to their parent node.
For example, if regexp for separator comments is `\\(arguments\|===\\)`, then indentation would look like:

```
subroutine sub(arg1, arg2)
! arguments
   ! arg1 must be a positive number
   integer, intent(in) :: arg1, arg2
! ===
  ! print arguments
  print *, arg1, arg2
end subroutine sub
```



### Smart end completion

The legacy mode provided smart end completion coupled to indentation, bound to key TAB. This is replicated by f90-ts-mode
using the treesitter generated AST. Lower, upper and title case of construct keywords is recovered and applied to
end statement, including whether to use `end`, `END` or `End`.


#### Indentation of whole structures

For incomplete block, indentation is sometimes not correct, due to an incomplete AST produced by the parser.
To easily indent a whole structure once it can be assumed to be complete structurally, indentation of the whole
structure closed by the `end` statement at point can be performed in conjunction with smart end completion.
If smart end completion has changed the end statement then indent-region is called for the whole block.
This experimental feature is controlled by customizable variable `f90-ts-smart-end-indent-if-changed`.


### Breaking and joining lines

Inspired by the legacy f90 mode as well. Continued lines can be created by breaking a line or reduced
by joining two consecutive lines connected by continuation symbol `&`.


#### Breaking lines

The function `f90-ts-break-line` breaks the current line at point and adds continuation symbols.
If the current line is a comment,
then the comment starter (like '!>', '!<' or similar) is extracted, including indentation offset after the
comment starter, and inserted into the new line to continue the comment.
The comment starter is found by regexp `f90-ts-comment-prefix-regexp`, which can be customized if necessary.

This can be bound to a key by
```
(define-key f90-ts-mode-map (kbd "<C-return>") #'f90-ts-break-line)
```

Whether a leading ampersand at the start of the new line is inserted is controlled by
`f90-ts-beginning-ampersand`. However, this has not yet been tested, in particular in conjunction
with list item alignment.


#### Joining lines

Two consecutives lines (skipping empty lines) connected by continuation symbol `&` can be joined by
`f90-ts-join-line-prev` and `f90-ts-join-line-next`.
The ampersand(s) and empty lines in between are removed.
One whitespace character is left after putting the second line at the end of the first line.
Comments at end of first line or in between the two lines are not allowed currently.
(It is not quite clear what should be done with such comments.)

The `prev` variant joins current line with previous (non-empty) line.
The `next` variant joins current line with next (non-empty) line.

If point is on an empty line within a continued statement, then nothing is joined.
Joining of comment lines or openmp statements is not yet implemented as well.



### Comment region

This is also a feature from the legacy f90 mode. The selected region is (un)commented with
a default or with a selectable prefix.
```
(define-key f90-ts-mode-map (kbd "C-c ;") #'f90-ts-comment-region-default)
(define-key f90-ts-mode-map (kbd "C-c '") #'f90-ts-comment-region-custom)
```

Default prefix `f90-ts-comment-region-prefix` and extra prefixes
`f90-ts-extra-comment-prefixes` can be customized as desired.
Standard prefixes like `!$omp`, `!>` and some others are predefined.


### Mark regions based on tree-sitter nodes

Regions can be selected, enlarged, shrunk or moved based on tree-sitter nodes:
Available functions are:

| Function                         | Description                                                                 |
|----------------------------------|-----------------------------------------------------------------------------|
| `f90-ts-enlarge-region`          | Find smallest parent node of existing region which is strictly larger       |
| `f90-ts-child0-region`           | Reduce existing region to a first (grand)child which is strictly smaller    |
| `f90-ts-prev-region`             | Move selected region to previous sibling                                    |
| `f90-ts-next-region`             | Move selected region to next sibling                                        |

Default Keybindings:
 * (define-key map (kbd "A-\\") #'f90-ts-enlarge-region)
 * (define-key map (kbd "A-0") #'f90-ts-child0-region)
 * (define-key map (kbd "A-[") #'f90-ts-prev-region)
 * (define-key map (kbd "A-]") #'f90-ts-next-region)



## Installation

The f90-ts-mode relies on the master branch of the patched treesitter fortran grammar at
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git).
Suggested patches are not yet merged into main treesitter fortran repo.

The grammar needs to be generated with `tree-sitter generate` (or via npm), and installed in emacs.


Currently, f90-ts-mode.el is not provided as a package. It needs to be copied into a directory,
where emacs can find it.


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
  (define-key f90-ts-mode-map (kbd "A-h p") #'treesit-inspect-node-at-point)
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
