# f90-ts-mode — Tree-sitter based Fortran 90 mode for Emacs

**f90-ts-mode** is a major mode for editing **Fortran 90 / Fortran 2003** (and newer)
based on Emacs’s built-in **Tree-sitter** support (requires Emacs 29+).

The mode is under **development**, features might only be partially implemented.

Currently it relies on a recent tree-sitter grammar version of fortran at
[official/tree-sitter-fortran](https://github.com/stadelmanma/tree-sitter-fortran).
There is also an upstream treesitter grammar fork, which might contain some fixes not yet merged
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran).



**NOTE**: The fortran grammar should be compiled with treesitter version 0.25.x, as emacs (including 30.2) does not yet support the 0.26 branch.
For example, queries are not translated as expected by the 0.26 branch.

The following can be used to check whether versions are correct:

(`M-:` = `eval-expression`)
* with `M-:` `(treesit-library-abi-version)` should be 15
* with `M-:` `(treesit-language-abi-version 'fortran)` should be 15
* `ldd bin_path_to_emacs/emacs | grep libtree-sitter` should show `libtree-sitter.so.0.25`



## Installation

The f90-ts-mode relies on the master branch of the patched treesitter fortran grammar at
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git).
Suggested patches are not yet merged into main treesitter fortran repo.

The grammar needs to be generated with `tree-sitter generate` (or via npm), and installed in emacs,
using the default instructions for installing a new grammar.


Currently, f90-ts-mode.el is not provided as a package. It needs to be copied into a folder,
where emacs can find it (e.g. via use-package).

The repository can be cloned by:
`git clone https://github.com/mscfd/emacs-f90-ts-mode.git path_to/emacs_f90_ts_mode`


### Setup

The mode itself and optionally the testing module can be loaded with `use-package`
placed somewhere in `init.el` (or elsewhere).


```elisp
(use-package f90-ts-mode
  ;; :ensure nil tells use-package NOT to try installing this from MELPA/ELPA.
  :ensure nil
  :load-path "path_to/emacs-f90-ts-mode"
  :mode ("\\.f90\\'" . f90-ts-mode)
  :commands (f90-ts-mode)

  :init
  (require 'treesit)
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((fortran "path_to/tree-sitter-fortran"))))

  :config
  (message "f90-ts-mode loaded")

  :bind (;; global binding that triggers the load
         ;; replaces the currently active frame with the log buffer,
         ;; should work in any buffer
         ("A-h k" . f90-ts-mode)
         ("A-h l" . f90-ts-log-show)

         ;; mode-specific bindings
         :map f90-ts-mode-map
         ("A-h m" . treesit-explore-mode)
         ("A-h P" . treesit-inspect-node-at-point)
         ("A-h p" . f90-ts-inspect-node-at-point)
         )
  )

;; only required for interactive testing
(use-package f90-ts-mode-test
  :ensure nil
  ;; add the test subdirectory specifically for test related stuff
  :load-path "path_to/emacs-f90-ts-mode/test"

  :commands (f90-ts-mode-test-run
             f90-ts-mode-test-update-erts-after
             f90-ts-mode-test-update-face-annotations)

  :config
  (message "f90-ts-mode-test loaded")

  :bind (;; global test commands
         ("A-h i" . f90-ts-mode-switch-custom)
         ("A-h t" . (lambda () (interactive)
                      (f90-ts-mode-test-run "^f90-ts-mode/")))
         ("A-h d" . (lambda () (interactive)
                      (f90-ts-mode-test-run "^f90-ts-mode/"
                                            f90-ts-mode-test-diff-command))))

  :init
  (require 'f90-ts-mode)
  (define-key f90-ts-mode-map (kbd "A-h u") #'f90-ts-mode-test-update-face-annotations)
  (require 'erts-mode)
  (define-key erts-mode-map (kbd "A-h u") #'f90-ts-mode-test-update-erts-after)
  )
```



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

| Face Name                                 | Description                                           |
|-------------------------------------------|-------------------------------------------------------|
| `f90-ts-font-lock-intrinsic-face`         | intrinsic procedures and functions                    |
| `f90-ts-font-lock-delimiter-face`         | delimiters such as commas and separators              |
| `f90-ts-font-lock-bracket-face`           | brackets and parentheses                              |
| `f90-ts-font-lock-operator-face`          | operators and assignments                             |
| `f90-ts-font-lock-openmp-face`            | openmp directives                                     |
| `f90-ts-font-lock-special-var-face`       | special variables (e.g. `self`, `this`)               |
| `f90-ts-font-lock-separator-comment-face` | separator comments (e.g. `!---------`, `! arguments`) |

Special variables are recognised by regexp matching with customizable variable `f90-ts-special-var-regexp`.
Separator comments are recognised by regexp matching with customizable variable `f90-ts-separator-comment-regexp`.

*Note*: Executing `M-x describe-face` can be used to find out which face is applied and to customize it if necessary.


### Indentation

Indentation is still missing quite a number of statements. But it covers most commonly used statements,
including some error cases which frequently happen during typing. Lines within incomplete and unfinished
blocks are mostly correctly inlined, but sometimes, the treesitter AST is just not usable.

Customizable variables for indentations are:

| Variable                         | Description                                                                 |
|----------------------------------|-----------------------------------------------------------------------------|
| `f90-ts-indent-toplevel`         | extra indentation applied to contain sections at the top level              |
| `f90-ts-indent-contain`          | extra indentation applied to nested contain sections                        |
| `f90-ts-indent-block`            | extra indentation applied to most blocks                                    |
| `f90-ts-indent-continued`        | extra indentation applied to continued lines                                |



*Remarks*
- statement blocks are features such as `functions`, `subroutines`, control statements (`do`, `if`, `select`)
  and other block structures (`associate`, `block` etc.)
- f90-ts-indent-toplevel is used to reduce the indentation of anything which is right below the program
  or (sub)module level. This is done as this indentation level usually does not improve readability,
  as almost everything except for very few lines (like module, contains and end line) is indented.

Currently implemented rules are:

| Rule Set                | Description                                                                              |
|-------------------------|------------------------------------------------------------------------------------------|
| openmp                  | openmp directives stored as comments starting with `!$` or `!$omp`                       |
| preproc                 | indentation of and at preprocessor directives                                            |
| comments                | for sequences of regular and separator comments                                          |
| continued lines         | multi-line statements, column alignment for lists (like variable declarations, arguments, etc.) |
| internal procedures     | `contains` sections and internal procedures in `programs`, `(sub)modules`, `functions` and `subroutines` |
| program / module        | `program`, `module`, and `submodule` bodies                                              |
| functions               | `function` and `subroutine` bodies, including `end function` / `end subroutine`          |
| translation unit        | some rules concerning toplevel translation unit and related ERROR cases                  |
| interfaces              | (abstract) interface blocks                                                              |
| derived types           | derived-type definitions                                                                 |
| if / then / else        | `if`, `elseif`, and `else` constructs                                                    |
| single block statements | `do`, `block`, and `associate` constructs                                                |
| select statements       | `select case` and `select type` statements                                               |
| catch-all               | final fallback rule for unmatched cases                                                  |


#### Indentation of multiline statement

Indentation of multiline statements is complex. Indentation for region works a bit differently than
indentation of a single line. The reason is that indentation of a single line can rotate through
eligible column given by similar items on previous lines:
```fortran
call sub_with_many_arguments(argx, another, one_more, &
                                   another2, one_more2, &
                             argy, just_this, &
                             argz)
```
Five options are currently implemented: `continued-line`, `rotate`, `keep-or-primary`,
`keep-or-rotate` and `always-primary`.
Moreover, `f90-ts-indent-list-always-include-default` controls whether simple indentation for
continued lines should always be added (for example even in an argument context as above).
Remark: currently options and variants are intended to experiment with and see what might work
and is worth keeping.

Behaviour of indentation of a region and of a line are controlled by `f90-ts-indent-list-region`
and `f90-ts-indent-list-line`, respectively.

TODO:
* implement further list like structures, refine existing once
* handle leading ampersand (related to `f90-ts-beginning-ampersand` for line breaks)


#### Separator comments

Separator comments are recognised by regexp `f90-ts-separator-comment-regexp`.
Highlighting is done with `f90-ts-font-lock-separator-comment-face`.
Additionally these special comments are also aligned differently.
Normal comments are indented like statements. Separator comments are aligned to their parent node.
For example, if the regexp for separator comments is `\\(!\\( arguments\|===\\)$\\)`,
indentation looks like:

```fortran
subroutine sub(arg1, arg2)
! arguments
   ! arg1 must be a positive number
   integer, intent(in) :: arg1, arg2
!===
  ! print arguments
  print *, arg1, arg2
end subroutine sub
```


### Smart end completion

The legacy mode provided smart end completion coupled to indentation, bound to key TAB. This is replicated by f90-ts-mode
using the treesitter generated AST. Lower, upper and title case of construct keywords is recovered and applied to
end statement, including whether to use `end`, `END` or `End`.


### Indentation of continued statements and blocks

For incomplete statements on continued lines or incomplete structure blocks,
indentation is sometimes not correct, due to an incomplete AST produced by the parser.

Indentation of continued statements from begin of statement to line at point is done by
`f90-ts-indent-and-complete-stmt`, which is bound to `C-<tab>`.

This same function also indents a whole block if executed at its `end struct` line.



### Breaking and joining lines

Inspired by the legacy f90 mode as well. Continued lines can be created by breaking a line or reduced
by joining two consecutive lines connected by continuation symbol `&`.
Functions for break and join operations are bound to `A-<return>` (break),
`A-<backspace>` (join with previous line) and `A-<delete>` (join with next line).


#### Breaking lines

The function `f90-ts-break-line` breaks the current line at point and adds continuation symbols.
If the current line is a comment,
then the comment starter (like '!>', '!<' or similar) is extracted, including indentation offset after the
comment starter, and inserted into the new line to continue the comment.
The comment starter is found by regexp `f90-ts-comment-prefix-regexp`, which can be customized if necessary.

Whether a leading ampersand at the start of the new line is inserted is controlled by
`f90-ts-beginning-ampersand`. However, this has not yet been tested, in conjunction with indentation
and in particular with list item alignment.


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



## Testing with ERT

The mode comes with a number of tests in `test/resources`, which cover part of the already
implemented features.
Registering and running tests is done in `test/f90-ts-mode-test.el`
Currently there are tests for indentation (`indent_*.erts`) and for font locking
(`font_lock_*.f90`).

Standard tests are named `f90-ts-mode/...`, whereas expensive tests start with `f90-ts-mode-extra/...`.
Registering is done semi-automatic in `f90-ts-mode-test-indent-register` and
`f90-ts-mode-test-font-lock-register`.

Tests are run with a prescribed set of custom variables. In particular indentation values are chosen
all differently, such that errors can be spotted more easily.

### Makefile

There is a Makefile for running the tests. Three targets are available: `test` (standard tests),
`test-extra` (expensive extra tests) and `test-all` for all tests.


### Indentation tests

Indentation tests are in erts file format. For indentation of an after part between `=-=` and `=-=-=`,
function `f90-ts-mode-test-update-erts-after` can be used (first remove point char `|` if present) to
help setting up a new test. Indentation tests have prep-fn preparation function (like remove indentation)
and an action function (like indent-region, indent-line all/single line).
The erts files are used to check various combinations.


### Font lock tests

Font lock tests are in f90 files, with caret assertion notation. This notation can be automatically
generated for new files or updated by `f90-ts-mode-test-update-face-annotations`.
The code is automatically indented by 1, as assertion lines start with a !, so that all faces
including those which would be at column 0 can be checked properly.
The generated annotations are exhaustive, including nil annotations to assert that part of the code
is not highlighted.

Note: fortran test code should NOT use the caret `^`, even in comments, as the ert parser gets confused.


### Custom tests

Custom tests are erts based tests with a custom `Code` block for each test (and thus do not fit the
prep-fn/action-fn scheme of the indentation tests above).
This is used to test indentation of just a region, break and join line operations and
to test the comment region functions.

New tests can easiy be added by placing a test file in `test/resources` and registering it
in `test/f90-ts-mode-test.el`. Registering looks like:
```elsip
(f90-ts-mode-test-prep-act-register
 "f90-ts-mode"
 '("indent_region_basic.erts"
   "indent_region_comments.erts"
   "indent_region_constructs.erts"
   "indent_region_preproc.erts")
 '(nil ; no modification
   f90-ts-mode-test--remove-indent
   f90-ts-mode-test--add-indent)
 '(f90-ts-mode-test--indent-by-region)
 )
```