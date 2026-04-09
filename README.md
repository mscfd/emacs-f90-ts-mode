# f90-ts-mode — Tree-sitter based Fortran 90 mode for Emacs

**f90-ts-mode** is a major mode for editing **Fortran 90 / Fortran 2003** (and newer)
based on Emacs’s built-in **Tree-sitter** support (requires Emacs 29+).

The mode is under **development**, features might only be partially implemented.


## Table of Contents

- [Installation](#installation)
  - [Tree-sitter grammar](#tree-sitter-grammar)
  - [Tree-sitter based mode](#tree-sitter-based-mode)
  - [Setup](#setup)
  - [Keybindings](#keybindings)
- [Features](#features)
  - [Syntax highlight](#syntax-highlight)
    - [Special variables](#special-variables)
    - [Comment keywords and rules](#comment-keywords-and-rules)
  - [Indentation](#indentation)
    - [Indentation of multiline statement](#indentation-of-multiline-statement)
    - [OpenMP and other special comments](#openmp-and-other-special-comments)
  - [Smart end completion](#smart-end-completion)
  - [Indentation of continued statements and blocks](#indentation-of-continued-statements-and-blocks)
  - [Xref](#xref)
  - [Breaking and joining lines](#breaking-and-joining-lines)
    - [Breaking lines](#breaking-lines)
    - [Joining lines](#joining-lines)
  - [Comment region](#comment-region)
  - [Mark regions based on tree-sitter nodes](#mark-regions-based-on-tree-sitter-nodes)
- [Testing with ERT](#testing-with-ert)
  - [Makefile](#makefile)
  - [Indentation tests](#indentation-tests)
  - [Font lock tests](#font-lock-tests)
  - [Custom tests](#custom-tests)
- [Logging and debugging](#logging-and-debugging)



## Installation

### Tree-sitter grammar

Currently, it relies on a recent tree-sitter grammar version of fortran at
[official/tree-sitter-fortran](https://github.com/stadelmanma/tree-sitter-fortran).
There is also an upstream treesitter grammar fork, which might contain some fixes not yet merged
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran).

**NOTE**:
In some cases, if official branch has not yet merged some PR, the current master branch of mscfd/tree-sitter-fortran
is mandatory.
For releases, mscfd/tree-sitter-fortran contains a tag corresponding to compatible f90-ts-mode release version.

**NOTE**: The fortran grammar should be compiled with treesitter version 0.25.x, as emacs (including 30.2) does not yet support the 0.26 branch.
For example, queries are not translated as expected by the 0.26 branch.

The following can be used to check whether versions are correct:

(`M-:` = `eval-expression`)
* with `M-:` `(treesit-library-abi-version)` should be 15
* with `M-:` `(treesit-language-abi-version 'fortran)` should be 15
* `ldd bin_path_to_emacs/emacs | grep libtree-sitter` should show `libtree-sitter.so.0.25`

The f90-ts-mode relies on the master branch of the patched treesitter fortran grammar at
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran.git).
Suggested patches are not yet merged into main treesitter fortran repo.

The grammar needs to be generated with `tree-sitter generate` (or via npm), and installed in emacs,
using the default instructions for installing a new grammar.


### Tree-sitter based mode

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

  ;; uncomment if Imenu entry in menu bar is desired
  ;;:hook (f90-ts-mode . (lambda () (imenu-add-to-menubar "Imenu")))

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

;; only required for testing
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


### Keybindings

The mode sets the following default mode local keybindings:
```elisp
(define-key f90-ts-mode-map (kbd "C-<tab>") #'f90-ts-indent-and-complete-stmt)
(define-key f90-ts-mode-map (kbd "<backtab>")         #'f90-ts-indent-for-tab-command-2) ; S-<tab>
(define-key f90-ts-mode-map (kbd "C-S-<iso-lefttab>") #'f90-ts-indent-for-tab-command-3) ; Linux
(define-key f90-ts-mode-map (kbd "C-<backtab>")       #'f90-ts-indent-for-tab-command-3) ; Windows?

(define-key f90-ts-mode-map (kbd "A-<return>") 'f90-ts-break-line)
(define-key f90-ts-mode-map (kbd "A-<backspace>") #'f90-ts-join-line-prev)
(define-key f90-ts-mode-map (kbd "A-<delete>") #'f90-ts-join-line-next)
(define-key f90-ts-mode-map (kbd "A-\\") #'f90-ts-enlarge-region)
(define-key f90-ts-mode-map (kbd "A-0") #'f90-ts-child0-region)
(define-key f90-ts-mode-map (kbd "A-[") #'f90-ts-prev-region)
(define-key f90-ts-mode-map (kbd "A-]") #'f90-ts-next-region)
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
- Imenu and a `Fortran` menu in the menu bar


### Syntax highlight

The mode has four levels of font-locking, which is controlled by customizable variable `treesit-font-lock-level`.

 - level 1: comment, preprocessor,
 - level 2: builtin, keyword, string, type,
 - level 3: constant, number,
 - level 4: function, variable, operator, bracket, delimiter.

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


#### Special variables

Special variables are recognised by regexp matching with customizable variable `f90-ts-special-var-regexp`.
For matched variables `f90-ts-font-lock-special-var-face` is used.


#### Comment keywords and rules

Keywords within comments can be coloured by `font-lock-warning-face` using customizable regexp
`f90-ts-comment-keyword-regexp`. This is intended to highlight words like `TODO` or `FIXME`.

Openmp, separator comments and other special comments can be customized by rules in `f90-ts-special-comment-rules'.
Among other things, each such rule has a slot for a face to be applied. For example, an openmp rule could be:
```
(:name "general openmp rule" :match "^!\\$\\(?:omp\\)?\\b" :indent
       column-0 :face f90-ts-font-lock-openmp-face)
```
for applying `f90-ts-font-lock-openmp-face` to openmp statements. Note that such statements are currently
parsed as comments.


### Indentation

Indentation supports most fortran statements. Coarrays are still missing. It also covers some error cases which
frequently happen during typing. Lines within incomplete and unfinished blocks are mostly correctly inlined,
but sometimes, the treesitter AST is just not usable.

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
- `f90-ts-indent-toplevel` is used to reduce the indentation of anything which is right below the program
  or (sub)module level. This is done as this indentation level usually does not improve readability,
  as almost everything except for very few lines (like `module`, `contains` and `end` line) is indented.


Currently implemented rules are:

| Rule Set                | Description                                                                                     |
|-------------------------|-------------------------------------------------------------------------------------------------|
| preproc                 | indentation of and at preprocessor directives                                                   |
| comments                | for sequences of regular comment and for special comments (openmp, separator, documentation     |
| continued lines         | multi-line statements, column alignment for lists (like variable declarations, arguments, etc.) |
| internal procedures     | `contains` sections and internal procedures in `programs`, `(sub)modules`, `functions` and `subroutines` |
| program / module        | `program`, `module`, and `submodule` bodies                                                     |
| functions               | `function` and `subroutine` bodies, including `end function` / `end subroutine`                 |
| translation unit        | some rules concerning toplevel translation unit and related ERROR cases                         |
| interfaces              | (abstract) interface blocks                                                                     |
| derived types, enums    | derived `type`, `enum` and `enumeration type` definitions                                       |
| if / then / else        | `if`, `elseif`, and `else` constructs                                                           |
| where                   | `where`-´elsewhere` statements                                                                  |
| single block statements | `do`, `block`, `associate` and `forall` constructs                                              |
| select statements       | `select case` and `select type` statements                                                      |
| catch-all               | final fallback rule for unmatched cases                                                         |


#### Indentation of multiline statement

Indentation of multiline statements is complex. Indentation for region works a bit differently than
indentation of a single line. The reason is that indentation of a single line can rotate through
eligible columns given by similar items on previous lines:
```fortran
call sub_with_many_arguments(argx, another, one_more, &
                                   another2, one_more2, &
                             argy, just_this, &
                             argz)
```
Five options are currently implemented: `continued-line`,  `primary`, `rotate`, `keep-or-primary`
and `keep-or-next`. Primary column is some outstanding column with respect to the context (like
the smallest column of arguments in the example above, or the column just right to the opening parenthesis.
The last three options `rotate`, `keep-or-primary` and `keep-or-next`, which collect and offer several
alignment columns, always include the continued line position among the set of columns.

Behaviour of indentation of a region and of a line are controlled by `f90-ts-indent-list-region`
and `f90-ts-indent-list-line`, respectively.
There are also variants `f90-ts-indent-list-line-2` and `f90-ts-indent-list-line-3`, which are
used by functions bound to Shift+TAB and Control+Shift+TAB by default.

Also check out [Continued statements and blocks](#indentation-of-continued-statements-and-blocks).

Remark: currently options and variants are intended to experiment with and see what might work
and is worth keeping. The additional keybindings for variant 2 and 3 also help with testing various
variants.


TODO:
* implement further list like structures, refine existing once
* handle leading ampersand (related to `f90-ts-beginning-ampersand` for line breaks)


#### OpenMP and other special comments

For special comments such as openmp statements, separator comments and documentation (like ford
and doxygen documentatio with comment starters like `!>` and `!<` or similar)
the customizable list `f90-ts-special-comment-rules` can be used to specify indentation
and faces for such comments.
A list entry looks like
```
(:name "openmp rule"
 :match "^!\\$\\(?:omp\\)?\\b"
 :indent indented
 :face f90-ts-font-lock-openmp-face)
```
providing a name (as documentation), a regexp or predicate function, and indentation type
and a face for synatx highlighting. Indentation types are `column-0`, `context` and `indented`.
With `column-0`, the matched comment is aligned to column 0. For `context`, the enclosing
statement is used for indentation. Option `indented` indents comments like code. If no special
rule matches, then comments are indented with `indented` and highlighted with `font-lock-comment-face`.

For example, if there is an entry
```
(:name "openmp rule"
 :match "\\(!\\( arguments\|===\\)$\\)"
 :indent context
 :face font-lock-comment-face)
```
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

Remark: Indentation hints `context` and `indented` are ignored if comment is within a continued line.
Only `column-0` is applied in the continued line context.


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


### Xref

The mode provides a minimal buffer local implementation of xref functions. In particular, the following
functions can be used to find definitions and references of symbols (keybindings are the default ones):

| Function: keybinding           | Description                          |
|--------------------------------|--------------------------------------|
| `xref-find-definitions`: `M-.` | Jump to definition                   |
| `xref-find-references`: `M-?`  | Find all references                  |
| `xref-find-apropos`: `C-M-.`   | Find symbols matching regexp pattern |
| `xref-go-back`: `M-,`          | Pop back                             |



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
Standard prefixes like `!!$ `, `!$omp `, `!> ` and some others are predefined.
Note that a trailing blank should be provided explicitly if desired.

Indentation of prefixes can be controlled by special comment rules,
see [Special comments](#openmp-and-other-special-comments).
With `f90-ts-comment-prefix-keep-indent` it is possible to control
whether indentation of commented code should be kept,
or wether the prefix should be inserted at the proper column without removing
any blanks to keep original indentation of commented code.


### Mark region based on tree-sitter nodes

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

## Logging and debugging

The following logging functions are available:

* `f90-ts-inspect-node`
* `f90-ts-log`
* `f90-ts-log-clear`
* `f90-ts-log-show`
* `f90-ts--indent-cache-print`
* `f90-ts-indent-rules-info` (using `fail-info-is`)

All write into a dedicated log buffer `*f90-ts-log*` with its own minor mode to allow some
dedicated keybindings.

By default nothing is logged and the buffer is empty. There are almost no (not even commented)
logging instruction in the code left. But the original extensive logging is available in
branch `logging`, which will be kept alive for the time being.
