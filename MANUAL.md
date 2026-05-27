# f90-ts-mode User Manual

This manual provides a comprehensive overview and technical documentation for **f90-ts-mode**,
a major mode for editing **Fortran 90 / Fortran 2003** (and newer) based on Emacs's built-in **Tree-sitter** support.

The mode is still under **development**. The [Roadmap](README.md#roadmap) lists missing or incomplete feature planned
for implementation.

[Back to README](README.md)


## Table of Contents

- [Installation](#installation)
  - [Tree-sitter grammar](#tree-sitter-grammar)
  - [Tree-sitter based mode](#tree-sitter-based-mode)
  - [Setup](#setup)
  - [Keybindings](#keybindings)
- [Features](#features)
  - [Syntax highlight and font lock faces](#syntax-highlight-and-font-lock-faces)
    - [Special variables](#special-variables)
    - [Comment regexps and rules](#comment-regexps-and-rules)
  - [Indentation](#indentation)
    - [Indentation of multiline statement](#indentation-of-multiline-statement)
    - [OpenMP and other special comments](#openmp-and-other-special-comments)
  - [Smart end completion](#smart-end-completion)
  - [Indentation of continued statements with leading ampersand](#indentation-of-continued-statements-with-leading-ampersand)
  - [Indentation of continued statements and blocks](#indentation-of-continued-statements-and-blocks)
  - [Indentation of statement labels](#indentation-of-statement-labels)
  - [Xref](#xref)
  - [Imenu](#imenu)
  - [Navigation menu](#navigation-menu)
  - [Navigation buffer](#navigation-buffer)
  - [Breaking and joining lines](#breaking-and-joining-lines)
    - [Breaking lines](#breaking-lines)
    - [Joining lines](#joining-lines)
  - [Comment region](#comment-region)
  - [Mark regions based on tree-sitter nodes](#mark-regions-based-on-tree-sitter-nodes)
- [Development and Testing](#development-and-testing)
  - [Logging](#logging)
  - [Testing with ERT](#testing-with-ert)
    - [Makefile](#makefile)
    - [Indentation tests](#indentation-tests)
    - [Font lock tests](#font-lock-tests)
    - [Custom tests](#custom-tests)



## Installation

### Tree-sitter grammar

Currently, the mode relies on a recent tree-sitter grammar version of Fortran at
[official/tree-sitter-fortran](https://github.com/stadelmanma/tree-sitter-fortran).
There is also an upstream Tree-sitter grammar fork, which might contain fixes not yet merged:
[mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran).


#### Installing the grammar in Emacs

Register the grammar repository in `treesit-language-source-alist`:

```elisp
(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((fortran "https://github.com/stadelmanma/tree-sitter-fortran"))))
```

Then compile and install the grammar once with:

```elisp
M-x treesit-install-language-grammar RET fortran RET
```

The installation can be verified with:

```elisp
(treesit-ready-p 'fortran)
```

**NOTE**:
The Fortran grammar should be compiled with Tree-sitter version `0.25.x`, as Emacs
(including 30.2) does not yet support the `0.26` branch correctly.
For example, queries are not translated as expected by the `0.26` branch.

The master branch at `mscfd/tree-sitter-fortran` mentioned above provides the parser generated with `0.25.10`.

The following can be used to check whether versions are correct:

(`M-:` = `eval-expression`)

* with `M-:` `(treesit-library-abi-version)` should be `15`
* with `M-:` `(treesit-language-abi-version 'fortran)` should be `15`
* `ldd bin_path_to_emacs/emacs | grep libtree-sitter` should show `libtree-sitter.so.0.25`

**Note**:
The parser generator step `tree-sitter generate` done with Tree-Sitter `0.26` seems to be
compatible with Emacs, but the library and ABI versions listed above must match.
This generator step of creating the parser source files is not necessary in general, as the
parser source files are already provided in the grammar repositories.


### Tree-sitter based mode

`f90-ts-mode` can be installed and enabled from MELPA using
`package-install` and `use-package`.

For development, the repository can also be cloned manually:

```bash
git clone https://github.com/mscfd/emacs-f90-ts-mode.git path_to/emacs-f90-ts-mode
```


### Setup

The mode itself and optionally the testing module can be loaded with `use-package`
placed somewhere in `init.el` (or elsewhere).

#### Standard installation (package manager)

```elisp
(use-package f90-ts-mode
  :ensure t
  :mode ("\\.f90\\'" . f90-ts-mode)

  :init
  (require 'treesit)

  ;; uncomment if Imenu entry in menu bar is desired
  ;; :hook (f90-ts-mode . (lambda () (imenu-add-to-menubar "Imenu")))

  :config
  (message "f90-ts-mode loaded")

  :bind (;; mode-specific bindings, adjust to your needs
         :map f90-ts-mode-map
         ;; transient popup
         ("A-<up>"        . #'f90-ts-transient)
         ;; shortcuts
         ("A-<return>"    . #'f90-ts-break-line)
         ("A-<backspace>" . #'f90-ts-join-line-prev)
         ("A-<delete>"    . #'f90-ts-join-line-next)
         ("A-\\"          . #'f90-ts-mark-region-enlarge)
         ("A-0"           . #'f90-ts-mark-region-shrink-child-first)
         ("A-["           . #'f90-ts-mark-region-prev-sibling)
         ("A-]"           . #'f90-ts-mark-region-next-sibling)))
```

#### Development setup (local clone)

First clone the repository of `f90-ts-mode` as mentioned above.

```elisp
(use-package f90-ts-mode
  :ensure nil
  :load-path "path_to/emacs-f90-ts-mode"
  :mode ("\\.f90\\'" . f90-ts-mode)

  :init
  (require 'treesit)

  ;; uncomment if Imenu entry in menu bar is desired
  ;; :hook (f90-ts-mode . (lambda () (imenu-add-to-menubar "Imenu")))

  :config
  ;; only required for development
  ;;(require 'f90-ts-log)

  (message "f90-ts-mode loaded")

  :bind (;; mode-specific bindings, adjust to your needs
         :map f90-ts-mode-map
         ;; transient popup
         ("A-<up>"        . #'f90-ts-transient)
         ;; shortcuts
         ("A-<return>"    . #'f90-ts-break-line)
         ("A-<backspace>" . #'f90-ts-join-line-prev)
         ("A-<delete>"    . #'f90-ts-join-line-next)
         ("A-\\"          . #'f90-ts-mark-region-enlarge)
         ("A-0"           . #'f90-ts-mark-region-shrink-child-first)
         ("A-["           . #'f90-ts-mark-region-prev-sibling)
         ("A-]"           . #'f90-ts-mark-region-next-sibling)))
```

#### Testing module

The testing helpers are only required for development and repository testing.

```elisp
(use-package f90-ts-mode-test
  :ensure nil

  ;; add the test subdirectory specifically for test related stuff
  :load-path "path_to/emacs-f90-ts-mode/test"

  :commands (f90-ts-mode-test-run
             f90-ts-mode-test-update-erts-after
             f90-ts-mode-test-update-face-annotations)

  :config
  (message "f90-ts-mode-test loaded")

  :init
  (require 'f90-ts-mode)

  (define-key f90-ts-mode-map
              (kbd "A-h u")
              #'f90-ts-mode-test-update-face-annotations)

  (require 'erts-mode)

  (define-key erts-mode-map
              (kbd "A-h u")
              #'f90-ts-mode-test-update-erts-after))
```



### Keybindings

The mode sets the following default mode-local keybindings:

```elisp
;; TAB family
(define-key map (kbd "C-<tab>")           #'f90-ts-indent-and-complete-stmt)
(define-key map (kbd "<backtab>")         #'f90-ts-indent-for-tab-command-2) ; S-<tab>
(define-key map (kbd "C-S-<iso-lefttab>") #'f90-ts-indent-for-tab-command-3) ; Linux
(define-key map (kbd "C-<backtab>")       #'f90-ts-indent-for-tab-command-3) ; Windows?

;; other keybindings inspired by f90-mode
(define-key map (kbd "C-<return>")        #'f90-ts-break-line)
(define-key f90-ts-mode-map (kbd "C-c ;") #'f90-ts-comment-region-default)
(define-key f90-ts-mode-map (kbd "C-c '") #'f90-ts-comment-region-custom)

;; C-c C-f — transient popup (see below)
(define-key map (kbd "C-c C-f") #'f90-ts-transient)
```

#### Transient popup (`C-c C-f`)

Pressing `C-c C-f` opens a transient keymap window, which lists all major
commands grouped by category.

The popup is defined as `f90-ts-transient` and covers:

| Section                   | Keys                            | Commands                                        |
|---------------------------|---------------------------------|-------------------------------------------------|
| **Indentation**           | `TAB` `s` `I` `E`               | Indent line / statement / region / smart end    |
| **Line editing**          | `RET` `j` `J`                   | Break line, join with prev/next                 |
| **Comment region**        | `c` `C`                         | Default and custom prefix                       |
| **Structural navigation** | `a` `e` `p` `n`                 | Procedure (beginning, end, prev, next)          |
|                           | `M-a` `M-e` `M-p` `M-n`         | Type (beginning, end, prev, next)               |
|                           | `C-M-a` `C-M-e` `C-M-p` `C-M-n` | Interface (beginning, end, prev, next)          |
| **Region**                | `r` `0` `[` `]`                 | Enlarge, child-0, prev, next                    |
| **Xref**                  | `.` `,` `/` `<` `>`             | Definitions, references, apropos, back, forward |
| **Navigation side panel** | `b` `f`                         | Open and focus nav buffer                       |

The entire popup can be bound to a different prefix by:

```elisp
(define-key f90-ts-mode-map "..." #'f90-ts-transient)
(keymap-unset f90-ts-mode-map "C-c C-f") ; remove default
```


## Features
(with inspiration from the legacy f90 mode in emacs)

- Syntax highlighting (font lock faces)
- Indentation of lines, regions, multiline statements and structure blocks
- Smart end completion
- Break lines with automatic continuation and comment starters for comment lines
- Join lines
- Comment region operations with configurable prefixes and indentation rules
- Mark regions based on tree-sitter nodes
- OpenMP and preprocessor directives
- Coarray keywords and statements
- `Imenu` and a `Fortran` menu in the menu bar
- Navigation (defun, things, Xref, tree as submenu and as side panel buffer)



### Syntax highlight and font lock faces

The mode has four levels of font-locking, which is controlled by customizable variable `treesit-font-lock-level`.

 - level 1: comment, preprocessor,
 - level 2: builtin, keyword, string, type,
 - level 3: constant, number,
 - level 4: function, variable, operator, bracket, delimiter.

Additionally to the usual faces, there are some extra custom faces:

| Face Name                                 | Description                                             |
|-------------------------------------------|---------------------------------------------------------|
| `f90-ts-font-lock-delimiter-face`         | delimiters such as commas and separators                |
| `f90-ts-font-lock-bracket-face`           | brackets and parentheses                                |
| `f90-ts-font-lock-operator-face`          | operators and assignments                               |
| `f90-ts-font-lock-openmp-face`            | openmp directives                                       |
| `f90-ts-font-lock-special-var-face`       | special variables (e.g. `self`, `this`)                 |
| `f90-ts-font-lock-separator-comment-face` | separator comments like `!---------` and `! arguments`) |


#### Special variables

Special variables are recognised by regexp matching with customizable variable `f90-ts-special-var-regexp`.
For matched variables `f90-ts-font-lock-special-var-face` is used.


#### Comment regexps and rules

Comments can be classified with different syntax highlighting (and indentation) by rules and regexps.

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

Indentation supports most fortran statements. It also covers some error cases which frequently happen during typing.
Lines within incomplete and unfinished blocks are mostly correctly inlined,
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
- `f90-ts-indent-toplevel` is intended to reduce the indentation of anything which is right below the program
  or (sub)module level. This is done as this indentation level usually does not improve readability,
  as almost everything except for very few lines (like `module`, `contains` and `end` line) is indented.


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
the smallest column of arguments in the example above, or the column just right to the opening parenthesis).
The last three options `rotate`, `keep-or-primary` and `keep-or-next`, which collect and offer several
alignment columns, always include the continued line position among the set of columns.

Behaviour of indentation of a region and of a line are controlled by `f90-ts-indent-list-region`
and `f90-ts-indent-list-line`, respectively.
There are also variants `f90-ts-indent-list-line-2` and `f90-ts-indent-list-line-3`, which are
used by functions bound to `<backtab>` (S-`TAB`) and `C-S-<iso-lefttab>` / `C-<backtab>` by default.

Also check out [Continued statements and blocks](#indentation-of-continued-statements-and-blocks).

Remark: three variants are offerend to allow selection of primary and continued line offset additionally
to the rotation option. The current setup offers keybindings for all three variants.



#### OpenMP and other special comments

For special comments such as openmp statements, separator comments and documentation (like ford
and doxygen documentation with comment starters like `!>` and `!<` or similar)
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
(:name "separator comment rule"
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


### Indentation of continued statements with leading ampersand

Leading ampersand in continued lines is optional. By setting `f90-ts-leading-ampersand` to non-nil,
indentation operations (and break line operation) insert a leading ampersand where applicable.
Indentation is done as if the leading ampersand is not present.
The leading ampersand can be placed at an absolute column or relative to indentation of the continued statement.
This is controlled by `f90-ts-leading-ampersand-style`.


### Indentation of continued statements and blocks

If at an end statement of a block like a subroutine, control statement etc., the whole block can be indented
automatically much like indent region.
If within a multiline statement, the continued statement from its first line up to point can also be intended
like an indent region.

This is done by `f90-ts-indent-and-complete-stmt`, which is bound to `C-<tab>` and to `s` in the transient popup.



### Indentation of statement labels

Statements after statement labels are indented as if there is no label present.
For the label itself there are two options. Either they are left-adjusted starting at a fixed
column number, or right-adjusted with last digit of the label at the fixed column number.
If there is not enough space, the statement itself is moved to the right to ensure at least one
blank after the label.
This is controlled by custom variable `f90-ts-stmt-label-column'.


### Xref

The mode provides a minimal buffer local implementation of xref functions. In particular, the following
functions can be used to find definitions and references of symbols. Default Emacs xref keybindings
apply, and all are also accessible via the transient popup (`C-c C-f`):

| Function                | Default | Popup | Description                  |
|-------------------------|---------|-------|------------------------------|
| `xref-find-definitions` | `M-.`   | `.`   | Jump to definition           |
| `xref-find-references`  | `M-?`   | `,`   | Find all references          |
| `xref-find-apropos`     | `C-M-.` | `/`   | Find symbols matching regexp |
| `xref-go-back`          | `M-,`   | `<`   | Pop back                     |
| `xref-go-forward`       | `C-M-,` | `>`   | Go forward                   |


### Imenu

The mode provides an Imenu implementation via `f90-ts-simple-imenu`.
Entries are grouped by `module`, `submodule`, `subroutine`, `function`, `module procedure`,
`derived type`, `interface` and `variable`.


### Navigation menu

Additionally to the Imenu grouping, the `Fortran` menu offers a submenu where the same imenu
items are structured as a tree reflecting the hierarchical structure of the source file,
with submenus for structures that contain other items.


### Navigation buffer

The navigation buffer provides a persistent side panel showing the structure of the current
Fortran source buffer. It is based on the same tree as offered in
the [Navigation menu](#navigation-menu) and a sparse version of the tree-sitter tree.

It can be opened with `f90-ts-nav-buffer-open` (`b` in the transient popup) and focused with
`f90-ts-nav-buffer-focus` (`f` in the transient popup).

The panel prints the tree, reflecting the nesting of program units.
Entries are colour-coded by kind using customizable faces:

| Face                        | Used for                                      |
|-----------------------------|-----------------------------------------------|
| `f90-ts-nav-module-face`    | `program`, `module`, `submodule`              |
| `f90-ts-nav-procedure-face` | `subroutine`, `function`, `interface`         |
| `f90-ts-nav-type-face`      | derived type definitions                      |
| `f90-ts-nav-variable-face`  | variable declarations                         |

Automatic synchronisation of the highlighted entry in the side panel with active source buffer is
controlled by `f90-ts-nav-buffer-auto-sync`. The sync is debounced via an idle timer
(see `f90-ts-nav-buffer-idle-delay`), so that rapid cursor movement does not cause
excessive updates.

The nav buffer is automatically refreshed after edits, also after the idle delay.

Keybindings in the navigation buffer:

| Key     | Function                        |
|---------|---------------------------------|
| `RET`   | Jump to entry in source buffer  |
| `SPC`   | Preview entry without leaving   |
| `n`/`p` | Move to next/previous entry     |
| `g`     | Refresh the navigation buffer   |
| `q`/`C-g` | Quit the navigation buffer    |


Note: the navigation buffer is still missing some features to be really useful.



### Breaking and joining lines

Continued lines can be created by breaking a line or reduced
by joining two consecutive lines connected by continuation symbol `&`.
Break and join operations are also useful for deleting empty lines and
breaking or joining comments with a common comment starter.

The line break function is bound to `C-<return>`.
All break and join functions are available via the transient popup (`C-c C-f`)
under keys `RET`, `j` and `J`.
These operations are inspired by the legacy f90 mode, but behave a bit differently.

#### Breaking lines

The function `f90-ts-break-line` breaks the current line at point and adds continuation symbols.
If the current line is a comment,
then the comment starter (like '!>', '!<' or similar) is extracted, including indentation offset after the
comment starter, and inserted into the new line to continue the comment.
The comment starter is found by regexp `f90-ts-comment-prefix-regexp`, which can be customized if necessary.

Whether a leading ampersand at the start of the new line is inserted is controlled by option
`f90-ts-leading-ampersand`.


#### Joining lines

Two consecutive lines connected by continuation symbol `&` can be joined by
`f90-ts-join-line-prev` and `f90-ts-join-line-next`.
The ampersand(s) in between are removed.
One whitespace character is left after putting the second line at the end of the first line.

If there are empty lines, then only the empty lines are removed, without joining the lines.
A second join operation can be used to actually join the lines. 

If there are comments at end of first line or in between the two lines, joining is not possible,
as it is not quite clear what should be done with such comments.

If point is on an empty line (not necessarily within a continued statement),
then previous (prev variant) or subsequent (next variant) empty lines are removed,
but nothing is joined in any case.

The `prev` variant joins current line with previous (non-empty) line.
The `next` variant joins current line with next (non-empty) line.

Current limitations are:
* Comments within string literals are not supported by the tree-sitter grammar itself.
Thus joining such strings is not possible anyway. Continued strings can be joined.
* Joining of openmp statements is not yet implemented.


### Comment region

This is also a feature inspired by the legacy f90 mode, but with a number of extensions.
The selected region is (un)commented with a default or with a selectable prefix from a customizable list.
Both functions are accessible via the transient popup (`C-c C-f`) under keys `c` and `C`, and also
have legacy-style direct bindings:

```elisp
(define-key f90-ts-mode-map (kbd "C-c ;") #'f90-ts-comment-region-default)
(define-key f90-ts-mode-map (kbd "C-c '") #'f90-ts-comment-region-custom)
```

Default prefix `f90-ts-comment-region-prefix` and extra prefixes
`f90-ts-extra-comment-prefixes` can be customized as desired.
Standard prefixes like `!!$ `, `!$omp `, `!> ` and some others are predefined.
Note that a trailing blank in the prefix to separate prefix from the code (as is often done)
must be provided explicitly.

Indentation of prefixes can be controlled by special comment rules,
see [Special comments](#openmp-and-other-special-comments).
With `f90-ts-comment-prefix-keep-indent` it is possible to control
whether indentation of commented code should be kept,
or whether the prefix should be inserted at the proper column without removing
any blanks to keep original indentation of commented code.


### Mark region based on tree-sitter nodes

Regions can be selected, enlarged, shrunk or moved based on tree-sitter nodes.
Blocks of comments with the same comment prefix are identified and dealt with like
the block would be represented by a node (which they are not).
This is particularly useful in conjunction with comment prefixes and
comment region operations.

Key bindings are provided in the transient popup (`C-c C-f`) under the Region section:

| Function                                | Popup key | Description                                                              |
|-----------------------------------------|-----------|--------------------------------------------------------------------------|
| `f90-ts-mark-region-enlarge`            | `r`       | Find smallest parent node of existing region which is strictly larger    |
| `f90-ts-mark-region-shrink-child-first` | `0`       | Shrink region to a first (grand)child which is strictly smaller          |
| `f90-ts-mark-region-prev-sibling`       | `[`       | Move selected region to previous sibling                                 |
| `f90-ts-mark-region-next-sibling`       | `]`       | Move selected region to next sibling                                     |


## Development and Testing

In the `test` folder, two modules for testing and logging during development are provided.
Tests are located in `test/resources`.

### Logging

The following logging functions are provided by `test/f90-ts-log.el`.

* `f90-ts-log-msg`
* `f90-ts-log-clear`
* `f90-ts-log-show`
* `f90-ts-log-inspect-node`
* `f90-ts-log-indent-print-state` (used by `log-state` in indentation rules)

These can be loaded and used by adding the load path to the test direction in `use-package`
and add a require in the config section:
```
  ...
  :init
  (require 'treesit)
  (add-to-list 'load-path "path_to/emacs-f90-ts-mode/test")
  ...
  :config
  (require 'f90-ts-log)
  ...
```

All logging and inspection functions write into a dedicated log buffer `*f90-ts-log*`
with its own minor mode to allow some dedicated keybindings.

By default nothing is logged and the buffer is empty.


### Testing with ERT

The mode comes with a number of tests in `test/resources`.
Registering and running tests is done in `test/f90-ts-mode-test.el`

Standard tests are named `f90-ts-mode-test-std--...`, whereas additional tests start with `f90-ts-mode-test-extra--...`.
Registering of tests is done semi-automatic in `f90-ts-mode-test-indent-register`, `f90-ts-mode-test-font-lock-register`
and other functions in `f90-ts-mode-test.el`.

Tests are run with a prescribed set of custom variables. In particular, indentation values are chosen
all differently, such that errors can be spotted more easily. Within erts files, custom variable can
and sometimes are overwritten to allow testing various aspects of the mode.


#### Makefile

There is a Makefile for running various tests. Relevant targets are:
* test-checkdoc
* test-byte-compile
* test-ert-std
* test-ert-extra
* test-ert-all
* test-ert-parallel (use with `make -j<N> test-ert-parallel` to run all ert tests in parallel)

During development if functions from `f90-ts-log.el` are used, testing fails as the log package
is not loaded (see [Logging](#logging)). To avoid this and suppress logging, the make command should be invoked by
```bash
make test-target-name DEV=1
```
which turns the log functions into no-op operations.



#### Indentation tests

Indentation tests are in erts file format. For indentation of an after part between `=-=` and `=-=-=`,
function `f90-ts-mode-test-update-erts-after` can be used (first remove point char `|` if present) to
help setting up a new test. Indentation tests have prep-fn preparation function (like remove indentation)
and an action function (like indent-region, indent-line all/single line).
The erts files are used to check various combinations.


#### Font lock tests

Font lock tests are in f90 files, with caret assertion notation. This notation can be automatically
generated for new files or updated by `f90-ts-mode-test-update-face-annotations`.
The code is automatically indented by 1, as assertion lines start with a !, so that all faces
including those which would be at column 0 can be checked properly.
The generated annotations are exhaustive, including nil annotations to assert that part of the code
is not highlighted.

Note: fortran test code should NOT use the caret `^`, even in comments, as the ert parser gets confused.


#### Other tests

There are a number of other tests, like tests for region and navigation operations, or with custom
action blocks for testing specific aspects not easily covered by the whole-buffer operations.
