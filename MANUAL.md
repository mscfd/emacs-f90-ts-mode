# f90-ts-mode User Manual

This manual provides a comprehensive overview and technical documentation for **f90-ts-mode**,
a major mode for editing **Fortran 90 / Fortran 2003** (and newer) based on Emacs's built-in **Tree-sitter** support.

The mode is still under **development**. The [Roadmap](README.md#roadmap) lists missing or incomplete feature planned
for implementation.

[Back to README](README.md)


## Table of Contents

- [Installation](#installation)
  - [Tree-sitter grammar](#tree-sitter-grammar)
    - [Which grammar revision to use](#which-grammar-revision-to-use)
    - [Installing the grammar in Emacs](#installing-the-grammar-in-emacs)
    - [Tree-sitter library and ABI versions](#tree-sitter-library-and-abi-versions)
  - [Tree-sitter based mode](#tree-sitter-based-mode)
  - [Setup](#setup)
    - [Standard installation via package manager](#standard-installation-via-package-manager)
    - [Optional packages](#optional-packages)
    - [Development setup via local clone](#development-setup-via-local-clone)
    - [Loading test package](#loading-test-package)
- [Keybindings](#keybindings)
- [Features](#features)
  - [Syntax highlight and font lock faces](#syntax-highlight-and-font-lock-faces)
    - [Syntax highlighting of error regions](#syntax-highlighting-of-error-regions)
    - [Special variables](#special-variables)
    - [Comment regexps and rules](#comment-regexps-and-rules)
  - [Indentation](#indentation)
    - [Indentation of multiline statement](#indentation-of-multiline-statement)
    - [OpenMP and other special comments](#openmp-and-other-special-comments)
  - [Smart end completion](#smart-end-completion)
  - [Indentation of continued statements with leading ampersand](#indentation-of-continued-statements-with-leading-ampersand)
  - [Indentation of continued statements and blocks](#indentation-of-continued-statements-and-blocks)
  - [Indentation of statement labels](#indentation-of-statement-labels)
  - [Abbreviations](#abbreviations)
  - [Xref](#xref)
  - [Imenu](#imenu)
  - [TOC buffer](#toc-buffer)
  - [Comment prefix](#comment-prefix)
  - [Breaking and joining lines](#breaking-and-joining-lines)
    - [Breaking lines](#breaking-lines)
    - [Joining lines](#joining-lines)
    - [Shifting line break](#Shifting-line-break)
  - [Filling lines and regions](#filling-lines-and-regions)
  - [Comment region](#comment-region)
  - [Mark region based on tree-sitter nodes](#mark-region-based-on-tree-sitter-nodes)
    - [Extending and reducing a region by siblings](#extending-and-reducing-a-region-by-siblings)
  - [Folding](#folding)
- [Development and Testing](#development-and-testing)
  - [Logging](#logging)
  - [Testing with ERT](#testing-with-ert)
    - [Makefile](#makefile)
    - [Indentation tests](#indentation-tests)
    - [Font lock tests](#font-lock-tests)
    - [Custom tests](#custom-tests)



## Installation

### Tree-sitter grammar

The mode relies on the Fortran grammar
[stadelmanma/tree-sitter-fortran](https://github.com/stadelmanma/tree-sitter-fortran)
(the upstream grammar).

The fork [mscfd/tree-sitter-fortran](https://github.com/mscfd/tree-sitter-fortran) might contain fixes and
extensions not yet merged upstream. It also provides tags like `emacs-f90-ts-mode-v0.6.x` to pin
grammar version compatible with corresponding f90-ts-mode versions. All minor releases (only changing the `x`)
are compatible with the same grammar tag.


#### Which grammar revision to use

Grammar changes can alter the tree structure, so a given version of the mode is only guaranteed to work
with certain grammar revisions.
For reproducible results, use the **tag** matching your mode version, for example
`emacs-f90-ts-mode-v0.6.x` for f90-ts-mode 0.6.x. This tag marks the grammar revision, which is tested against
all minor revisions including snapshots (like all `0.6.x` and `0.6.x-snapshot` commits on branch `main`, which
are published on melpa).
The `master` branch of the fork follows development and is tested in CI against
the development version of the mode.
If possible, the mode tries to be backwards compatible, so an older grammar version might work as well.


#### Installing the grammar in Emacs

Register the grammar repository in `treesit-language-source-alist`.
The optional third element is the revision (branch or tag):

```elisp
;; upstream grammar
(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((fortran "https://github.com/stadelmanma/tree-sitter-fortran"))))
```
or, grammar revision matching the mode version:
```elisp
(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((fortran "https://github.com/mscfd/tree-sitter-fortran" "emacs-f90-ts-mode-v0.6.x"))))
```

Then compile and install the grammar once with:

```elisp
M-x treesit-install-language-grammar RET fortran RET
```

The installation can be verified with:

```elisp
(treesit-ready-p 'fortran)
```

#### Tree-sitter library and ABI versions

Emacs links against the tree-sitter C library, whose version determines the range of grammar ABI versions
that can be loaded. `treesit-install-language-grammar` compiles the `src/parser.c` found in the grammar
repository; it does **not** run `tree-sitter generate`. The ABI of the committed `src/parser.c`
must therefore lie within the range supported by your Emacs. Otherwise regenerate the parser, e.g. with
`tree-sitter generate --abi 14` (or a `tree-sitter-cli` release producing that ABI), and rebuild.

| Library | ABI range | Typical setup                            |
|---------|-----------|------------------------------------------|
| 0.20.x  | 13-14     | Emacs 29.x                               |
| 0.24.x  | 13-14     | Emacs 30.x                               |
| 0.25.x  | 13-15     | Emacs 30.x                               |
| 0.26.x  | 13-15     | Emacs 31.x                               |

Combinations tested in CI (see `.github/workflows`):

| Emacs | tree-sitter library | grammar ABI |
|-------|---------------------|-------------|
| 29.1  | 0.20.8              | 14          |
| 29.3  | 0.20.8 (apt)        | 14          |
| 30.1  | 0.24.7              | 14          |
| 30.1  | 0.25.10             | 15          |
| 30.2  | 0.24.7              | 14          |
| 30.2  | 0.25.10             | 15          |
| 31.1  | 0.26.11             | 15          |

Each is tested with the current `master` and the compatibility tag of the grammar fork.
Other combinations may work but are not tested. In particular, Emacs 30.x with tree-sitter 0.26 is not
covered; if it causes problems, use 0.25.x.

The following can be used to check the versions of a running Emacs (`M-:` = `eval-expression`):

- `(treesit-library-abi-version)` gives the maximum ABI supported by the library
  (`(treesit-library-abi-version t)` the minimum)
- `(treesit-language-abi-version 'fortran)` gives the ABI of the installed grammar; it must be within that range
- `ldd bin_path_to_emacs/emacs | grep libtree-sitter` shows the linked library
  (e.g. `libtree-sitter.so.0.20` for Emacs 29.x from distribution packages)

**NOTE** (Emacs 29.x): there are a number of workarounds for bugs in the tree-sitter core library 0.20.x.
These might reduce performance in some cases. Please open an issue if anything fails or performance is bad.


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

Example setups provided below should be adjusted to ones requirements.

#### Standard installation via package manager

```elisp
(use-package f90-ts-mode
  :ensure t
  :mode (("\\.f90\\'" . f90-ts-mode)
         ;; preprocessor files
         ("\\.i90\\'" . f90-ts-mode))

  :init
  (require 'treesit)

  ;; uncomment if Imenu entry in menu bar is desired
  ;; :hook (f90-ts-mode . (lambda () (imenu-add-to-menubar "Imenu")))

  ;; uncomment to switch on a minor mode automatically
  ;; :hook (f90-ts-mode . hs-minor-mode)
  ;; :hook (f90-ts-mode . outline-minor-mode)
  ;; :hook (f90-ts-mode . abbrev-mode)

  :config
  (message "f90-ts-mode loaded")

  :bind (;; mode-specific bindings, adjust to your needs by
         ;; changing or adding new key bindings
         ;; (below are just some examples)
         :map f90-ts-mode-map
         ;; transient popup (additional shorter binding to "C-c C-f")
         ("A-<up>"        . #'f90-ts-transient)

         ("A-<return>"    . #'f90-ts-break-line)
         ("C-<return>"    . #'f90-ts-shift-line-break)
         ("A-<backspace>" . #'f90-ts-join-line-prev)
         ("A-<delete>"    . #'f90-ts-join-line-next)
         ("A-C-f"         . (lambda () (interactive)
                              (let ((f90-ts-fill-select-breakpoint-by 'interactive))
                                (f90-ts-fill-at-line))))

         ("A-\\"          . #'f90-ts-mark-region-enlarge)
         ("A-0"           . #'f90-ts-mark-region-shrink-child-first)
         ("A-9"           . #'f90-ts-mark-region-shrink-child-last)
         ("A-{"           . #'f90-ts-mark-region-first-sibling)
         ("A-["           . #'f90-ts-mark-region-prev-sibling)
         ("A-]"           . #'f90-ts-mark-region-next-sibling)
         ("A-}"           . #'f90-ts-mark-region-last-sibling)

         ;; extend or reduce the active region by siblings
         ;; (no default bindings outside of the transient popup)
         ("A-M-{"         . #'f90-ts-mark-region-extend-first-sibling)
         ("A-M-["         . #'f90-ts-mark-region-extend-prev-sibling)
         ("A-M-]"         . #'f90-ts-mark-region-extend-next-sibling)
         ("A-M-}"         . #'f90-ts-mark-region-extend-last-sibling)))
```

#### Optional packages

A table-of-content (TOC) side panel showing the structure of the source buffer
can be enabled by loading the optional package `f90-ts-toc`
(see [TOC buffer](#toc-buffer)):

```elisp
(use-package f90-ts-toc
  :after f90-ts-mode
  ;; demand loading after f90-ts-mode, otherwise entries for TOC functions
  ;; do not show up in the fortran menu and the transient popup
  :demand t)
```


#### Development setup via local clone

First clone the repository of `f90-ts-mode` as mentioned above.
Then modify the use-package block:

```elisp
(use-package f90-ts-mode
  :ensure nil
  :load-path "path_to/emacs-f90-ts-mode"
  ...

  :config
  ;; only required for logging
  (require 'f90-ts-log)
  (message "f90-ts-mode loaded")

  ...
```

#### Loading test package

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
(defvar-keymap f90-ts-mode-map
  ;; TAB commands
  "C-<tab>"           #'f90-ts-indent-and-complete-stmt
  "<backtab>"         #'f90-ts-indent-for-tab-command-2 ; S-<tab>
  "C-S-<iso-lefttab>" #'f90-ts-indent-for-tab-command-3 ; Linux
  "C-<backtab>"       #'f90-ts-indent-for-tab-command-3 ; Windows?

  ;; other keybindings inspired by f90-mode
  "C-<return>" #'f90-ts-break-line
  "C-c ;"      #'f90-ts-comment-region-default
  "C-c '"      #'f90-ts-comment-region-custom

  ;; C-c C-f prefix for the transient menu
  "C-c C-f" #'f90-ts-transient
  "C-`"     #'f90-ts-abbrev-menu)
```

#### Transient popup (`C-c C-f`)

Pressing `C-c C-f` opens a transient popup with the most frequently used commands.
Less frequently used commands are grouped in sub-menus (`C-g` returns to the parent menu).
Each sub-menu is also an ordinary command and can be bound directly.

| Section                | Keys                    | Commands                                                |
|------------------------|-------------------------|---------------------------------------------------------|
| **Indentation**        | `TAB` `s` `I`           | Indent and complete line / statement / region           |
|                        | `C-TAB` `C-I`           | Indent line / region                                    |
| **Line editing**       | `b` `j` `J` `C-s`       | Break line, join with prev/next, shift line break       |
| **Filling**            | `C-f` `C-b`             | Set fill column and fill method                         |
|                        | `f` `M-f` `M-j` `M-J`   | Fill region, line, with prev/next line                  |
| **Mark region**        | `r` `0` `9`             | Enlarge, shrink to first / last child                   |
|                        | `{` `[` `]` `}`         | First, prev, next and last sibling                      |
|                        | `M-{` `M-[` `M-]` `M-}` | Extend/reduce region to first, prev, next, last sibling |
|                        | `X`                     | Exchange mark and point                                 |
| **Comment region**     | `c` `C`                 | Default and custom prefix                               |
| **Procedure**          | `a` `e` `p` `n`         | Beginning, end, previous, next                          |
| **Xref**               | `.` `,` `/` `<` `>`     | Definitions, references, apropos, back, forward         |
| **More...**            | `t`                     | Sub-menu: derived types (`a e p n`), interfaces (`A E P N`) |
|                        | `h`                     | Sub-menu: hideshow (only if `hs-minor-mode` is active)  |
|                        | `o`                     | Sub-menu: outline (only if `outline-minor-mode` is active) |
|                        | `m`                     | Sub-menu: About, README, MANUAL (`a r m`), TOC side panel (`B F`, if `f90-ts-toc` is loaded) |


The entire transient popup can be bound to a different prefix with a use-package statement
in the `:bind` section as shown above, or simply by:

```elisp
(define-key f90-ts-mode-map "..." #'f90-ts-transient)
(keymap-unset f90-ts-mode-map "C-c C-f") ; remove default
```


## Features
(with inspiration from the legacy f90 mode in emacs)

- Almost all statements up to F2023
- Syntax highlighting (font lock faces)
- Indentation of lines, regions, multiline statements and structure blocks
- Alignment for multiline statements with rotation and other options
- Smart end completion
- Configurable leading ampersand and statement label positions
- Break line with automatic continuation and comment starters for comment lines
- Join with previous and next line
- Fill and rebalance operations for lines or regions (with rightmost breakpoint
  selection or interactive break and join session)
- Region selection based on tree-sitter nodes
- (Un)commenting regions with configurable prefixes and indentation rules
- Special comments like doc strings and separators
  (syntax highlighting and indentation options)
- Keyword highlighting in comments (like TODO, Remark etc.)
- OpenMP and preprocessor directives
- Coarray keywords and statements
- Imenu and a Fortran menu in the menu bar
- Navigation (defun, things, Xref)
- TOC side panel to show interactive table of content for a f90-ts buffer (optional package)
- Hideshow, outline and treesit-fold support
- Abbreviations via abbrev-mode or via a transient menu



### Syntax highlight and font lock faces

The mode has four levels of font-locking, which is controlled by customizable variable `treesit-font-lock-level`.

 - level 1: comment, preprocessor, error,
 - level 2: builtin, keyword, string, type,
 - level 3: constant, number escape-sequence,
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
| `f90-ts-font-lock-escape-face`            | for escaped quotes within strings                       |
| `f90-ts-font-lock-error-face`             | additional properties for errors                        |


#### Syntax highlighting of error regions

The syntax highlighting of error regions (spanned by an `ERROR` node) can be enabled or disabled by
`f90-ts-font-lock-error-show`. If nil, it is switched off. If it equals symbol `all`, the whole span
of the error node is fontified. Otherwise it should contain a positive integer for the number of lines,
to which the fontified span is limited, in case it spans more lines than the given maximum.
This is done in case error recovery of the tree-sitter parser does not work well and there are ERROR
nodes with a big span.


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

Additionally `f90-ts-indent-delete-trailing-whitespace` can be used to enable automatic deletion of
trailing whitespace characters of indented lines after each indentation operation.

*Remarks:*
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
Six options are currently implemented: `continued-line`,  `primary`, `rotate`, `keep-or-continued-line`,
`keep-or-primary` and `keep-or-rotate`. Primary column is some outstanding column with respect to the context (like
the smallest column of arguments in the example above, or the column just right to the opening parenthesis).
The options `rotate`, `keep-or-primary` and `keep-or-rotate`, which collect and offer several
alignment columns, always include the continued line position among the set of columns.

Behaviour of indentation of a region and of a line are controlled by `f90-ts-indent-list-region`
and `f90-ts-indent-list-line`, respectively.
There are also variants `f90-ts-indent-list-line-2` and `f90-ts-indent-list-line-3`, which are
used by functions bound to `<backtab>` (S-`TAB`) and `C-S-<iso-lefttab>` / `C-<backtab>` by default.

Also check out [Continued statements and blocks](#indentation-of-continued-statements-and-blocks).

*Remark:* three variants are offerend to allow selection of primary and continued line offset additionally
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

*Remark:* Indentation hints `context` and `indented` are ignored if comment is within a continued line.
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
This is controlled by custom variable `f90-ts-stmt-label-column`.


### Abbreviations

Emacs provides an abbrev-mode, which can be enabled by hand or automatically at load time, for example
in the use-package setup.
The mode provides abbreviations for many keywords. If the mode is enabled, typing "\`" followed by one or
two letters and then some other key inserts the corresponding keyword. Example: "\`su " inserts "subroutine ".
Abbreviations can be discovered by typing `` C-` ``, which opens a transient menu, showing the same
two letter codes. There are a few one letter codes: "c", "i", "r" and "t". These are shown as
"cc", "ii", "rr" and "tt" in the transient menu, to avoid common prefixes.

New abbreviations can be added through defcustom variable `f90-ts-extra-abbrevs`. This also allows
to overwrite default abbrevs.


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


### TOC buffer

The optional package `f90-ts-toc` provides a TOC buffer: a persistent side panel showing the
structure of the current Fortran source buffer as a table of content tree. The tree is based on a
sparse version of the tree-sitter tree, restricted to relevant nodes, and reflects the nesting of
program units. The panel is used to get an overview, to jump to entries, and to mark, comment or
narrow to whole structures.

The package must be loaded explicitly, see [Optional packages](#optional-packages).
It can be opened with `f90-ts-toc-buffer-open` (`B` in the transient popup, also in the fortran
menu) and focused with `f90-ts-toc-buffer-focus` (`F` in the transient popup). Opening the panel
never changes the source buffer. Pressing `q` (or `C-g`) in the panel closes it, kills the TOC
buffer and stops all background activity.

#### Entries

The following entries are shown, each with a short label (see `f90-ts-toc-short-labels`) and the
name of the entity:

| Entry                                              | Label (short) | Face                        |
|----------------------------------------------------|---------------|-----------------------------|
| `program`, `module`, `submodule`                   | `prog` `mod` `smod` | `f90-ts-toc-module-face`    |
| `subroutine`, `function`, `module procedure`      | `sub` `fun` `proc`  | `f90-ts-toc-procedure-face` |
| `interface` blocks (also without a generic name)   | `ifc`         | `f90-ts-toc-procedure-face` |
| procedure statements inside interface blocks       | `proc`        | `f90-ts-toc-procedure-face` |
| derived type definitions                           | `type`        | `f90-ts-toc-type-face`      |
| variable declarations                              | `var`         | `f90-ts-toc-variable-face`  |
| `contains` separator                               |               | `f90-ts-toc-separator-face` |

Procedure statements are only listed inside interface blocks, type-bound procedures are not shown.
Entries are indented according to their nesting depth. If a module, submodule, program, procedure or
type has entries after its `contains` keyword, a separator line with the text of
`f90-ts-toc-contains-label` (default `---contains---`) is inserted. Set it to `nil` to omit the separator.

#### Synchronisation with the source buffer

The panel follows the selected window. A single idle timer (delay `f90-ts-toc-buffer-idle-delay`,
default 0.5 s) takes care of

- switching the panel to the current `f90-ts-mode` buffer, if another source buffer is selected,
- refreshing the tree after edits in the source buffer,
- moving point in the panel to the entry belonging to point in the source buffer, if
  `f90-ts-toc-buffer-auto-sync` is non-nil.

The timer only runs while the TOC buffer exists and does nothing unless the panel is visible,
so the panel is never opened automatically. Because everything is debounced by the idle timer,
rapid cursor movement does not cause excessive updates. A changed idle delay takes effect when
the panel is opened again. The tree can also be rebuilt explicitly with `g`.

The entry belonging to a position is the closest entry whose start is on or before the position.
Variable entries and procedure statements in interfaces are only associated with positions on
their own source line.

#### Keybindings in the TOC buffer

Pressing `?` in the panel opens a transient menu which lists all commands. Every key also works
without opening the menu first.

| Key              | Function                                                                  |
|------------------|---------------------------------------------------------------------------|
| `RET`            | Jump to entry in source buffer                                            |
| `SPC`            | Preview entry: show it in the source window and flash its line, stay in panel |
| `n` / `p`        | Next / previous line                                                      |
| `m`              | Mark / unmark the node of the entry in the source buffer                  |
| `M`              | Mark / unmark the node including preceding doc comments                   |
| `c`              | Comment / uncomment the node with the default prefix                      |
| `C`              | Comment / uncomment the node with a prefix chosen from a list             |
| `w`              | Narrow the source buffer to the node / widen again                        |
| `v`              | Toggle display of variable entries                                        |
| `i`              | Toggle display of interface entries                                       |
| `d`              | Toggle doc comments belonging to an entry                                 |
| `a`              | Toggle abbreviated labels                                                 |
| `t`              | Cycle shortening of long names (`truncate`, `left`, `right`)              |
| `s`              | Toggle panel between left and right side of the frame                     |
| `g`              | Rebuild the TOC buffer                                                    |
| `?`              | Open the transient menu                                                   |
| `q` / `C-g`      | Quit the panel                                                            |

Mouse: a click previews the entry, a double click or middle click jumps to it, and a right click
opens a context menu with the same commands.

#### Node operations

The operations `m`, `M`, `c`, `C` and `w` act on the tree-sitter node of the entry at point,
trimmed in the same way as in the [mark region](#mark-regions-based-on-tree-sitter-nodes) operations.
They signal an error if the source buffer was modified since the TOC buffer was last rendered
(refresh with `g`) or if the node is outside of the accessible (narrowed) part of the source buffer.

- *Mark* selects the node in the source buffer and, as a toggle, deactivates the mark again.
  `M` extends the region to the start of the doc comments in front of the node.
- *Comment* (un)comments the node using the [comment region](#comment-region) rules.
  `c` uses the default prefix `f90-ts-comment-region-prefix`. `C` prompts for a prefix from
  `f90-ts-extra-comment-prefixes` and the default prefix, like `f90-ts-comment-region-custom`.
- *Narrow* narrows the source buffer to the node and puts point at its start. If the source buffer is
  already narrowed to exactly this node, it is widened. On a variable entry the structure the variable
  is declared in is used. Narrowing to a structure inside the narrowed region narrows further.

While the source buffer is narrowed, all entries stay visible, but entries outside of the narrowed
region are dimmed with `f90-ts-toc-dimmed-face`. Moving point (or clicking) to a dimmed line
*walks* in the tree: the source buffer is widened and narrowed to the structure of that line.
This allows to drill down or step up through modules, procedures and types by just moving in the panel.

#### Doc comments

If `f90-ts-toc-include-doc-comments` is non-nil (default), comments directly in front of a structure
(subroutine, function, type, interface, ...) belong to its entry. This aims at documentation comments
such as `!>` used by doxygen or FORD. The comment immediately preceding the structure always counts,
even if separated by an empty line; further comments before it only count if there is no empty line
between them. Point in such a comment is then synchronised to the entry, and narrowing includes the
doc comments. Variable entries have no doc comments. Marking with (`M`) and without (`m`) doc comments
is always available. Toggle with `d`.

#### Customization

All options are in the customization group `f90-ts-toc` (`M-x customize-group RET f90-ts-toc`).

| Option                                 | Default          | Description                                                                          |
|----------------------------------------|------------------|--------------------------------------------------------------------------------------|
| `f90-ts-toc-buffer-side`               | `left`           | Side of the frame (`left` or `right`); toggle with `s`                               |
| `f90-ts-toc-buffer-width`              | `40`             | Width of the panel                                                                   |
| `f90-ts-toc-buffer-auto-sync`          | `t`              | Point in the panel follows point in the source buffer                                |
| `f90-ts-toc-buffer-idle-delay`         | `0.5`            | Idle time in seconds before the panel is updated                                     |
| `f90-ts-toc-show-variables`            | `t`              | Show variable declarations; toggle with `v`                                          |
| `f90-ts-toc-show-interface-procedures` | `t`              | Show interface blocks; toggle with `i`                                               |
| `f90-ts-toc-include-doc-comments`      | `t`              | Preceding doc comments belong to an entry; toggle with `d`                           |
| `f90-ts-toc-abbreviate-labels`         | `t`              | Use short labels; toggle with `a`                                                    |
| `f90-ts-toc-short-labels`              | see above        | Alist from entry kind to short label                                                 |
| `f90-ts-toc-name-shortening`           | `truncate`       | How names not fitting into the panel are shown: `truncate`, `left`, `right`; cycle with `t` |
| `f90-ts-toc-ellipsis`                  | `"…"`            | Single character marking a cut name for `left` and `right`                           |
| `f90-ts-toc-contains-label`            | `---contains---` | Text of the `contains` separator, `nil` for none                                     |

With `left` or `right`, long names are shortened from that side and the cut is marked by
`f90-ts-toc-ellipsis`, so that the interesting end of a name stays visible
(e.g. `…me_which_does_not_fit` or `very_long_name_which_…`). The panel is re-rendered if its
window width changes. The full name is shown as tooltip.

Changes of `f90-ts-toc-buffer-side` and `f90-ts-toc-buffer-idle-delay` take effect when the panel is
opened again, the toggle command `s` moves a visible panel immediately.
The faces `f90-ts-toc-module-face`, `f90-ts-toc-procedure-face`, `f90-ts-toc-type-face`,
`f90-ts-toc-variable-face`, `f90-ts-toc-separator-face` and `f90-ts-toc-dimmed-face` can be customized
as usual.


### Comment prefix

Various operations, in particular break, join, fill and mark operations require to match and extract
comment prefixes.
The comment prefix is found by regexp `f90-ts-comment-prefix-regexp` in combination with separator
`f90-ts-comment-prefix-separator-regexp`, which can be customized if necessary.

To be precise the comment prefix is matched and extracted by match groups 1, 2 or 3 of
```elisp
  (concat "^\\(?:"
          "\\(" f90-ts-openmp-prefix-regexp "\\)\\(?:\\s-\\|$\\)"
          "\\|\\(" f90-ts-comment-prefix-regexp "\\)"
          (when f90-ts-comment-prefix-separator-regexp
            (concat "\\(?:" f90-ts-comment-prefix-separator-regexp "\\|$\\)"))
          ;; if nothing matches, then extract the leading comment starter
          "\\|\\(!\\)"
          "\\)"))
```
where the single `!` is always matched to cover all not yet matched prefixes.


### Breaking and joining lines

Continued lines can be created by breaking a line or reduced
by joining two consecutive lines connected by continuation symbol `&`.
Break and join operations are also useful for deleting empty lines and
breaking or joining comments with a common comment starter.

The line break function is bound to `C-<return>`.
All break and join functions are available via the transient popup (`C-c C-f`)
under keys `RET`, `j`, `J` and `C-s`.
These operations are inspired by the legacy f90 mode, but behave a bit differently.

#### Breaking lines

The function `f90-ts-break-line` breaks the current line at point and adds continuation symbols.
If the current line is a comment, then the comment prefix (like '!>', '!<' or similar) is extracted,
including indentation offset after the comment prefix, and inserted into the new line to continue the comment.
See [Comment prefix](#comment-prefix) for details.

Whether a leading ampersand at the start of the new line is inserted is controlled by option
`f90-ts-leading-ampersand`.


#### Joining lines

Consecutive lines can be joined by `f90-ts-join-line-prev` and `f90-ts-join-line-next`
under various circumstances, which are:

- two consecutive lines connected by continuation symbol `&`.
- some statement followed by empty line(s) (join-line-prev)
- empty line(s) followed by some statement (join-line-next)
- comments with the same comment prefix (see [Comment prefix](#comment-prefix) for details)
- comment after a continued line (join-line-prev)
- continued line followed by a comment (join-line-next)
- continued string literal

The ampersand(s) or comment prefixes in between are removed, where necessary, adding
one whitespace character where applicable.

If there are empty lines, then only the empty lines are removed, without joining the lines.
A second join operation can be used to actually join the lines.

Current limitations are:
* Joining of openmp statements is not yet implemented.


#### Shifting line break

Changing the point at which a line is continued is sufficiently common and thus provided as `f90-ts-shift-line-break`.
It breaks the current line at point and joins the trailing part with the next line, effectively shifting the break point.


### Filling lines and regions

The mode provides filling and rebalancing operations. It breaks overlong lines (exceeding `fill-column`)
and joins continued lines or comments with matching prefix where possible to rebalance the code and
fill up the column space as much as possible.



#### Fill Operations

These operations can be accessed via the transient popup (`C-c C-f`) under the **Fill/Rebalance** section:

* `f` (`f90-ts-fill-region`): Fills the active region. If no region is active, it processes the entire buffer.
* `M-f` (`f90-ts-fill-at-line`): Breaks and joins the current line at point.
* `M-j` (`f90-ts-fill-prev-line`): Fills the current line and the previous line.
* `M-J` (`f90-ts-fill-next-line`): Fills the current line and the next line.

With `C-f` and `C-b`, the transient popup allows to configure fill column and fill method.



#### Breakpoint Selection
When a line exceeds the target `fill-column`, the fill function must decide where to safely split the statement or comment.
This behavior is controlled by the customizable variable `f90-ts-fill-select-breakpoint-by`, which offers two modes:

* `rightmost`: (default) Automatically picks the rightmost eligible break point before the `fill-column` limit.
                It avoids breaking at awkward positions (like immediately before an opening parenthesis or within
                a `%` component selection) unless strictly necessary.
* `interactive`: Pauses the fill operation and allows to manually rotate through possible valid breakpoints
                 using an interactive break and join session. The mode allows to break lines beyond fill-column.
    * `left`, `right`, `home`, `end`, `r`, `C-p` and `C-n` to walk through breakpoints with the cursor.
    * `RET` to confirm the break.
    * `BACKSPACE` to perform a normal join with the previous line.
    * `DEL` to perform a normal a normal join with the next line.
    * `SPC` to skip and possibly extend the fill region by one non-empty line to continue with rebalancing.
    * `q` to skip the line and keep as is.
    * `C-g` to abort the fill session.

*Remark:* The current `fill-column` and breakpoint selection method can be overwritten on the fly
using `C-f` and `C-b` inside the transient menu.


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

Marked regions can be selected, enlarged, shrunk, moved, extended or reduced based on the tree-sitter node structure.
Defcustom variable `f90-ts-mark-region-order' can be used to configure whether the point after
a marked region operation should be placed at start, at end or if order of active region should
be preserved (defaulting to end position if no active region is present).

Blocks of nodes of the same kind are grouped and and dealt with like
the block would be represented by a node in the tree (which they are not).
Currently comments with the same comment prefix are recognised (see [Comment prefix](#comment-prefix)).
(Other groups like use statements or public statements could be added in the future.)

Identifying comments with the same comment prefix and including those in mark region operations
is particularly useful in conjunction with comment region operations.

Key bindings are provided in the transient popup (`C-c C-f`) under the Region section:

| Function                                  | Popup key | Description                                                               |
|-------------------------------------------|-----------|---------------------------------------------------------------------------|
| `f90-ts-mark-region-enlarge`              | `r`       | Find smallest parent node of existing region which is strictly larger     |
| `f90-ts-mark-region-shrink-child-first`   | `0`       | Shrink region to a first (grand)child which is strictly smaller           |
| `f90-ts-mark-region-shrink-child-last`    | `9`       | Shrink region to a last (grand)child which is strictly smaller            |
| `f90-ts-mark-region-first-sibling`        | `{`       | Move selected region to first sibling                                     |
| `f90-ts-mark-region-prev-sibling`         | `[`       | Move selected region to previous sibling                                  |
| `f90-ts-mark-region-next-sibling`         | `]`       | Move selected region to next sibling                                      |
| `f90-ts-mark-region-last-sibling`         | `}`       | Move selected region to last sibling                                      |
| `f90-ts-mark-region-extend-first-sibling` | `M-{`     | Extend region to first sibling, or reduce it to its first node (or group) |
| `f90-ts-mark-region-extend-prev-sibling`  | `M-[`     | Extend region by previous sibling, or drop its last node (or group)       |
| `f90-ts-mark-region-extend-next-sibling`  | `M-]`     | Extend region by next sibling, or drop its first node (or group)          |
| `f90-ts-mark-region-extend-last-sibling`  | `M-}`     | Extend region to last sibling, or reduce it to its last node (or group)   |

The last four operations are described in
[Extending and reducing a region by siblings](#extending-and-reducing-a-region-by-siblings).


#### Extending and reducing a region by siblings

The sibling operations `{` `[` `]` `}` *move* the region to another node. The extend/reduce operations
`M-{` `M-[` `M-]` `M-}` instead change the region by whole siblings, similar to growing or shrinking a selection
of statements. They require an active region and are available in the transient popup (section
*Mark sibling region*) and in the *Mark region* sub-menu of the Fortran menu. There are no default
keybindings outside of the popup, see [Setup](#standard-installation-via-package-manager) for example bindings.

Only the end of the region where point is located (the *active end*) is moved, the other end stays fixed.
Whether the region grows or shrinks depends on the direction of the command and on which end point is at.
The direction is the direction in which the active end travels through the siblings:

| Point at | `M-[` (previous)   | `M-]` (next)        | `M-{` (first)                  | `M-}` (last)                   |
|----------|--------------------|---------------------|--------------------------------|--------------------------------|
| start    | extend by prev     | reduce (drop first) | extend start to first sibling  | reduce to last node or group   |
| end      | reduce (drop last) | extend by next      | reduce to first node or group  | extend end to last sibling     |

After the operation, point is at the same end of the region as before. Note that
`f90-ts-mark-region-order` is not used here, as it would otherwise move the active end.
Use `X` (`exchange-point-and-mark`) in the popup to switch the active end.


### Folding

The mode supports `hs-minor-mode` and `outline-minor-mode`. Both are optional and independent.

Hideshow folds blocks from their start to their own end statement.
Outline treats program units (and other nodes matching `f90-ts--outline-predicate`) as headings,
with the heading level given by the nesting depth in the syntax tree. Outline provides
level-based overviews (`outline-hide-sublevels`) and sibling/parent navigation.

Neither mode is enabled automatically. These can be enable by hooks (see [Setup](#setup)).
Their most common commands are available in the transient popup (`@` and `$`) while the mode is active.

*Remarks:*
- Some outline functions like `outline-promote` and `outline-demote` have no meaning in this context,
  as levels come from the code structure.
- The body of a heading extends to the next heading, so closing `end` lines can be hidden together
  with the preceding heading.
- Support for external package `treesit-fold` is submitted but still pending. Use a local clone of
  the fork `https://github.com/mscfd/treesit-fold`, checkout the branch `f90-ts` and setup the
  cloned repository via use-package.


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

Registering of tests is done semi-automatic in `f90-ts-mode-test-indent-register`, `f90-ts-mode-test-font-lock-register`
and other functions in `f90-ts-mode-test.el`.

Tests are run with a prescribed set of custom variables. In particular, indentation values are chosen
all differently (and thus using a rather unsual non-practical setup), such that errors can be spotted
more easily. Within erts files, custom variables can and sometimes are overwritten to allow testing
various aspects of the mode.


#### Makefile

There is a Makefile for running various tests. Relevant targets are:
* test-checkdoc
* test-byte-compile
* test-all
* test-all-parallel
* test-ert-all
* test-ert-parallel

Targets ending with parallel run one emacs instance per ert test to allow parallel execution
with `make -j<N> test-*-parallel`. Otherwise one emacs process runs all ert tests sequentially.

During development, if functions from `f90-ts-log.el` are used, testing fails (intended!) as the log package
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
