# f90-ts-mode

Tree-sitter-based major mode for editing Fortran (Fortran 90 / 2003 and
newer) in free source form in Emacs. It requires Emacs 29+.

The mode is inspired by f90-mode in Emacs core. Alongside modern
Tree-sitter-based functionality, it aims to provide and enhance features
such as smart end completion and region commenting that have made f90-mode
productive and enjoyable to use.

This project is under active [development](#roadmap).
For a comprehensive overview see [MANUAL.md](MANUAL.md).

### Changelog (recent)

**11-2026**
- Alignment rules for trailing comments within continued lines as well as
  those following a statement added.

**10-2026**
- Extend/reduce variants for mark region operations based on siblings
  (first, last, previous, next) added.
- Package f90-ts-nav replaced by improved f90-ts-toc providing a side panel with a
  table of content of a f90 buffer, see `MANUAL.md' for details.  This
  optional package requires its own use-package statement.
- Alignment rules for improved indentation of variable declarations with
  initialisation added.
- Infinite loop caused by `f90-ts-indent-delete-trailing-whitespace` fixed.
  This happened if region ended at last line without a trailing newline.
- Right-align-at-comma options for array literals added,
  see `f90-ts-indent-array-align`.
- Changes required by updates to the proposed string_literal branch added.
- Support for new keyword `non_recursive` added (requires new grammar).
- Support for new grammar version with a different tree structure in some
  type related statements (allocate, select type) added.  Both new and old
  grammar CST variants are supported.
- Prefix of ERT test names shortened `f90-ts-mode-test--' (no distinction
  between std and extra anymore).
- Transient menu restructured and decomposed.
- Support for hideshow and outline added.

**09-2026**
 - `f90-ts-mode.el` decomposed into several smaller packages. Experimental
   `f90-ts-nav` (tree in fortran menu and tree view in side panel) has been
   made optional and requires a separate use-package to load it.
 - `f90-ts-indent-delete-trailing-whitespace` added to automatically delete
   trailing whitespace after indentation operation.
 - Font locking of interface name in deferred procedure declaration fixed.
 - Handling of trailing whitespace characters in thing-end-of-X navigation added.
 - Support for Emacs 29 + tree-sitter 0.20.x added (tested with 29.1, 29.3 and tree-sitter 0.20.8).
 - Fontification of error nodes fixed if line limitting is enabled.
 - Some issues in comment-region operations fixed (preserve indentation,
   preserve trailing whitespace where possible, keep existing alignment
   with keep options, do not operate outside of region boundaries, add
   missing function `f90-ts-indent-region`).
 - Indentation after uncommenting lines in comment-region operation on
   commented lines of code containing leading ampersand or statement label fixed.
 - Missing option `keep-or-continued-line` added to
   `f90-ts--indent-options-alist` for indentation selection options.
 - Syntax highlighting, indentation, break/join/fill etc. for string literals improved.
   This requires a proposed (but not yet merged) tree-sitter language grammar extension.
   See issues [Continued strings at grammar repo](https://github.com/stadelmanma/tree-sitter-fortran/issues/193)
   and [Continued strings at mode repo](https://github.com/mscfd/emacs-f90-ts-mode/issues/127),
   and PR [Parse parts, continuation and comments in string literals](https://github.com/stadelmanma/tree-sitter-fortran/pull/198)
   The grammar extension can be enabled by
   ```elisp
   (setq treesit-language-source-alist
     '((fortran "https://github.com/mscfd/tree-sitter-fortran" "string_literal_emacs")))
   ```
   It resides in the feature branch `string_literal_emacs`.
 - Testing with emacs 31.1 and tree-sitter 0.26 added.


## Overview

f90-ts-mode provides a modern Fortran editing experience using Tree-sitter,
including syntax highlighting, indentation, navigation, and structural editing features.

### Features

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
- TOC side panel to show interactive table of content for a f90-ts buffer
  (see [optional package](#4-enable-optional-packages))
- Hideshow, outline and treesit-fold support

## Keybindings

The mode provides direct keybindings for the most frequent operations like indentation with `TAB`
and a **transient popup** for discoverability of all commands:

| Key                            | Description                            |
|--------------------------------|----------------------------------------|
| `C-c C-f`                      | Open the transient command popup       |
| `<tab>`                        | Indent and complete line               |
| `C-<tab>`                      |  Indent and complete statement (block) |
| `<backtab>` (shift `<tab>`)    | Indent and complete line variant 2     |
| `C-S-<iso-lefttab>` (Linux)    | Indent and complete line variant 3     |
| `C-<backtab>`       (Windows?) | Indent and complete line variant 3     |
| `C-<return>`                   | Break line                             |
| `C-c ;`                        | Comment region (default prefix)        |
| `C-c '`                        | Comment region (custom prefix)         |

Pressing `C-c C-f` opens a transient popup, grouping all major commands by category.

For the full keybinding reference see the
[Keybindings section in the manual](MANUAL.md#keybindings).


## Installation

This mode requires **Emacs 29+** and a compatible Tree-sitter Fortran grammar and compatible
shared core libraries.

The mode is tested with:
* Emacs 29.1 + tree-sitter 0.20.8 (ABI version 14)
* Emacs 29.3 + tree-sitter 0.20.8 (ABI version 14)
* Emacs 30.1 + tree-sitter 0.24.7 (ABI version 14)
* Emacs 30.1 + tree-sitter 0.25.10 (ABI version 15)
* Emacs 30.2 + tree-sitter 0.24.7 (ABI version 14)
* Emacs 30.2 + tree-sitter 0.25.10 (ABI version 15)
* Emacs 31.1 + tree-sitter 0.26.11 (ABI version 15)

Detailed technical requirements and troubleshooting can be found
in [MANUAL.md](MANUAL.md#installation).

The mode can be installed through melpa as outlined below.
Alternatively, the repository can be cloned and setup by hand.
For more details see [MANUAL.md](MANUAL.md#installation).

Installation steps are:

1. Install a compatible Tree-sitter Fortran grammar.

The mode relies on the `tree-sitter-fortran` grammar.
Register the grammar repository in Emacs:

```elisp
(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((fortran "https://github.com/stadelmanma/tree-sitter-fortran"))))
```
For improved string literal parsing, use:
```elisp
(setq treesit-language-source-alist
      (append treesit-language-source-alist
              '((fortran "https://github.com/mscfd/tree-sitter-fortran" "string_literal_emacs"))))
```
This feature branch has not yet been merged. The mode will use the additional parser data
to improve syntax highlighting, indentation and break/join/fill operations. It also parses
valid syntax not handled by the official grammar.


Then compile and install it once with:

```elisp
M-x treesit-install-language-grammar RET fortran RET
```


2. Install the mode from melpa via package-install

Install the f90-ts-mode package via `package-install`.


3. Enable the mode

The mode can be activated by `M-x f90-ts-mode`.
To enable it automatically, add a use-package section to init.el.
Below is an example with custom keybindings.
It will automatically be loaded when opening a file with extension `.f90`.

```elisp
(use-package f90-ts-mode
  :ensure t
  :mode ("\\.f90\\'" . f90-ts-mode)

  :init
  (require 'treesit)

  ;; uncomment if Imenu entry in menu bar is desired
  ;; :hook (f90-ts-mode . (lambda () (imenu-add-to-menubar "Imenu")))

  ;; uncomment to switch on hideshow and outline minor mode automatically
  ;; :hook (f90-ts-mode . hs-minor-mode)
  ;; :hook (f90-ts-mode . outline-minor-mode)

  :config
  (message "f90-ts-mode loaded")

  :bind (;; mode-specific bindings, adjust to your needs
         :map f90-ts-mode-map
         ;; transient popup (additional shorter binding to "C-c C-f")
         ("A-<up>"        . #'f90-ts-transient)
         ;; examples for shortcuts
         ("A-<return>"    . #'f90-ts-break-line)
         ("C-<return>"    . #'f90-ts-shift-line-break)
         ("A-<backspace>" . #'f90-ts-join-line-prev)
         ("A-<delete>"    . #'f90-ts-join-line-next)
         ("A-l"           . (lambda () (interactive)
                              (let ((f90-ts-fill-select-breakpoint-by 'interactive))
                                (f90-ts-fill-at-line))))

         ("A-\\"          . #'f90-ts-mark-region-enlarge)
         ("A-0"           . #'f90-ts-mark-region-shrink-child-first)
         ("A-9"           . #'f90-ts-mark-region-shrink-child-last)
         ("A-{"           . #'f90-ts-mark-region-first-sibling)
         ("A-["           . #'f90-ts-mark-region-prev-sibling)
         ("A-]"           . #'f90-ts-mark-region-next-sibling)
         ("A-}"           . #'f90-ts-mark-region-last-sibling)))
```

4. Enable optional packages

A TOC side panel with a table-of-content tree (see [TOC buffer](MANUAL.md#toc-buffer)) can be
enabled by loading the optional package `f90-ts-toc`:

```elisp
(use-package f90-ts-toc
  :after f90-ts-mode
  ;; demand loading after f90-ts-mode, otherwise entries for TOC functions
  ;; do not show up in menus
  :demand t)
```


*Remark:*
The readme and manual documents can be easily loaded from github and opened via the fortran menu
or the transient popup keybinding.
For easily viewing `README.md` and `MANUAL.md` in a buffer instead of a browser,
install the package `markdown-mode`, whose view mode is used if installed.


## Customization

All options can be found under `M-x customize-group RET f90-ts` and its subgroups.
The group can also be reached via the Fortran menu added by the mode.


## Contributing

Contributions and feature requests are welcome. Please open an issue or pull request on GitHub.

When reporting a bug, please include a small code snippet, showing the issue or desired behaviour.


## Roadmap

There are a number of features still missing or incomplete.
The following list provides features planned for implementation (somewhat ordered by priority):

- More list contexts for alignment in continued lines.
  There are a number of list like contexts, which are not yet supported, but for which proper
  alignment would be nice.
- Fill operations with lower column width (before joining).
- Fill operation similar to `f90-fill-paragraph`. In conjunction with mark operations: determine interesting
  nodes within tree as region (like: statements, structure block, subroutine/function level).
- Node based sibling walk operations (like goto trimmed beginning or trimmed end of next sibling). This would
  complement the mark region operations to easily extend an existing region.
- Make indentation and alignment aware of fill-column: Do not suggest an indentation if the line exceeds fill-column.
- Support for (context-aware) `completion-at-point-function` (capf).
- Electric insert similar to `f90-electric-insert`.
- Indentation for labeled do loops, like:
```f90
  do 123 i = 1,10
     do 123 j = 1,10
        print *, i, j
  123 end do
```
  (Remark: the end do statement has one real and one virtual node to match the number of nested loops.)
