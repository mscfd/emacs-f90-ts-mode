# f90-ts-mode

Tree-sitter based major mode for editing Fortran (Fortran 90 / 2003 and
newer) in free source form in Emacs. It requires Emacs 30+.
The mode is inspired by f90-mode in emacs core.

This project is under active development, see [Roadmap](#roadmap)
For a comprehensive overview see [MANUAL.md](MANUAL.md).

## Overview

f90-ts-mode provides a modern Fortran editing experience using Tree-sitter,
including syntax highlighting, indentation, navigation, and structural editing features.

### Features

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


## Keybindings

The mode provides direct keybindings for the most frequent operations like indentation with `TAB`
and a **transient popup** for discoverability of all commands:

| Key                            | Description                      |
|--------------------------------|----------------------------------|
| `C-c C-f`                      | Open the transient command popup |
| `C-<tab>`                      | Indent & complete line           |
| `C-<return>`                   | Break line                       |
| `C-c ;`                        | Comment region (default prefix)  |
| `C-c '`                        | Comment region (custom prefix)   |

Pressing `C-c C-f` opens a transient popup grouping all major commands by category.

For the full keybinding reference see the
[Keybindings section in the manual](MANUAL.md#keybindings).


## Installation

This mode requires **Emacs 30+** and a compatible Tree-sitter Fortran grammar.
Detailed technical requirements and troubleshooting can be found in [MANUAL.md](MANUAL.md#installation)


1.  Install a compatible Tree-sitter Fortran grammar.
The mode relies on the tree-sitter-fortran grammar, which can be cloned by:

```
git clone https://github.com/stadelmanma/tree-sitter-fortran
```

The grammar can be registered and installed directly within Emacs:
```elisp
(setq treesit-language-source-alist
      '((fortran "https://github.com/stadelmanma/tree-sitter-fortran")))

;; Run this command once to compile the grammar:
;; M-x treesit-install-language-grammar RET fortran RET
```


2.  Clone this repository:

```
git clone https://github.com/mscfd/emacs-f90-ts-mode.git
```


3.  Add it to the `load-path` and configure with `use-package`.

Here is a small setup for the mode:

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
                '((fortran "path_to/tree-sitter-fortran")))))
```


## Roadmap

There are a number of features still missing or incomplete.
The following list provides features planned for implementation (somewhat ordered by priority):

- Complement alignment options for strings on continued lines.
  (Also allow comments within continued strings. This is not yet supported by the fortran grammar.)
- Fill operations similar to `f90-fill-region` and `f90-fill-paragraph`.
- Support for (context-aware) `completion-at-point-function` (capf).
- More list contexts. There is a number of list like contexts, which are not yet supported,
  but for which proper alignment would be nice.
- Enlarge and shrink region operations:
   * Handle comment regions with same prefix as virtual nodes. This helps with comment region operations.
   * Add other child selection options (select last child, search child, etc.)
- `undo-boundary` to group internal changes to blocks of changes for undo.
  This mainly concerns complex indentation operations (like indentation of statements or region).
- Electric insert similar to `f90-electric-insert`.
