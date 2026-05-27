# f90-ts-mode

Tree-sitter based major mode for editing Fortran (Fortran 90 / 2003 and
newer) in free source form in Emacs. It requires Emacs 30+.
The mode is inspired by f90-mode in emacs core.

This project is under active [development](#roadmap).
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
In particular tree-sitter ABI version 15 and tree-sitter library version 0.25.x
are mandatory.
Detailed technical requirements and troubleshooting can be found
in [MANUAL.md](MANUAL.md#installation).

Once available, the mode can be installed through melpa as outlined below.
Alternatively, the repository can be cloned and setup by hand.
For more details see [MANUAL.md](MANUAL.md#installation).

Installation step are:

1. Install a compatible Tree-sitter Fortran grammar.

The mode relies on the `tree-sitter-fortran` grammar.
Register the grammar repository in Emacs:

```elisp
(setq treesit-language-source-alist
      '((fortran "https://github.com/stadelmanma/tree-sitter-fortran")))
```

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

  :config
  (message "f90-ts-mode loaded")

  :bind (;; mode-specific bindings, adjust to your needs
         :map f90-ts-mode-map
         ;; transient popup (additional shorter binding to "C-c C-f")
         ("A-<up>"        . #'f90-ts-transient)
         ;; shortcuts (just some examples)
         ("A-<return>"    . #'f90-ts-break-line)
         ("A-<backspace>" . #'f90-ts-join-line-prev)
         ("A-<delete>"    . #'f90-ts-join-line-next)
         ("A-\\"          . #'f90-ts-mark-region-enlarge)
         ("A-0"           . #'f90-ts-mark-region-shrink-child-first)
         ("A-9"           . #'f90-ts-mark-region-shrink-child-last)
         ("A-{"           . #'f90-ts-mark-region-first-sibling)
         ("A-["           . #'f90-ts-mark-region-prev-sibling)
         ("A-]"           . #'f90-ts-mark-region-next-sibling)
         ("A-}"           . #'f90-ts-mark-region-last-sibling)))
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
   * Add other child selection options (select last child, search child, etc.)
- Electric insert similar to `f90-electric-insert`.
