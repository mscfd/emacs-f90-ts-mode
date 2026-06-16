# f90-ts-mode

Tree-sitter based major mode for editing Fortran (Fortran 90 / 2003 and
newer) in free source form in Emacs. It requires Emacs 30+.

The mode is inspired by f90-mode in emacs core. Besides features expected from a mode,
it tries to provide and enhance f90-mode's additional core features like smart-end completion,
comment region, and other, which made f90-mode such an outstanding and enjoyable mode.

This project is under active [development](#roadmap).
For a comprehensive overview see [MANUAL.md](MANUAL.md).

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
- (Un)commenting regions with configurable prefixes and indentation rules
- Special comments like doc strings and separators
  (syntax highlighting and indentation options)
- Keyword highlighting in comments (like TODO, Remark etc.)
- OpenMP and preprocessor directives
- Coarray keywords and statements
- Region selection based on tree-sitter nodes
- Imenu and a Fortran menu in the menu bar
- Navigation (defun, things, Xref, side panel tree)


### Recently added, changed or improved

**06-2026**
 - Indentation within and after preprocessor blocks fixed
 - Trailing blank part `\\(\\s-+\\|$\\)` in defcustom regexps
   `f90-ts-comment-prefix-regexp` and `f90-ts-openmp-prefix-regexp` has
   been removed from the defcustom definitions and is now always appended
   internally. If these variables have been customized, please adjust.


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


## Customization

All options can be found under `M-x customize-group RET f90-ts` and its subgroups.
The group can also be reached via the Fortran menu added by the mode.


## Contributing

Contributions and feature requests are welcome. Please open an issue or pull request on GitHub.

When reporting a bug, please include a small code snippet, showing the issue or desired behaviour.


## Roadmap

There are a number of features still missing or incomplete.
The following list provides features planned for implementation (somewhat ordered by priority):

- Complement alignment options for strings on continued lines. Refine font locking for string, using
  different faces for quotes, continuation symbols and the string itself.
  Allow comments within continued strings.
  This all requires extension of the grammar itself.
  See issues [Continued strings at grammar repo](https://github.com/stadelmanma/tree-sitter-fortran/issues/193)
  and [Continued strings at mode repo](https://github.com/mscfd/emacs-f90-ts-mode/issues/127)
- Fill operations similar to `f90-fill-region` and `f90-fill-paragraph`.
- Support for (context-aware) `completion-at-point-function` (capf).
- More list contexts for alignment in continued lines.
  There are a number of list like contexts, which are not yet supported, but for which proper
  alignment would be nice.
- Electric insert similar to `f90-electric-insert`.
