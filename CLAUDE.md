# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs 29+ configuration. Single-file setup in `init.el` (~300 lines) with `lexical-binding: t`. No build system, tests, or CI — changes are validated by restarting Emacs or evaluating with `M-x eval-buffer`.

## Architecture

Everything lives in `init.el`, organized into `;;;` comment-delimited sections in this order:

1. **Package management** — MELPA/GNU/NonGNU ELPA archives, built-in `use-package` with `always-ensure t`
2. **Basic settings** — UI, scrolling, indentation (spaces, width 4), UTF-8, Korean input
3. **Visual** — doom-themes (doom-one), doom-modeline, nerd-icons
4. **Completion** — Vertico + Orderless + Marginalia + Consult (minibuffer); Company (in-buffer)
5. **Code tooling** — Flycheck, Eglot (built-in LSP), EditorConfig, YASnippet
6. **Language support** — TypeScript/TSX, Go, Python (all tree-sitter based), Angular HTML (web-mode)
7. **Git** — Magit, diff-hl
8. **Navigation/editing** — Projectile, Treemacs, Avy, multiple-cursors
9. **Terminal** — vterm, Claude Code integration
10. **Global keybindings**

## Conventions

- **Package declarations**: Always use `use-package` with `:hook`, `:bind`, `:custom`, `:config` keywords. Built-in packages use `:ensure nil`.
- **Custom functions**: Prefix with `my/` (e.g., `my/go-before-save`, `my/flycheck-set-local-tslint`).
- **Tree-sitter grammars**: Pinned to specific versions/commits in `treesit-language-source-alist` for ABI 14 compatibility. Legacy modes remapped via `major-mode-remap-alist`.
- **LSP**: Eglot with `eglot-ensure` hooks per language mode. `eglot-autoshutdown t` to clean up after last buffer closes.
- **Go save hook**: `my/go-before-save` runs `eglot-format-buffer` + organize-imports on save.
- **Comments**: Some inline comments are in Korean.

## Key Custom Keybindings

| Prefix | Purpose |
|--------|---------|
| `C-c p` | Projectile commands |
| `C-c t` | Treemacs |
| `C-c a c` | Launch Claude Code in vterm |
| `C-c a r` | Send region/buffer to Claude for review |
| `C-x g` | Magit status |
| `M-s r/f/l` | Consult ripgrep/find/line |

## Adding a New Language

1. Add grammar entry to `treesit-language-source-alist` (pin the version)
2. Add to the `dolist` auto-install loop
3. Add `major-mode-remap-alist` entry if replacing a legacy mode
4. Add `eglot-ensure` hook in the eglot `use-package` block
5. Add `auto-mode-alist` entry for file extensions

## Adding a New Package

Use a `use-package` declaration following the existing pattern. Place it in the appropriate section of `init.el`.
