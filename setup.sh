#!/usr/bin/env bash
set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

step() { echo -e "\n${BLUE}==>${NC} ${1}"; }
ok()   { echo -e "  ${GREEN}✓${NC} ${1}"; }
skip() { echo -e "  ${YELLOW}→${NC} ${1} (already installed)"; }
err()  { echo -e "  ${RED}✗${NC} ${1}" >&2; }

EMACS_DIR="$(cd "$(dirname "$0")" && pwd)"

########################################
# 1. Prerequisites
########################################
step "Checking prerequisites"

# Emacs 29+
if ! command -v emacs &>/dev/null; then
    err "Emacs not found. Install Emacs 29+ first."
    exit 1
fi
EMACS_VER=$(emacs --version | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1)
EMACS_MAJOR=${EMACS_VER%%.*}
if (( EMACS_MAJOR < 29 )); then
    err "Emacs 29+ required (found ${EMACS_VER})"
    exit 1
fi
ok "Emacs ${EMACS_VER}"

# Homebrew
if ! command -v brew &>/dev/null; then
    err "Homebrew not found. Install from https://brew.sh"
    exit 1
fi
ok "Homebrew"

# Node.js / npm
if ! command -v node &>/dev/null || ! command -v npm &>/dev/null; then
    err "Node.js/npm not found. Install Node.js first."
    exit 1
fi
ok "Node.js $(node --version)"

# C compiler
if ! command -v cc &>/dev/null; then
    err "C compiler not found. Install Xcode Command Line Tools: xcode-select --install"
    exit 1
fi
ok "C compiler (cc)"

########################################
# 2. Homebrew packages
########################################
step "Installing Homebrew packages"

for pkg in libvterm ripgrep; do
    if brew list "$pkg" &>/dev/null; then
        skip "$pkg"
    else
        brew install "$pkg"
        ok "$pkg"
    fi
done

########################################
# 3. LSP servers
########################################
step "Installing LSP servers"

# npm global packages
for pkg in typescript-language-server typescript pyright; do
    if npm list -g "$pkg" &>/dev/null; then
        skip "$pkg"
    else
        npm install -g "$pkg"
        ok "$pkg"
    fi
done

# cmake-language-server via pipx
if command -v cmake-language-server &>/dev/null; then
    skip "cmake-language-server"
else
    if ! command -v pipx &>/dev/null; then
        step "Installing pipx (needed for cmake-language-server)"
        brew install pipx
        pipx ensurepath
    fi
    pipx install cmake-language-server
    ok "cmake-language-server"
fi

########################################
# 4. Emacs packages (batch mode)
########################################
step "Installing Emacs packages"

emacs --batch --eval "
(progn
  (require 'package)
  (setq package-archives
        '((\"melpa\"  . \"https://melpa.org/packages/\")
          (\"gnu\"    . \"https://elpa.gnu.org/packages/\")
          (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\")))
  (package-initialize)
  (package-refresh-contents)

  (dolist (pkg '(doom-themes doom-modeline nerd-icons which-key
                 vertico orderless marginalia consult company
                 editorconfig flycheck flycheck-eglot
                 magit diff-hl projectile
                 web-mode yasnippet yasnippet-snippets
                 rainbow-delimiters treemacs avy multiple-cursors
                 exec-path-from-shell vterm))
    (if (package-installed-p pkg)
        (message \"  → %s (already installed)\" pkg)
      (package-install pkg)
      (message \"  ✓ %s\" pkg)))

  ;; claude-code-ide from GitHub
  (if (package-installed-p 'claude-code-ide)
      (message \"  → claude-code-ide (already installed)\")
    (package-vc-install \"https://github.com/manzaltu/claude-code-ide.el\")
    (message \"  ✓ claude-code-ide\")))"

########################################
# 5. Tree-sitter grammars
########################################
step "Installing tree-sitter grammars"

emacs --batch -l "${EMACS_DIR}/init.el" --eval "
(progn
  (dolist (lang '(typescript tsx go gomod python c cpp cmake))
    (if (treesit-language-available-p lang)
        (message \"  → %s (already installed)\" lang)
      (treesit-install-language-grammar lang)
      (message \"  ✓ %s\" lang))))"

########################################
# 6. vterm module
########################################
step "Compiling vterm module"

VTERM_DIR=$(find "${EMACS_DIR}/elpa" -maxdepth 1 -type d -name 'vterm-*' | head -1)
if [ -z "$VTERM_DIR" ]; then
    err "vterm package directory not found in elpa/"
    exit 1
fi

if [ -f "${VTERM_DIR}/vterm-module.so" ] || [ -f "${VTERM_DIR}/vterm-module.dylib" ]; then
    skip "vterm-module"
else
    mkdir -p "${VTERM_DIR}/build"
    cmake -S "$VTERM_DIR" -B "${VTERM_DIR}/build"
    cmake --build "${VTERM_DIR}/build"
    ok "vterm-module"
fi

########################################
# 7. Nerd Icons font
########################################
step "Installing Nerd Icons font"

if fc-list 2>/dev/null | grep -qi "nerd"; then
    skip "Nerd Icons font"
else
    emacs --batch --eval "
(progn
  (require 'nerd-icons)
  (nerd-icons-install-fonts t))"
    ok "Nerd Icons font"
fi

########################################
# 8. NanumGothicCoding font
########################################
step "Installing NanumGothicCoding font"

if fc-list 2>/dev/null | grep -q "NanumGothicCoding"; then
    skip "NanumGothicCoding"
else
    brew install --cask font-nanum-gothic-coding
    ok "NanumGothicCoding"
fi

########################################
# Done
########################################
echo ""
echo -e "${GREEN}All done!${NC} Start Emacs and verify with:"
echo "  emacs --batch -l init.el"
