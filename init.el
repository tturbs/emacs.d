;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Package Management
(require 'package)
(setq package-archives
      '(("melpa"   . "https://melpa.org/packages/")
        ("gnu"     . "https://elpa.gnu.org/packages/")
        ("nongnu"  . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package is built-in since Emacs 29
(require 'use-package)
(setq use-package-always-ensure t)

;;; Basic Settings
(setq inhibit-startup-screen t
      initial-scratch-message nil
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings '(not docstrings unresolved free-vars)
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(set-face-attribute 'default nil :height 100)
(electric-pair-mode 1)
(global-hl-line-mode 1)
(setq display-line-numbers-type 'relative)

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; 한글 폰트 설정
(set-fontset-font t 'hangul (font-spec :family "NanumGothicCoding"))

;; 기본 입력기를 한글로 설정
(setq default-input-method "korean-hangul")

;; y/n instead of yes/no
(setq use-short-answers t)

;; Remember recent files and cursor position
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)

;;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28))

;;; Icons (required by doom-modeline)
(use-package nerd-icons)

;;; Which Key - show available keybindings
(use-package which-key
  :config
  (which-key-mode))

;;; Completion Framework
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find)
         ("M-s l"   . consult-line)))

;;; In-buffer Completion (autocomplete)
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t))

;;; EditorConfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Syntax Checking
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  ;; 내장 tslint 체커를 typescript-ts-mode에서도 동작하게 확장
  (flycheck-add-mode 'typescript-tslint 'typescript-ts-mode)
  ;; 프로젝트 로컬 tslint 자동 탐색
  (defun my/flycheck-set-local-tslint ()
    "Set flycheck tslint executable to project-local node_modules."
    (when-let* ((root (locate-dominating-file default-directory "tslint.json"))
                (bin (expand-file-name "node_modules/.bin/tslint" root))
                (_ (file-executable-p bin)))
      (setq-local flycheck-typescript-tslint-executable bin)))
  (add-hook 'typescript-ts-mode-hook #'my/flycheck-set-local-tslint))

;; eglot 진단을 flycheck로 통합
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;; Project Management
(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;;; Tree-sitter grammars (Emacs 29+)
;; Emacs 29.3 uses tree-sitter ABI 14; pin grammars to compatible versions
;; Go grammar must use commit before method_spec -> method_elem rename
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.23.2" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.23.2" "tsx/src")
        (go         "https://github.com/tree-sitter/tree-sitter-go"
                    "b82ab803d887002a0af11f6ce63d72884580bf33")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod"
                    "v1.1.0")
        (python     "https://github.com/tree-sitter/tree-sitter-python"
                    "v0.23.6")
        (c          "https://github.com/tree-sitter/tree-sitter-c"
                    "v0.23.4")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp"
                    "v0.23.4")
        (cmake      "https://github.com/uyha/tree-sitter-cmake"
                    "v0.5.0")))

;; Auto-install missing grammars
(dolist (lang '(typescript tsx go gomod python c cpp cmake))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

;; Remap to tree-sitter modes
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (python-mode     . python-ts-mode)
        (go-mode         . go-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)))

;;; LSP (eglot is built-in since Emacs 29)
(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode        . eglot-ensure)
         (go-ts-mode         . eglot-ensure)
         (python-ts-mode     . eglot-ensure)
         (c-ts-mode          . eglot-ensure)
         (c++-ts-mode        . eglot-ensure)
         (cmake-ts-mode      . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(cmake-ts-mode . ("cmake-language-server"))))

;;; TypeScript (using built-in typescript-ts-mode with tree-sitter)
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;; Angular HTML templates
(use-package web-mode
  :mode ("\\.component\\.html\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t))

;;; Go (using built-in go-ts-mode with tree-sitter)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

;; go format & imports on save
(defun my/go-before-save ()
  (when (derived-mode-p 'go-ts-mode)
    (eglot-format-buffer)
    (eglot-code-actions nil nil "source.organizeImports" t)))
(add-hook 'before-save-hook #'my/go-before-save)

;;; C/C++
(add-to-list 'auto-mode-alist '("\\.c\\'"   . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"   . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))

;;; CMake
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'"          . cmake-ts-mode))

;;; Python
(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4))

;;; Snippets
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; Parentheses / Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; File Tree
(use-package treemacs
  :bind ("C-c t" . treemacs))

;;; Movement
(use-package avy
  :bind (("C-'"   . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)))

;;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;; Exec Path From Shell (ensure Emacs sees the same PATH as shell)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; Terminal Emulator
(use-package vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-term-environment-variable "xterm-256color")
  (vterm-enable-input-method t)
  :config
  (add-to-list 'vterm-keymap-exceptions "C-\\"))

;;; Claude Code IDE (MCP 기반 통합)
(unless (package-installed-p 'claude-code-ide)
  (package-vc-install "https://github.com/manzaltu/claude-code-ide.el"))

(use-package claude-code-ide
  :ensure nil
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; Helpful Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

(provide 'init)
;;; init.el ends here
