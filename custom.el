(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(edit-indirect markdown-mode claude-code-ide yasnippet-snippets which-key web-mode vterm vertico treemacs rainbow-delimiters projectile orderless multiple-cursors marginalia magit flycheck-eglot exec-path-from-shell editorconfig eat doom-themes doom-modeline diff-hl consult company))
 '(package-vc-selected-packages
   '((claude-code-ide :vc-backend Git :url "https://github.com/manzaltu/claude-code-ide.el")))
 '(safe-local-variable-values
   '((eval load
           (expand-file-name "dev.el"
                             (locate-dominating-file default-directory ".dir-locals.el"))
           t)
     (projectile-project-run-cmd . "npm start")
     (projectile-project-test-cmd . "npm test")
     (projectile-project-compile-cmd . "npm run build"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
