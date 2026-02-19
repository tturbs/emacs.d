(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-mode edit-indirect markdown-mode claude-code-ide yasnippet-snippets which-key web-mode vterm vertico treemacs rainbow-delimiters projectile orderless multiple-cursors marginalia magit flycheck-eglot exec-path-from-shell editorconfig eat doom-themes doom-modeline diff-hl consult company))
 '(package-vc-selected-packages
   '((claude-code-ide :vc-backend Git :url "https://github.com/manzaltu/claude-code-ide.el")))
 '(safe-local-variable-values
   '((eval progn
           (when
               (fboundp 'gofmt-before-save)
             (add-hook 'before-save-hook #'gofmt-before-save nil t))
           (when
               (require 'dap-mode nil t)
             (require 'dap-dlv-go nil t)
             (let
                 ((root
                   (file-name-as-directory
                    (or
                     (when-let*
                         ((proj
                           (project-current)))
                       (project-root proj))
                     default-directory))))
               (dap-register-debug-template "IQE-Hub: Backend Debug"
                                            (list :type "go" :request "launch" :name "IQE-Hub: Backend Debug" :mode "debug" :program
                                                  (concat root "src/main.go")
                                                  :cwd
                                                  (concat root "src/")
                                                  :args
                                                  (vector "-config"
                                                          (concat root ".vscode/debug.yaml")
                                                          "-www"
                                                          (concat root "app/www"))
                                                  :buildFlags "-tags=debug" :dlvToolPath "dlv"))
               (dap-register-debug-template "IQE-Hub: Debug Test at Point"
                                            (list :type "go" :request "launch" :name "IQE-Hub: Debug Test at Point" :mode "test" :program nil :cwd
                                                  (concat root "src/")
                                                  :dlvToolPath "dlv")))))
     (eval add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
     (eval load
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
