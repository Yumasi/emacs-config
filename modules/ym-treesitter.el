(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq my-go-tsauto-config
        (make-treesit-auto-recipe
         :lang 'go
         :ts-mode 'go-ts-mode
         :remap '(go-mode)
         :url "https://github.com/tree-sitter/tree-sitter-go"
         :revision "v0.19.0"
         :source-dir "src"
         :ext "\\.go\\'"))

  (add-to-list 'treesit-auto-recipe-list my-go-tsauto-config)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(customize-set-variable 'treesit-font-lock-level 4)

(provide 'ym-treesitter)
