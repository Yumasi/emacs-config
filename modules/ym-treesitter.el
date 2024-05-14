(use-package tree-sitter-langs
  :config
  (tree-sitter-langs-install-latest-grammar t))

(setq major-mode-remap-alist '((c-mode . c-ts-mode)
                               (c++-mode . c++-ts-mode)
                               (go-mode . go-ts-mode)
                               (python-mode . python-ts-mode)))

(customize-set-variable 'treesit-font-lock-level 4)

(provide 'ym-treesitter)
