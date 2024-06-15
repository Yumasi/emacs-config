(use-package tree-sitter-langs
  :demand t
  :diminish 'tree-sitter-mode
  :config
  (tree-sitter-langs-install-latest-grammar t)
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(customize-set-variable 'treesit-font-lock-level 4)

(provide 'ym-treesitter)
