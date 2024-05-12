(defmacro treesit-install-unless-available (lang)
  `(unless (treesit-language-available-p ',lang)
     (treesit-install-language-grammar ',lang)))

;; Populate the sources for treesitter's language libraries.
(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")))

(treesit-install-unless-available elisp) ; TODO: figure out the correct major mode for this
(treesit-install-unless-available go)

(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)))

(provide 'ym-treesitter)
