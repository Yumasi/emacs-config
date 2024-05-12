(use-package go-mode)

(general-add-hook '(c-mode-hook
                    go-mode-hook)
                  'eglot-ensure)

(provide 'ym-lsp)
