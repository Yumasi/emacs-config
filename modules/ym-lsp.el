(use-package go-mode)

(general-add-hook '(c-ts-mode-hook
                    go-ts-mode-hook
                    python-ts-mode-hook)
                  'eglot-ensure)

(provide 'ym-lsp)
