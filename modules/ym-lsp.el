(use-package go-mode)

(defun ym-eglot-format-on-save ()
  "This function is meant to be used in BEFORE-SAVE-HOOK.
Formats the buffer if eglot is managing it."

  (when (eglot-managed-p)
    (eglot-format-buffer)))


(general-add-hook '(c-ts-mode-hook
                    go-ts-mode-hook
                    python-ts-mode-hook
                    zig-mode-hook)
                  'eglot-ensure)

(add-hook 'before-save-hook #'ym-eglot-format-on-save)
(add-to-list 'eglot-ignored-server-capabilities ':inlayHintProvider)

(provide 'ym-lsp)
