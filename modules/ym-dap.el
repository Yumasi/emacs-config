(use-package jsonrpc)
(use-package dape
  :config
  (ym/leader-def
    :states 'normal
    :keymaps 'override
    "d" (general-key "C-x C-a"))

  (dape-breakpoint-global-mode)
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
  (add-hook 'kill-emacs #'dape-breakpoint-save)
  (add-hook 'after-init #'dape-breakpoint-load)
  :custom
  (dape-info-variable-table-row-config `((name . 40)
                                         (value . 100)
                                         (type . 30))))

(provide 'ym-dap)
