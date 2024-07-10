(use-package casual-lib
  :ensure (:host github :repo "kickingvegas/casual-lib"))
(use-package casual-ibuffer
  :ensure (:host github :repo "kickingvegas/casual-ibuffer")) ; TODO: temporary until put on MELPA

(ym/leader-def
  :states 'normal
  :keymaps '(override ibuffer-mode)
  "?" 'casual-ibuffer-tmenu)

(provide 'ym-ibuffer)
