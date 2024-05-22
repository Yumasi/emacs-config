(use-package transient)

(use-package magit
  :config
  (setf transient-values '((magit-commit "--allow-empty")))
  :general
  (ym/leader-def
    :states 'normal
    :keymaps 'override
    "g g" 'magit-status))

(provide 'ym-magit)
