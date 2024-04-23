(use-package magit
  :config
  (setf transient-values '((magit-commit "--allow-empty")))
  :general
  (ym/leader-def 'normal
    "g g" 'magit-status))

(provide 'ym-magit)
