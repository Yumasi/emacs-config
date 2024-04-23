(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-override-mode)
  (general-create-definer ym/leader-def
    :prefix "SPC"))

(provide 'ym-general)
