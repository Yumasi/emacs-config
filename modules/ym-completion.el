(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-auto t))

(provide 'ym-completion)
