(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay '(1.0 . 1.0))
  (corfu-popupinfo-max-height 20))

(provide 'ym-completion)
