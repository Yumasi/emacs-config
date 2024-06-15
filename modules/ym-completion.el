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

(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'ym-completion)
