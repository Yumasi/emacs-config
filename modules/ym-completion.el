(use-package orderless
  :config
  (add-to-list 'completion-styles-alist
               '(tab completion-basic-try-completion ignore
                 "Completion style which provides TAB completion only."))
  :custom
  (completion-styles '(tab orderless basic))
  ;; (completion-category-overrides '((file (styles basic partial-completion))
  ;;                                  (eglot (styles . (orderless flex)))))
  )

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("RET" . nil)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
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

(setq tab-always-indent 'complete)

(provide 'ym-completion)
