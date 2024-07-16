(use-package orderless
  :config
  (add-to-list 'completion-styles-alist
               '(tab completion-basic-try-completion ignore
                 "Completion style which provides TAB completion only."))
  :custom
  (completion-styles '(tab orderless basic))
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless)))))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :config
  (defun ym/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-dabbrev
                       #'cape-file))))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . ym/eglot-capf))

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
  (corfu-auto-delay 1)
  (corfu-auto-prefix 2)
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
