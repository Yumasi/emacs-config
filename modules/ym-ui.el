;; Setup theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

;; Setup fonts
(set-face-attribute 'default nil :font "Iosevka" :height 155)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Futura" :weight 'regular :height 1.0)
(set-face-attribute 'mode-line nil :family "Iosevka" :height 0.9)
(set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 0.9)

;; Setup Whitespace mode
(setq
 whitespace-style
 '(face
   tabs
   space-before-tab
   trailing
   indentation
   tab-mark
   missing-newline-at-eof))

(defun ym-enable-whitespace-mode ()
  (whitespace-mode 1)
  (diminish 'whitespace-mode))

(add-hook 'prog-mode-hook #'ym-enable-whitespace-mode)

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package eldoc-box
  :hook ((eglot-managed-mode lisp-mode lisp-data-mode) . eldoc-box-hover-at-point-mode)
  :diminish 'eldoc-box-hover-at-point-mode)

(provide 'ym-ui)
