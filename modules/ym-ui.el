;; Setup then load Modus themes
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-syntax '(alt-syntax)
      modus-themes-mixed-fonts t
      modus-themes-links '(italic background)
      modus-themes-mode-line '(accented)
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (accented intense)))
      modus-themes-fringes '(intense)
      modus-themes-hl-line '(accented)
      modus-themes-subtle-line-numbers t
      modus-themes-paren-match '(intense bold)
      modus-themes-region '(accented)
      modus-themes-variable-pitch-ui t
      modus-themes-tabs-accented t)
(load-theme 'modus-vivendi)

;; Setup fonts
(set-face-attribute 'default nil :font "Iosevka" :height 160)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 160)
(set-face-attribute 'variable-pitch nil :font "Futura" :weight 'regular :height 160)
(set-face-attribute 'mode-line nil :family "Iosevka" :height 0.8)
(set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 0.8)

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

(provide 'ym-ui)
