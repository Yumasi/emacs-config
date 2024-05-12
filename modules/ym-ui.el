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
      modus-themes-variable-pitch-ui t)
(load-theme 'modus-vivendi)

(provide 'ym-ui)
