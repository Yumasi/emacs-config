(use-package lispy
  :hook ((emacs-lisp-mode scheme-mode) . lispy-mode)
  :diminish 'lispy-mode
  :config
  (setq lispy-safe-delete t
        lispy-safe-copy t
        lispy-safe-paste t
        lispy-safe-actions-no-pull-delimiters-into-comments t))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :diminish 'lispyville-mode
  :config
  (lispyville-set-key-theme '(operators
                              c-w
                              prettify
                              text-objects
                              atom-movememt
                              additional-movement
                              commentary
                              slurp/barf-cp
                              escape)))

(provide 'ym-lisp)
