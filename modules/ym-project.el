(require 'project)

;; Configure compilation mode
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output t)

(ym/leader-def
  :states 'normal
  :keymaps 'override
  "p" (general-key "C-x p")) ; Rebind project.el to <leader> p

(provide 'ym-project)
