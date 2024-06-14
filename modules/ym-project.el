(require 'project)

;; Configure compilation mode
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output t)

(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-regexp "Find regexp")
        (project-find-dir "Find directory")
        (magit-project-status "Magit" ?v)
        (project-eshell "Eshell")))

(ym/leader-def
  :states 'normal
  :keymaps 'override
  "p" (general-key "C-x p")) ; Rebind project.el to <leader> p

(provide 'ym-project)
