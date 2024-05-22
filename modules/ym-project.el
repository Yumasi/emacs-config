(require 'project)

(ym/leader-def
  :states 'normal
  :keymaps 'override
  "p" (general-key "C-x p")) ; Rebind project.el to <leader> p

(provide 'ym-project)
