(use-package evil
  :init
  (setf evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  :general-config
  (ym/leader-def
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'execute-extended-command)
  (ym/leader-def
    :states 'normal
    :keymaps 'override
    "f s" 'save-buffer
    "w" 'evil-window-map
    "h" 'help-command
    "b k" 'kill-this-buffer
    "b b" 'switch-to-buffer))

(use-package evil-collection
  :after evil
  :diminish 'evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'ym-evil)
