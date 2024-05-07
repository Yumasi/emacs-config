(use-package evil
  :init
  (setf evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  :general-config
  (ym/leader-def 'normal
    "f s" 'save-buffer
    "w" 'evil-window-map
    "h" 'help-command))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'ym-evil)
