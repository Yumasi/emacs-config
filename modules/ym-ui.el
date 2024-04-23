(defun ym-disable-modeline ()
  (interactive)
  (setq mode-line-format nil))

(defun ym-after-init-scratch ()
  (with-current-buffer "*scratch*"
    (ym-disable-modeline)
    (nano-modeline-text-mode)))

(use-package nano-modeline
  :hook (((prog-mode text-mode) . ym-disable-modeline)
         (prog-mode . nano-modeline-prog-mode)
         (text-mode . nano-modeline-text-mode)
         (org-mode . nano-modeline-org-mode)
         (after-init . ym-after-init-scratch))
  :config
  (nano-modeline-prog-mode t))

(use-package nano-theme
  :ensure (:host github :repo "rougier/nano-theme")
  :config
  (load-theme 'nano t))

;;(use-package nano-minibuffer
;;  :ensure (:host github :repo "rougier/nano-minibuffer")
;;  :config
;;  (nano-minibuffer-mode))

(provide 'ym-ui)
