(setf inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode
      initial-scratch-message nil
      inhibit-compacting-font-caches t
      vc-follow-symlinks t)

(setq-default indent-tabs-mode nil
	      require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Make Emacs a clean boi
(use-package no-littering
  :config
  (setq backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backups/")))))

(setf menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil
      set-fringe-mode 10
      scroll-margin 4
      scroll-conservatively 100)

(setq user-full-name "Guillaume Pagnoux"
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      enable-remote-dir-locals t)

(global-auto-revert-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when (file-directory-p custom-file)
  (load custom-file))

(provide 'ym-core)
