;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; Make UTF-8 the default coding system (set-language-environment "UTF-8") ;; Minimal startup
(setf inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setf straight-use-package-by-default t)

;;; Utils
(defmacro add-hook! (hook f)
  `(add-hook ',hook ',f))

;;; Basic config
(setf backup-directory-alist `(("." . "~/.emacs.personal/.file-backups")))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; UI

;; Disable useless UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Also clear the variables
(setf menu-bar-mode nil ; Disable menubar
      tool-bar-mode nil ; Disable toolbar
      scroll-bar-mode nil ; Disable scrollbar
      tooltip-mode nil ; Disable tooltips
      set-fringe-mode 10) ; Give us some margins

(setf visible-bell t) ; Scream at me quietly

;; Fonts
(set-face-attribute 'default nil :font "Iosevka" :height 160)
(custom-set-faces
  '(mode-line ((t (:family "Iosevka" :height 0.9))))
  '(mode-line-inactive ((t (:family "Iosevka" :height 0.9)))))

;; Theme
(use-package doom-themes
  :config
  (setf doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-monokai-pro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Line numbers
(add-hook! prog-mode-hook display-line-numbers-mode)

;;; Packages

;; Evil
(use-package evil
  :init
  (setf evil-want-keybinding nil)
  :config (evil-mode 1))
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Magit
(use-package magit)

;; Org
(setf org-todo-keywords '((sequence "PROJ" "TODO" "STRT" "|" "DONE")) ; Extra todo keywords
      org-log-done 'time) ; Log time when marking a todo as done

;;; Languages support

;; Meson

(use-package meson-mode)

;; Reset the gc threshold to some reasonable value
(setf gc-cons-threshold (* 16 1024 1024)) ;; 16 MB

;;; init.el ends here
