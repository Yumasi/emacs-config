;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; Make UTF-8 the default coding system (set-language-environment "UTF-8") ;; Minimal startup
(setf inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Native comp
(setq native-comp-async-report-warnings-errors nil)

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
  "Add F to HOOK"
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
(set-face-attribute 'mode-line nil :family "Iosevka" :height 0.8)
(set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 0.8)

;; EXWM - Only on Linux
(when (eq window-system 'x)
  (defun user/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (use-package exwm
    :init
    (require 'exwm-randr)
    :config
    (exwm-randr-enable)
    (setf exwm-workspace-number 1)

    (add-hook 'exwm-update-class-hook #'user/exwm-update-class)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    (setf exwm-input-prefix-keys
	  '(?\C-x
	    ?\C-u
	    ?\M-x
	    ?\C-\ )) ; Ctrl+Space
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
    (setf exwm-input-global-keys
	  `(
	    ([?\s-r] . exwm-reset)
	    ([?\s-h] . windmove-left)
	    ([?\s-j] . windmove-down)
	    ([?\s-k] . windmove-up)
	    ([?\s-l] . windmove-right)

	    ([?\s-d] . (lambda (command)
			 (interactive (list (read-shell-command "$ ")))
			 (start-process-shell-command command nil command)))

	    ([?\s-w] . exwm-workspace-switch)

	    ,@(mapcar (lambda (i)
			`(,(kbd (format "s-%d" i)) .
			  (lambda ()
			    (interactive)
			    (exwm-workspace-switch-create ,i))))
		      (number-sequence 1 9))))
    (exwm-enable)))

;; Theme
(use-package doom-themes
  :config
  (setf doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-monokai-pro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Which key
(use-package which-key
  :init (which-key-mode)
  :config
  (setf which-key-idle-delay 0.3))

;; Rainbow delemiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :init (doom-modeline-mode 1)
  :config
  ;; Display the battery
  (display-battery-mode t)
  ;; Display the time
  (setf display-time-format "%a %d %b %R"
	display-time-default-load-average nil)
  (display-time-mode 1)


  (setf doom-modeline-buffer-encoding nil))

;; Magit
(use-package magit)

;; Vertico
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setf completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Org
(setf org-todo-keywords '((sequence "PROJ" "TODO" "STRT" "|" "DONE")) ; Extra todo keywords
      org-log-done 'time) ; Log time when marking a todo as done

;;; Languages support

;; Meson

(use-package meson-mode)

;; Reset the gc threshold to some reasonable value
(setf gc-cons-threshold (* 16 1024 1024)) ;; 16 MB

;;; init.el ends here
