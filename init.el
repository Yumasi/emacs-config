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
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Who am I ?
(setf user-full-name "Guillaume Pagnoux"
      user-mail-address "gpagnoux@gmail.com")

;; Shell to use
(setf shell-file-name "/usr/bin/zsh")

;; Follow symlinks
(setf vc-follow-symlinks t)

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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

(defalias 'yes-or-no-p 'y-or-n-p) ; One letter answers

;; Fonts
(set-face-attribute 'default nil :font "Iosevka" :height 160)
(set-face-attribute 'mode-line nil :family "Iosevka" :height 0.8)
(set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 0.8)

;; Whitespace mode
(use-package whitespace
  :init
  (setf
   whitespace-style
   '(face
     tabs
     space-before-tab
     spaces
     trailing
     identation
     space-mark
     tab-mark
     missing-newline-at-eof))
  :hook
  (prog-mode . (lambda ()
                (whitespace-mode 1))))

;; EXWM - Only on Linux
(when (eq window-system 'x)
  (defun yum/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (use-package exwm
    :custom
    (exwm-replace nil "Don't ask to replace the current window manager")
    :config
    (require 'exwm-randr)
    (setf exwm-randr-workspace-output-plist
          '(
            1 "eDP"
            2 "eDP"
            3 "eDP"
            4 "eDP"
            5 "eDP"
            6 "HDMI-A-0"
            7 "HDMI-A-0"
            8 "HDMI-A-0"
            9 "HDMI-A-0"
            0 "HDMI-A-0"))
    (exwm-randr-enable)
    (setf exwm-workspace-number 10)

    (add-hook 'exwm-update-class-hook #'yum/exwm-update-class)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    (setf exwm-input-prefix-keys
          '(?\C-x
            ?\C-h
            ?\M-x
            ?\C-\ )) ; Ctrl+Space
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
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
            ([?\s-t] . exwm-floating-toggle-floating)

            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))
    (exwm-enable)))

;; Theme
(use-package doom-themes
  :config
  (setf doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-octagon t)
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

;; Scroll margin
(setf scroll-margin 4
      scroll-conservatively 100)

;; Fill column indicator
(add-hook! prog-mode-hook display-fill-column-indicator-mode)

;; Fancy icons in dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;; Packages

;; Evil
(use-package general
  :config
  (general-override-mode)
  (general-create-definer
    yum/leader-keys
    :states '(normal
              insert
              visual
              emacs)

    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package evil
  :init
  (setf evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(general-define-key
 :states 'insert
 "C-g" 'evil-normal-state)

;; Helpful
(use-package helpful
  :general
  (yum/leader-keys
    "h" '(:ignore t :which-key "help")
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h o" 'helpful-symbol
    "h k" 'helpful-key))

;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  ;; Display the column number
  (column-number-mode 1)
  ;; Display the battery
  (display-battery-mode t)
  ;; Display the time
  (setf display-time-format "%a %d %b %R"
        display-time-default-load-average nil)
  (display-time-mode 1)


  (setf doom-modeline-buffer-encoding nil))

;; Magit
(use-package magit
  :config
  (setf transient-values '((magit-commit "--signoff" "--allow-empty"))
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (yum/leader-keys
    "g" '(:ignore t :which-key "git")
    "g s" 'magit-status))

;; Vertico
(use-package vertico
  :straight (:files ("*.el" "extensions/*.el"))
  :init
  (vertico-mode)

  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous))

(use-package orderless
  :init
  (setf completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package projectile
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/repo")
    (setq projectile-project-search-path '("~/repo")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Org
(use-package org
  :config
  (setf org-todo-keywords '((sequence "PROJ" "TODO" "STRT" "|" "DONE"))  ; Extra todo keywords
        org-log-done 'time  ; Log time when marking a todo as done
        org-startup-indented t))

(use-package org-bullets
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Direnv
(use-package direnv
  :config (direnv-mode))

;; Vterm
(use-package vterm
  :custom
  (vterm-shell "/usr/bin/fish")
  (vterm-buffer-name-string "*vterm - %s*")
  :config
  (setq vterm-timer-delay 0.01))

;;; Editing

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-autodownload t))

(setq-default indent-tabs-mode nil
              require-final-newline t)
(add-hook! before-save-hook delete-trailing-whitespace)

;;; Languages support

;; Meson

(use-package meson-mode
  :defer t)

;; Bitbake
(use-package bitbake
  :defer t
  :custom
  (bitbake-poky-directory nil)
  (bitbake-build-directory nil))

;;; Global keybindings

(yum/leader-keys
  :keymaps 'override
  "SPC" '(execute-extended-command :which-key "M-x")

  "a" '(:ignore t :which-key "apps")
  "a p" 'proced
  "a s" 'eshell
  "a t" 'vterm-other-window

  "b" '(:ignore t :which-key "buffers")
  "b k" 'kill-this-buffer
  "b b" 'ido-switch-buffer

  "f" '(:ignore t :which-key "files")
  "f d" 'dired
  "f f" 'find-file
  "f s" 'save-buffer

  "w" '(evil-window-map :which-key "windows")
  "p" '(projectile-command-map :which-key "projects"))

;; Reset the gc threshold to some reasonable value
(setf gc-cons-threshold (* 16 1024 1024)) ;; 16 MB

;; Load custom stuff
(when (file-directory-p custom-file)
  (load custom-file))
;;; init.el ends here
