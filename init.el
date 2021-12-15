;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; Make UTF-8 the default coding system (set-language-environment "UTF-8") ;; Minimal startup
(setf inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-compacting-font-caches t)

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

;; No littering
(use-package no-littering)

;;; Basic config
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
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 160)
(set-face-attribute 'variable-pitch nil :font "San Francisco Text" :weight 'regular :height 160)
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
    (setf exwm-randr-workspace-monitor-plist
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
            0 "HDMI-A-0"
            6 "DisplayPort-0"
            7 "DisplayPort-0"
            8 "DisplayPort-0"
            9 "DisplayPort-0"
            0 "DisplayPort-0"))
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

;; Zoom mode
(use-package zoom
  :custom
  (zoom-ignored-major-modes '(vterm-mode))
  :config
  (zoom-mode 1))

;; Theme
(use-package doom-themes
  :config
  (setf doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
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

;; Highlight TODOs & stuff
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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
        evil-want-C-w-in-emacs-state t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(general-define-key
 :states 'insert
 "C-g" 'evil-normal-state)

;; Helpful
(use-package helpful)

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
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
(defun yum/org-setup ()
  (setf line-spacing 0.1
        left-margin-width 2
        right-margin-width 2)

  (variable-pitch-mode 1)
  (org-indent-mode 1))

(use-package org
  :hook (org-mode . yum/org-setup)
  (org-indent-mode . (lambda () (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))
  :config
  (setf org-todo-keywords '((sequence "PROJ(p)" "TODO(t)" "STRT(s)" "REVIEW(r)" "|" "DONE(d)"))  ; Extra todo keywords. More sequences can be defined.
        org-ellipsis " ▼"
        org-log-done 'time  ; Log time when marking a todo as done
        org-startup-indented t
        org-hide-emphasis-markers nil
        org-pretty-entities t
        org-fontify-quote-and-verse-blocks t
        org-agenda-start-with-log-mode t)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "San Francisco Text" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch org-block))
  (set-face-attribute 'org-document-title nil :height 1.4)
  (set-face-attribute 'org-ellipsis nil :foreground "#5e5e5e" :height 0.7)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :background "#23242f" :inherit '(fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))

  (yum/leader-keys
    :keymaps 'org-mode-map
    "m" '(:ignore t :which-key "org-mode")
    "m d" '(:ignore t :which-key "date")
    "m d s" 'org-schedule
    "m d d" 'org-deadline

    "m t" 'org-todo))

(use-package org-superstar
  :defer t
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package visual-fill-column
  :init
  (defun yum/org-visual-fill ()
    (setf fill-column 79
          visual-fill-column-width 100
          visual-fill-column-center-text t)

    (visual-fill-column-mode 1)
    (auto-fill-mode 1))

  :hook (org-mode . yum/org-visual-fill))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Direnv
(use-package direnv
  :config (direnv-mode))

;; Vterm
(use-package vterm
  :straight
  `(
    :pre-build (
                ("rm" "-fr" "build")
                ("mkdir" "build")
                ("bash" "-c" "cd \"$1\" && cmake .. && make" "--" ,(concat (straight--repos-dir "emacs-libvterm") "build"))))

  :custom
  (vterm-shell "/usr/bin/fish")
  (vterm-buffer-name-string "*vterm - %s*")
  :config
  (setq vterm-timer-delay 0.01))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-cd-auto-create-buffer t)
  :config
  (add-to-list 'display-buffer-alist
      '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
        (display-buffer-reuse-window display-buffer-in-direction)
        (direction . bottom)
        (dedicated . nil)
        (reusable-frames . visible)
        (window-height . 0.3)))
  :general
  (:keymaps 'vterm-mode-map
            "<f2>" 'vterm
            "<f3>" 'vterm-toggle-forward
            "<f4>" 'vterm-toggle-backward)
  (:keymaps 'override
            "<f1>" 'vterm-toggle
            "C-<f1>" 'vterm-toggle-cd))

;;; Editing

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-autodownload t
        parinfer-rust-library-directory (expand-file-name ".parinfer-rust/" user-emacs-directory)))

(setq-default indent-tabs-mode nil
              require-final-newline t)
(add-hook! before-save-hook delete-trailing-whitespace)

(use-package flycheck
  :config
  (global-flycheck-mode))

;; Docker stuff
(use-package docker-tramp)
(use-package docker)
(use-package dockerfile-mode)

;;; Languages support

;; Meson

(use-package meson-mode
  :defer t)

;; Bitbake
(use-package bitbake
  :defer t
  :mode (("\\.bb\\'" . bitbake-mode)
         ("\\.bbappend'" . bitbake-mode)
         ("\\.inc'" . bitbake-mode))
  :custom
  (bitbake-poky-directory nil)
  (bitbake-build-directory nil)
  :config
  (setf default-tab-width 8))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setf markdown-command "multimarkdown"))

;; Systemd
(use-package systemd
  :defer t)

;;; Global keybindings

(yum/leader-keys
  :keymaps 'override
  "SPC" '(execute-extended-command :which-key "M-x")

  "o" '(:ignore t :which-key "open")
  "o a" 'org-agenda
  "o d" 'docker
  "o p" 'proced
  "o s" 'eshell

  "b" '(:ignore t :which-key "buffers")
  "b k" 'kill-this-buffer
  "b b" 'ido-switch-buffer

  "f" '(:ignore t :which-key "files")
  "f d" 'dired
  "f f" 'find-file
  "f s" 'save-buffer

  "g" '(:ignore t :which-key "git")
  "g s" 'magit-status

  "h" '(:ignore t :which-key "help")
  "h f" 'helpful-callable
  "h F" 'describe-face
  "h v" 'helpful-variable
  "h o" 'helpful-symbol
  "h k" 'helpful-key

  "w" '(evil-window-map :which-key "windows")
  "p" '(projectile-command-map :which-key "projects"))

;; Reset the gc threshold to some reasonable value
(setf gc-cons-threshold (* 16 1024 1024)) ;; 16 MB

;; Load custom stuff
(when (file-directory-p custom-file)
  (load custom-file))
;;; init.el ends here
