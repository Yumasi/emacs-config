;; *- lexical-binding: t; -*-

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.config/emacs/modules")

(require 'ym-elpaca)

(require 'ym-core)
(require 'ym-ui)

;; Packages setup
(require 'ym-evil)
(require 'ym-which-key)
(require 'ym-magit)

(setq gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
