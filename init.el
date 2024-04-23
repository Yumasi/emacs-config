;; *- lexical-binding: t; -*-

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.config/emacs/modules")

(require 'ym-elpaca)

(require 'ym-core)
(require 'ym-general) ; Make general.el available early

;; Packages setup
(require 'ym-evil)
(require 'ym-magit)
(require 'ym-project)
(require 'ym-which-key)

(require 'ym-ui)

(setq gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
