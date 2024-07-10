;; *- lexical-binding: t; -*-

;; Add configuration modules to load path
(add-to-list 'load-path (concat (file-name-as-directory user-emacs-directory) "modules"))

(require 'ym-elpaca)

(require 'ym-core)
(require 'ym-ui)
(require 'ym-general) ; Make general.el available early

;; Packages setup
(require 'ym-direnv)
(require 'ym-evil)
(require 'ym-magit)
(require 'ym-project)
(require 'ym-vertico)
(require 'ym-which-key)
(require 'ym-ibuffer)
(require 'ym-yas)

(require 'ym-editorconfig)
(require 'ym-treesitter)

(require 'ym-completion)
(require 'ym-langs)
(require 'ym-lsp)
(require 'ym-dap)

;; Configure tramp
(require 'ym-tramp)

(setq gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
