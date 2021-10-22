;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; Make UTF-8 the default coding system
(set-language-environment "UTF-8")

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


;; Reset the gc threshold to some reasonable value
(setf gc-cons-threshold (* 16 1024 1024)) ;; 16 MB

;;; init.el ends here
