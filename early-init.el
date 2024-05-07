;; Make startup time faster by prevent the gc to run too often. This will be
;; reset later.
(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-async-report-warnings-errors nil
      load-prefer-newer noninteractive
      warning-minimum-level :error)

(setq package-enable-at-startup nil
      mode-line-format nil
      frame-inhibit-implied-resize t
      ;; Disable UI elements
      default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)))


;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
