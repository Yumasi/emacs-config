(add-to-list 'load-path (concat (file-name-as-directory user-emacs-directory) "modules/langs"))

(require 'ym-guile)
(require 'ym-markdown)
(require 'ym-zig)

(provide 'ym-langs)
