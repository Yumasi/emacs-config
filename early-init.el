;;; early-init.el -*- lexical-binding: t; -*-

;; early-init.el to setup things before packages and UI are initialized.

;; Make startup time faster by prevent the gc to run too often. This will be
;; reset later.
(setf gc-cons-threshold most-positive-fixnum)

;; Needed for straight.el
(setf package-enable-at-startup nil)
