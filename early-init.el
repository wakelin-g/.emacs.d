;; -*- lexical-binding: t -*-

;; This file is loaded before package.el is initialized, and
;; before the first graphical frame is initialized in Emacs 27+.

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1)))

(setq package-enable-at-startup t)

(setq default-frame-alist
      (append default-frame-alist
              '((left   . 200)
                (top    . 200)
                (width  . 130)
                (height . 31))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
