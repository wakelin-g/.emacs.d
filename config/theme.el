;;; -*- lexical-binding: t -*-

(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-mode-line '(borderless)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-paren-match '(bold)
        modus-themes-syntax '(green-strings)
        modus-themes-subtle-line-numbers t
        modus-themes-headings '((t . (bold rainbow)))
        modus-themes-scale-headings t)
  (load-theme 'modus-operandi-tritanopia :no-confirm))
