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

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 25)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
