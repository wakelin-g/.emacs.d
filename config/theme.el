;;; -*- lexical-binding: t -*-

(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-mode-line '(borderless)
        modus-themes-region '(bg-only)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-paren-match '(bold)
        modus-themes-syntax '(green-strings)
        modus-themes-subtle-line-numbers t
        modus-themes-headings '((t . (bold rainbow)))
        modus-themes-common-palette-overrides
        '((date-deadline magenta-warmer)
          (date-scheduled green-cooler)
          (date-weekday fg-main)
          (date-event fg-dim)
          (date-now blue)
          (prose-done fg-alt)
          (prose-todo yellow)
          (fg-prose-block-delimiter fg-main)
          (bg-prose-block-contents bg-magenta-nuanced)
          (bg-prose-block-delimiter bg-lavender))
        modus-themes-scale-headings t)
  (load-theme 'modus-operandi t))

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 25)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

