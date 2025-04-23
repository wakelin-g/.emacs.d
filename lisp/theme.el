;;; -*- lexical-binding: t -*-

(use-package modus-themes
  :straight t
  :ensure t
  :disabled
  :config
  ;; (setq modus-themes-custom-auto-reload nil
  ;;       modus-themes-mixed-fonts t
  ;;       modus-themes-variable-pitch-ui t
  ;;       modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil
  ;;       modus-themes-completions '((t . (extrabold)))
  ;;       modus-themes-prompts '(extrabold)
  ;;       modus-themes-headings
  ;;       '((agenda-structure . (variable-pitch light 2.2))
  ;;         (agenda-date . (variable-pitch regular 1.3))
  ;;         (t . (regular 1.15))))
  ;; (setq modus-themes-common-palette-overrides nil)
  ;; (modus-themes-load-theme 'modus-operandi-deuteranopia)

  ;; stolen from karthink
  (setq modus-themes-common-palette-overrides
        `((date-common cyan)
          (date-deadline red-warmer)
          (date-event magenda-warmer)
          (date-holiday blue)
          (date-now yellow-warmer)
          (date-scheduled magenta-cooler)
          (date-weekday )
          (date-weekend )
          (mail-recipient )
          (fg-heading-1 )
          (fg-heading-2 )
          (fg-heading-3 )
          (fg-line-number-inactive )
          (fg-line-number-active fg-main)
          (fg-line-number-inactive unspecified)
          (fg-line-number-active unspecified)
          (bg-region bg-sage)
          (fg-region unspecified)
          (comment yellow-cooler)
          (string green-cooler)
          (fringe unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          ;; tab-bar themeing
          (bg-tab-bar bg-main)
          (bg-tab-current bg-cyan-intense)
          (bg-tab-other bg-inactive)))
  (setq modus-operandi-palette-overrides
        '((bg-mode-line-active bg-blue-intense)
          (fg-mode-line-active fg-main)
          (fg-heading-1 "#a01f64")
          (fg-heading-2 "#2f5f9f")
          (fg-heading-3 "#1a8388")))
  (setq modus-vivendi-palette-overrides
        `((fg-main "#d6d6d4")
          (bg-main "#121212")
          (bg-region bg-lavender)
          (bg-main "#090909")
          (fg-heading-1 magenda-faint)
          (bg-main "#181A1B")
          (bg-mode-line-active bg-lavender)
          (fg-mode-line-active "#ffffff")))
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-bold-constructs t
        modus-themes-prompts '(bold background)
        modus-themes-variable-pitch-ui nil
        modus-themes-headings
        '((0 . (1.35))
          (1 . (1.30))
          (2 . (1.24))
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome))))
  (modus-themes-load-theme 'modus-operandi))

(use-package doom-themes
  :straight t
  :disabled
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven t))
