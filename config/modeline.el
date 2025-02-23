;;; -*- lexical-binding: t -*-

;; (use-package doom-modeline
;;   :straight t
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-support-imenu t
;;         doom-modeline-height 25
;;         doom-modeline-bar-width 0.1
;;         doom-modeline-hud t
;;         doom-modeline-buffer-file-name-style 'truncate-nil
;;         doom-modeline-icon t
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-major-mode-color-icon t
;;         doom-modeline-buffer-state-icon t
;;         doom-modeline-buffer-modification-icon t
;;         doom-modeline-time-icon t
;;         doom-modeline-buffer-name t))

(setq sml/no-confirm-load-theme t)
(use-package smart-mode-line
  :straight t
  :init
  (setq sml/theme 'respectful)
  (sml/setup))
