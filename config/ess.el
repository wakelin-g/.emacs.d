;;; -*- lexical-binding: t -*-

;; emacs speaks statistics
(use-package ess
  :straight t
  :config
  (setq ess-fancy-comments nil)
  (add-hook 'ess-mode-hook (lambda () (ess-set-style 'RStudio)))
  (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)
  (define-key inferior-ess-mode-map "\C-cw" 'ess-execute-screen-options)
  :init (require 'ess-site))
