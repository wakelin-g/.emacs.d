;;; -*- lexical-binding: t -*-

;; emacs speaks statistics
(use-package ess
  :straight t
  :config
  (setq ess-fancy-comments nil)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (setq ess-eval-visibly-p nil)
  (add-hook 'ess-mode-hook (lambda () (ess-set-style 'RStudio)))
  (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (add-to-list 'mode-line-process '(:eval (nth ess--busy-count ess-busy-strings)))))
  (define-key inferior-ess-mode-map (kbd "C-c w") 'ess-execute-screen-options)
  :init
  (require 'ess-site))
