;;; -*- lexical-binding: t -*-

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode))

;; global mappings
(define-key global-map (kbd "C-x k") 'gw/kill-this-buffer)
(define-key global-map (kbd "C-c rr") 'gw/org-roam-rg-search)
(define-key global-map (kbd "C-c ri") 'org-roam-node-insert)
(define-key global-map (kbd "C-x C-r") 'counsel-recentf)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c z") 'reveal-in-osx-finder)
(define-key global-map (kbd "C-g") #'gw/keyboard-quit-dwim)

;; mode-specific mappings
(define-key calendar-mode-map "]" #'org-reverse-datetree-calendar-next)
(define-key calendar-mode-map "[" #'org-reverse-datetree-calendar-previous)
(define-key calendar-mode-map (kbd "RET") #'org-reverse-datetree-display-entry)
(define-key evil-normal-state-map (kbd "L") 'tab-next)
(define-key evil-normal-state-map (kbd "H") 'tab-previous)

