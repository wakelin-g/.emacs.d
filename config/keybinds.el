;;; -*- lexical-binding: t -*-

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode))

(global-set-key (kbd "C-x k") 'gw/kill-this-buffer)
(global-set-key (kbd "C-c rr") 'gw/org-roam-rg-search)
(global-set-key (kbd "C-c ri") 'org-roam-node-insert)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)

(define-key calendar-mode-map "]" #'org-reverse-datetree-calendar-next)
(define-key calendar-mode-map "[" #'org-reverse-datetree-calendar-previous)
(define-key calendar-mode-map (kbd "RET") #'org-reverse-datetree-display-entry)
