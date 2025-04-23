;;; -*- lexical-binding: t -*-

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode))

(define-prefix-command 'gw/leader-map)

(define-key global-map (kbd "C-c a")   'org-agenda)
(define-key global-map (kbd "C-c c")   'org-capture)
(define-key global-map (kbd "C-x k")   'gw/kill-this-buffer)
(define-key global-map (kbd "C-x C-r") 'counsel-recentf)
(define-key global-map (kbd "C-x b")   'consult-buffer)
(define-key global-map (kbd "C-x rb")  'consult-bookmark)
(define-key global-map (kbd "C-c z")   'reveal-in-osx-finder)
(define-key global-map (kbd "C-g")     'gw/keyboard-quit-dwim)
(define-key global-map (kbd "C-c dg")  'deadgrep)

(define-key calendar-mode-map "]" #'org-reverse-datetree-calendar-next)
(define-key calendar-mode-map "[" #'org-reverse-datetree-calendar-previous)
(define-key calendar-mode-map (kbd "RET") #'org-reverse-datetree-display-entry)

(define-key global-map (kbd "s-[") 'tab-bar-switch-to-prev-tab)
(define-key global-map (kbd "s-]") 'tab-bar-switch-to-next-tab)
(define-key global-map (kbd "s-t") 'tab-bar-new-tab)
(define-key global-map (kbd "s-w") 'tab-bar-close-tab)

(define-key dired-mode-map (kbd "C-x p") 'dired-preview-mode)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key org-mode-map (kbd "C-c b") 'citar-insert-citation)
