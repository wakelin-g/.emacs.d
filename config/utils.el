(use-package counsel
  :straight t
  :commands (counsel-git-grep counsel-switch-buffer))

(use-package swiper
  :straight t
  :commands (swiper)
  :config
  (setq swiper-goto-start-of-match t))

(use-package find-file-in-project
  :straight t
  :commands (find-file-in-project))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode) . highlight-numbers-mode))

(use-package nerd-icons-completion
  :straight (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion"))

(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(use-package reveal-in-osx-finder
  :straight t)
