;;; -*- lexical-binding: t -*-

(use-package nerd-icons
  :straight t
  :ensure t
  :custom
  (nerd-icons-font-family "IosevkaTerm Nerd Font"))
(use-package nerd-icons-completion
  :straight t
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :straight t
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; tab-bar
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-tab-hints t
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

