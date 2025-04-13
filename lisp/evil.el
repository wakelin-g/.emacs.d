;;(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(setq evil-want-Y-yank-to-eol t)

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'isearch)
  :config
  (evil-mode)
  (setq evil-ex-search-case 'sensitive))
(use-package undo-fu
  :straight t)
(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))
(use-package evil-org
  :straight t
  :demand t
  :hook ((org-mode) . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(calendar navigation todo shift))
  (evil-org-agenda-set-keys))
(use-package evil-commentary
  :straight t
  :demand t
  :config
  (evil-commentary-mode))
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(with-eval-after-load 'evil
  (evil-set-leader '(normal) (kbd "<SPC>")))

(evil-define-key 'normal 'global (kbd "<leader>k") 'counsel-git-grep)
(evil-define-key 'normal elfeed-search-mode-map (kbd "B") 'gw/elfeed-search-browse-background-url)
