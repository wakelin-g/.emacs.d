(use-package yasnippet
  :straight t
  :ensure t
  :hook ((text-mode prog-mode snippet-mode) . yas-minor-mode)
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (yas-reload-all))
(use-package yasnippet-snippets
  :straight t)

(define-key yas-minor-mode-map (kbd "M-z") 'yas-expand)
(define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-k") 'yas-prev-field)
