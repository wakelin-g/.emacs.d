(use-package corfu
  :straight (corfu :type git :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el")
                   :host github
                   :repo "minad/corfu")
  :config
  (setq corfu-cycle nil
        corfu-count 14
        corfu-min-width 80
        corfu-max-width corfu-min-width
        corfu-auto-prefix 2
        corfu-auto-delay 0.25
        corfu-on-exact-match 'insert
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary nil
        corfu-preview-current t
        corfu-preselect 'valid)
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
