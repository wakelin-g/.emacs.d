(use-package corfu
  :straight (corfu :type git :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el")
                   :host github
                   :repo "minad/corfu")
  :hook (((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
         ((shell-mode eshell-mode) . my/corfu-shell-settings)
         (minibuffer-setup . my/corfu-enable-always-in-minibuffer))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . nil)
              ("M-RET" . corfu-insert)
              ("M-." . corfu-show-location)
              ("M-h" . nil)
              ([remap next-line] . nil)
              ([remap previous-line] . nil)
              ("M-." . corfu-info-location)
              ("C-h" . corfu-info-documentation))
  :config
  (setq corfu-count 8
        corfu-auto t
        corfu-cycle t
        corfu-min-width 80
        corfu-auto-prefix 4
        corfu-auto-delay 0.07
        corfu-scroll-margin 5
        corfu-on-exact-match 'insert
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary nil
        corfu-preview-current t
        corfu-preselect 'prompt)
  (use-package corfu-info
    :bind (:map corfu-map ("M-g" . nil)))
  (use-package corfu-history :defer 3 :config (corfu-history-mode 1))
  (use-package corfu-popupinfo
    :config (corfu-popupinfo-mode 1)
    :bind (:map corfu-map
                ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))

  (defvar my-corfu-minibuffer-exclude-modes (list read-passwd-map)
    "Minibuffer-local keymaps for which Corfu should be disabled.")
  (defvar my-corfu-minibuffer-exclude-commands
    '(org-ql-find)
    "Minibuffer commands for which Corfu should be disabled.")
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq this-command my-corfu-minibuffer-exclude-commands)
                (memq (current-local-map)
                      my-corfu-minibuffer-exclude-modes))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (use-package consult
    :bind (:map corfu-map
                ("M-m" . corfu-mode-to-minibuffer)
                ("C-<tab>" . corfu-move-to-minibuffer))
    :config
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

  (defun my/corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
                corfu-auto          nil)
    (setq-local corfu-map (copy-keymap corfu-map)
                completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))
  (defun corfu-insert-and-send ()
    (interactive)
    (corfu-insert)
    (cond
     ((and (derived-mode-p ('eshell-mode) (fboundp 'eshell-send-input))
           (eshell-send-input))
      (derived-mode-p 'comint-mode)
      (comint-send-input))))

  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  :straight t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; trying prescient for completion sorting
(use-package prescient
  :straight t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-sort-length-enable nil)
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.7)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode 1))
(use-package corfu-prescient
  :straight t
  :after (corfu prescient)
  :custom
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil)
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode 1))
(use-package vertico-prescient
  :straight t
  :after (vertico prescient)
  :custom
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))
