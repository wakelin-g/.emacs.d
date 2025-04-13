;;; -*- lexical-binding: t -*-

;; (setq mode-line-default-help-echo nil
;;       show-help-function nil)
;; (defvar mode-line-cleaner-alist
;;   `((company-mode . " ⇝")
;;     (corfu-mode . " ⇝")
;;     (yas-minor-mode .  " ")
;;     (smartparens-mode . " ()")
;;     (evil-smartparens-mode . "")
;;     (eldoc-mode . "")
;;     (abbrev-mode . "")
;;     (evil-snipe-local-mode . "")
;;     (evil-owl-mode . "")
;;     (evil-rsi-mode . "")
;;     (evil-commentary-mode . "")
;;     (ivy-mode . "")
;;     (counsel-mode . "")
;;     (wrap-region-mode . "")
;;     (rainbow-mode . "")
;;     (which-key-mode . "")
;;     (undo-tree-mode . "")
;;     ;; (undo-tree-mode . " ⎌")
;;     (auto-revert-mode . "")
;;     ;; Major modes
;;     (lisp-interaction-mode . "λ")
;;     (hi-lock-mode . "")
;;     (python-mode . "Py")
;;     (emacs-lisp-mode . "Eλ")
;;     (nxhtml-mode . "nx")
;;     (dot-mode . "")
;;     (scheme-mode . " SCM")
;;     (matlab-mode . "M")
;;     (org-mode . " ORG";; "⦿"
;;               )
;;     (valign-mode . "")
;;     (eldoc-mode . "")
;;     (org-cdlatex-mode . "")
;;     (cdlatex-mode . "")
;;     (org-indent-mode . "")
;;     (org-roam-mode . "")
;;     (visual-line-mode . "")
;;     (latex-mode . "TeX")
;;     (outline-minor-mode . " ֍" ;; " [o]"
;;                         )
;;     (hs-minor-mode . "")
;;     (matlab-functions-have-end-minor-mode . "")
;;     (org-roam-ui-mode . " UI")
;;     (abridge-diff-mode . "")
;;     ;; Evil modes
;;     (evil-traces-mode . "")
;;     (latex-extra-mode . "")
;;     (strokes-mode . "")
;;     (flymake-mode . "fly")
;;     (sideline-mode . "")
;;     (god-mode . ,(propertize "God" 'face 'success))
;;     (gcmh-mode . ""))
;;   "Alist for `clean-mode-line'.

;;   ; ;; When you add a new element to the list, keep in mind that you
;;   ; ;; must pass the correct minor/major mode symbol and a string you
;;   ; ;; want to use in the modeline *in lieu of* the original.")

;; (defun clean-mode-line ()
;;   (cl-loop for cleaner in mode-line-cleaner-alist
;;            do (let* ((mode (car cleaner))
;;                      (mode-str (cdr cleaner))
;;                      (old-mode-str (cdr (assq mode minor-mode-alist))))
;;                 (when old-mode-str
;;                   (setcar old-mode-str mode-str))
;;                 (when (eq mode major-mode)
;;                   (setq mode-name mode-str)))))
;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

(setq doom-modeline-hud t)
(use-package doom-modeline
  :straight t
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (display-time) ;; needed to show time in bar
  (setq doom-modeline-spc-face-overrides             (list :family (face-attribute 'fixed-pitch :family))
        doom-modeline-support-imenu                  t
        doom-modeline-height                         25
        doom-modeline-bar-width                      0 ;; this apparent can fuck spacing up
        doom-modeline-hud                            t ;; wat do
        doom-modeline-buffer-file-name-style         'auto
        doom-modeline-icon                           t

        doom-modeline-major-mode-icon                t
        doom-modeline-major-mode-color-icon          t

        doom-modeline-buffer-state-icon              t
        doom-modeline-buffer-modification-icon       t

        doom-modeline-lsp                            t
        doom-modeline-lsp-icon                       t

        doom-modeline-time                           t
        doom-modeline-time-live-icon                 t
        doom-modeline-time-analogue-clock            nil
        display-time-default-load-average            nil ;; hides load average from time

        doom-modeline-buffer-name                    t
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-minor-modes                    t
        doom-modeline-modal                          t
        doom-modeline-modal-modern-icon              t
        doom-modeline-battery                        t
        doom-modeline-display-misc-in-all-mode-lines nil
        doom-modeline-buffer-file-name-function      #'identity
        doom-modeline-env-enable-python              t
        doom-modeline-env-load-string                "..."))

(provide 'setup-modeline)
