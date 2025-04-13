;; (use-package auctex
;;   :straight t
;;   :config
;;   (setq TeX-parse-self t
;;         TeX-auto-save t
;;         TeX-auto-local ".auctex-auto"
;;         TeX-style-local ".auctex-style"
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-method 'synctex
;;         TeX-source-correlate-start-server nil
;;         TeX-electric-sub-and-superscript t
;;         TeX-save-query nil)
;;   (setq-default TeX-master t))

;; (use-package preview
;;   :hook (LaTeX-mode . LaTeX-preview-setup)
;;   :config
;;   (setq-default preview-scale 1.4
;;                 preview-scale-function
;;                 (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
;;   (setq preview-auto-cache-preamble nil))

;; (use-package cdlatex
;;   :straight t
;;   :hook ((LaTeX-mode . cdlatex-mode)
;;          (LaTeX-mode . org-cdlatex-mode))
;;   :config
;;   (setq cdlatex-use-dollar-to-ensure-math nil))
