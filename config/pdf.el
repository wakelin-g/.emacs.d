(use-package pdf-tools
  :straight (pdf-tools :type git :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools")
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdftools
  :straight (org-pdftools :type git :files ("org-pdftools.el" "org-pdftools-pkg.el") :host github :repo "fuxialexander/org-pdftools")
  :hook (org-mode . org-pdftools-setup-link))

(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
