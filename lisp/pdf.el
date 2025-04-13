;;; -*- lexical-binding: t -*-

(use-package pdf-tools
  :straight (pdf-tools :type git :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools")
  :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :straight (org-noter :type git :files ("*.el" "modules" (:exclude "*-test-utils.el" "*-devel.el") "org-noter-pkg.el") :host github :repo "org-noter/org-noter")
  :ensure t
  :after (org pdf-tools org-pdftools)
  :config
  (setq org-noter-default-notes-file-names '("notes.org")
        org-noter-notes-search-path '("~/Dropbox/org/noter")
        org-noter-separate-notes-from-heading t))

(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
