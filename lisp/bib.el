;;; -*- lexical-binding: t -*-

(setq gw/default-bibliography `(,(expand-file-name "~/Dropbox/org/roam/library.bib" nil)))

(setq citar-bibliography gw/default-bibliography
	  citar-at-point-function 'embark-act
	  ;; citar-format-reference-function 'citar-citeproc-format-reference
      citar-org-roam-subdir "reference/"
      citar-notes-paths '("~/Dropbox/org/roam/reference/")
	  bibtex-completion-notes-path "~/Dropbox/org/roam"
	  bibtex-completion-bibliography gw/default-bibliography
	  org-cite-global-bibliography gw/default-bibliography
	  bibtex-completion-pdf-field "file")

(use-package citar
  :straight t
  :after (org org-roam)
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  :config
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green ;; should this be `nerd-icon-green' instead?
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange ;; should this be `nerd-icon-orange' instead?
              :v-adjust -0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue ;; should this be `nerd-icon-blue' instead?
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icons-green) ;; should this be `nerd-icon-green' instead?
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons)))

(use-package embark-consult
  :straight t
  :after (org-roam)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package citar-embark
  :straight t
  :after (org-roam)
  :ensure t
  :init
  (citar-embark-mode))
(use-package citar-org-roam
  :straight t
  :after (citar org-roam)
  :ensure t
  :config
  (setq citar-org-roam-template-fields
        '((:citar-citekey "key")
          (:citar-title   "title")
          (:citar-author  "author" "editor")
          (:citar-date    "date" "year" "issued")
          (:citar-pages   "pages")
          (:citar-type    "=type=")))
  (defun citar-add-org-noter-document-property (key &optional entry)
    "Set various properties PROPERTIES drawer when new citar note is created."
    (interactive)
    (let* ((file-list-temp (list (citar--select-resource key :files t)))
           (file-path-temp (alist-get 'file file-list-temp))
           (cite-author (cdr (citar-get-field-with-value '(author) key)))
           (cite-url (cdr (citar-get-field-with-value '(url) key))))
      (org-set-property "NOTER_DOCUMENT" file-path-temp)
      (org-set-property "Custom_ID"      key)
      (org-set-property "AUTHOR"         cite-author)
      (org-set-property "URL"            cite-url)
      (org-roam-ref-add (concat "@" key))
      (org-id-get-create)))
  (advice-add 'citar-create-note :after #'citar-add-org-noter-document-property)
  :init
  (citar-org-roam-mode))

(defun citar-add-org-noter-document-property ()
  "Set various properties PROPERTIES drawer when new citar note created."
  (interactive)
  (let* ((key (car (cdr (car (org-collect-keywords '("CITAR_KEY"))))))
         (file-list-temp (list (citar--select-resource key :files t)))
         (file-path-temp (alist-get 'file file-list-temp))
         (cite-author (cdr (citar-get-field-with-value'(author) key)))
         (cite-doi (cdr (citar-get-field-with-value '(doi) key)))
         (cite-year (cdr (citar-get-field-with-value '(year) key)))
         (cite-month (cdr (citar-get-field-with-value '(month) key)))
         (cite-journal (cdr (citar-get-field-with-value '(journal) key))))
    (org-set-property "NOTER_DOCUMENT" file-path-temp)
    (org-set-property "Custom_ID" key)
    (org-set-property "AUTHOR" cite-author)
    (org-set-property "DOI" cite-doi)
    (org-set-property "YEAR" cite-year)
    (org-set-property "MONTH" cite-month)
    (org-roam-ref-add (concat "@" key))
    (org-id-get-create)))
