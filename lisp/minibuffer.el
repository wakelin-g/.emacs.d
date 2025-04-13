;;; -*- lexical-binding: t -*-

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :bind (("M-s M-g" . consult-grep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)
         ("M-s M-i" . consult-imenu)))

(use-package embark
  :straight t
  :ensure t
  :bind (("C-." . embark-act)
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))
(use-package embark-consult
  :straight t
  :ensure t)
(use-package wgrep
  :straight t
  :ensure t
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic orderless))
      )
   (orderless-component-separator 'orderless-escapable-split-on-space)
   (orderless-matching-styles
    '(orderless-literal
      orderless-prefixes
      orderless-initialism
      orderless-regexp
      ))
   (orderless-style-dispatchers
    '(prot-orderless-literal-dispatcher
      prot-orderless-strict-initialism-dispatcher
      prot-orderless-flex-dispatcher
      ))
   :init
   (defun orderless--strict-*-initialism (component &optional anchored)
     "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
     (orderless--separated-by
         '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
       (cl-loop for char across component collect `(seq word-start ,char))
       (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
       (when (eq anchored 'both)
         '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

   (defun orderless-strict-initialism (component)
     "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
     (orderless--strict-*-initialism component))

   (defun prot-orderless-literal-dispatcher (pattern _index _total)
     "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
     (when (string-suffix-p "=" pattern)
       `(orderless-literal . ,(substring pattern 0 -1))))

   (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
     "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
     (when (string-suffix-p "," pattern)
       `(orderless-strict-initialism . ,(substring pattern 0 -1))))

   (defun prot-orderless-flex-dispatcher (pattern _index _total)
     "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
     (when (string-suffix-p "." pattern)
       `(orderless-flex . ,(substring pattern 0 -1))))
   ))

(use-package marginalia
  :ensure t
  :straight t
  :after (consult vertico)
  :init (marginalia-mode 1)
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

(use-package consult-dir
  :straight t
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
