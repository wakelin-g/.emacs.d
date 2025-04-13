;;; -*- lexical-binding: t -*-

;; common lisp
(use-package slime
  :straight t)
(use-package paredit
  :straight t)
(use-package rainbow-delimiters
  :straight t)

(setq inferior-lisp-program "sbcl")

;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(require 'paredit)
(defun override-slime-del-key ()
  (define-key slime-repl-mode-map
              (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-del-key)

;(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(require 'rainbow-delimiters)

(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

(use-package logos
  :straight t
  :bind (("C-x n n" . logos-narrow-dwim)
         ("C-x ]" . logos-forward-page-dwim)
         ("C-x [" . logos-backward-page-dwim)
         ("M-]" . logos-forward-page-dwim)
         ("M-[" . logos-backward-page-dwim)
         ("<f9>" . logos-focus-mode))
  :config
  (setq logos-outlines-are-pages t
        logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
          (conf-toml-mode . "^\\[")))
  (setq-default logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (defun my/tab-bar-show-toggle ()
    (when logos-focus-mode
      (logos-set-mode-arg 'tab-bar-mode -1)))
  (add-hook 'logos-focus-mode-hook #'my/tab-bar-show-toggle)

  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1)))
  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

;; python
(use-package python-mode
  :config
  (when (executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt --classic")))

(use-package python-mls
  :disabled
  :straight t
  :hook
  (inferior-python-mode . python-mls-mode))

(use-package conda
  :straight t
  :commands conda-env-activate
  :hook (eshell-first-time-mode . conda-env-initialize-eshell)
  :init
  (conda-env-initialize-interactive-shells)
  :config
  (setq conda-anaconda-home "/Users/griffen/mambaforge")
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  (add-to-list
   'global-mode-string
   '(:eval
     (list
      (if conda-env-current-name
          (propertize (concat "(conda: " conda-env-current-name ") ")
                      'face 'font-lock-builtin-face
                      'help-echo "Conda environment"
                      'mouse-face '(:box 1)
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1
                                  (lambda () (interactive)
                                    (conda-env-activate))))
        "")))))

(use-package elpy
  :straight t
  :config
  (elpy-enable))

(use-package jupyter
  :disabled
  :straight t)

(use-package quarto-mode
  :straight t)

(custom-set-variables '(python-indent-guess-indent-offset t))
(custom-set-variables '(python-indent-guess-indent-offset-verbose nil))

