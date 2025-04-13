;;; -*- lexical-binding: t -*-

(use-package dired
  :commands dired
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . toggle-truncate-lines))
  :bind
  (("C-x D" . list-directory)
   :map dired-mode-map
   ("M-s f" . nil)
   ("M-s g" . nil)
   ("," . dired-up-directory)
   ("." . dired-find-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-AGFhlv --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t
        dired-dwim-target t
        insert-directory-program "gls")
  (advice-add 'dired-view-file :around
              (defun dired-view-other-buffer-a (orig-fn &rest args)
                (cl-letf (((symbol-function 'view-file) #'view-file-other-window))
                  (funcall orig-fn)))))

(use-package dired
  :if (>= emacs-major-version 28)
  :defer
  :config
  (setq dired-switches-in-mode-line 'as-is
        dired-do-revert-buffer t
        dired-mark-region t))
(use-package dired-aux
  :defer
  :config
  (setq dired-vc-rename-file t))
(use-package dired-preview
  :straight t
  :config
  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (window-width . 0.3)))
  (setq dired-preview-display-action-alist #'my-dired-preview-to-the-right)
  (setq dired-preview-delay 0.25
        dired-preview-max-size (expt 2 20)
        dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")))
(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;; not sure if these are any good
;; (use-package dired-filter
;;   :ensure t
;;   :after dired)
(use-package dired-x
  :after dired
  :config
  (setq dired-omit-mode 1)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (require 'ls-lisp)
  (setq directory-free-space-program nil)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (when-let (cmd (cond ((eq system-type "darwin") "open")
                       ((eq system-type "gnu/linux") "xdg-open")
                       ((eq system-type "windows-nt") "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))
(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist")
  :after dired
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward))
  :config (dired-hist-mode 1))

(use-package dired-rsync
  :straight t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync))
  :config
  (setq dired-rsync-options "-avh --progress"))
