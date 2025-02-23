;;; -*- lexical-binding: t -*-

(if (executable-find "gls")
    (progn
      (setq insert-directory-program "gls")
      (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
  (setq dired-listing-switches "-ahlF"))

(use-package dirvish
  :straight (dirvish :type git :files (:defaults "extensions/*.el" "dirvish-pkg.el") :host github :repo "alexluigit/dirvish")
  :init
  (dirvish-override-dired-mode)
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  :bind
  (("C-c f" . dirvish)
   :map dirvish-mode-map
   ("a" . dirvish-quick-access)
   ("f" . dirvish-file-info-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("h" . dirvish-history-jump)
   ("s" . dirvish-quicksort)
   ("v" . dirvish-vc-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(with-eval-after-load 'dirvish
  (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))
  (setq dirvish-mode-line-height 21)
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  (setq dirvish-path-separators (list "  " "  " "  "))
  (setq dirvish-side-width 38)
  (setq dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dirvish-listing-switches "-l --almost-all --human-readable --group-directories-first --no-groups"))

(with-eval-after-load 'dirvish-quick-access
  (setopt dirvish-quick-access-entries
   '(("h" "~/"            "Home")
     ("d" "~/Downloads"   "Downloads")
     ("m" "/Volumes"      "Volumes")
     ("o" "~/orgmode"     "Org"))))
