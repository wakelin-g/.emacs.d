;;; -*- lexical-binding: t -*-

;; core settings
(setq ring-bell-function 'ignore
      visible-bell t
      use-dialog-box nil
      confirm-kill-processes nil
      compilation-scroll-output t
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      make-pointer-invisible t
      sentence-end-double-space nil
      cursor-in-non-selected-windows 'hollow
      highlight-nonselected-windows t
      display-line-numbers-type 'relative)

;; set env on macOS?

;; macOS key modifiers
(setq mac-command-modifier 'meta) ;; make cmd meta
(setq mac-option-modifier 'super) ;; make opt key super

;; global defaults
(setq-default
 fill-column 100 ;; especially for olivetti-mode, 80 isn't enough
 word-wrap t
 indent-tabs-mode nil
 require-final-newline t
 tab-width 4
 default-directory "~/")

;; core modes
(tooltip-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)
(column-number-mode t)
(global-subword-mode t)
(save-place-mode 1)
(global-so-long-mode t)
;; (global-display-line-numbers-mode t)
(global-auto-revert-mode t)
(global-eldoc-mode t)
(global-hi-lock-mode t)
(electric-pair-mode t)

;; display line numbers in prog-mode only
(defun gw/display-line-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'gw/display-line-numbers-hook)

;; alias yes.no
(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title
;; (setq frame-title-format '((:eval (if (buffer-file-name)
;;                                       (abbreviate-file-name (buffer-file-name))
;;                                     "%b"))))
(setq frame-title-format '((:eval (format "%s" (cdr (assoc 'name (tab-bar--current-tab)))))))

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; no lockfiles
(setq create-lockfiles nil)

;; history
(setq history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      recentf-max-saved-items 100
      recentf-max-menu-items 100
      recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
(savehist-mode 1)
(recentf-mode 1)

;; scrolling
(setq scroll-margin 2)
(setq scroll-conservatively scroll-margin)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(6 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-inhibit-click-time nil)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(setq next-error-recenter (quote (4)))
(setq fast-but-imprecise-scrolling nil)
(setq jit-lock-defer-time 0)

;; clipboard support
(setq select-enable-clipboard t)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq mouse-yank-at-point t)

;; undo
(fset 'undo-auto-amalgamate 'ignore) ;; don't group undo steps
(setq undo-limit 67108864) ;; increase undo limits
(setq undo-strong-limit 100663296) ;; increase strong limit
(setq undo-outer-limit 1006632960) ;; outer limit what is this

;; indentation
(setq-default evil-indent-convert-tabs nil)
(setq-default evil-shift-round nil)
(setq-default indent-tabs-mode nil)

;; font
(set-face-attribute 'default nil
                    :family "IosevkaTerm Nerd Font Mono"
                    :height 200)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Aile"
                    :height 200)
