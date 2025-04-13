;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file is loaded before package.el is initialized, and
;; before the first graphical frame is initialized in Emacs 27+.
;;
;;; Code:

(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq idle-update-delay 1.0)

;; defer gc
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1)))

;; prevent loading any packages prior to init.el
(setq package-enable-at-startup t)

;; increase scrolling performance
(setq redisplay-skip-fontification-on-input t)
(setq fast-but-imprecise-scrolling t)

;; faster to disable these before they have been initialized
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; start scratch buffer in `fundamental-mode' instead (performance boost)
(setq initial-major-mode 'fundamental-mode)

;; ensure eln-cache is set
(unless (version-list-<
         (version-to-list emacs-version)
         '(28 0 1 0))
  (when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory))))

(setq default-frame-alist
      (append default-frame-alist
              '((left   . 200)
                (top    . 200)
                (width  . 145)
                (height . 30))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;; early-init.el ends here
