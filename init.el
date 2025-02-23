;;; init.el --- Init file -*- lexical-binding: t -*-

;; reset gc threshold after early-init
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.1)

;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'server)
(unless (server-running-p)
  (server-start))

;; load other config files
(load (expand-file-name "gw-funcs.el" user-emacs-directory))
(setq custom-file (concat user-emacs-directory "custom.el"))

(dolist
    (file
     (directory-files
      (concat (expand-file-name user-emacs-directory) "config")
              t
              "^.[^#].+el$"))
     (load-file file))

(load custom-file :no-error-if-file-is-missing)

;; so this works if I do it here but not in the config/theme.el....
(add-hook 'server-after-make-frame-hook (lambda ()
                                          (modify-all-frames-parameters
                                           '((right-divider-width . 40)
                                             (internal-border-width . 25)))
                                          (dolist (face '(window-divider
                                                          window-divider-first-pixel
                                                          window-divider-last-pixel))
                                            (face-spec-reset-face face)
                                            (set-face-foreground face (face-attribute 'default :background)))
                                          (set-face-background 'fringe (face-attribute 'default :background))))

;;; init.el ends here
