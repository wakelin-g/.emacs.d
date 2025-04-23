;;; init.el --- Init file -*- lexical-binding: t -*-

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

;; for emacsclient daemon
(require 'server)
(unless (server-running-p)
  (server-start))

(load (expand-file-name "gw-funcs.el" user-emacs-directory))
(setq custom-file (concat user-emacs-directory "custom.el"))

(use-package org
  :defer t
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

(load (expand-file-name "lisp/setup-modeline" user-emacs-directory))
(load (expand-file-name "lisp/better-buffers" user-emacs-directory))
;; (load (expand-file-name "lisp/setup-reading" user-emacs-directory))
; (load (expand-file-name "lisp/nano-agenda" user-emacs-directory))

(dolist
    (file
     (directory-files
      (concat (expand-file-name user-emacs-directory) "lisp")
      t
      "^.[^#].+el$"))
  (load-file file))

(load custom-file :no-error-if-file-is-missing)

;; (add-hook 'server-after-make-frame-hook (lambda ()
;; 					  (modify-all-frames-parameters
;; 					   '((right-divider-width . 40)
;; 					     (internal-border-width . 5)))
;; 					  (dolist (face '(window-divider
;; 							  window-divider-first-pixel
;; 							  window-divider-last-pixel))
;; 					    (face-spec-reset-face face)
;; 					    (set-face-foreground face (face-attribute 'default :background)))
;; 					  (set-face-background 'fringe (face-attribute 'default :background))))


;; NOTE: THIS OVERRIDES CORE EMACS SOURCE CODE BECAUSE FUCKING
;; DROPBOX DOES SOMETHING WEIRD AND SOME SAVANT FROM THE DOOM
;; EMACS DISCORD GAVE ME THIS AND IT WORKED! IN THE FUTURE, THIS
;; COULD AND WILL LIKELY BREAK!

(defun file-notify--call-handler (watch desc action file file1)
  "Call the handler of WATCH with the arguments DESC, ACTION, FILE and FILE1."
  (when (or
         ;; If there is no relative file name for that
         ;; watch, we watch the whole directory.
         (null (file-notify--watch-filename watch))
         ;; File matches.
         (string-equal
          (file-notify--watch-filename watch)
          (file-name-nondirectory file))

         ;; Directory matches.
         ;;  FIXME: What purpose would this condition serve?
         ;;  Doesn't it just slip through events for files
         ;;  having the same name as the last component of the
         ;;  directory of the file that we are really watching?
         ;;(string-equal
         ;; (file-name-nondirectory file)
         ;; (file-name-nondirectory (file-notify--watch-directory watch)))

         ;; File1 matches.
         (and (stringp file1)
              (string-equal (file-notify--watch-filename watch)
                            (file-name-nondirectory file1))))
    (when file-notify-debug
      (message
       "file-notify-callback %S %S %S %S %S %S %S"
       desc action file file1 watch
       (file-notify--watch-absolute-filename watch)
       (file-notify--watch-directory watch)))
    (when (file-notify--watch-callback watch)
      (funcall (file-notify--watch-callback watch)
             (if file1
                 (list desc action file file1)
               (list desc action file))))))/
