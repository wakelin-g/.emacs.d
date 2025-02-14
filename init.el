;;; init.el --- Init file -*- lexical-binding: t; -*-
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq-default frame-title-format "%b %& emacs")
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(setq mac-command-modifier 'meta) ;; make cmd meta
(setq mac-option-modifier nil) ;; make opt key nothing
(setq use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p) ;; use just y or n for text-mode prompts

(setq-default indicate-empty-lines t) ;; show empty lines
(setq cursor-in-non-selected-windows 'hollow) ;; keep cursors and highlights in current window only
(setq highlight-nonselected-windows t) ;; keep selection region when changing windows
(setq bidi-display-reordering nil) ;; what is bidirectional text support

(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq make-pointer-invisible t)
(setq sentence-end-double-space nil)

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

;;; editing options
;; undo
(fset 'undo-auto-amalgamate 'ignore) ;; don't group undo steps
(setq undo-limit 67108864) ;; increase undo limits
(setq undo-strong-limit 100663296) ;; increase strong limit
(setq undo-outer-limit 1006632960) ;; outer limit what is this

;; case sensitivity
(setq-default case-fold-search nil) ;; case-sensitive search
(setq dabbrev-case-fold-search nil) ;; case-sensitive abbreviations
(setq-default search-upper-case nil) ;; case-sensitive counsel

;; indentation
(setq default-tab-width 4)
(setq tab-width 4)
(setq default-fill-column 80)
(setq fill-column 80)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)

;; packages
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

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
(straight-use-package '(org :type built-in))

;; theme
(load (expand-file-name "themes/timu-macos-theme.el" user-emacs-directory)) ;; load theme
(require 'timu-macos-theme)
(load-theme 'timu-macos t)
(customize-set-variable 'timu-macos-org-intense-colors t)

;; evil
(setq evil-want-C-u-scroll t) ;; need this before evil is loaded apparently
(setq evil-want-keybinding nil)
(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode)
  (setq evil-ex-search-case 'sensitive))
(straight-use-package '(undo-fu :type git :host codeberg :repo "ideasman42/emacs-undo-fu")) ;; Thin wrapper for undo
(straight-use-package '(evil-numbers :type git :host github :repo "juliapath/evil-numbers")) ;; increment & decrement
(straight-use-package '(evil-surround :type git :host github :repo "emacs-evil/evil-surround"))
(global-evil-surround-mode 1)

(use-package evil-org
  :straight t
  :demand t
  :hook ((org-mode) . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(calendar navigation todo shift))
  (evil-org-agenda-set-keys))

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode))

;; (use-package ivy
;;   :demand t
;;   :config
;;   (ivy-mode)
;;
;;   ;; Always show half the window height. Why?
;;   ;; .. useful when searching through large lists of content.
;;   (setq ivy-height-alist `((t . ,(lambda (_caller) (/ (frame-height) 2)))))
;;
;;   ;; VIM style keys in ivy (holding Control).
;;   (define-key ivy-minibuffer-map (kbd "C-j") 'next-line)
;;   (define-key ivy-minibuffer-map (kbd "C-k") 'previous-line)
;;   (define-key ivy-minibuffer-map (kbd "C-h") 'minibuffer-keyboard-quit)
;;   (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-done)
;;
;;   ;; open and next
;;   (define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-next-line-and-call)
;;   (define-key ivy-minibuffer-map (kbd "C-M-k") 'ivy-previous-line-and-call)
;;   (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-done)
;;
;;   ;; so we can switch away
;;   (define-key ivy-minibuffer-map (kbd "C-w") 'evil-window-map))

(use-package company ;; autocomplete backend
  :straight t
  :commands (company-complete-common company-dabbrev)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 40)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  ;; Key-map: hold Control for VIM motion
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") 'company-abort)
  (define-key company-active-map (kbd "<C-return>") 'company-complete-selection)
  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous))

;; swiper - interactive buffer searching
(use-package swiper
  :straight t
  :commands (swiper)
  :config
  (setq swiper-goto-start-of-match t))

;; counsel - interactive project-wide searching
(use-package counsel
  :straight t
  :commands (counsel-git-grep counsel-switch-buffer))

;; interactively narrow down files to find in project
(use-package find-file-in-project
  :straight t
  :commands (find-file-in-project))

;; highlight numbers
(use-package highlight-numbers
  :straight t
  :hook ((prog-mode) . highlight-numbers-mode))

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :weight 'light
                    :height 200)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka"
                    :weight 'light
                    :height 200)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka"
                    :weight 'light
                    :height 200)

;; emacs speaks statistics
(use-package ess
  :straight t
  :config
  (setq ess-fancy-comments nil)
  (add-hook 'ess-mode-hook (lambda () (ess-set-style 'RStudio)))
  (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)
  (define-key inferior-ess-mode-map "\C-cw" 'ess-execute-screen-options)
  :init (require 'ess-site))
(use-package quarto-mode :straight t)

;;; DISPLAY OPTIONS
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-widen t)
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
(setq column-number-mode t)
(show-paren-mode 1)
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq-default truncate-lines t)

(setq default-frame-alist
      (append default-frame-alist
              '((left   . 200)
                (top    . 200)
                (width  . 150)
                (height . 30))))

;;; MODES
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local tab-width 8)
            (setq-local evil-shift-width 2)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns '("*.org"))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local tab-width 2)
            (setq-local evil-shift-width 2)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns '("*.el"))
            (modify-syntax-entry ?- "w")
            (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (setq-local tab-width 4)
            (setq-local evil-shift-width 4)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns '("*.py"))))
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local tab-width 4)
            (setq-local evil-shift-width 4)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns '("*.sh"))))
(add-hook 'c-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local c-basic-offset 4)
            (setq-local tab-width 4)
            (setq-local evil-shift-width 4)
            (setq-local indent-tabs-mode nil)
            (setq-local ffip-patterns
                        '("*.c" "*.cpp" "*.cxx" "*.cc" "*.h" "*.hh" "*.hpp" "*.hxx" "*.inl"))
            (modify-syntax-entry ?_ "w")))

;;; KEYMAP
(use-package evil-collection
  :straight (evil-collection :type git :files (:defaults "modes" "evil-collection-pkg.el") :host github :repo "emacs-evil/evil-collection")
  :after evil
  :config
  (evil-collection-init))
(with-eval-after-load 'evil
                      (evil-set-leader '(normal) (kbd "<SPC>"))
                      (evil-define-key 'normal 'global (kbd "<leader>k") 'find-file-in-project)
                      (evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-git-grep)
                      (evil-define-key 'normal 'global (kbd "<leader>s") 'swiper)
                      (evil-define-key 'normal 'global (kbd "<leader>b") 'counsel-switch-buffer))

;; can I set org-mode variables here?
(setq org-directory "~/orgmode"
      ;; org-refile-targets '((org-agenda-files :maxlevel .5))
      org-refile-use-outline-path t
      org-agenda-files '("todo.org")
      org-log-into-drawer t
      org-log-done t
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-use-property-inheritance t
      org-startup-with-inline-images t
      org-adapt-indentation t
      org-hide-leading-stars t
      org-confirm-babel-evaluate nil
      org-pretty-entities t
      org-return-follows-links t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-auto-align-tags                t
      org-tags-column                    -80
      org-fold-catch-invisible-edits     'show-and-error
      org-special-ctrl-a/e               t
      org-insert-heading-respect-content t)

(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autoemphasis   t
      org-appear-autolinks      t
      org-appear-autosubmarkers t
      org-hide-emphasis-markers t
      org-appear-trigger 'manual)
(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start
                                     nil
                                     t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop
                                     nil
                                     t)
                           ))

(use-package org-superstar
  :straight t
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◉" "○" "⚬" "◈" "◇"))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-list-alst '(("TODO" . 9744)
                                              ("WAIT" . 9744)
                                              ("READ" . 9744)
                                              ("PROG" . 9744)
                                              ("DONE" . 9745)))
  :hook (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :straight t
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))
  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
        `(
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))
          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))
          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil
						    :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t
						                :crop-left t :margin 0 :face 'org-date)))))))

(add-hook 'org-mode-hook 'svg-tag-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(plist-put org-format-latex-options :scale 1.5)
(use-package
  org-fragtog
  :straight t
  :hook (org-mode-hook . org-fragtog-mode))

(defun my/prettify-symbols-setup ()
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[X]" . "") prettify-symbols-alist)
  (push '("[-]" . "" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)
  (push '(":PROPERTIES:" . "") prettify-symbols-alist)
  (push '(":projects:" . "") prettify-symbols-alist)
  (push '(":work:"     . "") prettify-symbols-alist)
  (push '(":inbox:"    . "") prettify-symbols-alist)
  (push '(":task:"     . "") prettify-symbols-alist)
  (push '(":thesis:"   . "") prettify-symbols-alist)
  (push '(":uio:"      . "") prettify-symbols-alist)
  (push '(":emacs:"    . "") prettify-symbols-alist)
  (push '(":learn:"    . "") prettify-symbols-alist)
  (push '(":code:"     . "") prettify-symbols-alist)
  (prettify-symbols-mode))

(setq org-capture-templates
      '(("t" "[t]ask" entry (file+headline "~/orgmode/todo.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("d" "[d]eadline" entry (file+headline "~/orgmode/deadlines.org" "Deadlines")
         "* %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("n" "[n]ote" entry (file+headline "~/orgmode/notes.org" "Notes")
         "% %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("b" "[b]ookmark" entry (file+headline "~/orgmode/bookmarks.org" "Bookmarks")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("e" "[e]ln" entry (file+function "~/orgmode/eln.org" org-reverse-datetree-goto-date-in-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("s" "[s]hopping" checkitem (file+olp "~/orgmode/shopping.org" "Shopping")
         "+ [ ] %?")))
(add-hook 'org-mode-hook #'toggle-word-wrap)
(setq org-startup-folded t)

(setq-default org-reverse-datetree-level-formats
              '("%Y"
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time)))
                "%Y W%W"
                "%Y-%m-%d %A"))

(use-package org-roam
  :straight t
  :after org
  :bind (("C-c r i" . org-roam-node-insert)
         ("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find))
  :config
  (setq org-roam-directory (file-truename "~/orgmode/roam")
        org-roam-db-location (file-truename "~/orgmode/roam/org-roam.db")
        org-attach-id-dir "assets/")
  (org-roam-db-autosync-enable))
(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(use-package org-reverse-datetree :straight t)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)))
;; end org

;; elfeed
(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
        '("https://www.nature.com/nature.rss")))
(global-set-key (kbd "C-x w") 'elfeed)

(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-entry-mode)

(use-package doom-modeline
  :straight t
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-support-imenu t
        doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-hud t
        doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-time-icon t
        doom-modeline-buffer-name t))

;; vertico
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
  ;; :general
  ;; (:keymaps '(normal insert visual motion)
  ;;           "M-." #'vertico-repeat
  ;;           )
  ;; (:keymaps 'vertico-map
  ;;           "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
  ;;           "<escape>" #'minibuffer-keyboard-quit
  ;;           "?" #'minibuffer-completion-help
  ;;           "C-M-n" #'vertico-next-group
  ;;           "C-M-p" #'vertico-previous-group
  ;;           ;; Multiform toggles
  ;;           "<backspace>" #'vertico-directory-delete-char
  ;;           "C-w" #'vertico-directory-delete-word
  ;;           "C-<backspace>" #'vertico-directory-delete-word
  ;;           "RET" #'vertico-directory-enter
  ;;           "C-i" #'vertico-quick-insert
  ;;           "C-o" #'vertico-quick-exit
  ;;           "M-o" #'kb/vertico-quick-embark
  ;;           "M-G" #'vertico-multiform-grid
  ;;           "M-F" #'vertico-multiform-flat
  ;;           "M-R" #'vertico-multiform-reverse
  ;;           "M-U" #'vertico-multiform-unobtrusive
  ;;           "C-l" #'kb/vertico-multiform-flat-toggle
  ;;           )
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

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
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
                 cand)))
  )
;; (use-package vertico
;;   :ensure t
;;   :hook (after-init . vertico-mode)
;;   :custom
;;   (vertico-scroll-margin 0)
;;   (vertico-count 13)
;;   (vertico-resize t)
;;   (vertico-cycle t)
;;   :init
;;   (vertico-mode))
;; (use-package savehist
;;   :init
;;   (savehist-mode))
;; (use-package emacs
;;   :custom
;;   (enable-recursive-minibuffers t)
;;   (read-extended-command-predicate #'command-completion-default-include-p)
;;   :init
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   ('((file (styles basic-remote
                    orderless
                    ))
      ))
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

(straight-use-package '(marginalia :type git :host github :repo "minad/marginalia"))
(setq marginalia-max-relative-age 0)
(setq marginalia-align 'right)
(marginalia-mode 1)

(straight-use-package '(nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion"))
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

;; recentf
(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-max-saved-items 200)
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :config
  (recentf-mode 1)
  :bind (("\C-x\ \C-r" . counsel-recentf)))

;; pdftools
(use-package pdf-tools
  :straight (pdf-tools :type git :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools")
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdftools
  :straight (org-pdftools :type git :files ("org-pdftools.el" "org-pdftools-pkg.el") :host github :repo "fuxialexander/org-pdftools")
  :hook (org-mode . org-pdftools-setup-link))

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult"))

(require 'server)
(unless (server-running-p)
  (server-start))

(load (expand-file-name "gw-funcs.el" user-emacs-directory))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(global-set-key (kbd "C-x k") 'gw/kill-this-buffer)
(global-set-key (kbd "C-c rr") 'gw/org-roam-rg-search)
(global-set-key (kbd "C-c ri") 'org-roam-node-insert)

;;; init.el ends here
