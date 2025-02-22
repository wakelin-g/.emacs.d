;;; -*- lexical-binding: t -*-

(straight-use-package '(org :type built-in))

(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
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
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-cancel '((t (:inherit (bold error org-todo)))) "")

(setq org-directory                        "~/orgmode"
      ;; org-refile-targets '((org-agenda-files :maxlevel .5))
      org-refile-use-outline-path          t
      org-agenda-files                     '("todo.org" "planner.org")
      org-agenda-tags-column               0
      org-ellipsis                         "…"
      org-log-into-drawer                  t
      org-log-done                         t
      org-log-redeadline                   'time
      org-enforce-todo-dependencies        t
      org-log-reschedule                   'time
      org-use-property-inheritance         t
      org-startup-with-inline-images       t
      org-startup-indented                 t
      org-adapt-indentation                t
      org-hide-leading-stars               nil
      org-confirm-babel-evaluate           nil
      org-pretty-entities                  t
      org-return-follows-links             t
      org-src-fontify-natively             t
      org-src-tab-acts-natively            t
      org-fontify-done-headline            t
      org-fontify-quote-and-verse-blocks   t
      org-fontify-whole-heading-line       t
      org-edit-src-content-indentation     0
      org-fold-catch-invisible-edits       'show-and-error
      org-special-ctrl-a/e                 t
      org-auto-align-tags                  nil
      org-tags-column                      0
      org-insert-heading-respect-content   t
      org-appear-autoemphasis              t
      org-appear-autolinks                 t
      org-appear-autosubmarkers            t
      org-hide-emphasis-markers            t
      org-appear-trigger                   'manual
      org-startup-folded                   t
      ;; org-todo-keywords
      ;; (quote ((sequence "TODO(t)" "NEXT(t)" "|" "DONE(d)")
      ;;         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
      org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "PROJ(p)"  ; A project, which usually contains other tasks
         "LOOP(r)"  ; A recurring task
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        (sequence
         "|"
         "OKAY(o)"
         "YES(y)"
         "NO(n)"))
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-active)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel))
      org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . success)))

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear"))

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.25))))

;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
;;               ("NEXT" :inherit (org-todo region) :foreground "#81A1C1" :weight bold)
;;               ("DONE" :inherit (org-todo region) :foreground "#30343D" :weight bold)
;;               ("WAITING" :inherit (org-todo region) :foreground "#EBCB8B" :weight bold)
;;               ("HOLD" :inherit (org-todo region) :foreground "#8FBCBB" :weight bold)
;;               ("CANCELLED" :inherit (org-todo region) :foreground "30343D" :weight bold)
;;               ("MEETING" :inherit (org-todo region) :foreground "#A3B38C" :weight bold)
;;               ("PHONE" :inherit (org-todo region) :foreground "#A3B38C" :weight bold))))

;; latex code preview
(plist-put org-format-latex-options :scale 2)
(use-package
  org-fragtog
  :straight t
  :hook (org-mode-hook . org-fragtog-mode))

;; my capture templates
(setq org-capture-templates
      '(("t" "[t]asks")
        ("ti" "[t]ask -> Inbox header" entry (file+headline "~/orgmode/todo.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("tl" "[t]ask -> Lab header" entry (file+headline "~/orgmode/todo.org" "Lab")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("tp" "[t]ask -> Personal header" entry (file+headline "~/orgmode/todo.org" "Personal")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("td" "[t]ask -> Deadline header" entry (file+headline "~/orgmode/todo.org" "Deadlines")
         "* %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("n" "[n]ote" entry (file+headline "~/orgmode/notes.org" "Notes")
         "% %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("b" "[b]ookmark" entry (file+headline "~/orgmode/bookmarks.org" "Bookmarks")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("e" "[e]ln" entry (file+function "~/orgmode/eln.org" org-reverse-datetree-goto-date-in-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("p" "[p]lanner" entry (file+function "~/orgmode/planner.org" org-reverse-datetree-goto-date-in-file)
         "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("s" "[s]hopping" checkitem (file "~/orgmode/shopping.org")
         "+ [ ] %?")))


(setq-default org-reverse-datetree-level-formats
              '("%Y"
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time)))
                "%Y W%W"
                "%Y-%m-%d %A"))

;; roam
(use-package org-roam
  :straight t
  :after org
  :bind (("C-c r i" . org-roam-node-insert)
         ("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find))
  :config
  (setq org-roam-directory (file-truename "~/orgmode/roam")
        
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

(use-package org-reverse-datetree
  :straight t)

(let ((straight-current-profile 'pinned))
  (straight-use-package 'gnuplot)
  (straight-use-package 'gnuplot-mode)
  (add-to-list 'straight-x-pinned-packages
               '("gnuplot" . "7138b139d2dca9683f1a81325c643b2744aa1ea3")
               '("gnuplot-mode" . "601f6392986f0cba332c87678d31ae0d0a496ce7")))

;; add R to babel langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (gnuplot . t)))

;; replaces both org-bullets and 
(use-package org-modern
  :straight t
  :config
  (setq
   org-modern-label-border 0.4
   org-modern-star 'replace
   org-modern-hide-stars 'leading
   org-modern-timestamp t
   org-modern-table t
   org-modern-priority nil)
  (global-org-modern-mode))
;; this needs to be here instead of config/core.el because of loading order
(set-face-attribute 'org-modern-symbol nil :family "Iosevka" :height 200)
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

(use-package org-sticky-header
  :straight (org-sticky-header :type git :host github :repo "alphapapa/org-sticky-header")
  :hook (org-mode . org-sticky-header-mode)
  :config
  (org-sticky-header-mode))

(defadvice org-agenda (around split-vertically activate)
  (let ((split-window-threshold nil))
    ad-do-it))
