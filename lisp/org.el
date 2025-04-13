;;; -*- lexical-binding: t -*-

(defface gw/org-emphasis-strike-through
  '((((class color) (min-colors 88) (background light))
     :strike-through "#972500" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :strike-through "#ef8b50" :foreground "#a8a8a8"))
  "My strike-through emphasis for Org.")
(setq org-directory                         "~/org"
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-outline-path-complete-in-steps    nil
      org-refile-use-outline-path           t

      ;; org-format-latex-header               nil ;; I need to use this for some reason. probably with new org-latex-preview
      org-ellipsis                          "…"
      org-log-into-drawer                   t
      org-log-done                          t
      org-log-redeadline                    'time
      org-enforce-todo-dependencies         t
      org-log-reschedule                    'time
      org-use-property-inheritance          t
      org-startup-with-inline-images        t
      org-startup-indented                  t
      org-adapt-indentation                 t
      org-hide-leading-stars                nil
      org-confirm-babel-evaluate            nil
      org-pretty-entities                   t
      org-use-sub-superscripts              '{}
      org-return-follows-links              t
      org-fontify-done-headline             t
      org-fontify-quote-and-verse-blocks    t
      org-fontify-whole-heading-line        t
      org-edit-src-content-indentation      0
      org-fold-catch-invisible-edits        'show-and-error
      org-special-ctrl-a/e                  t

      ;; these are new
      org-agenda-search-view-always-boolean t
      org-agenda-restore-windows-after-quit t
      org-agenda-show-future-repeats        nil
      org-agenda-window-setup               'only-window
      org-agenda-span                       'day
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done     t
      org-agenda-skip-deadline-if-done      t
      org-agenda-dim-blocked-tasks          t
      org-agenda-format-date                "\n%A, %-e %B %Y"
      org-deadline-warning-days             3

      org-confirm-babel-evaluate            nil
      org-auto-align-tags                   nil
      org-tags-column                       80
      org-insert-heading-respect-content    t
      org-hide-emphasis-markers             t
      ;; org-emphasis-alist
      ;; '(("*" bold)
      ;;   ("/" italic)
      ;;   ("_" underline)
      ;;   ("=" org-verbatim verbatim)
      ;;   ("~" org-code verbatim)
      ;;   ("+" gw/org-emphasis-strike-through))

      ;; change how emphasis are
      ;; org-emphasis-alist
      ;; '(("*" bold)
      ;;   ("/" (italic :foreground "ForestGreen"))
      ;;   ("_" underline)
      ;;   ("=" org-verbatim verbatim)
      ;;   ("~" org-code verbatim)
      ;;   ("+" gw/org-emphasis-strike-through))

      org-startup-folded                    t
      ;; org-highlight-latex-and-related       '(latex script entities)
      org-latex-pdf-process                 (list "latexmk -xelatex %f")
      ;; org-todo-keywords
      ;; (quote ((sequence "TODO(t)" "NEXT(t)" "|" "DONE(d)")
      ;;         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

      ;; from doom
      org-todo-keywords
      '((sequence
         "TODO(t)"                      ; A task that needs doing & is ready to do
         "PROJ(p)"                      ; A project, which usually contains other tasks
         "LOOP(r)"                      ; A recurring task
         "STRT(s)"                      ; A task that is in progress
         "WAIT(w)"                      ; Something external is holding up this task
         "HOLD(h)"                      ; This task is paused/on hold because of me
         "IDEA(i)"                      ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"                      ; Task successfully completed
         "KILL(k)")                     ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"                       ; A task that needs doing
         "[-](S)"                       ; Task is in progress
         "[?](W)"                       ; Task is being held up or paused
         "|"
         "[X](D)")                      ; Task was completed
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

(use-package org-latex-preview
  :config
  (plist-put org-latex-preview-appearance-options :page-width 0.5)
  (plist-put org-latex-preview-appearance-options :scale 2)
  (setq org-latex-preview-auto-ignored-commands
	    '(next-line previous-line mwheel-scroll
		            scroll-up-command scroll-down-command))
  (setq org-latex-preview-numbered t)
  (setq org-latex-preview-live t)
  (setq org-latex-preview-live-debounce 0.25)

  (defun gw/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun gw/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun gw/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
		          ((or (eq (org-element-type elem) 'latex-environment)
		               (string-match-p "^\\\\\\[" (org-element-property :value elem))))
		          (img (overlay-get ov 'display))
		          (prop `(space :align-to (- center (0.55 . ,img))))
		          (justify (propertize " " 'display prop 'face 'default)))
	    (overlay-put ov 'justify justify)
	    (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
	    (progn
	      (add-hook 'org-latex-preview-overlay-open-functions
		            #'gw/org-latex-preview-uncenter nil :local)
	      (add-hook 'org-latex-preview-overlay-close-functions
		            #'gw/org-latex-preview-recenter nil :local)
	      (add-hook 'org-latex-preview-overlay-update-functions
		            #'gw/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-open-functions
		           #'gw/org-latex-preview-uncenter)
      (remove-hook 'org-latex-preview-overlay-close-functions
		           #'gw/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
		           #'gw/org-latex-preview-center)))
  :hook ((org-mode . org-latex-preview-auto-mode)
         (org-mode . org-latex-preview-center-mode)))

(org-latex-preview-auto-mode)

(setq org-capture-templates
      '(("t" "[t]asks")
        ("ti" "[t]ask -> Inbox header" entry (file+headline "~/Dropbox/org/todo.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("tl" "[t]ask -> Lab header" entry (file+headline "~/Dropbox/org/todo.org" "Lab")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("tp" "[t]ask -> Personal header" entry (file+headline "~/Dropbox/org/todo.org" "Personal")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("n" "[n]ote" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
         "% %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("e" "[e]ln" entry (file+function "~/Dropbox/org/eln.org" org-reverse-datetree-goto-date-in-file)
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("p" "[p]lanner" entry (file+function "~/Dropbox/org/planner.org" org-reverse-datetree-goto-date-in-file)
         "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
        ("s" "[s]hopping" checkitem (file "~/Dropbox/org/shopping.org")
         "+ [ ] %?")
        ("u" "[u]rl capture from Safari" entry
         (file+olp+datetree "~/Dropbox/org/webpages.org")
         "* %i    :safari:url:\n%U\n\n")
        ("w" "[w]ebpage capture from Safari (url+clipboard)" entry
         (file+olp+datetree "~/Dropbox/org/webpages.org")
         "* %?    :safari:note:\n%U\n\n%i\n")))
(setq-default org-reverse-datetree-level-formats
              '("%Y"
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time)))
                "%Y W%W"
                "%Y-%m-%d %A"))
(use-package org-reverse-datetree
  :straight t)

;; roam
(setq org-roam-directory (file-truename "~/Dropbox/org/roam")
      org-attach-id-dir "assets/"
      org-link-frame-setup '((file . find-file)))
(use-package org-roam
  :straight t
  :ensure t
  :after org
  :bind (("C-c r i" . org-roam-node-insert)
	     ("C-c r l" . org-roam-buffer-toggle)
	     ("C-c r f" . org-roam-node-find)
	     ("C-c r c" . org-roam-capture)
	     ("C-c r r" . org-roam-ref-find))
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t
           :immediate-finish t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("n" "literature note" plain "%?"
           :target
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
            "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("c" "citar literature note" plain "%?"
           :target (file+head "%(expand-file-name citar-org-roam-subdir org-roam-directory)/${citar-citekey}.org"
                              "#+title: Notes on: ${citar-title}\n#+subtitle: ${citar-author}, ${citar-date}")
           :unnarrowed t))
        citar-org-roam-capture-template-key "c")
  (setq org-roam-node-display-template (concat "${title:80} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package org-roam-ui
  :straight t
  :after (org org-roam)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-modern
  :straight t
  :after org
  :hook ((org-modern-mode . gw/org-modern-spacing))
  :config
  (defun gw/org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1 0.0)))
  (setq org-modern-todo t
        org-modern-star 'replace
	    org-modern-hide-stars nil
        org-modern-horizontal-rule t
	    org-modern-timestamp t
        org-modern-keyword "‣ "
	    org-modern-table nil
	    org-modern-priority nil))
(global-org-modern-mode 1)

;; download
(use-package org-download
  :straight t
  :after org
  :config
  (setq org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s"
        ;; org-download-screenshot-method "screencapture -ci"
        org-download-timestamp t
        org-download-method 'directory
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-heading-lvl nil
        org-image-actual-width 600
        org-image-align 'center)
  (setq-default org-download-image-dir "~/Dropbox/org/roam/images")
  (require 'org-download)
  :bind ("C-M-y" . org-download-screenshot))

;; ob
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (emacs-lisp . t)
   (shell . t)
   (python . t)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package olivetti
  :straight t)

;; org-src config
;; (use-package org-src
;;   :after org
;;   :config
;;   (when (fboundp 'LaTeX-mode)
;;     (setf (alist-get "latex" org-src-lang-modes
;;                      nil nil #'equal)
;;           'LaTeX))
;;   (setq-default
;;    org-src-tab-acts-natively t
;;    org-src-preserve-indentation t
;;    org-src-window-setup 'plain)
;;   (advice-add 'org-src-font-lock-fontify-block
;;               :after
;;               (defun gw/no-latex-background-face (lang start end)
;;                 (when (equal lang "latex")
;;                   (alter-text-property
;;                    start end 'face
;;                    (lambda (l) (remove 'org-block l)))))))

;; org-agenda config
(use-package org-agenda
  :after org
  :commands org-agenda
  :hook (org-agenda-finalize . hl-line-mode)
  :bind (:map org-agenda-mode-map
              ("D" . org-agenda-day-view)
              ("W" . org-agenda-week-view)
              ("w" . org-agenda-refile)
              ("S-SPC" . org-agenda-show-scroll-down))
  :config
  (setq org-show-notification-timeout 10)
  (setq org-agenda-files '("~/Dropbox/org/todo.org"
                           "~/Dropbox/org/planner.org"
                           "~/Dropbox/org/reminders.org"
                           "~/Dropbox/org/calendar.org"))
  (setq-default
   org-agenda-span 7
   org-agenda-restore-windows-after-quit t
   org-agenda-window-setup 'current-window
   org-stuck-projects '("TODO=\"PROJECT\"|TODO=\"SUSPENDED\"" ("TODO" "DEFERRED") nil "")
   org-agenda-use-time-grid t
   org-agenda-todo-ignore-scheduled nil
   org-agenda-text-search-extra-files nil
   org-agenda-tags-column 'auto
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-show-all-dates nil
   org-agenda-inhibit-startup t
   org-agenda-include-diary nil
   org-agenda-follow-indirect nil
   org-agenda-default-appointment-duration nil)

  (add-hook 'org-agenda-after-show-hook
            (lambda () (recenter (floor (window-height) 6))))
  (advice-add 'org-agenda-do-tree-to-indirect-buffer :after
              (defun gw/org-agenda-collapse-indirect-buffer-tree (arg)
                (with-current-buffer org-last-indirect-buffer
                  (org-ctrl-c-tab) (org-fold-show-entry 'hide-drawers))))
  (defun gw/org-agenda-next-section (arg)
    (interactive "P")
    (when (> arg 0)
      (dotimes (_ arg)
        (when-let ((m (text-property-search-forward 'face 'org-agenda-structure t t)))
          (goto-char (prop-match-beginning m))
          (forward-char 1)))))
  (defun gw/org-agenda-previous-section (arg)
    (interactive "P")
    (when (> arg 0)
      (dotimes (_ arg)
        (when-let ((m (text-property-search-backward 'face 'org-agenda-structure nil nil)))
          (goto-char (prop-match-end m))))))
  (defun gw/org-todo-age (&optional pos)
    (if-let* ((entry-age (org-todo-age-time pos))
              (days (time-to-number-of-days entry-age)))
        (cond
         ((< days 1)   "today")
         ((< days 7)   (format "%dd" days))
         ((< days 30)  (format "%.1fw" (/ days 7.0)))
         ((< days 358) (format "%.1fM" (/ days 30.0)))
         (t            (format "%.1fY" (/ days 365.0))))
      ""))
  (defun gw/org-todo-age-time (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
        (time-subtract (current-time)
                       (org-time-string-to-time stamp)))))
  (defun gw/org-current-is-todo ()
    (member (org-get-todo-state) '("TODO" "STARTED")))
  (defun gw/org-agenda-should-skip-p ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        (setq should-skip-entry t))
      (when (/= (point)
                (save-excursion
                  (org-goto-first-child)
                  (point)))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (and (org-current-is-todo)
                     (not (org-get-scheduled-time (point)))
                     (not (org-get-deadline-time (point))))
            (setq should-skip-entry t))))
      (when (and (not should-skip-entry)
                 (save-excursion
                   (unless (= (org-outline-level) 1)
                     (outline-up-heading 1 t))
                   (not (member (org-get-todo-state)
                                '("PROJECT" "TODO")))))
        (setq should-skip-entry t))
      should-skip-entry))
  (defun gw/org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (when (gw/org-agenda-should-skip-p)
      (or (outline-next-heading)
          (goto-char (point-max)))))
  (setq org-agenda-custom-commands
        '(("n" "Project Next Actions" alltodo ""
           ((org-agenda-overriding-header "Project Next Actions")
            (org-agenda-skip-function #'gw/org-agenda-skip-all-siblings-but-first)))
          ("P" "All Projects" tags "TODO=\"PROJECT\"&LEVEL>1|TODO=\"SUSPENDED\""
           ((org-agenda-overriding-header "All Projects")))
          ("i" "Inbox" tags "CATEGORY=\"Inbox\"&LEVEL=1"
           ((org-agenda-overriding-header "Uncategorized items")))
          ("W" "Waiting Tasks" tags "W-TODO=\"DONE\"|TODO={WAITING\\|DELEGATED}"
           ((org-agenda-overriding-header "Waiting/delegated tasks:")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))))
          ("D" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
           ((org-agenda-overriding-header "Deadlined tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(category-up))))

          ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
           ((org-agenda-overriding-header "Scheduled tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled))
            (org-agenda-sorting-strategy '(category-up))
            (org-agenda-prefix-format "%-11c%s ")))

          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT\\|DEFERRED\\|MAYBE}"
           ((org-agenda-overriding-header "Unscheduled tasks: ")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            (org-agenda-files '("~/Dropbox/org/todo.org"))))

          ("~" "Maybe tasks" tags "TODO=\"MAYBE\""
           ((org-agenda-overriding-header "Maybe tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            ;; (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            ))

          ("K" "Habits" tags "STYLE=\"habit\""
           ((my/org-habit-show-graphs-everywhere t)
            (org-agenda-overriding-header "Habits:")
            (org-habit-show-all-today t)))
          
          ("o" "Overview"
           ((tags-todo "*"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))

                        (org-agenda-skip-function
                         `(org-agenda-skip-entry-if
                           'notregexp ,(format "\\[#%s\\]" ;;(char-to-string org-priority-highest)
                                               "\\(?:A\\|B\\|C\\)")))
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "⛤ Important\n")))
            (agenda ""
                    ((org-agenda-overriding-header "\n🕐 Today\n")
                     (org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-block-separator nil)))
            (agenda "" ((org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "\n📅 Next three days\n")))
            (tags "CATEGORY=\"Inbox\"&LEVEL=1"
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\n📧 Inbox\n")))
            (agenda ""
                    ((org-agenda-time-grid nil)
                     (org-agenda-start-on-weekday nil)
                     ;; We don't want to replicate the previous section's
                     ;; three days, so we start counting from the day after.
                     (org-agenda-start-day "+3d")
                     (org-agenda-span 14)
                     (org-agenda-show-all-dates nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-block-separator nil)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "\n🞜 Upcoming deadlines (+14d)\n")))
            (tags "TODO=\"PROJECT\"&LEVEL>1&CATEGORY=\"Research\""
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\n⨕ Research\n")))
            ;; (alltodo ""
            ;;  ((org-agenda-overriding-header "Project Next Actions")
            ;;   (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
            (todo "WAITING"
                  ((org-agenda-overriding-header "\n💤 On Hold\n")
                   (org-agenda-block-separator nil))))))))

(defun stag-misanthropic-capture (&rest r)
  (delete-other-windows))
(advice-add #'org-capture-place-template :after 'stag-misanthropic-capture)

(use-package ob-async
  :straight t
  :init
  (require 'ob-async))
