;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys


;;; --- GENERAL SETTINGS ---

(recentf-mode 1)
(savehist-mode 1)
(setq! scroll-margin 2
       scroll-conservatively scroll-margin
       scroll-step 1
       centaur-tabs-enable-key-bindings t
       centaur-tabs-cycle-scope 'default
       centaur-tabs-show-count t
       mouse-wheel-scroll-amount '(6 ((shift) . 1))
       mouse-wheel-progressive-speed nil
       mouse-wheel-inhibit-click-time nil
       scroll-preserve-screen-position t
       scroll-error-top-bottom t
       next-error-recenter (quote (4))
       fast-but-imprecise-scrolling nil
       jit-lock-defer-time 0
       evil-want-C-u-scroll nil
       evil-want-C-u-delete nil
       display-line-numbers-type nil
       fill-column 120
       initial-major-mode 'org-mode)

;; fonts - used to use "IosevkaTerm Nerd Font Mono" and "Iosevka Aile".
;; Trying out "AporeticSerifMono Nerd Font Mono" and "LMRoman10".
(setq doom-font (font-spec :family "AporeticSerifMono Nerd Font Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "LMRoman10" :size 20))

;; theme
(setq doom-theme 'ef-light)

;; no confirm to kill
(setq confirm-kill-emacs nil)

;; make command and option keys to meta and super
(if (featurep :system 'macos)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super))

;; default frame size and title
(setq! default-frame-alist
       (append default-frame-alist
               '((left . 200)
                 (top . 200)
                 (width . 145)
                 (height . 30))))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ? buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" "  ●  %s") project-name))))))

;; rules for popup windows (and windows in general)
(set-popup-rules!
  '(("^\\*elfeed-summary\\*$" :actions (display-buffer-full-frame) :save t :quit t :autosave ignore)
    ("^\\*elfeed-search\\*$" :select t :size 0.75 :side right :save t)))

;;; --- PACKAGES ---
;; org
(after! org
  (setq! org-capture-templates
         '(("t" "[t]asks")
           ("ti" "[t]ask -> Inbox header" entry (file+headline "~/Dropbox/org/todo.org" "Inbox")
            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
           ("tl" "[t]ask -> Lab header" entry (file+headline "~/Dropbox/org/todo.org" "Lab")
            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
           ("tp" "[t]ask -> Personal header" entry (file+headline "~/Dropbox/org/todo.org" "Personal")
            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
           ("td" "[t]ask -> Deadline header" entry (file+headline "~/Dropbox/org/todo.org" "Deadlines")
            "* %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines-after 1)
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
  (setq org-latex-preview-appearance-options (plist-put org-latex-preview-appearance-options :scale 0.8))
  (setq org-latex-preview-appearance-options (plist-put org-latex-preview-appearance-options :density 600))
  (setq org-latex-preview-appearance-options (plist-put org-latex-preview-appearance-options :background "Transparent")))

;; modeline
;; (after! doom-modeline
;;   (setq doom-modeline-buffer-file-name-style 'file-name
;;         doom-modeline-always-show-macro-register t
;;         doom-modeline-enable-word-count nil
;;         doom-modeline-buffer-encoding t
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-bar-width 0
;;         doom-modeline-height 25
;;         doom-modeline-modal nil))

;; (setq-default mode-line-format
;;               '("%e"
;;                 prot-modeline-kbd-macro
;;                 prot-modeline-narrow
;;                 prot-modeline-input-method
;;                 prot-modeline-buffer-status
;;                 " "
;;                 prot-modeline-buffer-identification
;;                 " "
;;                 prot-modeline-major-mode
;;                 prot-modeline-process
;;                 " "
;;                 prot-modeline-vc-branch
;;                 " "
;;                 prot-modeline-flymake
;;                 " "
;;                 prot-modeline-align-right
;;                 prot-modeline-misc-info))

;; themes
(use-package! ef-themes
  :ensure t
  :config
  (setq ef-themes-headings
        '((0 . (1.50))
          (1 . (1.28))
          (2 . (1.22))
          (3 . (1.17))
          (4 . (1.14))
          (t . (1.1))))
  (defun my/ef-themes-extra-faces ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(aw-leading-char-face ((,c :foreground ,fg-mode-line
                                   :height 1.5 :weight semi-bold))))))
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-extra-faces))

;; elfeed
(use-package! elfeed
  :commands (elfeed elfeed-update)
  :config
  (setq! rmh-elfeed-org-files (list "~/.emacs.d.bak/elfeed-feeds.org"))
  (setq-default elfeed-save-multiple-enclosures-without-asking t
                elfeed-search-clipboard-type 'CLIPBOARD
                elfeed-search-filter "@1-month-ago +unread -news"
                elfeed-search-date-format '("%Y-%m-%d" 10 :left)
                elfeed-search-title-min-width 45
                elfeed-search-title-max-width 120)
  (defun my/elfeed-search-browse-background-url ()
    "Open current `elfeed' entry (or region entries) in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (cl-assert (memq system-type '(darwin)) t "open command is macOS only")
              (start-process (concat "open " (elfeed-entry-link entry))
                             nil "open" "--background" (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

;; tabline
(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' controls buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode', `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-agenda-mode
                        org-src-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")
    ((memq major-mode '(elfeed-search-mode
                        elfeed-show-mode))
     "Elfeed")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

;; focus mode
(use-package! logos
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
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t
                olivetti-body-width 0.7
                olivetti-minimum-body-width 80
                olivetti-recall-visual-line-mode-entry-state t)
  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (defun my/centaur-tab-bar-show-toggle ()
    (when logos-focus-mode
      (logos-set-mode-arg 'centaur-tabs-mode -1)))
  (add-hook 'logos-focus-mode-hook #'my/centaur-tab-bar-show-toggle)

  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1)))
  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

;; latex previewing in org mode
(use-package! org-latex-preview
  :config
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (add-hook 'org-mode-hook 'org-latex-preview-center-mode)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  (setq org-latex-preview-live t
        org-latex-preview-live-debounce 0.25
        org-highlight-latex-and-related '(latex script entities))

  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
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
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                   #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                   #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                   #'my/org-latex-preview-uncenter))))

;; elfeed summary (trying this)
(use-package! elfeed-summary
  :config
  (defface elfeed-entry-1
    '((t :foreground "#89f"))
    "Marks an important Elfeed entry.")
  (defface elfeed-entry-2
    '((t :foreground "#f77"))
    "Marks an important Elfeed entry.")
  (defface elfeed-entry-3
    '((t :foreground "#0a0"))
    "Marks an important Elfeed entry.")
  (setq! elfeed-summary-settings
         '((auto-tags (:max-level . 5)
            (:faces '(elfeed-entry-1 elfeed-entry-2 elfeed-entry-3))
            (:original-order . nil)))))

;; eww config
(use-package! eww
  :hook (eww-after-render . (lambda () (setq line-spacing 0.1)))
  :config
  (setq eww-browse-url-new-window-is-tab nil
        eww-auto-rename-buffer t))

;;; --- FUNCTION DEFINITIONS ---

(defun gw/kill-this-buffer ()
  "Kill current buffer without prompt."
  (interactive)
  (kill-buffer (current-buffer)))

(defun gw-elfeed--format-tags (tags sign)
  "Prefix SIGN to each tag in TAGS."
  (mapcar (lambda (tag)
            (format "%s%s" sign tag))
          tags))

(defun gw-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((crm-separator " ")
           (elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (gw-elfeed--format-tags db-tags "+"))
           (minus-tags (gw-elfeed--format-tags db-tags "-"))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags nil t))
           (input (cons elfeed-search-filter tags)))
      (setq elfeed-search-filter (substring (format "%s" input) 1 -1)))
    (elfeed-search-update :force)))

;; (defun my/reader-display-buffer (buf &optional _)
;;   (pop-to-buffer buf `((display-buffer-reuse-window display-buffer-in-direction)
;;                        (direction . ,(if (> (window-width) 130)
;;                                          'right 'above))
;;                        (window-height . 0.72)
;;                        (window-width . 0.64))))

;; (defvar my/reader-names '("*elfeed-entry*"))
;; (defvar my/reader-modes '(elfeed-show-mode eww-mode))
;; (defvar my/reader-list-modes '(elfeed-search-mode))

;; (defvar my/reader-quit-functions
;;   (cl-pairlis my/reader-modes
;;               '(kill-buffer-and-window
;;                 quit-window)))

;; (defvar my/reader-list-quit-functions
;;   (cl-pairlis my/reader-list-modes
;;               '(elfeed-search-quit-window
;;                 quit-window)))

;; (defvar my/reader-list-next-prev-functions
;;   (cl-pairlis my/reader-list-modes
;;               '((next-line . previous-line)
;;                (next-line . previous-line))))

;; (defvar my/reader-list-show-functions
;;   (cl-pairlis my/reader-list-modes
;;               '(elfeed-search-show-entry)))

;; (defun my/reader-show ()
;;   (interactive)
;;   (let ((elfeed-show-entry-switch #'my/reader-display-buffer)
;;         (win (selected-window))
;;         (show-buf))
;;     (save-excursion
;;       (call-interactively (alist-get (buffer-mode) my/reader-list-show-functions)))
;;     (setq show-buf (current-buffer))
;;     (select-window win)
;;     (setq-local other-window-scroll-buffer show-buf)))

;; (defun my/reader-prev (&optional arg)
;;   (interactive "p")
;;   (unless (bobp)
;;     (when-let ((prev-fun (cdr (alist-get (buffer-mode) my/reader-list-next-prev-functions))))
;;       (dotimes (or arg 1)
;;         (funcall prev-fun))
;;       (my/reader-show))))

;; (defun my/reader-next (&optional arg)
;;   (interactive "p")
;;   (unless (eobp)
;;     (when-let ((next-fun (car (alist-get (buffer-mode) my/reader-list-next-prev-functions))))
;;       (dotimes (or arg 1)
;;         (funcall next-fun))
;;       (my/reader-show))))

;; (eval-when-compile
;;     (defmacro with-reader-window (&rest body)
;;       "Execute BODY with a visible elfeed entry buffer as current."
;;       (declare (indent defun))
;;       `(when-let ((win
;;                    (or
;;                     (seq-some #'get-buffer-window my/reader-names)
;;                     (seq-some (lambda (w)
;;                                 (and (memq
;;                                       (buffer-mode (window-buffer w))
;;                                       my/reader-modes)
;;                                  w))
;;                      (window-list)))))
;;         (with-selected-window win
;;          ,@body)
;;         t)))

;; (defvar my/reader-pixel-scroll nil
;;   "Whether reader-mode scrolling should be animated.")

;; (defun my/reader-scroll-up-command ()
;;   (interactive)
;;   (let ((scroll-error-top-bottom nil)
;;         (pulse-flag))
;;     (unless
;;         (with-reader-window
;;           (if my/reader-pixel-scroll
;;               (progn
;;                 (pixel-scroll-precision-interpolate
;;                  (- (* (default-line-height) next-screen-context-lines)
;;                     (window-text-height nil t))
;;                  nil 1)
;;                 (my/pulse-momentary-line))
;;             (condition-case-unless-debug nil
;;                 (scroll-up-command)
;;               (error (run-at-time 0 nil #'my/reader-next 1)))))
;;       (my/reader-show))))

;; (defun my/reader-scroll-down-command ()
;;   (interactive)
;;   (let ((scroll-error-top-bottom nil)
;;         (pulse-flag))
;;     (with-reader-window
;;       (if my/reader-pixel-scroll
;;           (progn
;;             (pixel-scroll-precision-interpolate
;;              (- (window-text-height nil t)
;;                 (* (default-line-height) next-screen-context-lines))
;;              nil 1)
;;             (my/pulse-momentary-line))
;;         (condition-case-unless-debug nil
;;             (scroll-down-command)
;;           (error (run-at-time 0 nil #'my/reader-prev 1)))))))

;; (defun my/reader-browse-url (&optional arg)
;;   "docstring"
;;   (interactive "P")
;;   (with-reader-window (my/search-occur-browse-url arg)))

;; (defun my/reader-push-button (&optional arg)
;;   "docstring"
;;   (interactive)
;;   (with-reader-window (my/avy-link-hint (selected-window))))

;; (defun my/reader-imenu ()
;;   (interactive)
;;   (with-reader-window (consult-imenu)))

;; (defun my/reader-top ()
;;   (interactive)
;;   (or
;;    (with-reader-window (beginning-of-buffer))
;;    (beginning-of-buffer)))

;; (defun my/reader-bottom ()
;;   (interactive)
;;   (or
;;    (with-reader-window (end-of-buffer))
;;    (end-of-buffer))
;;   (when (eq (line-beginning-position) (point))
;;     (forward-line -1)))

;; (defun my/reader-quit-window ()
;;     (interactive)
;;     (or (with-reader-window
;;           (when-let ((quit-fun (alist-get (buffer-mode) my/reader-quit-functions)))
;;             (call-interactively quit-fun)
;;             t))
;;         (call-interactively (alist-get (buffer-mode) my/reader-list-quit-functions))))

;; (defun my/reader-center-images ()
;;     "Center images in document. Meant to be added to a post-render
;; hook."
;;     (let* ((inhibit-read-only t)
;;            (pixel-buffer-width (shr-pixel-buffer-width))
;;            match)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (setq match (text-property-search-forward
;;                             'display nil

;;                             (lambda (_ p) (image-at-point-p))))
;;           (when-let ((size (car (image-size
;;                                  (prop-match-value match) 'pixels)))
;;                      ((> size 150))
;;                      (ov (make-overlay (1- (prop-match-beginning match))
;;                           (prop-match-beginning match))))
;;             (overlay-put ov 'evaporate t)
;;             (overlay-put
;;              ov 'after-string
;;              (propertize
;;               " " 'face 'default
;;               'display `(space :align-to (- center (0.58 . ,(prop-match-value match)))))))))))

(defun gw/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open. Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case, use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;; --- ORG ---

(setq! gw/default-bibliography '("/Users/griffen/Dropbox/org/roam/library.bib"))
(setq! bibtex-completion-notes-path "~/Dropbox/org/roam"
       bibtex-completion-bibliography gw/default-bibliography
       org-cite-global-bibliography gw/default-bibliography
       bibtex-completion-pdf-field "file"
       org-directory "~/Dropbox/org"
       org-use-property-inheritance t
       org-agenda-files '("~/Dropbox/org/todo.org" "~/Dropbox/org/planner.org" "~/Dropbox/org/reminders.org" "~/Dropbox/org/calendar.org")
       org-agenda-span 7
       org-agenda-start-on-weekday 1
       org-startup-with-inline-images t
       org-hide-emphasis-markers t
       org-edit-src-content-indentation 0
       org-startup-with-latex-preview t)

;; TESTING THIS
(setq +org-capture-frame-parameters '((name . "doom-capture")
                                      (width . 170)
                                      (height . 110)
                                      (transient . t)
                                      (menu-bar-lines . 1)))

(setq! org-roam-directory "~/Dropbox/org/roam"
       org-roam-capture-templates
       '(("m" "main" plain
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t
          :immediate-finish t)
         ("r" "reference" plain
          "%?"
          :if-new
          (file+head "reference/${title}.org" "#+title: ${title}\n")
          :immediate-finish t
          :unnarrowed t)
         ("n" "literature note" plain
          "%?"
          :target
          (file+head
           "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
           "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
          :unnarrowed t)
         ("c" "citar literature note" plain
          "%?"
          :target
          (file+head "%(expand-file-name citar-org-roam-subdir org-roam-directory)/${citar-citekey}.org"
                     "#+title: ${citar-title}\n#+subtitle: ${citar-author}, ${citar-date}\n#+created: %U\n#+last_modified: %U\n\n")
          :unnarrowed t)))
(setq! citar-org-roam-capture-template-key "c")
(setq! org-roam-node-display-template (concat "${title:80} " (propertize "${tags:10}" 'face 'org-tag)))

(after! citar-org-roam
  (citar-org-roam-mode)
  (setq! citar-org-roam-template-fields
         '((:citar-citekey "key")
           (:citar-title "title")
           (:citar-author "author")
           (:citar-date "date" "year" "issued")
           (:citar-pages "pages")
           (:citar-type "=type="))
  citar-bibliography gw/default-bibliography
  citar-org-roam-subdir "reference/"
  citar-notes-paths '("~/Dropbox/org/roam/reference/")))

;; org-modern settings
(setq! org-modern-todo t
       org-modern-star 'replace
       org-modern-hide-stars nil
       org-modern-horizontal-rule t
       org-modern-timestamp t
       ;; org-modern-keyword "‣ "
       org-modern-keyword nil
       org-modern-table t
       org-modern-priority nil
       org-modern-block-name '("‣ " . "‣ ")
       org-modern-block-fringe 16)
(global-org-modern-mode 1)

;; make title of org file bigger
(custom-set-faces!
  '(org-document-title :height 1.3))

(setq! org-adapt-indentation t)

(map! :after org
      :prefix "C-c"
      "ri" #'org-roam-node-insert
      "rl" #'org-roam-buffer-toggle
      "rf" #'org-roam-node-find
      "rc" #'org-roam-capture
      "rr" #'org-roam-ref-find
      "ro" #'citar-open-note
      "rn" #'citar-create-note)

(use-package! websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-start t
        org-roam-ui-open-on-start t))

(use-package! org-download
  :after org
  :config
  (setq org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s"
        org-download-timestamp t
        org-download-method 'directory
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-heading-lvl nil
        org-image-actual-width 600
        org-image-align 'center)
  (setq-default org-download-image-dir "~/Dropbox/org/roam/images")
  (require 'org-download)
  :bind ("C-M-y" . org-download-screenshot))

;; for music symbols
(add-to-list 'org-entities-user
             '("dsharp" "\\dsharp" nil "" "" "1D12A" "𝄪"))
(add-to-list 'org-entities-user
             '("dflat" "\\dflat" nil "" "" "1D12B" "𝄫"))
(add-to-list 'org-entities-user
             '("natural" "\\natural" nil "" "" "266E" "♮"))
(add-to-list 'org-latex-classes
             '("extarticle"
               "\\documentclass{extarticle}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; hide the "#+" prefixing keywords in org files
(defvar my-org-hidden-keywords
  '(title author date email tags options filetags))
(defun org-hide-keywords ()
  (save-excursion
    (let (beg end ov)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\(^[ \t]*#\\+\\)\\("
                      (mapconcat (lambda (kw)
                                   (format "%s:\s"(symbol-name kw)))
                                 my-org-hidden-keywords "\\|")
                      "\\)")
              nil t)
        (setq beg (match-beginning 1)
              end (match-end 2)
              ov  (make-overlay beg end))
    (overlay-put ov 'invisible t)))))
(add-hook 'org-mode-hook 'org-hide-keywords)

;;; --- KEYMAPS ---
(map! :map yas-minor-mode-map
      "M-z" #'yas-expand)
(map! :map yas-keymap
      "M-j" #'yas-next-field-or-maybe-expand)
(map! :map yas-keymap
      "M-k" #'yas-prev-field)

;; I can't figure out how to make these keymaps take precedence without doing it like this
(after! elfeed
  (evil-define-key 'normal 'elfeed-search-mode-map (kbd "B") 'my/elfeed-search-browse-background-url)
  (evil-define-key 'normal 'elfeed-show-mode-map (kbd "B") 'my/elfeed-search-browse-background-url))

(map! :prefix "C-x"
      "k" #'gw/kill-this-buffer
      "C-r" #'consult-recent-file
      "b" #'consult-buffer
      "rb" #'consult-bookmark)

(map! :prefix "M-s"
      "M-g" #'consult-grep
      "M-f" #'consult-find
      "M-o" #'consult-outline
      "M-l" #'consult-line
      "M-b" #'consult-buffer
      "M-i" #'consult-imenu)

(map! :prefix "C-c"
      "a" #'org-agenda
      ;; "c" #'org-capture
      "c" #'+org-capture/open-frame
      "z" #'reveal-in-osx-finder
      "dg" #'deadgrep)

(map! "C-g" #'gw/keyboard-quit-dwim)

(map! :map dired-mode-map
      "C-x p" 'dired-preview-mode)

;; tabs
(map! "s-]" #'+tabs:next-or-goto)
(map! "s-[" #'+tabs:previous-or-goto)
(map! "s-w" #'centaur-tabs-delete-tab)

;; citations
(map! :map org-mode-map
      "C-c b" #'citar-insert-citation)

;; for tab behaviour
(map! :after logos
      "C-x n n" #'logos-narrow-dwim
      "C-x ]" #'logos-forward-page-dwim
      "C-x [" #'logos-backward-page-dwim
      "M-]" #'logos-forward-page-dwim
      "M-[" #'logos-backward-page-dwim
      "<f9>" #'logos-focus-mode)

;; for searching org-roam dir with consult
(use-package! consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

(map! :after consult-org-roam
      "C-c r s" #'consult-org-roam-search
      "C-c r f" #'consult-org-roam-file-find
      "C-c r b" #'consult-org-roam-backlinks)

(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

(map! :after latex
      :map cdlatex-mode-map
      :localleader
      :desc "Insert math symbol"
      "i" #'cdlatex-math-symbol
      :desc "Begin environment"
      "e" #'cdlatex-environment)

(map! :after eww
      "M-s W" #'eww-search-words
      :map eww-mode-map
      "D" #'eww-download
      "d" #'my/scroll-up-half
      "u" #'my/scroll-down-half
      "U" #'eww-up-url
      "^" #'eww-up-url
      "i" #'imenu)
