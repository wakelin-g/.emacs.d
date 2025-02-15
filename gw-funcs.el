;; -*- lexical-binding: t -*-
(defun gw/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun gw/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun gw/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun gw/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun gw/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun gw/save-and-close-current-buffer ()
  (if (get-buffer buffer)
      (let ((b (get-buffer buffer)))
        (save-buffer b)
        (kill-buffer b))))

(defun gw/kill-all-except-current (&optional arg)
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted")))

(defun gw/reload-emacs-config ()
  (interactive)
  (load (expand-file-name "init.el" user-emacs-directory)))

(defun gw/open-buffer-file-mac ()
  "Open current buffer file using macOS `open' command."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(defun gw/fzf ()
  "Select folder to fzf."
  (interactive)
  (let ((current-prefix-arg '-))
    (call-interactively 'counsel-fzf)))

(defun gw/rg ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'counsel-rg)))

;;; org
(defun gw/org-agenda-caller (letter)
  "Calls specific org agenda view specified by letter arg."
  (org-agenda nil letter))

(defun gw/org-schedule-tomorrow ()
  "Org schedule for tomorrow (+1d)."
  (interactive)
  (org-schedule t "+1d"))

(defun gw/org-refile-this-file ()
  "Org refile to only headers in current file, 5 levels."
  (interactive)
  (let ((org-refile-targets '((nil . (:maxlevel 5)))))
    (org-refile)))

(defun gw/refresh-org-agenda-from-afar ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (save-window-excursion
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo))))

(defun gw/org-done-keep-todo()
  "Mark an org todo item as done while keeping its former keyword intact, and archive.

For example, * TODO ItemA -> * DONE TODO ItemA"
  (interactive)
  (let ((state (org-get-todo-state)) (tag (org-get-tags)) (todo (org-entry-get (point) "TODO"))
        post-command-hook)
    (if (not (eq state nil))
        (progn (org-back-to-heading)
               (org-todo "DONE")
               (org-set-tags tag)
               (beginning-of-line)
               (forward-word)
               (insert (concat " " todo)))
      (user-error "Not a TODO."))
    (run-hooks 'post-command-hook)))

(defun gw/org-done-keep-todo-archive()
  "Same as gw/org-done-keep-todo, but also archives heading."
  (interactive)
  (let ((state (org-get-todo-state)) (tag (org-get-tags)) (todo (org-entry-get (point) "TODO"))
        post-command-hook)
    (if (not (eq state nil))
        (progn (org-back-to-heading)
               (org-todo "DONE")
               (org-set-tags tag)
               (beginning-of-line)
               (forward-word)
               (insert (concat " " todo))
               (org-archive-subtree-default))
      (user-error "Not a TODO."))
    (run-hooks 'post-command-hook)))

(defun gw/org-archive-ql-search ()
  "Input or select a tag to search in archive files."
  (interactive)
  (let * ((choices '("a" "b" "c" "d" "e" "f")) ;; need to figure out what my normal tags will be
          (org (completing-read "Tag: " choices)))
       (org-ql-search
        (directory-files-recursively
         (expand-file-name "org-archive" org-directory) ".org_archive")
        (and (or (property "ARCHIVE_ITAGS" ,tag) (tags ,tag)) (or (todo) (done))))))

(defun gw/org-occur-unchecked-boxes ()
  "Show unchecked org-mode checkboxes. Ignore items with a `†' at EOL unless run with C-u."
  (interactive "P")
  (if (equal '(4) arg)
      (occur "\\[ \\].*†$")
    (occur "\\[ \\].*[^†]$")))

(defun gw/kill-window (name)
  "Kill window by pressing <q>."
  (select-window
   (car (seq-filter
         (lambda (window)
           (equal name (buffer-name (window-buffer window))))
         (window-list-1 nil 0 t)))))

(defun gw/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun gw/follow-link ()
  (interactive)
  (message (string= (car (org-thing-at-point)) "link")))
(provide 'gw-funcs)

(defun gw/kill-this-buffer ()
  "Kill current buffer instantly."
  (interactive)
  (kill-buffer (current-buffer)))

(defun gw/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(defun gw/elfeed-search-browse-background-url ()
  "Open current `elfeed' entry (or region entries) in browser without losing focus."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (mapc (lambda (entry)
            (start-process (concat "open " (elfeed-entry-link entry))
                           nil "open" "--background" (elfeed-entry-link entry))
            (elfeed-untag entry 'unread)
            (elfeed-search-update-entry entry))
          entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))
