;; -*- lexical-binding: t -*-

(defun gw/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

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
(defun gw/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

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

(defun gw/org-refile-to-planner (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file "~/orgmode/planner.org" arg))
(defun gw/org-refile-to-planner-today (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file "~/orgmode/planner.org" (current-time)))

(defun gw/finder-path ()
  "Return path of frontmost Finder window, or empty string.

Asks Finder for the path using AppleScript via `osascript', so
this can take a second or two to execute."
  (let ($applescript $result)
    (setq $applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")
    (setq $result (ns-do-applescript $applescript))
    (if $result
        (string-trim $result)
      "")))

(defun gw/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

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

(defun gw/delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end   (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (< (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun gw/smart-delete-trailing-whitespace ()
  "Invoke `gw/delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (gw/delete-trailing-whitespace-except-current-line)))

(defun gw/toggle-auto-trailing-ws-removal ()
  "Toggle trailing whitespace removal."
  (interactive)
  (if (member #'gw/smart-delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'gw/smart-delete-trailing-whitespace)
        (message "Disabled auto remove trailing whitespace."))
    (add-hook 'before-save-hook #'gw/smart-delete-trailing-whitespace)
    (message "Enabled auto remove trailing whitespace.")))
;; (add-hook 'before-save-hook #'gw/smart-delete-trailing-whitespace) ; I think something is wrong with this

(use-package timeout
  :straight (timeout :type git :host github :repo "karthink/timeout"))

(defun gw/brew-update-upgrade ()
  (interactive)
  (with-output-to-temp-buffer "*brew-output*"
    (shell-command "brew update && brew upgrade &"
                   "*brew-output*"
                   "*Messages*")
    (pop-to-buffer "*brew-output*")))

(defun gw/brew-update ()
  (interactive)
  (with-output-to-temp-buffer "*brew-output*"
    (async-shell-command "brew update"
                         "*brew-output*"
                         "*Messages*")
    (pop-to-buffer "*brew-output*")
    (read-only-mode)))

(defun gw/brew-upgrade ()
  (interactive)
  (with-output-to-temp-buffer "*brew-output*"
    (async-shell-command "brew upgrade"
                         "*brew-output*"
                         "*Messages*")
    (pop-to-buffer "*brew-output*")
    (read-only-mode)))

(defun gw/tidy-html ()
  "Tidies HTML content using `tidy'."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "tidy -i -w 120 -q"
   (current-buffer)
   t
   "*Tidy Error Buffer*"
   t))

(defun gw/jq-current-buffer ()
  "Run shell command on current buffer and revert."
  (interactive)
  (shell-command
   (format "/opt/homebrew/bin/jq %s" (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t))

(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
