(use-package elfeed
  :straight t
  :commands (elfeed elfeed-update)
  :config
  (use-package elfeed-org
    :straight t
    :config
    (require 'elfeed-org)
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed-feeds.org")))
  ;; (setq elfeed-feeds
  ;;       '("https://www.nature.com/nature.rss" ;; Naure
  ;;         "https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=science" ;; Science
  ;;         "https://pubmed.ncbi.nlm.nih.gov/rss/search/1fOfQYMp1QUMVg9HoG06sLlkYcemijOCBRLY1dFeN0JogJugEm/?limit=100&utm_campaign=pubmed-2&fc=20250117092618" ;; Twist2
  ;;         "http://rss.sciencedirect.com/publication/science/00928674"
  ;;         "http://elife.elifesciences.org/rss/recent.xml" ;; eLife
  ;;         "https://journals.plos.org/plosbiology/feed/atom" ;; PLoS Biology
  ;;         "http://www.pnas.org/rss/current.xml" ;; PNAS
  ;;         "http://rss.sciencedirect.com/publication/science/22111247"  ;; Cell Reports
  ;;         "https://rss.sciencedirect.com/publication/science/19345909" ;; Cell Stem Cell
  ;;         "https://rss.sciencedirect.com/publication/science/09550674" ;; Curr Opin Cell Biol
  ;;         "https://rss.sciencedirect.com/publication/science/0959437X" ;; Curr Opin Gen Dev
  ;;         "https://rss.sciencedirect.com/publication/science/24523100" ;; Curr Opin Sys Biol
  ;;         "https://journals.biologists.com/rss/site_1000005/1000005.xml" ;; Development
  ;;         "https://rss.sciencedirect.com/publication/science/15345807" ;; Dev Cell
  ;;         "http://www.nature.com/ncomms/current_issue/rss" ;; Nature Comm
  ;;         "http://feeds.nature.com/ng/rss/current" ;; Nature Gen
  ;;         "http://www.nature.com/nmeth/current_issue/rss" ;; Nature Methods
  ;;         "https://pubmed.ncbi.nlm.nih.gov/rss/search/14YzNBPGjPa5Oq-1ALt3vUGjAC4ish-IOxqVHLuo9c-4GVCkY4/?limit=50&utm_campaign=pubmed-2&fc=20250117094320" ;; Rudnicki
  ;;         "https://pubmed.ncbi.nlm.nih.gov/rss/search/1xCFUMSbAMYdtC8KJFgKAIIAYo-4xPQwAHWYwe_JY77Z1dmEg0/?limit=50&utm_campaign=pubmed-2&fc=20250117093424" ;; Shendure
  ;;         "https://pubmed.ncbi.nlm.nih.gov/rss/search/10IsjDYWWF_hcNrfAx1dT3XTb8sIiPJbB2iIh0P8a7fBSSPQPB/?limit=15&utm_campaign=pubmed-2&fc=20250117092818" ;; Freda
  ;;         ))
  (setq-default elfeed-save-multiple-enclosures-without-asking t
                elfeed-search-clipboard-type 'CLIPBOARD
                elfeed-search-filter "#50 +unread "
                elfeed-search-date-format '("%Y-%m-%d" 10 :left)
                elfeed-search-title-min-width 45
                elfeed-search-title-max-width 120)

  (eval-when-compile
    (defmacro elfeed-with-open-entry (&rest body)
      "Execute BODY with a visible elfeed entry as current."
      (declare (indent defun))
      `(when-let ((win
                   (or (get-buffer-window "*elfeed-entry*")
                       (seq-some (lambda (w)
                                   (and (memq
                                         (buffer-mode (window-buffer w))
                                         '(elfeed-show-mode eww-mode))
                                        w))
                                 (window-list)))))
         (with-selected-window win
           ,@body))))
  (defun my/elfeed-search-browse-url (&optional arg)
    (interactive "P")
    (elfeed-with-open-entry
      (my/search-occur-browse-url arg)))
  (timeout-debounce! 'elfeed-search--live-update 0.24)

  (defun my/reader-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

  (defun elfeed-search-show-entry-pre (&optional lines)
    "Returns a function to scroll forward or back in the elfeed
search results, displaying entries without switching to them."
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (let ((elfeed-show-entry-switch #'my/reader-display-buffer))
        (call-interactively #'elfeed-search-show-entry))
      (when-let ((win (get-buffer-window "*elfeed-search*")))
        (select-window win)
        (setq-local other-window-scroll-buffer
                    (get-buffer "*elfeed-entry*")))
      (unless elfeed-search-remain-on-entry (forward-line -1))))
  (keymap-set elfeed-search-mode-map "M-RET" 'elfeed-search-show-entry)
  (keymap-set elfeed-search-mode-map "RET" (elfeed-search-show-entry-pre))
  (keymap-set elfeed-search-mode-map "M-n" (elfeed-search-show-entry-pre 1))
  (keymap-set elfeed-search-mode-map "M-p" (elfeed-search-show-entry-pre -1))

  (defun my/elfeed-search-imenu ()
    (interactive)
    (elfeed-with-open-entry
      (consult-imenu)))
  (defun elfeed-search-eww-open (&optional use-generic-p)
    "Visit the current entry in your browser using `eww'."
    (interactive "P")
    (let ((buffer (current-buffer))
          (entries (elfeed-search-selected))
          (browse-url-browser-function #'eww-browse-url)
          (eww-retrieve-command 'sync))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-with-open-entry
                    (if use-generic-p
                        (browse-url-generic it)
                      (browse-url it)
                      (eww-readable))))
      (with-current-buffer buffer
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))))
  (defun my/elfeed-show-quit-menu ()
    (interactive)
    (if (window-live-p (get-buffer-window "*elfeed-search*"))
        (progn
          (kill-buffer-and-window)
          (select-window (get-buffer-window "*elfeed-search*")))
      (kill-buffer (current-buffer))))
  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in elfeed."
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))
  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll down or go to previous feed item in elfeed."
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  (defun my/elfeed-search-scroll-up-command (&optional arg)
    (interactive "^P")
    (if-let ((show-win (seq-some
                        (lambda (w)
                          (let* ((bm (buffer-mode (window-buffer w))))
                            (and (memq bm '(elfeed-show-mode eww-mode)) w)))
                        (window-list))))
        (with-selected-window show-win
          (elfeed-scroll-up-command arg))
      (funcall (elfeed-search-show-entry-pre 1) 1)))
  (defun my/elfeed-search-scroll-down-command (&optional arg)
    (interactive "^P")
    (if-let ((show-win (seq-some
                        (lambda (w)
                          (let* ((bm (buffer-mode (window-buffer w))))
                            (and (memq bm '(elfeed-show-mode eww-mode)) w)))
                        (window-list)))
             (_ (widow-live-p show-win)))
        (with-selected-window show-win
          (elfeed-scroll-down-command arg))
      (funcall (elfeed-search-show-entry-pre -1) 1)))
  (defun elfeed-search-tag-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG."
    (lambda ()
      "Toggle a tag on an elfeed search selection."
      (interactive)
      (elfeed-search-toggle-all mytag)))
  (keymap-set elfeed-search-mode-map "l" (elfeed-search-tag-as 'later))
  (keymap-set elfeed-search-mode-map "d" (elfeed-search-tag-as 'junk))
  (bind-key "l" (elfeed-search-tag-as 'later) elfeed-search-mode-map)
  (bind-key "u" (elfeed-search-tag-as 'unread) elfeed-search-mode-map)

  (defun elfeed-show-tag-as (mytag)
    "Returns a function that tags an elfeed entry or selection as MYTAG."
    (lambda ()
      "Toggle a tag on an elfeed entry being displayed."
      (interactive)
      (elfeed-tag elfeed-show-entry mytag)
      (elfeed-search-update-entry elfeed-show-entry)
      (unless elfeed-search-remain-on-entry (elfeed-show-next))))

  (keymap-set elfeed-show-mode-map "l" (elfeed-show-tag-as 'later))
  (keymap-set elfeed-show-mode-map "d" (elfeed-show-tag-as 'junk))

  (bind-key "l" (elfeed-show-tag-as 'later) elfeed-show-mode-map)
  (bind-key "U" (elfeed-show-tag-as 'unread) elfeed-show-mode-map)

  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "The Linux Experiment"
                                :entry-title "Linux News"
                                :add '(news listen)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "Skill Up"
                                :entry-title "This Week"
                                :add '(news listen)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "TechLinked"
                                :add '(news listen)))
  (add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)
  (defun elfeed-declickbait-entry (entry)
    (when-let ((title (elfeed-entry-title entry))
               (feed (elfeed-entry-feed entry))
               (feed-url (elfeed-feed-url feed))
               (youtube-p (string-match-p "youtube\\.com" feed-url)))
      (plist-put (elfeed-meta--plist entry) :title
                 (elfeed-title-transform title))))
  (defun elfeed-title-transform (title)
    "Declickbait string TITLE."
    (let* ((trim "\\(?:\\(?:\\.\\.\\.\\|[!?]\\)+\\)")
           (arr (split-string title nil t trim))
           (s-table (copy-syntax-table)))
      (modify-syntax-entry ?\' "w" s-table)
      (with-syntax-table s-table
        (mapconcat (lambda (word)
                     (cond
                      ((member word '("AND" "OR" "IF" "ON" "IT" "TO"
                                      "A" "OF" "VS" "IN" "FOR" "WAS"
                                      "IS" "BE" "SO" "BY"))
                       (downcase word))
                      ((member word '("WE" "DAY" "HOW" "WHY" "NOW" "OLD"
                                      "NEW" "MY" "TOO" "GOT" "GET" "THE"
                                      "ONE" "DO" "YOU" "BAD" "ALL" "CAN"
                                      "HE" "EAT" "BUT" "AN" "WAY" "NOT"))
                       (capitalize word))
                      ((and (> (length word) 3)
                            (string-match-p "\\`[A-Z\\.\\?\\!\\':,’\\-]*\\'"
                                            word))
                       (capitalize word))
                      (t (replace-regexp-in-string
                          (rx (group punct) (group (any "T" "M" "S" "D")))
                          (lambda (m)
                            (concat (match-string 1 m)
                                    (downcase (match-string 2 m))))
                          word))))
                   arr " "))))
  (defun my/elfeed-search-by-day (dir)
    (lambda (&optional arg)
      (interactive "p")
      (let* ((entry (elfeed-search-selected :ignore-region))
             (this-day (or (and (string-match-p ".*@\\(.+\\)--.*" elfeed-search-filter)
                                (time-to-seconds
                                 (encode-time
                                  (parse-time-string
                                   (concat (replace-regexp-in-string
                                            ".*@\\([^[:space:]]+?\\)--.*" "\\1"
                                            elfeed-search-filter)
                                           " 00:00:00 Z")))))
                           (and entry
                                (elfeed-entry-date entry))
                           (time-to-seconds
                            (current-time))))
             (next-day (time-add this-day (days-to-time (or arg 1))))
             (next-next-day (time-add next-day (days-to-time (or arg 1))))
             (next-next-next-day (time-add next-next-day (days-to-time (or arg 1))))
             (prev-day (time-subtract this-day (days-to-time (or arg 1))))
             from to)
        (pcase dir
          ('next (setq from next-next-day
                       to   next-next-next-day))
          ('prev (setq from this-day
                       to   next-day))
          (_     (setq from next-day
                       to   next-next-day)))
        (let ((elfeed-search-date-format '("%Y-%m-%d" 10 :left)))
          (setq elfeed-search-filter (concat (replace-regexp-in-string
                                              " @[^[:space:]]*" ""
                                              elfeed-search-filter)
                                             " @"  (elfeed-search-format-date from)
                                             "--" (elfeed-search-format-date to))))
        (elfeed-search-update :force))))
  (defun my/elfeed-random-date ()
    (interactive)
    (let* ((from
            (time-to-seconds
             (encode-time
              (parse-time-string
               (format "%d-%02d-%02d 00:00:00 Z"
                       (+ 2012 (cl-random 10))
                       (1+ (cl-random 11))
                       (1+ (cl-random 30)))))))
           (to (time-add from (days-to-time 5)))
           (date-string
            (concat " @" (elfeed-search-format-date from)
                    "--" (elfeed-search-format-date to))))
      (setq elfeed-search-filter
            (concat (replace-regexp-in-string
                     " @[^[:space:]]*" ""
                     elfeed-search-filter)
                    date-string)))
    (elfeed-search-update :force))
  (define-key elfeed-search-mode-map (kbd ".") (my/elfeed-search-by-day 'this))
  (define-key elfeed-search-mode-map (kbd "b") (my/elfeed-search-by-day 'next))
  (define-key elfeed-search-mode-map (kbd "f") (my/elfeed-search-by-day 'prev))
  (define-key elfeed-search-mode-map (kbd "`") 'my/elfeed-random-date)
  (defvar my/elfeed-db-all-tags nil)

  (defvar elfeed-search-filter-map
    (let ((map (copy-keymap minibuffer-local-map)))
      (define-key map (kbd "TAB") 'minibuffer-complete)
      map))

  (defun elfeed-search-live-filter (&optional refresh-tags)
    "Filter the elfeed-search buffer as the filter is written.

With prefix-arg REFRESH-TAGS, refresh the cached completion metadata."
    (interactive "P")
    (unwind-protect
        (let* ((elfeed-search-filter-active :live)
               (completions (mapcan (lambda (tag) (list (concat "+" (symbol-name tag))
                                                        (concat "-" (symbol-name tag))))
                                    (if (or refresh-tags (not my/elfeed-db-all-tags))
                                        (elfeed-db-get-all-tags)
                                      my/elfeed-db-all-tags)))
               (completion-styles '(basic partial-completion))
               (minibuffer-completion-table
                (completion-table-dynamic
                 (lambda (string)
                   (cond
	                ((string-match "\\(^\\|.* (?\\)\\([^ ]*\\)$" string)
                     (mapcar (lambda (compl)
			                   (concat (match-string-no-properties 1 string) compl))
		                     (all-completions (match-string-no-properties 2 string)
					                          completions)))
	                (t (list string)))))))
          (setq elfeed-search-filter
                (read-from-minibuffer "Filter: " elfeed-search-filter elfeed-search-filter-map)))
      (elfeed-search-update :force)))
  (with-eval-after-load 'corfu
    (add-to-list 'my-corfu-minibuffer-exclude-modes
                 elfeed-search-filter-map))

  ;; Add tag-completion to the tag/untag commands in elfeed-search
  (defun elfeed-search-tag-all (tag)
    "Apply TAG to all selected entries."
    (interactive (let* ((completions (if (not my/elfeed-db-all-tags)
                                         (elfeed-db-get-all-tags)
                                       my/elfeed-db-all-tags))
                        (minibuffer-completion-table completions))
                   (list (intern (read-from-minibuffer
                                  "Tag: " nil elfeed-search-filter-map)))))
    (let ((entries (elfeed-search-selected)))
      (elfeed-tag entries tag)
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun elfeed-search-untag-all (tag)
    "Remove TAG from all selected entries."
    (interactive (let* ((completions
                         (cl-reduce
                          (lambda (t1 e2) (cl-union t1 (elfeed-entry-tags e2)))
                          (elfeed-search-selected) :initial-value nil))
                        (minibuffer-completion-table completions))
                   (list (intern (read-from-minibuffer
                                  "Tag: " nil elfeed-search-filter-map)))))
    (let ((entries (elfeed-search-selected)))
      (elfeed-untag entries tag)
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (advice-add
   'elfeed-search-parse-filter :filter-return
   (defun my/elfeed-search-parse-filter-whitespace (parsed)
     (prog1 parsed
       (dolist (field '(:feeds :not-feeds))
         (cl-callf (lambda (feed-list)
                     (mapcar (lambda (feed) (replace-regexp-in-string "-" " " feed))
                             feed-list))
             (plist-get parsed field))))))

  (defun my/elfeed-search-make-feed-filter (filter-char)
    "Narrow elfeed search to the current feed or its inverse."
    (lambda (&optional reset)
      (interactive "P")
      (if reset
          (let ((filter (split-string elfeed-search-filter)))
            (setq elfeed-search-filter
                  (string-join (cl-delete-if
                                (lambda (s) (eq (aref s 0) filter-char))
                                filter)
                               " ")))
        (when-let ((fc (concat " " (make-string 1 filter-char)))
                   (feed-titles
                    (cl-delete-duplicates
                     (mapcar
                      (lambda (e)
                        (elfeed-feed-title
                         (elfeed-entry-feed e)))
                      (elfeed-search-selected)))))
          (setq elfeed-search-filter
                (concat elfeed-search-filter fc
                        (mapconcat (lambda (title)
                                     (replace-regexp-in-string " " "-" title))
                                   feed-titles fc)))))
      (elfeed-search-update :force)))

  (define-key elfeed-search-mode-map (kbd "~") (my/elfeed-search-make-feed-filter ?~))
  (define-key elfeed-search-mode-map (kbd "=") (my/elfeed-search-make-feed-filter ?=))

  (defun my/elfeed-quick-switch-filter ()
    (interactive)
    (bookmark-jump
     (consult--read
      (consult--bookmark-candidates)
      :prompt "Elfeed bookmark: "
      :initial "elfeed/")))

  (defun my/elfeed-zoom-image ()
    (interactive)
    (elfeed-with-open-entry
      (save-excursion
        (goto-char (window-start))
        (when (text-property-search-forward 'image-url)
          (forward-char -1)
          (shr-zoom-image)))))

  (define-key elfeed-search-mode-map (kbd "z") #'my/elfeed-zoom-image)
  (define-key elfeed-show-mode-map (kbd "z") #'my/elfeed-zoom-image)

  (use-package setup-reading
    :demand
    :bind (:map elfeed-search-mode-map
                ("RET" . my/reader-show)
                ("M-n" . my/reader-next)
                ("M-p" . my/reader-prev)
                ("q" . my/reader-quit-window)
                ("SPC" . my/reader-scroll-up-command)
                ("S-SPC" . my/reader-scroll-down-command)
                ("DEL" . my/reader-scroll-down-command)
                ("M-s i" . my/reader-imenu)
                ("i" . my/reader-imenu)
                ("<tab>" . my/reader-push-button)
                ("M-s u" . my/reader-browse-url)
                ("<" . my/reader-top)
                (">" . my/reader-bottom)
                :map elfeed-show-mode-map
                ("SPC" . elfeed-scroll-up-command)
                ("S-SPC" . elfeed-scroll-down-command)
                ("w" . elfeed-show-yank)
                ("B" . elfeed-show-eww-open)
                ("x" . elfeed-search-browse-url)
                ("D" . elfeed-show-save-enclosure)
                ("d" . my/scroll-up-half)
                ("u" . my/scroll-down-half)
                ([remap elfeed-kill-buffer] . my/elfeed-show-quit-window)
                :map elfeed-search-mode-map
                ([remap elfeed-search-quit-window] . my/reader-quit-window)
                ("M-RET" . elfeed-search-show-entry)
                ("w" . elfeed-search-yank)
                ("C-<tab>" . my/elfeed-quick-switch-filter)
                ("B" . elfeed-search-eww-open)
                ("x" . elfeed-search-browse-url)))
  )

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

;; why do these break emacs?
                                        ;(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
                                        ;(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
                                        ;(add-to-list 'evil-emacs-state-modes 'elfeed-entry-mode)

(use-package pocket-reader
  :straight t
  :config
  ;; this is required to stop some weird ghost item bug
  (setq pocket-reader-default-queries '(":unread")))
(require 'pocket-reader)

