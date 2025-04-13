(require 'eww)
(require 'gw-common)

(defun gw-eww--get-urls ()
  "Get all links in current buffer."
  (let ((links nil))
    (save-excursion
      (goto-char (point-min))
      (while (text-property-search-forward 'face 'shr-link)
        (when-let* ((position (point))
                    (button (button-at position)))
          (push
           (list position
                 (button-label button)
                 (shr-url-at-point nil))
           links))))
    (nreverse links)))

(defun gw-eww-buffer-url-prompt ()
  "Prompt for a url in the current buffer."
  (when-let* ((completion-extra-properties `(:annotation-function ,#'gw-eww--annotate-with-url))
              (link-data (gw-eww--get-urls))
              (candidates (mapcar
                           (pcase-lambda (`(,position ,name ,_))
                             (format "%s	%s" position name))
                           link-data))
              (table (gw-common-completion-table-no-sort nil candidates))
              (selection
               (completing-read
                (format-prompt "Select link in the current page" nil)
                table))
              (index (string-to-number (car (split-string selection "\t")))))
    (assoc index link-data)))

(defun gw-eww-visit-url-on-page (&optional new-buffer)
  "Visit url among those in current buffer using completion.
With optional NEW-BUFFER as a argument, visit the url in a new
buffer instead of the current one."
  (interactive "P" eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer."))
  (if-let* ((data (gw-eww-buffer-url-prompt))
            (url (nth 2 data)))
      (eww url new-buffer)
    (error "Cannot find URL in data `%s'" data)))

(defun gw-eww-jump-to-url-on-page ()
  "Go to the position of a URL among those in current buffer."
  (interactive nil eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer."))
  (if-let* ((data (gw-eww-buffer-url-prompt))
            (position (car data)))
      (goto-char position)
    (error "Cannot position in data `%s'" data)))

(defvar gw-eww--occur-feed-regexp
  (concat "\\(rss\\|atom\\)\\+xml.\\(.\\|\n\\)"
          ".*href=[\"']\\(.*?\\)[\"']")
  "Regular expression to match web feeds in HTML source.")

(defun gw-eww-find-feed ()
  "Produce bespoke buffer with RSS/Atom links from XML source."
  (interactive nil eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer."))
  (let* ((url (or (plist-get eww-data :start)
                  (plist-get eww-data :contents)
                  (plist-get eww-data :home)
                  (plist-get eww-data :url)))
         (title (or (plist-get eww-data :title) url))
         (source (plist-get eww-data :source))
         (buf-name (format "*feeds: %s # eww*" title)))
    (with-temp-buffer
      (insert source)
      (occur-1 gw-eww--occur-feed-regexp "\\3" (list (current-buffer)) buf-name))
    (when (get-buffer buf-name)
      (with-current-buffer (get-buffer buf-name)
        (let ((inhibit-read-only t)
              (base-url (replace-regexp-in-string "\\(.*/\\)[^/]+\\'" "\\1" url)))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (goto-char (line-beginning-position))
            (when (and (looking-at prot-common-url-regexp)
                       (not (looking-at (format "%s.*" url))))
              (insert base-url))
            (forward-line 1)))))))

(defun gw-eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)
  (eww-open-in-new-buffer))
