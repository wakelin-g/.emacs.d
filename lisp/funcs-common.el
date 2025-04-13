;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup gw-common ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun gw-common-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun gw-common-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun gw-common-number-integer-positive-p (n)
  "Test if N is a positive integer."
  (if (gw-common-number-integer-p n)
      (> n 0)
    (error "%s is not a positive integer" n)))

;;;###autoload
(defun gw-common-number-negative (n)
  "Make N negative."
  (if (and (numberp n) (> n 0))
      (* -1 n)
    (error "%s is not a valid positive number" n)))

;;;###autoload
(defun gw-common-reverse-percentage (number percent change-p)
  "Determine the original value of NUMBER given PERCENT.

CHANGE-P should specify the increase or decrease.  For simplicity,
nil means decrease while non-nil stands for an increase.

NUMBER must satisfy `numberp', while PERCENT must be `natnump'."
  (unless (numberp number)
    (user-error "NUMBER must satisfy numberp"))
  (unless (natnump percent)
    (user-error "PERCENT must satisfy natnump"))
  (let* ((pc (/ (float percent) 100))
         (pc-change (if change-p (+ 1 pc) pc))
         (n (if change-p pc-change (float (- 1 pc-change)))))
    ;; FIXME 2021-12-21: If float, round to 4 decimal points.
    (/ number n)))

;;;###autoload
(defun gw-common-percentage-change (n-original n-final)
  "Find percentage change between N-ORIGINAL and N-FINAL numbers.

When the percentage is not an integer, it is rounded to 4
floating points: 16.666666666666664 => 16.667."
  (unless (numberp n-original)
    (user-error "N-ORIGINAL must satisfy numberp"))
  (unless (numberp n-final)
    (user-error "N-FINAL must satisfy numberp"))
  (let* ((difference (float (abs (- n-original n-final))))
         (n (* (/ difference n-original) 100))
         (round (floor n)))
    ;; FIXME 2021-12-21: Any way to avoid the `string-to-number'?
    (if (> n round) (string-to-number (format "%0.4f" n)) round)))

;; REVIEW 2023-04-07 07:43 +0300: I just wrote the conversions from
;; seconds.  Hopefully they are correct, but I need to double check.
(defun gw-common-seconds-to-minutes (seconds)
  "Convert a number representing SECONDS to MM:SS notation."
  (let ((minutes (/ seconds 60))
        (seconds (% seconds 60)))
    (format "%.2d:%.2d" minutes seconds)))

(defun gw-common-seconds-to-hours (seconds)
  "Convert a number representing SECONDS to HH:MM:SS notation."
  (let* ((hours (/ seconds 3600))
         (minutes (/ (% seconds 3600) 60))
         (seconds (% seconds 60)))
    (format "%.2d:%.2d:%.2d" hours minutes seconds)))

;;;###autoload
(defun gw-common-seconds-to-minutes-or-hours (seconds)
  "Convert SECONDS to either minutes or hours, depending on the value."
  (if (> seconds 3599)
      (gw-common-seconds-to-hours seconds)
    (gw-common-seconds-to-minutes seconds)))

;;;###autoload
(defun gw-common-rotate-list-of-symbol (symbol)
  "Rotate list value of SYMBOL by moving its car to the end.
Return the first element before performing the rotation.

This means that if `sample-list' has an initial value of `(one
two three)', this function will first return `one' and update the
value of `sample-list' to `(two three one)'.  Subsequent calls
will continue rotating accordingly."
  (unless (symbolp symbol)
    (user-error "%s is not a symbol" symbol))
  (when-let* ((value (symbol-value symbol))
              (list (and (listp value) value))
              (first (car list)))
    (set symbol (append (cdr list) (list first)))
    first))

;;;###autoload
(defun gw-common-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

;;;###autoload
(defun gw-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;;;###autoload
(defun gw-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;; NOTE 2023-08-12: I tried the `clear-message-function', but it did
;; not work.  What I need is very simple and this gets the job done.
;;;###autoload
(defun gw-common-clear-minibuffer-message (&rest _)
  "Print an empty message to clear the echo area.
Use this as advice :after a noisy function."
  (message ""))

;;;###autoload
(defun gw-common-disable-hl-line ()
  "Disable Hl-Line-Mode (for hooks)."
  (hl-line-mode -1))

;;;###autoload
(defun gw-common-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

;;;###autoload
(defun gw-common-page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

;;;###autoload
(defun gw-common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

(defun gw-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun gw-common-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

;;;###autoload
(defun gw-common-read-data (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

;;;###autoload
(defun gw-common-completion-category ()
  "Return completion category."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

;; Thanks to Omar Antolín Camarena for providing this snippet!
;;;###autoload
(defun gw-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;;;###autoload
(defun gw-common-completion-table-no-sort (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.
Like `gw-common-completion-table' but also disable sorting."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (display-sort-function . ,#'identity))
      (complete-with-action action candidates string pred))))

;; Thanks to Igor Lima for the `gw-common-crm-exclude-selected-p':
;; <https://github.com/0x462e41>.
;; This is used as a filter predicate in the relevant prompts.
(defvar crm-separator)

;;;###autoload
(defun gw-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

;; The `gw-common-line-regexp-p' and `gw-common--line-regexp-alist'
;; are contributed by Gabriel: <https://github.com/gabriel376>.  They
;; provide a more elegant approach to using a macro, as shown further
;; below.
(defvar gw-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `gw-common-line-regexp-p'.")

(defun gw-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`gw-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type gw-common--line-regexp-alist))))))

;; The `gw-common-shell-command-with-exit-code-and-output' function is
;; courtesy of Harold Carr, who also sent a patch that improved
;; `gw-eww-download-html' (from the `gw-eww.el' library).
;;
;; More about Harold: <http://haroldcarr.com/about/>.
(defun gw-common-shell-command-with-exit-code-and-output (command &rest args)
  "Run COMMAND with ARGS.
Return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (buffer-string))))

(defvar gw-common-url-regexp
  (concat
   "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
   "[.@]"
   "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
  "Regular expression to match (most?) URLs or email addresses.")

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun gw-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let* ((source (auth-source-search :host host)))
    (if (eq prop :secret)
        (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

;;;###autoload
(defun gw-common-parse-file-as-list (file)
  "Return the contents of FILE as a list of strings.
Strings are split at newline characters and are then trimmed for
negative space.

Use this function to provide a list of candidates for
completion (per `completing-read')."
  (split-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n" :omit-nulls "[\s\f\t\n\r\v]+"))

(defun gw-common-ignore (&rest _)
  "Use this as override advice to make a function do nothing."
  nil)

;; NOTE 2023-06-02: The `gw-common-wcag-formula' and
;; `gw-common-contrast' are taken verbatim from my `modus-themes'
;; and renamed to have the prefix `gw-common-' instead of
;; `modus-themes-'.  This is all my code, of course, but I do it this
;; way to ensure that this file is self-contained in case someone
;; copies it.

;; This is the WCAG formula: <https://www.w3.org/TR/WCAG20-TECHS/G18.html>.
(defun gw-common-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such #123456."
  (cl-loop for k in '(0.2126 0.7152 0.0722)
           for x in (color-name-to-rgb hex)
           sum (* k (if (<= x 0.03928)
                        (/ x 12.92)
                      (expt (/ (+ x 0.055) 1.055) 2.4)))))

;;;###autoload
(defun gw-common-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (gw-common-wcag-formula c1) 0.05)
               (+ (gw-common-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

;;;; EXPERIMENTAL macros (not meant to be used anywhere)

;; TODO 2023-09-30: Try the same with `cl-defmacro' and &key
(defmacro gw-common-if (condition &rest consequences)
  "Separate the CONSEQUENCES of CONDITION semantically.
Like `if', `when', `unless' but done by using `:then' and `:else'
keywords.  The forms under each keyword of `:then' and `:else'
belong to the given subset of CONSEQUENCES.

- The absence of `:else' means: (if CONDITION (progn CONSEQUENCES)).
- The absence of `:then' means: (if CONDITION nil CONSEQUENCES).
- Otherwise: (if CONDITION (progn then-CONSEQUENCES) else-CONSEQUENCES)."
  (declare (indent 1))
  (let (then-consequences else-consequences last-kw)
    (dolist (elt consequences)
      (let ((is-keyword (keywordp elt)))
        (cond
         ((and (not is-keyword) (eq last-kw :then))
          (push elt then-consequences))
         ((and (not is-keyword) (eq last-kw :else))
          (push elt else-consequences))
         ((and is-keyword (eq elt :then))
          (setq last-kw :then))
         ((and is-keyword (eq elt :else))
          (setq last-kw :else)))))
    `(if ,condition
         ,(if then-consequences
              `(progn ,@(nreverse then-consequences))
            nil)
       ,@(nreverse else-consequences))))

;;;###autoload
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If BUFFER-OR-NAME is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

(provide 'gw-common)
;;; gw-common.el ends here
