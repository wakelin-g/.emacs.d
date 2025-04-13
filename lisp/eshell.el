;;; -*- lexical-binding: t -*-

(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))

;; ---- would love to get all this working but seems cl-lib functions not loaded? ----
;; (defun eshell-fn-on-files (f1 f2 args)
;;   "Call F1 on first element in list, ARGS.
;; Call F2 on rest of the elements in ARGS."
;;   (unless (null args)
;;     (let ((filenames (flatten-list args)))
;;       (funcall f1 (car filenames))
;;       (when (cdr filenames)
;;         (mapcar f2 (cdr filenames))))
;;     ;; Return empty string, as results:
;;     ""))
;; (defun eshell/less (&rest files)
;;   "Expanded alias to `view-file'."
;;   (eshell-fn-on-files 'view-file
;;                       'view-file-other-window files))
;; (defun eshell/do (&rest args)
;;   "Execute commands over lst."
;;   (seq-let (cmd lst) (-split-on "::" args)
;;     (dolist (file
;;              (flatten-list (append lst)))
;;       (add-to-list 'cmd file)
;;       (eshell-named-command
;;        (car cmd) (cdr cmd)))))

;; (defvar ha-eshell-ebbflow-buffername "*eshell-edit*"
;;   "The name of the buffer that eshell can use to store temporary input/output.")

;; (defun ha-eshell-ebbflow-return ()
;;   "Close the ebb-flow window and return to Eshell session."
;;   (interactive)
;;   (if (and (boundp 'ha-eshell-ebbflow-return-buffer)
;;            (bufferp 'ha-eshell-ebbflow-return-buffer))
;;       (pop-to-buffer ha-eshell-ebbflow-return-buffer)
;;     (bury-buffer)))
;; (define-minor-mode ebbflow-mode
;;   "Editing a flow from the Eshell ebb command, so flow can pull it back."
;;   :lighter " ebb"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c C-q") 'ha-eshell-ebbflow-return)
;;             map))
;; (when (fboundp 'evil-define-key)
;;   (evil-define-key 'normal ebbflow-mode-map "Q" 'ha-eshell-ebbflow-return))

;; (defun eshell/flow (&rest args)
;;   "Output the contents of one or more buffers as a string.
;; Usage: flow [OPTION] [BUFFER ...]
;;     -h, --help         show this usage screen
;;     -l, --lines        output contents as a list of lines
;;     -w, --words        output contents as a list of space-separated elements "
;;   (let* ((options (eshell-getopts '((:name words :short "w" :long "words")
;;                                     (:name lines :short "l" :long "lines")
;;                                     (:name string :short "s" :long "string")
;;                                     (:name help :short "h" :long "help"
;;                                            :help eshell/flow))
;;                                   args))
;;          (buffers (gethash 'parameters options))
;;          (content (thread-last buffers
;;                                (-map 'eshell-flow-buffer-contents)
;;                                (s-join "\n"))))
;;     (if (gethash 'help options)
;;         (error (documentation 'eshell/flow))
;;       (unless buffers
;;         (setq content
;;               (eshell-flow-buffer-contents ha-eshell-ebbflow-buffername)))
;;       (cond
;;        ((gethash 'words options) (split-string content))
;;        ((gethash 'lines options) (split-string content "\n"))
;;        (t content)))))

;; (defun eshell-flow-buffer-contents (buffer-name)
;;   (when buffer-name
;;     (save-window-excursion
;;       (switch-to-buffer (get-buffer buffer-name))
;;       (buffer-substring-no-properties (point-min) (point-max)))))
;; (defun eshell-flow-buffers (buffers)
;;   (if buffers
;;       (--map (cond
;;               ((bufferp it) it)
;;               ((stringp it) (get-buffer it))
;;               (t (error (format "Illegal argument of type %s: %s\n%s"
;;                                 (type-of arg) it
;;                                 (documentation 'eshell/flow)))))
;;              buffers)
;;     (list (get-buffer ha-eshell-ebbflow-buffername))))

;; (defun eshell/ebb (&rest args)
;;   "Insert text content into *eshell-edit* buffer, or if not text is given, the output of last command.
;; Usage: ebb [OPTION] [text content]
;;     -h, --help    show this usage screen
;;     -m, --mode    specify the major-mode for the *eshell-edit* buffer, e.g. json
;;     -n, --newline separate the text contents by newlines (this is default)
;;     -s, --spaces  separate the text contents by spaces, instead of newlines
;;     -b, --begin   add text content to the beginning of the *eshell-edit* buffer
;;     -e, --end     add text content to the end of *eshell-edit* buffer
;;     -i, --insert  add text content to *eshell-edit* at point"
;;   (let* ((options  (eshell-getopts '((:name insert      :short "i" :long "insert")
;;                                      (:name append      :short "e" :long "end")
;;                                      (:name prepend     :short "b" :long "begin")
;;                                      (:name newline     :short "n" :long "newline")
;;                                      (:name spaces      :short "s" :long "spaces")
;;                                      (:name mode-option :short "m" :long "mode" :parameter string)
;;                                      (:name help        :short "h" :long "help"
;;                                             :help eshell/ebb))
;;                                    args))
;;          (location (cond
;;                     ((gethash 'insert  options) :insert)
;;                     ((gethash 'append  options) :append)
;;                     ((gethash 'prepend options) :prepend)
;;                     (t                          :replace)))
;;          (params   (gethash 'parameters options)))

;;     (if (seq-empty-p params)
;;         ((ha-eshell-ebb-output location))
;;       (ha-eshell-ebb-string location (gethash 'spaces options) params))

;;     ;; At this point, we are in the `ha-eshell-ebbflow-buffername', and
;;     ;; the buffer contains the inserted data. Did we specify a major-mode?
;;     (when-let ((mode-option (gethash 'mode-option options)))
;;       (if (s-starts-with? "js" mode-option)
;;           (js-json-mode)  ; Or should we just go to json-ts-mode?
;;         (funcall (intern (concat mode-option "-mode")))))

;;     ;; Flip on the minor mode-option so we can close the window later on:
;;     (ebbflow-mode +1)
;;     (goto-char (point-min)))

;;   nil) ; Return `nil' so that it doesn't print anything in `eshell'.

;; (defun ha-eshell-ebb-switch-to-buffer (insert-location)
;;   "Switch to `ha-eshell-ebbflow-buffername' and get the buffer ready for new data."
;;   (let ((return-buffer (current-buffer)))

;;     (if-let ((ebbwindow (get-buffer-window ha-eshell-ebbflow-buffername)))
;;         (select-window ebbwindow)
;;       (switch-to-buffer ha-eshell-ebbflow-buffername)
;;       (setq-local ha-eshell-ebbflow-close-window t))

;;     (setq-local ha-eshell-ebbflow-return-buffer return-buffer)
;;     (ebbflow-mode)

;;     (cl-case insert-location
;;       (:append  (goto-char (point-max)))
;;       (:prepend (goto-char (point-min)))
;;       (:insert   nil)
;;       (:replace (delete-region (point-min) (point-max))))))

;; (defun ha-eshell-ebb-string (insert-location space-separator-p command-results)
;;   "Insert the COMMAND-RESULTS into the `ha-eshell-ebbflow-buffername`.
;; Contents are placed based on INSERT-LOCATION and, if given, separated
;; by SEPARATOR (which defaults to a space)."
;;   (let* ((sep (if space-separator-p " " "\n"))
;;          (str (string-join (-flatten command-results) sep)))
;;     (ha-eshell-ebb-switch-to-buffer insert-location)
;;     (insert str)))

;; (defun ha-eshell-ebb-command (insert-location command-parts)
;;   "Call `eshell-command' with the COMMAND-PARTS.
;; Inserts the output into `ha-eshell-ebbflow-buffername'"
;;   (let ((command-string (string-join command-parts " ")))
;;     (ha-eshell-ebb-switch-to-buffer insert-location)
;;     (eshell-command command-string t)))

;; (defun ha-eshell-ebb-files (insert-location files)
;;   "Insert the FILES at the INSERT-LOCATION tin `ha-eshell-ebbflow-buffername'."
;;   (ha-eshell-ebb-switch-to-buffer insert-location)
;;   (dolist (file files)
;;     (insert-file file)
;;     (insert "\n")))

;; (defun ha-eshell-last-output ()
;;   "Return contents of the last command execusion in an Eshell buffer."
;;   (let ((start  (save-excursion
;;                    (goto-char eshell-last-output-start)
;;                    (re-search-backward eshell-prompt-regexp)
;;                    (next-line)
;;                    (line-beginning-position)))
;;         (end    eshell-last-output-start))
;;     (buffer-substring-no-properties start end)))

;; (defun ha-eshell-ebb-output (insert-location)
;;   "Grab output from previous eshell command, inserting it into our buffer.
;; Gives the INSERT-LOCATION to `ha-eshell-ebb-switch-to-buffer'."
;;   (let ((contents (ha-eshell-last-output)))
;;     (ha-eshell-ebb-switch-to-buffer insert-location)
;;     (insert contents)))

;; (defun eshell/x (&rest args)
;;   "Return a cell of information from the previous command in an Eshell buffer.
;; The first ARGS is the line number (one-based), and the second
;; ARGS, if given, is the column where the fields are separated by
;; whitespace.

;; This allows a sequence of commands like, where you don't have to
;; copy/paste the output (if it is simple), for instance:

;;     $ ls
;;     ...
;;     $ ls -l { x 2 3 }

;; If the initial argument is a string instead of a number, then it
;; returns the first word that starts with that it."
;;   (defun x-cells (table row col)
;;     (let* ((newlines (rx (one-or-more (any "\n" "\r"))))
;;            (fields   (rx (one-or-more (any "\t" " "))))
;;            (rows     (split-string table newlines t))
;;            (line     (nth row rows)))
;;       (if col
;;         (nth col (split-string line fields t))
;;       line)))

;;   (defun x-match (text starter)
;;     (let ((words (split-string text nil t)))
;;       (--first (s-starts-with? starter it) words)))

;;   (let* ((arg1     (first args))
;;          (arg2     (second args))
;;          (contents (ha-eshell-last-output)))
;;     (cond
;;      ((numberp arg1) (x-cells contents arg1 arg2))
;;      ((stringp arg1) (x-match contents arg1))
;;      (t              contents))))

;; (defvar ha-lsd (executable-find "lsd")
;;   "Location of the `lsd' program, if installed.")

;; (defun ha-eshell-ls-files (&optional directory)
;;   "Return a list of directories in DIRECTORY or `default-directory', if null."
;;   (let ((default-directory (or directory default-directory)))
;;     (if ha-lsd
;;         (shell-command-to-list (format "%s --icon always" ha-lsd))
;;       (directory-files default-directory nil
;;                        (rx string-start
;;                            (not (any "." "#"))
;;                            (one-or-more any)
;;                            (not "~")
;;                            string-end)))))
;; (defun ha-eshell-ls-filename (filename padded-fmt &optional directory)
;;   "Return a prettized version of FILE based on its attributes.
;; Formats the string with PADDED-FMT."
;;   (let ((file (expand-file-name (if (string-match (rx (group alpha (zero-or-more any))) filename)
;;                                     (match-string 1 filename)
;;                                   filename)
;;                                 directory))
;;         (import-rx  (rx "README"))
;;         (image-rx   (rx "." (or "png" "jpg" "jpeg" "tif" "wav") string-end))
;;         (code-rx    (rx "." (or "el" "py" "rb") string-end))
;;         (docs-rx    (rx "." (or "org" "md") string-end)))
;;     (format padded-fmt
;;             (cond
;;              ((file-directory-p file)
;;               (propertize filename 'face 'eshell-ls-directory))
;;              ((file-executable-p file)
;;               (propertize filename 'face 'eshell-ls-executable))
;;              ((string-match import-rx file)
;;               (propertize filename 'face '(:foreground "orange")))
;;              ((string-match image-rx file)
;;               (propertize filename 'face 'eshell-ls-special))
;;              ((file-symlink-p file)
;;               (propertize filename 'face 'eshell-ls-symlink))
;;              ((not (file-readable-p file))
;;               (propertize filename 'face 'eshell-ls-unreadable))
;;              (t
;;               filename)))))
;; (defun ha-eshell-ls (&optional directory)
;;   "Return a formatted string of files for a directory.
;; The string is a pretty version with columns and whatnot."
;;   (let* ((files   (ha-eshell-ls-files (or directory default-directory)))
;;          (longest (--reduce-from (max acc (length it)) 1 files))
;;          (width   (window-total-width))
;;          (columns (/ width (+ longest 3)))
;;          (padded  (if ha-lsd
;;                       (format "%%-%ds " longest)
;;                     (format "• %%-%ds " longest))))
;;     (cl-flet* ((process-lines (files)
;;                               (s-join "" (--map (ha-eshell-ls-filename it padded directory) files)))
;;                (process-files (table)
;;                               (s-join "\n" (--map (process-lines it) table))))

;;       (concat (process-files (seq-partition files columns)) "\n\n"))))
;; (defun ha-eshell-ls-directory (directory)
;;   "Print the DIRECTORY name and its contents."
;;   (let ((dir (file-truename directory)))
;;     (concat
;;      (propertize dir 'face '(:foreground "gold" :underline t))
;;      ":\n"
;;      (ha-eshell-ls dir))))
;; (defun eshell/lsd (&rest args)
;;   (let ((lsd (ha-find-executable "lsd")))
;;     (cond
;;      ;; I expect to call this function without any arguments most of the time:
;;      ((and lsd (null args))
;;       (ha-eshell-ls))
;;      ;; Called with other directories? Print them all, one at a time:
;;      ((and lsd (--none? (string-match (rx string-start "-") it) args))
;;       (mapconcat 'ha-eshell-ls-directory args ""))
;;      ;; Calling the function with -l or other arguments, don't bother. Call ls:
;;      (t (eshell/ls args)))))

;; (defun eshell-getopts (defargs args)
;;   "Return hash table of ARGS parsed against DEFARGS.
;; Where DEFARGS is an argument definition, a list of plists.
;; For instance:
;;    '((:name number :short \"n\"                 :parameter integer :default 0)
;;      (:name title  :short \"t\" :long \"title\" :parameter string)
;;      (:name debug  :short \"d\" :long \"debug\"))

;; If ARGS, a list of _command line parameters_ is something like:

;;     '(\"-d\" \"-n\" \"4\" \"--title\" \"How are that\" \"this\" \"is\" \"extra\")

;; The hashtable return would contain these entries:

;;     debug t
;;     number 4  ; as a number
;;     title \"How are that\" ; as a string
;;     parameters (\"this\" \"is\" \"extra\") ; as a list of strings "
;;   (let ((retmap    (make-hash-table))
;;         (short-arg (rx string-start "-" (group alnum)))
;;         (long-arg  (rx string-start "--" (group (1+ any)))))

;;     ;; Let's not pollute the Emacs name space with tiny functions, as
;;     ;; well as we want these functions to have access to the "somewhat
;;     ;; global variables", `retmap' and `defargs', we use the magical
;;     ;; `cl-labels' macro to define small functions:

;;     (cl-labels ((match-short (str defarg)
;;                   ;; Return t if STR matches against DEFARG's short label:
;;                   (and (string-match short-arg str)
;;                        (string= (match-string 1 str)
;;                                 (plist-get defarg :short))))

;;                 (match-long (str defarg)
;;                   ;; Return t if STR matches against DEFARG's long label:
;;                   (and (string-match long-arg str)
;;                        (string= (match-string 1 str)
;;                                 (plist-get defarg :long))))

;;                 (match-arg (str defarg)
;;                   ;; Return DEFARG if STR matches its definition (and it's a string):
;;                   (when (and (stringp str)
;;                              (or (match-short str defarg)
;;                                  (match-long str defarg)))
;;                     defarg))

;;                 (find-argdef (str)
;;                   ;; Return entry in DEFARGS that matches STR:
;;                   (first (--filter (match-arg str it) defargs)))

;;                 (process-args (arg parm rest)
;;                   (when arg
;;                     (let* ((defarg (find-argdef arg))
;;                            (key    (plist-get defarg :name)))
;;                       (cond
;;                        ;; If ARG doesn't match any definition, add
;;                        ;; everything else to PARAMETERS key:
;;                        ((null defarg)
;;                         (puthash 'parameters (cons arg rest) retmap))

;;                        ((plist-get defarg :help)
;;                         (error (documentation (plist-get defarg :help))))

;;                        ;; If argument definition has a integer parameter,
;;                        ;; convert next entry as a number and process rest:
;;                        ((eq (plist-get defarg :parameter) 'integer)
;;                         (puthash key (string-to-number parm) retmap)
;;                         (process-args (cadr rest) (caddr rest) (cddr rest)))

;;                        ;; If argument definition has a parameter, use
;;                        ;; the next entry as the value and process rest:
;;                        ((plist-get defarg :parameter)
;;                         (puthash key parm retmap)
;;                         (process-args (cadr rest) (caddr rest) (cddr rest)))

;;                        ;; No parameter? Store true for its key:
;;                        (t
;;                         (puthash key t retmap)
;;                         (process-args (first rest) (second rest) (cdr rest))))))))

;;       (process-args (first args) (second args) (cdr args))
;;       retmap)))
;; (defun eshell/set (&rest args)
;;   "Creates a buffer local variable.
;; The first parameters of ARGS is the name of the variable.
;; The other parameters are the values. If not given, the
;; variable is deleted."
;;   (let* ((var (car args))
;;          (var-sym (make-symbol var))
;;          ;; Convert value to a string
;;          (val (pcase (seq-length (cdr args))
;;                 (0 nil)
;;                 (1 (format "%s" (cadr args)))
;;                 (_ (thread-last (cdr args)
;;                                 (seq-map 'eshell-stringify)
;;                                 (s-join " "))))))
;;     (if val
;;         (progn
;;           (set (make-local-variable var-sym) val)
;;           (setenv var val))

;;       ;; If we don't get both a variable and a value, let's try to
;;       ;; delete the variable:
;;       (makunbound var-sym)
;;       (setenv var))))
;; (defun eshell/e (&rest files)
;;   "Essentially an alias to the `find-file' function."
;;   (eshell-fn-on-files 'find-file 'find-file-other-window files))

;; (defun eshell/ee (&rest files)
;;   "Edit one or more files in another window."
;;   (eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

;; (defalias 'eshell/emacs 'eshell/e)
;; (defalias 'eshell/vi    'eshell/e)
;; (defalias 'eshell/vim   'eshell/e)
;; (defalias 'eshell/nvim  'eshell/e)
;; (defalias 'eshell/more  'eshell/less)
;; (defalias 'eshell/view  'eshell/less)
