(require 'server) ;; this is probably not needed; already in init.el
(unless (server-running-p)
  (server-start))

(use-package noflet ;; this package is used for local variable binding (I think)
  :straight (noflet :type git :host github :repo "elp-revive/noflet"))
(require 'noflet)
(require 'cl-lib)

(use-package org-mac-link ;; this package grabs stuff from osx applications
  :straight (org-mac-link :type git :host gitlab :repo "aimebertrand/org-mac-link")
  :ensure t)

;; for general capture frame spawning (C-CMD-c)
(defun timu-func-make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (top . 100)
                (left . 500)
                (height . 25)
                (width . 80)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

;; destroy spawned frame on capture-finalize (C-c C-c)
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; destroy spawned frame on capture-destroy (C-c C-k)
(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;;; ------ SAFARI INTERACTIONS --------
(defun timu-func-url-safari-capture-to-org ()
  "Gets URL from frontmost Safari window."
  (interactive)
  (org-capture-string (org-mac-link-safari-get-frontmost-url) "u")
  (ignore-errors)
  (org-capture-finalize))

(defun timu-func-cmd-with-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun timu-func-convert-applescript-to-html (contents)
  "Return the Applescript's clipboard CONTENTS into a packed array.
Convert and return this encoding into a UTF-8 string."
  (cl-flet ((hex-pack-bytes (tuple)
              (string-to-number (apply 'string tuple) 16)))
    (let* ((data (-> contents (substring 10 -2) (string-to-list)))
           (byte-seq (->> data (-partition 2) (mapcar #'hex-pack-bytes))))
      (decode-coding-string
       (mapconcat #'byte-to-string byte-seq "") 'utf-8))))

(defun timu-func-get-mac-clipboard ()
  "Return a list where the first entry is either :html or :text.
The second is the clipboard contents."
  (cl-destructuring-bind (exit-code contents)
      (timu-func-cmd-with-exit-code
       "/usr/bin/osascript" "-e" "the clipboard as \"HTML\"")
    (if (= 0 exit-code)
        (list :html (timu-func-convert-applescript-to-html contents))
      (list :text (shell-command-to-string
                   "/usr/bin/osascript -e 'the clipboard'")))))
(defun timu-func-org-clipboard ()
  "Return the contents of the clipboard in `org-mode' format."
  (cl-destructuring-bind (type contents) (timu-func-get-mac-clipboard)
    (with-temp-buffer
      (insert contents)
      (if (eq :html type)
          (shell-command-on-region
           (point-min) (point-max)
           (concat (executable-find "pandoc") " -f html -t org --wrap=none") t t)
        (shell-command-on-region
         (point-min) (point-max)
         (concat (executable-find "pandoc") " -f markdown -t org --wrap=none") t t))
      (buffer-substring-no-properties (point-min) (point-max)))))
(defun timu-func-org-yank-clipboard ()
  "Yank the contents of the Mac clipboard in an `org-mode' compatible format."
  (interactive)
  (insert (timu-func-org-clipboard)))
(defun timu-func-safari-capture-to-org ()
  "Call `org-capture-string' on the contents of the Apple clipboard.
Use `org-mac-link-safari-get-frontmost-url' to capture content from Safari.
Triggered by a custom macOS Quick Action Automator workflow."
  (interactive)
  (org-capture-string (timu-func-org-clipboard) "w")
  (ignore-errors)
  (insert (org-mac-link-safari-get-frontmost-url))
  (org-capture-finalize))
