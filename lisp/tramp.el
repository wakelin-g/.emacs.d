(use-package tramp
  :straight (tramp :type git :host github :repo "emacs-straight/tramp" :files ("*" (:exclude ".git")))
  :config
  (setq tramp-verbose 4
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=10"
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)
        tramp-copy-size-limit nil
        tramp-default-method "scpx")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(defun tramp-abort ()
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))
