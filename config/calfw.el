;;; -*- lexical-binding: t -*-

(use-package calfw
  :straight (:files (:defaults "*.el")
                    :fork (:host github :repo "haji-ali/emacs-calfw"))
  :bind (("C-c A" . my-calendar)
         :map cfw:calendar-mode-map)
  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
              cfw:refresh-calendar-buffer
              cfw:org-create-source
              cfw:cal-create-source)
  :custom
  (cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
  :preface
  (defun my-calendar ()
    (interactive)
    (let ((buf (get-buffer "*cfw-calendar*")))
      (if buf
          (pop-to-buffer buf nil)
        (cfw:open-calendar-buffer
         :contents-sources
         (list (cfw:org-create-source "Dark Blue")
               (cfw:cal-create-source "Dark Orange"))
         :view 'two-weeks))))
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  (custom-set-faces
   ;; '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 1.0 :inherit variable-pitch))))
   '(cfw:face-title ((t (:foreground "#2a2a2a" :weight bold :height 1.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#3f3000" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :weight bold)))
   '(cfw:face-holiday ((t :background "#1a1a1a" :foreground "#8c5353" :weight bold)))
   '(cfw:face-grid ((t :foreground "#a9a9a9")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "#00ffff")))
   '(cfw:face-day-title ((t :background "#1a1a1a")))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "#bc8f8f" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "#a9a9a9" :inherit cfw:face-day-title)))
   '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
   '(cfw:face-today ((t :background: "#1a1a1a" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f")))
   '(cfw:face-toolbar ((t :foreground "#36648b")))
   '(cfw:face-toolbar-button-off ((t :foreground "#1a1a1a" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "#7f7f7f" :weight bold))))
  )

(use-package calfw-blocks
  :straight (:host github
                   :repo "ml729/calfw-blocks"
                   :fork (:host github :repo "haji-ali/calfw-blocks")))

(setq calfw-blocks-earliest-visible-time '(6 0)
      calfw-blocks-lines-per-hour 2
      calfw-blocks-show-time-grid t
      calfw-blocks-show-current-time-indicator t
      calfw-blocks-default-event-length 1
      calfw-blocks-min-block-width 5
      cfw:highlight-today t)

(use-package calfw-cal :straight t)
(use-package calfw-org
  :straight t
  :config
  (setq cfw:org-agenda-schedule-args '(:deadline :timestamp :sexp)))

(use-package maccalfw
  :commands maccalfw-open
  :straight (:host github
                   :repo "haji-ali/maccalfw"
                   :branch "ical"
                   :post-build (with-demoted-errors "Error post-building maccalfw: %S"
                                 (require 'maccalfw)
                                 (maccalfw--load-module))
                   :protocol ssh
                   :files ("*.el"
                           ("src" . "src"))))

(add-hook 'cfw:calendar-mode-hook 'display-line-numbers-mode)

(defun gw/side-by-side-agenda-view ()
  (progn
    (org-agenda nil "a")
    (split-window-right)
    (org-agenda-redo)
    (split-window-below)
    (other-window 1)
    (cfw:open-org-calendar)
    (setq org-agenda-sticky t)
    (other-window 1)
    (org-agenda nil "p")
    (setq org-agenda-sticky nil)))
(defun gw/show-my-agenda ()
  (interactive)
  (let ((tab-bar-index (tab-bar--tab-index-by-name "Agenda")))
    (if tab-bar-index
        (tab-bar-select-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Agenda")
        (gw/side-by-side-agenda-view)
        (message "Agenda loaded")))))
