;;; -*- lexical-binding: t -*-

;; (use-package nerd-icons
;;   :straight t
;;   :custom
;;   (nerd-icons-font-family "IosevkaTerm Nerd Font"))

(use-package all-the-icons
  :straight t)
(when (display-graphic-p)
  (require 'all-the-icons))

;; colorizer
(use-package colorful-mode
  :straight t
  :hook '((prog-mode . colorful-mode)
          (text-mode . colorful-mode)))

;; stolen from https://gist.github.com/DivineDominion/e15c152f2fad785f4e1167b9a4df548b
(use-package emacs
  :straight '(org :type built-in)
  :config
  (defface gw/tab-bar-numbers
    '((t
       :inherit tab-bar
       :family "Iosevka"
       :weight light))
    "Face for tab numbers in both active and inactive tabs.")
  (defvar gw/circle-numbers-alist
    '((0 . "⓪")
      (1 . "➀")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")
  (defun gw/tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (concat
       (propertize
        (when (and tab-bar-tab-hints (< i 10)) (alist-get i gw/circle-numbers-alist))
        'face 'gw/tab-bar-numbers)
       " "
       (propertize
        (concat (alist-get 'name tab)
                (or (and tab-bar-close-button-show
                         (not (eq tab-bar-close-button-show
                                  (if current-p 'non-selected 'selected)))
                         tab-bar-close-button)
                    ""))
        'face (funcall tab-bar-tab-face-function tab))
       " ")))
  (setq tab-bar-tab-name-format-function #'gw/tab-bar-tab-name-format-default
        tab-bar-tab-hints t)
  (setq tab-bar-close-button-show nil
        tab-bar-close-button " \x00d7 ")
  (setq tab-bar-new-button-show nil
        tab-bar-new-button " + ")
  (setq tab-bar-separator nil)
  (setq tab-bar-format
        '(tab-bar-format-tabs-groups
          tab-bar-separator
          tab-bar-format-align-right
          tab-bar-format-global))
  (display-battery-mode -1)
  (setq display-time-format "%Y-%m-%d %H:%M")
  (setq display-time-default-load-average nil)
  (display-time-mode +1)
  (mapcar (lambda (tab-number)
            (let ((funname (intern (format "gw/tab-bar-select-%d" tab-number)))
                  (docstring (format "Select tab %d by its absolute number." tab-number))
                  (key (kbd (format "%d" tab-number)))
                  (super-key (kbd (format "s-%d" tab-number))))
              (eval-expression `(defun ,funname ()
                                  ,docstring
                                  (interactive)
                                  (tab-bar-select-tab ,tab-number)))
              (eval-expression `(define-key tab-prefix-map ,key ',funname))
              (eval-expression `(global-set-key ,super-key ',funname))))
          (number-sequence 1 9))
  :hook
  (tab-bar-mode . tab-bar-history-mode)
  (after-init . tab-bar-mode)
  :bind
  ("s-t" . tab-bar-new-tab)
  ("s-w" . tab-bar-close-tab)
  ("s-T" . tab-bar-undo-close-tab)
  (:map tab-prefix-map
        ("<left>" . tab-bar-switch-to-prev-tab)
        ("<right>" . tab-bar-switch-to-next-tab)
        ("n" . tab-bar-new-tab)))
