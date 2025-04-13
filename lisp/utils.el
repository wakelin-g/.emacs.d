(use-package cl-lib
  :straight (:type built-in)
  :init (defun first (elt) (car elt))
  :commands (first))

(use-package counsel
  :straight t
  :commands (counsel-git-grep counsel-switch-buffer))

(use-package swiper
  :straight t
  :commands (swiper)
  :config
  (setq swiper-goto-start-of-match t))

(use-package find-file-in-project
  :straight t
  :commands (find-file-in-project))

;; only uncomment when needed
;; (use-package highlight-numbers
;;   :straight t
;;   :hook ((prog-mode) . highlight-numbers-mode))

(use-package reveal-in-osx-finder
  :straight t)

;; fix path bug with gui emacs
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;; deadgrep
(use-package deadgrep
  :straight t
  :ensure t)

;; save last position in file
(use-package saveplace
  :straight nil
  :ensure nil)
(with-eval-after-load 'saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))


;; csv mode
(use-package csv-mode
  :straight t)
(defun gw/csv-highlight (&optional separator)
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (pos-bol) (pos-eol)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex
                                         (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2
             for c in colors
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
(add-hook 'csv-mode-hook 'gw/csv-highlight)

;; interactive/previewing regexp replacement
(use-package visual-replace
  :straight t
  :defer t
  :bind (:map isearch-mode-map
              ("C-c r" . visual-replace-from-isearch)))

(use-package discover-my-major
  :straight t
  :bind ("C-h C-m" . discover-my-major))

(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
