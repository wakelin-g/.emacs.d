;;; -*- lexical-binding: t -*-

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "" 'face `(:foreground )))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize (user-login-name) 'face `(:foreground (modus-themes-get-color-value 'red)))
         (propertize "@" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize (system-name) 'face `(:foreground "#268bd2"))
         (propertize "]──[" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground (modus-themes-get-color-value 'yellow)))
         (propertize "]──[" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize (concat (eshell/pwd)) 'face `(:foreground (modus-themes-get-color-value 'bg-region)))
         (propertize "]\n" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize "└─>" 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground (modus-themes-get-color-value 'cyan)))
         )))
