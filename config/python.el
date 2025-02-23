;;; -*- lexical-binding: t -*-

(use-package uv-mode
  :straight t
  :hook (python-mode . uv-mode-auto-activate-hook))
