;;; use-terminal.el --- Emacs as a terminal          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :defines vterm-mode-map
  :bind
  (:map vterm-mode-map
   ([remap kill-this-buffer] . widowmaker-kill-buffer-with-process))
  :custom
  (vterm-keymap-exceptions
   '("C-c" "C-g" "C-h" "C-l" "C-u" "C-x" "C-y"
     "M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
     "M-o" "M-x" "M-y")))

;;; use-terminal.el ends here
