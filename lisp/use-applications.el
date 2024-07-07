;;; use-applications.el --- Emacs as X               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; REST Client

(use-package restclient
  :hook
  (restclient-mode . display-line-numbers-mode)
  :mode ((rx ".http" eos) . restclient-mode))

;;;; Terminal

;; TODO Allow C-o for quick pastes

(use-package vterm
  :defines vterm-mode-map
  :bind
  (:map vterm-mode-map
   ([remap kill-current-buffer] . widowmaker-kill-buffer-with-process))
  :custom
  (vterm-keymap-exceptions
   '("C-c" "C-g" "C-h" "C-l" "C-u" "C-x" "C-y"
     "M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
     "M-:" "M-o" "M-x" "M-y")))

;;; use-applications.el ends here
