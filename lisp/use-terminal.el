;;; use-terminal.el --- Emacs as a terminal          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO Advice vterm motions to support shift

(use-package vterm
  :defines vterm-mode-map
  :bind
  (:map vterm-mode-map
   ([remap kill-this-buffer] . widowmaker-kill-buffer-with-process)))

;;; use-terminal.el ends here
