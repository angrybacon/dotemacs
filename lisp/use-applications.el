;;; use-applications.el --- Emacs as X               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; REST Client

(use-package restclient
  :hook
  (restclient-mode . display-line-numbers-mode)
  :mode ((rx ".http" eos) . restclient-mode))

;;;; Terminal

(use-package vterm
  :defines vterm-mode-map
  :bind
  (:map vterm-mode-map
   ([remap kill-current-buffer] . widowmaker-kill-buffer-with-process))
  :config
  (define-advice set-window-vscroll
      (:after (&rest _) toggle-scroll)
    (when (eq major-mode 'vterm-mode)
      (if (> (window-end) (buffer-size))
          (when vterm-copy-mode (vterm-copy-mode-done nil))
        (vterm-copy-mode 1))))
  :custom
  (vterm-keymap-exceptions
   '("C-c" "C-g" "C-h" "C-l" "C-u" "C-x" "C-y"
     "M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
     "M-:" "M-o" "M-x" "M-y")))

;;; use-applications.el ends here
