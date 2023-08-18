;;; use-display.el --- Configure window geometry     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Olivetti

(use-package olivetti
  :bind
  ("<left-margin> <mouse-1>" . ignore)
  ("<right-margin> <mouse-1>" . ignore))

;;;; Pop-Ups

(use-package popper
  :defines popper-mode-map
  :functions popper-group-by-project
  :bind
  ("s-\"" . popper-toggle-latest)
  ("s-<tab>" . popper-cycle)
  (:map popper-mode-map
   ("C-s-<tab>" . popper-toggle-type))
  :config
  (with-eval-after-load 'project
    (setq-default popper-group-function #'popper-group-by-project))
  :custom
  (popper-display-control nil)
  (popper-echo-dispatch-keys nil)
  (popper-echo-lines 1)
  (popper-mode-line nil)
  (popper-reference-buffers
   `(,(rx bos "*EGLOT")
     ,(rx bos "*Messages*" eos)
     ,(rx bos "*Process List*" eos)
     ,(rx bos "*eldoc")
     ,(rx bos "*eshell")
     ,(rx bos "*terminal")
     eshell-mode
     flymake-diagnostics-buffer-mode
     help-mode
     helpful-mode
     magit-process-mode
     vterm-mode))
  :hook
  (after-init . popper-mode)
  (after-init . popper-echo-mode))

;;;; Window Management

(use-package shackle
  :custom
  (shackle-default-size (szadek-get 'popup-size .33))
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules
   `((compilation-mode                :align below :popup t)
     (flymake-diagnostics-buffer-mode :align below :popup t)
     (magit-process-mode              :align below :popup t)
     ("*Messages*"                    :align below :popup t)
     (,(rx bos "*EGLOT")              :align below :popup t :regexp t)
     (debugger-mode                   :align below :popup t :select t)
     (embark-collect-mode             :align below :popup t :select t)
     (grep-mode                       :align below :popup t :select t)
     ("*Process List*"                :align below :popup t :select t)
     ("*Warnings*"                    :align below :popup t :select t)
     ("*dired-check-process output*"  :align below :popup t :select t)
     ("*eldoc*"                       :align below :popup t :select t)
     ("*eshell*"                      :align below :popup t :select t)
     (,(rx bos "*terminal")           :align below :popup t :select t :regexp t)
     (help-mode                       :align left  :popup t :select t :size 82)
     (helpful-mode                    :align left  :popup t :select t :size 82)))
  (shackle-select-reused-windows t)
  :hook
  (after-init . shackle-mode))

(use-package widowmaker
  :load-path "lisp/widowmaker"
  :bind
  ("s-'" . widowmaker-terminal-dwim)
  ("s-h" . windmove-left)
  ("s-j" . windmove-down)
  ("s-k" . windmove-up)
  ("s-l" . windmove-right)
  ("s-w" . delete-window)
  ("s-W" . kill-this-buffer)
  :commands
  widowmaker-kill-buffer-with-process
  widowmaker-olivetti-automatic-toggle
  widowmaker-olivetti-body-less
  widowmaker-olivetti-body-more
  widowmaker-olivetti-body-reset
  widowmaker-placement-center
  widowmaker-placement-cycle
  widowmaker-shackle-set-window-side
  widowmaker-terminal-dwim
  :hook
  (after-init . winner-mode)
  (window-configuration-change . widowmaker-olivetti-maybe)
  :init
  (advice-add 'shackle--display-buffer-aligned-window :after
    #'widowmaker-shackle-set-window-side))

;;; use-display.el ends here
