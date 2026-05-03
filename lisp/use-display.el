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
  ("s-\"" . popper-toggle)
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
     ,(rx bos "*HTTP Response")
     ,(rx bos "*Messages*" eos)
     ,(rx bos "*Process List*" eos)
     ,(rx bos "*eldoc")
     ,(rx bos "*eshell")
     ,(rx bos "*terminal")
     agent-shell-mode
     eshell-mode
     flymake-diagnostics-buffer-mode
     help-mode
     helpful-mode
     magit-process-mode
     shell-mode
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
   `(("*Async-native-compile-log*"    ,@me/shackle-below)
     ("*Disabled Command*"            ,@me/shackle-below)
     ("*Messages*"                    ,@me/shackle-below)
     ("*Process List*"                ,@me/shackle-below :select t)
     ("*Python*"                      ,@me/shackle-below)
     ("*Shell Command Output*"        ,@me/shackle-below)
     ("*Warnings*"                    ,@me/shackle-below)
     ("*eldoc*"                       ,@me/shackle-below)
     ("*eshell*"                      ,@me/shackle-below)
     ("COMMIT_EDITMSG"                ,@me/shackle-below)
     (,(rx bos "*Customize Group:")   ,@me/shackle-left :regexp t)
     (,(rx bos "*EGLOT")              ,@me/shackle-below :regexp t :select t)
     (,(rx bos "*HTTP Response")      ,@me/shackle-below :regexp t)
     (,(rx bos "*terminal:")          ,@me/shackle-below :regexp t :select t)
     (agent-shell-mode                ,@me/shackle-left :select t)
     (compilation-mode                ,@me/shackle-below :select t)
     (debugger-mode                   ,@me/shackle-below)
     (embark-collect-mode             ,@me/shackle-below)
     (flymake-diagnostics-buffer-mode ,@me/shackle-below)
     (grep-mode                       ,@me/shackle-below)
     (help-mode                       ,@me/shackle-left)
     (helpful-mode                    ,@me/shackle-left)
     (magit-process-mode              ,@me/shackle-below)))
  (shackle-select-reused-windows t)
  :hook
  (after-init . shackle-mode)
  :preface
  (defvar me/shackle-below '(:align below :popup t))
  (defvar me/shackle-left '(:align left :popup t :size 83)))

(use-package widowmaker
  :load-path "lisp/widowmaker"
  :bind
  ("s-'" . widowmaker-terminal-dwim)
  ("s-h" . windmove-left)
  ("s-j" . windmove-down)
  ("s-k" . windmove-up)
  ("s-l" . windmove-right)
  ("s-w" . delete-window)
  ("s-W" . kill-current-buffer)
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
  widowmaker-terminal-window
  :hook
  (after-init . winner-mode)
  (window-configuration-change . widowmaker-olivetti-maybe)
  :init
  (advice-add 'shackle--display-buffer-aligned-window :after
    #'widowmaker-shackle-set-window-side))

;;; use-display.el ends here
