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
   `(,(rx bos "*HTTP Response")
     ,(rx bos "*eldoc*" eos)
     agent-shell-mode
     eshell-mode
     flymake-diagnostics-buffer-mode
     help-mode
     helpful-mode
     messages-buffer-mode
     shell-mode
     vterm-mode))
  :hook
  (after-init . popper-mode)
  (after-init . popper-echo-mode))

;;;; Window Management

(use-package entrave
  :load-path "lisp/entrave"
  :custom
  (entrave-rules
   `((,(rx bos "*Disabled Command*" eos)  :bottom)
     (,(rx bos "*EGLOT")                  :bottom :select)
     (,(rx bos "*HTTP Response")          :bottom)
     (,(rx bos "*Warnings*" eos)          :bottom :select)
     (,(rx bos "*eldoc*" eos)             :bottom)
     (,(rx bos "*eshell")                 :bottom :select)
     (,(rx bos "*shell")                  :bottom :select)
     (,(rx bos "*terminal")               :bottom :select)
     (Custom-mode                         :left)
     (agent-shell-mode                    :left :select)
     (compilation-mode                    :bottom :select)
     (flymake-diagnostics-buffer-mode     :bottom)
     (help-mode                           :left)
     (helpful-mode                        :left)
     (inferior-python-mode                :bottom)
     (magit-process-mode                  :bottom)
     (messages-buffer-mode                :bottom :select)
     (occur-mode                          :bottom :select)
     (process-menu-mode                   :bottom :select)
     (vc-annotate-mode                    :bottom)
     (xref--xref-buffer-mode              :bottom)))
  :hook
  (after-init . entrave-mode))

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
