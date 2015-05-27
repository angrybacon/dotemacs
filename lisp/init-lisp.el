;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Lisp buffers
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package lisp-mode
  :delight lisp-interaction-mode "lisp")


;; Built-in
;; TOFIX: Could not load emacs-lisp-mode
(use-package emacs-lisp-mode
  :delight emacs-lisp-mode "elisp")


;;─────────────────────────────────────────────────────────────────────────────
;; End init-lisp.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-lisp)
