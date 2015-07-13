;;─────────────────────────────────────────────────────────────────────────────
;; Show parent namespace at the top of the active buffer
;;─────────────────────────────────────────────────────────────────────────────


(use-package semantic
  :init
  (setq
   semanticdb-default-save-directory "~/.emacs.d/semanticdb/"
   semantic-idle-breadcrumbs-separator " > ")
  :config
  ;; FIXME: This is not restricted to Python buffers
  (add-hook 'python-mode-hook
            (lambda ()
              ;; TODO: Find a mode that display a namespace of the top line, instead of item at cursor position
              (semantic-mode t)
              (semantic-idle-breadcrumbs-mode t))))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-semantic.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-semantic)
