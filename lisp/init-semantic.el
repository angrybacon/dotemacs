;;─────────────────────────────────────────────────────────────────────────────
;; Show parent namespace at the top of the active buffer
;;─────────────────────────────────────────────────────────────────────────────


(use-package semantic
  :init
  (setq
   semanticdb-default-save-directory "~/.emacs.d/semanticdb/"
   semantic-idle-breadcrumbs-separator " > ")
  :config
  (semantic-mode t)
  (global-semantic-idle-breadcrumbs-mode t))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-semantic.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-semantic)
