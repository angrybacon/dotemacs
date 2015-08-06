;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-semantic.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)
(defvar semantic-idle-breadcrumbs-separator)
(defvar semanticdb-default-save-directory)


;;─────────────────────────────────────────────────────────────────────────────
;; Show parent namespace at the top of the active buffer
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package semantic
  :init
  (setq
   semantic-idle-breadcrumbs-separator " > "
   semanticdb-default-save-directory "~/.emacs.d/semanticdb/")
  :config
  ;; FIXME: This is not restricted to Python buffers
  (add-hook 'python-mode-hook
            (lambda ()
              ;; TODO: Find a mode that display a namespace of the top line, instead of item at cursor position
              (semantic-mode t)
              (semantic-idle-breadcrumbs-mode t))))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-semantic.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-semantic)
