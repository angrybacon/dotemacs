(require 'use-package)


;; (defvar semantic-idle-breadcrumbs-separator)
;; (defvar semanticdb-default-save-directory)


;; Built-in
;; (use-package semantic
;;   :init
;;   (setq
;;    semantic-idle-breadcrumbs-separator " > "
;;    semanticdb-default-save-directory "~/.emacs.d/semanticdb/")
;;   :config
;;   ;; FIXME: This is not restricted to Python buffers
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               ;; TODO: Find a mode that display a namespace of the top line, instead of item at cursor position
;;               (semantic-mode t)
;;               (semantic-idle-breadcrumbs-mode t))))


(provide 'init-semantic)
