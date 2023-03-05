;;; use-lsp.el --- Language Server Protocol settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Code References

(use-package xref
  :ensure nil
  :bind
  ([remap xref-find-apropos] . xref-find-definitions)
  ([remap xref-find-definitions] . xref-find-definitions-other-window)
  :config
  (with-eval-after-load 'evil
    (evil-define-key* 'motion xref--xref-buffer-mode-map
      (kbd "<backtab>") #'xref-prev-group
      (kbd "<return>") #'xref-goto-xref
      (kbd "<tab>") #'xref-next-group)))

;;;; LSP Client

(defun me/eglot-shutdown-project ()
  "Kill the LSP server for the current project if it exists."
  (when-let ((server (eglot-current-server)))
    (ignore-errors (eglot-shutdown server))))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (advice-add 'project-kill-buffers :before #'me/eglot-shutdown-project)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :hook
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (typescript-ts-base-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp))

;;; use-lsp.el ends here
