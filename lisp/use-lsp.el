;;; use-lsp.el --- Language Server Protocol settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Code References

(use-package xref
  :ensure nil
  :bind
  ([remap xref-find-apropos] . xref-find-definitions)
  ([remap xref-find-definitions] . xref-find-definitions-other-window))

;;;; LSP Client

(declare-function eglot-current-server "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function me/project-save "use-project")

(defun me/eglot-shutdown-project ()
  "Kill the LSP server for the current project if it exists."
  (when-let ((server (eglot-current-server)))
    (ignore-errors (eglot-shutdown server))))

(defvar me/eglot-inlay-hints-automatic nil
  "Whether `eglot-inlay-hints-mode' should be enabled by default.")

(defun me/eglot-inlay-hints-maybe ()
  "Maybe enable `eglot-inlay-hints-mode'.
See `me/eglot-inlay-hints-automatic'."
  (if me/eglot-inlay-hints-automatic
      (eglot-inlay-hints-mode 1)
    (eglot-inlay-hints-mode -1)))

(defun me/eglot-inlay-hints-toggle ()
  "Toggle `me/eglot-inlay-hints-automatic'.
Also toggle `eglot-inlay-hints-mode' accordingly."
  (interactive)
  (let ((value me/eglot-inlay-hints-automatic))
    (eglot-inlay-hints-mode (if value -1 1))
    (setq-default me/eglot-inlay-hints-automatic (not value))))

(defun me/eglot-initialize-typescript ()
  "Bootstrap TypeScript-specific customizations.
See https://github.com/typescript-language-server/typescript-language-server."
  (defvar eglot-server-programs)
  (let ((target-mode 'typescript-ts-mode)
        (target-modes '(js-base-mode typescript-ts-base-mode)))
    (setq-default
     eglot-server-programs
     (cl-substitute-if
      (cons
       target-modes
       '("typescript-language-server" "--stdio" :initializationOptions
         (:preferences
          (:importModuleSpecifierPreference "non-relative"
           :includeInlayEnumMemberValueHints t
           :includeInlayFunctionLikeReturnTypeHints t
           :includeInlayFunctionParameterTypeHints t
           :includeInlayParameterNameHints "literals" ; "none" | "literals" | "all"
           :includeInlayParameterNameHintsWhenArgumentMatchesName t
           :includeInlayPropertyDeclarationTypeHints t
           :includeInlayVariableTypeHints t
           :includeInlayVariableTypeHintsWhenTypeMatchesName t
           :organizeImportsCaseFirst "upper"
           :organizeImportsCollation "unicode" ; "ordinal" | "unicode"
           :organizeImportsIgnoreCase :json-false
           :quotePreference "single"))))
      (lambda (program)
        (if (listp (car program))
            (member target-mode (car program))
          (eq target-mode program)))
      eglot-server-programs))))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (advice-add 'eglot--format-markup :filter-return
    (lambda (output) (replace-regexp-in-string (rx "\n```" eos) "" output))
    '((name . me/eglot--trim-block-end)))
  (advice-add 'project-kill-buffers :before #'me/eglot-shutdown-project)
  (me/eglot-initialize-typescript)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :hook
  (eglot-managed-mode . me/eglot-inlay-hints-maybe)
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (typescript-ts-base-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp))

;;; use-lsp.el ends here
