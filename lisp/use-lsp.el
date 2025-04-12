;;; use-lsp.el --- Language Server Protocol settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Code References

(declare-function xref--find-xrefs "xref")
(declare-function xref--read-identifier "xref")

;; NOTE Unused currently
(defun me/xref-find-references-other-window (identifier)
  "Find IDENTIFIER references in other window.
Like `xref-find-references' but switch to the other window."
  (interactive (list (xref--read-identifier "Find references of: ")))
  (xref--find-xrefs identifier 'references identifier 'window))

;;;; LSP Client

(declare-function cl-substitute-if "cl-seq")
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

(defun me/eglot-initialize-scss ()
  "Bootstrap SCSS-specific customizations."
  (defvar eglot-server-programs)
  (push '(scss-mode . ("vscode-css-language-server" "--stdio"))
        eglot-server-programs))

(defun me/eglot-initialize-typescript ()
  "Bootstrap TypeScript-specific customizations.
See https://github.com/typescript-language-server/typescript-language-server."
  (let ((target-modes '(tsx-ts-mode typescript-ts-mode))
        (preferences
         '(:importModuleSpecifierPreference "non-relative"
           :includeInlayEnumMemberValueHints t
           :includeInlayFunctionLikeReturnTypeHints t
           :includeInlayFunctionParameterTypeHints t
           :includeInlayParameterNameHints "literals" ; none | literals | all
           :includeInlayParameterNameHintsWhenArgumentMatchesName t
           :includeInlayPropertyDeclarationTypeHints t
           :includeInlayVariableTypeHints t
           :includeInlayVariableTypeHintsWhenTypeMatchesName t
           :organizeImportsCaseFirst "upper"
           :organizeImportsCollation "unicode" ; ordinal | unicode
           :organizeImportsIgnoreCase :json-false)))
    (add-to-list
     'eglot-server-programs
     `(,target-modes
       "typescript-language-server" "--stdio"
       :initializationOptions (:preferences ,preferences)))))

(use-package eglot
  :ensure nil
  :config
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (advice-add 'eglot--format-markup :filter-return
    (lambda (output) (replace-regexp-in-string (rx "\n```" eos) "" output))
    '((name . me/eglot--trim-block-end)))
  (me/eglot-initialize-scss)
  (me/eglot-initialize-typescript)
  :custom
  ;; NOTE Progress is handled by the custom mode-line package directly
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0))
  :hook
  (eglot-managed-mode . me/eglot-inlay-hints-maybe)
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (python-base-mode . eglot-ensure)
  (scss-mode . eglot-ensure)
  (typescript-ts-base-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp))

;;; use-lsp.el ends here
