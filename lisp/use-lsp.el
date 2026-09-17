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

;;;; L(anguage) S(erver) P(rotocol) Client

(declare-function cl-substitute-if "cl-seq")
(declare-function eglot-completion-at-point "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function me/project-save "use-project")

(defun me/eglot-configure-hover ()
  "Configure `eldoc-documentation-functions' to allow multiline entries."
  (setq-local
   eldoc-documentation-functions
   (cl-substitute
    #'(lambda (f)
        "Wrap F like `eglot-hover-eldoc-function', but skip the :echo cookie."
        (eglot-hover-eldoc-function (lambda (data &rest _ignore) (funcall f data))))
    'eglot-hover-eldoc-function
    eldoc-documentation-functions)))

(defun me/eglot-configure-scss ()
  "Register the SCSS server program and return its preferences."
  (defvar eglot-server-programs)
  (let ((modes '(scss-mode))
        (command '("vscode-css-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs `(,modes ,@command)))
  '(:lint (:duplicateProperties "warning"
           :float "warning"
           :important "warning"
           :zeroUnits "warning")))

(defun me/eglot-configure-typescript ()
  "Register the TypeScript server program and return its preferences."
  (defvar eglot-server-programs)
  (let ((modes '(tsx-ts-mode typescript-ts-mode))
        (command '("npx" "tsc" "--lsp" "--stdio")))
    (add-to-list 'eglot-server-programs `(,modes ,@command)))
  '(:preferences (:importModuleSpecifier "non-relative"
                  :preferTypeOnlyAutoImports t)))

(defun me/eglot-events-buffer-toggle ()
  "Toggle `eglot-events-buffer-config' between quiet and verbose."
  (interactive)
  (let ((value (if (zerop (plist-get eglot-events-buffer-config :size))
                   '(:size 2000000 :format full)
                 '(:size 0))))
    (message "[Lsp] events buffer configuration updated: `%S'"
             (setq eglot-events-buffer-config value))))

(use-package eglot
  :ensure nil
  :config
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (define-advice eglot-completion-at-point (:filter-return (f) add-source)
    ;; NOTE TypeScript 7 moved the module source shown for same-named
    ;;      auto-import candidates from the classic `detail' completion field to
    ;;      the newer `labelDetails.description' field. Eglot's annotation
    ;;      function only reads `detail', falling back to the completion kind
    ;;      name (e.g. "Function") when it is absent, so Corfu silently drops
    ;;      that disambiguation hint. Insert `labelDetails.description' into
    ;;      that fallback chain, ahead of the kind name.
    (when-let* ((props (nthcdr 3 f)))
      (plist-put
       props :annotation-function
       (lambda (proxy)
         (let* ((item (get-text-property 0 'eglot--lsp-item proxy))
                (detail (plist-get item :detail))
                (detail (and (stringp detail) (not (string= detail "")) detail))
                (details (plist-get item :labelDetails))
                (description (plist-get details :description))
                (description (and (stringp description)
                                  (not (string= description "")) description))
                (kind (plist-get item :kind))
                (annotation (or detail
                                description
                                (cdr (assoc kind eglot--kind-names)))))
           (when annotation
             (concat " " (propertize annotation
                                     'face 'completions-annotations)))))))
    f)
  (setq-default
   ;; NOTE Not a `defcustom' so `:custom' cannot reliably configure it
   eglot-workspace-configuration `(:scss ,(me/eglot-configure-scss)
                                   :typescript ,(me/eglot-configure-typescript)))
  :custom
  (eglot-autoshutdown t)
  (eglot-code-action-indications '(eldoc-hint))
  (eglot-events-buffer-config '(:size 0))
  ;; NOTE Let Tree-Sitter handle the highlighting.
  ;;      Who thought that was a good idea to enable by default?
  (eglot-ignored-server-capabilities '(:semanticTokensProvider))
  :hook
  (eglot-managed-mode . me/eglot-configure-hover)
  (json-ts-mode . eglot-ensure)
  (python-base-mode . eglot-ensure)
  (scss-mode . eglot-ensure)
  (typescript-ts-base-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp))

;;; use-lsp.el ends here
