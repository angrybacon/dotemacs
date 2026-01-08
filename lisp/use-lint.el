;;; use-lint.el --- Configure linters                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Compilation

(use-package flymake :ensure nil)

(use-package flyover
  :custom
  (flyover-border-style 'slant)
  (flyover-show-error-id t)
  (flyover-show-icon nil)
  (flyover-virtual-line-type 'line-no-arrow)
  :hook
  (flymake-mode . flyover-mode))

;;;; Prettier

(defun me/prettier-markdown-parser ()
  "Return the Prettier parser for the current file.
Use the current buffer file extension if possible or fallback to the default
Markdown parser."
  (if-let* ((name (buffer-file-name))
            (extension (file-name-extension name))
            (_ (string-equal extension "mdx")))
      '(mdx)
    '(markdown)))

(use-package prettier
  :config
  (add-to-list
   'prettier-major-mode-parsers
   `(markdown-mode . ,#'me/prettier-markdown-parser))
  :custom
  (prettier-mode-sync-config-flag nil)
  :init
  (add-to-list 'safe-local-eval-forms '(prettier-mode)))

;;;; Whitespaces

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-display-mappings '((tab-mark ?\t [?› ?\t])))
  (whitespace-global-modes '(conf-mode prog-mode text-mode))
  (whitespace-line-column nil)
  (whitespace-style '(empty face lines-tail tab-mark tabs trailing))
  :hook
  (after-init . global-whitespace-mode))

;;; use-lint.el ends here
