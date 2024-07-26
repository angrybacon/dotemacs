;;; use-lint.el --- Configure linters                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Compilation

(use-package flymake
  :ensure nil
  :custom
  (flymake-fringe-indicator-position nil))

(use-package flymake-eslint
  :functions flymake-eslint-enable
  :preface
  (defun me/flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

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
  (whitespace-global-modes '(prog-mode text-mode))
  (whitespace-line-column nil)
  (whitespace-style '(empty face lines-tail tab-mark tabs trailing))
  :hook
  (after-init . global-whitespace-mode))

;;; use-lint.el ends here
