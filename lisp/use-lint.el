;;; use-lint.el --- Configure linters                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Compilation

(use-package flymake :ensure nil)

(use-package flyover
  :disabled
  :custom
  (flyover-base-height .9)
  (flyover-border-style 'none)
  (flyover-show-error-id t)
  (flyover-show-icon nil)
  (flyover-virtual-line-type 'line-no-arrow)
  :hook
  (flymake-mode . flyover-mode))

;;;; Fill-Column

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (after-init . global-display-fill-column-indicator-mode))

;;;; Prettier

(use-package prettier
  :custom
  (prettier-mode-sync-config-flag nil))

;;;; Whitespaces

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-display-mappings '((tab-mark ?\t [?› ?\t])))
  (whitespace-global-modes '(conf-mode prog-mode text-mode))
  (whitespace-line-column nil)
  (whitespace-style '(empty face tab-mark tabs trailing))
  :hook
  (after-init . global-whitespace-mode))

;;; use-lint.el ends here
