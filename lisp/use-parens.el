;;; use-parens.el --- Quality of life with parens    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package electric-pair
  :ensure nil
  :hook
  (after-init . electric-pair-mode))

(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :hook
  (after-init . show-paren-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; use-parens.el ends here
