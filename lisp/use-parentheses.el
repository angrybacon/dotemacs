;;; use-parentheses.el --- Parentheses management    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package electric-pair
  :ensure nil
  :hook
  (after-init . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; use-parentheses.el ends here
