;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-flycheck.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)
(defvar zenburn/orange)
(defvar zenburn/red-2)


;;─────────────────────────────────────────────────────────────────────────────
;; Add support for on-the-fly snytax checking
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :init
  (setq
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-flake8rc "~/.flake8rc"
   flycheck-pylintrc "~/.pylintrc")
  ;; TODO: Find a linter for JavaScript
  ;; TODO: Fix linters for HTML and Emacs Lisp
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  ;; (add-hook 'html-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'flycheck-mode)
  :config
  (set-face-attribute 'flycheck-error nil :underline zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-error nil :foreground zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-warning nil :foreground zenburn/orange)
  (set-face-attribute 'flycheck-info nil :background 'unspecified)
  (set-face-attribute 'flycheck-warning nil :underline zenburn/orange))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-flycheck.elé
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-flycheck)
