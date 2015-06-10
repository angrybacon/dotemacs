;;─────────────────────────────────────────────────────────────────────────────
;; Add support for on-the-fly snytax checking
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :init
  (setq
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-flake8rc "~/.flake8rc")
  (add-hook 'scss-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  :config
  (set-face-attribute 'flycheck-error nil :underline zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-error nil :foreground zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-warning nil :foreground zenburn/orange)
  (set-face-attribute 'flycheck-info nil :underline zenburn/cyan)
  (set-face-attribute 'flycheck-warning nil :underline zenburn/orange))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-flycheck.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-flycheck)
