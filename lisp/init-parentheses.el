;;─────────────────────────────────────────────────────────────────────────────
;; Add support for intelligent parentheses
;;─────────────────────────────────────────────────────────────────────────────


;; http://www.emacswiki.org/emacs/HighlightParentheses
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  ;; :init
  ;; (setq hl-paren-colors `(,zenburn/orange ,zenburn/red+1 ,zenburn/red-1 ,zenburn/red-2))
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))


;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-parentheses.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-parentheses)
