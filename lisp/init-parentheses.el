;;─────────────────────────────────────────────────────────────────────────────
;; Add support for intelligent parentheses
;;─────────────────────────────────────────────────────────────────────────────


;; http://www.emacswiki.org/emacs/HighlightParentheses
(use-package highlight-parentheses
  :ensure t
  ;; :init
  ;; (setq hl-paren-colors `(,zenburn/orange ,zenburn/red+1 ,zenburn/red-1 ,zenburn/red-2))
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))


;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (setq sp-autoinsert-quote-if-followed-by-closing-pair t)
  (sp-pair "{" "}" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-5")
  (sp-pair "{{" "}}")
  (sp-pair "[[" "]]")
  :config
  (smartparens-global-mode 1)
  :bind*
  (("M-<backspace>" . sp-unwrap-sexp)
   ("M-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-forward-slurp-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-parentheses.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-parentheses)
