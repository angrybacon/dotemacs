;;; init-parentheses.el --- Configure behavior of parentheses, brackets and the like

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure highlight-parentheses
;;=============================================================================


;; http://www.emacswiki.org/emacs/HighlightParentheses
(use-package highlight-parentheses
  :defines
  (zenburn/green+2
   zenburn/green
   zenburn/green-1)
  :preface
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
  :init (setq-default hl-paren-colors `(,zenburn/green+2 ,zenburn/green ,zenburn/green-1))
  :config (global-highlight-parentheses-mode t))


;;=============================================================================
;; Configure smartparens
;;=============================================================================


;; https://github.com/Fuco1/smartparens
(use-package smartparens

  :defer t

  :functions (sp-pair)

  :bind
  (("M-<backspace>" . sp-unwrap-sexp)
   ("M-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-forward-slurp-sexp)
   ("M-S-<left>" . sp-backward-slurp-sexp)
   ("M-S-<right>" . sp-backward-barf-sexp))

  :init
  (require 'smartparens-config)
  (setq-default sp-autoinsert-quote-if-followed-by-closing-pair t)

  :config
  (sp-pair "{{" "}}")
  (sp-pair "[[" "]]")
  (smartparens-global-mode 1))


(provide 'init-parentheses)
;;; init-parentheses.el ends here
