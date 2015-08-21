;;; init-parentheses.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: abbrev, convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure behavior of parentheses, brackets and the like.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)
(defvar zenburn/green+2)
(defvar zenburn/green)
(defvar zenburn/green-1)


;;=============================================================================
;; Configure Highlight Parentheses
;;=============================================================================


;; Website: http://www.emacswiki.org/emacs/HighlightParentheses
(use-package highlight-parentheses
  :ensure t
  :init
  (setq hl-paren-colors `(,zenburn/green+2 ,zenburn/green ,zenburn/green-1))
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))


;;=============================================================================
;; Configure Smartparens
;;=============================================================================


;; Website: https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (setq sp-autoinsert-quote-if-followed-by-closing-pair t)

  ;; FIXME: This will mess up with Helm's M-x
  ;; (sp-pair "(" ")" :wrap "C-(")
  ;; (sp-pair "{" "}" :wrap "C-{")
  ;; (sp-pair "[" "]" :wrap "C-[")

  (sp-pair "{{" "}}")
  (sp-pair "[[" "]]")
  :config
  (smartparens-global-mode 1)
  :bind
  (("M-<backspace>" . sp-unwrap-sexp)
   ("M-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-forward-slurp-sexp)
   ("M-S-<left>" . sp-backward-slurp-sexp)
   ("M-S-<right>" . sp-backward-barf-sexp)))


(provide 'init-parentheses)
;;; init-parentheses.el ends here
