;;; init-flycheck.el --- Enable linters for several modes

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 2 June 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure flycheck
;;=============================================================================


;; https://github.com/flycheck/flycheck
(use-package flycheck

  :defer t

  :defines
  (zenburn/blue+1
   zenburn/orange
   zenburn/red-1)

  :bind
  (("C-c e l" . list-flycheck-errors)
   ("C-c e p" . flycheck-previous-error)
   ("C-c e n" . flycheck-next-error))

  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'flycheck-mode)

  :config
  (setq-default
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)
   flycheck-flake8rc "~/.flake8rc"
   flycheck-jshintrc "~/.jshintrc"
   flycheck-pylintrc "~/.pylintrc")
  (set-face-attribute 'flycheck-error nil :underline zenburn/red-1)
  (set-face-attribute 'flycheck-info nil :underline zenburn/blue+1)
  (set-face-attribute 'flycheck-warning nil :underline zenburn/orange)
  (set-face-attribute 'flycheck-fringe-error nil :foreground zenburn/red-1)
  (set-face-attribute 'flycheck-fringe-info nil :foreground zenburn/blue+1)
  (set-face-attribute 'flycheck-fringe-warning nil :foreground zenburn/orange))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
