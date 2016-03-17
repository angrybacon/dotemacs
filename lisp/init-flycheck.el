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
  (zenburn/orange
   zenburn/red-2)

  :init
  (setq-default
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)
   flycheck-jshintrc "~/.jshintrc"
   flycheck-flake8rc "~/.flake8rc"
   flycheck-pylintrc "~/.pylintrc")
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'flycheck-mode)

  :config
  (set-face-attribute 'flycheck-error nil :underline zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-error nil :foreground zenburn/red-2)
  (set-face-attribute 'flycheck-fringe-warning nil :foreground zenburn/orange)
  (set-face-attribute 'flycheck-info nil :background 'unspecified)
  (set-face-attribute 'flycheck-warning nil :underline zenburn/orange))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
