;;; init-semantic.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 1 Jun 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Todo.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


;; (require 'use-package)
;; (defvar semantic-idle-breadcrumbs-separator)
;; (defvar semanticdb-default-save-directory)


;;=============================================================================
;; Add semantic breadcrumbs
;;=============================================================================


;; Website: https://github.com/tuhdo/semantic-stickyfunc-enhance
;; (use-package stickyfunc-enhance
;;   :ensure t
;;   :init
;;   (require 'stickyfunc-enhance)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   :config
;;   (defun me/enable-semantic-maybe ()
;;     "Maybe enable `semantic-mode'."
;;     (if (derived-mode-p 'python-mode)
;;         (semantic-mode 1)
;;       (semantic-mode -1)))
;;   (add-hook 'change-major-mode-hook #'me/enable-semantic-maybe))


(provide 'init-semantic)
;;; init-semantic.el ends here
