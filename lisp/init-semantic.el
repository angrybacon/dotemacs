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
;; Silence byte-compiler
;;=============================================================================


;; (defvar semantic-idle-breadcrumbs-separator)
;; (defvar semanticdb-default-save-directory)


;;=============================================================================
;; Add semantic breadcrumbs
;;=============================================================================


;; (require 'use-package)


;; Built-in
;; (use-package semantic
;;   :init
;;   (setq
;;    semantic-idle-breadcrumbs-separator " > "
;;    semanticdb-default-save-directory "~/.emacs.d/semanticdb/")
;;   :config
;;   ;; FIXME: This is not restricted to Python buffers
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               ;; TODO: Find a mode that display a namespace of the top line, instead of item at cursor position
;;               (semantic-mode t)
;;               (semantic-idle-breadcrumbs-mode t))))


(provide 'init-semantic)
;;; init-semantic.el ends here
