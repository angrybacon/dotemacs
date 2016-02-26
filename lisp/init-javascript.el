;;; init-javascript.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure JavaScript mode.

;;; Code:


;;=============================================================================
;; Configure JS Mode
;;=============================================================================


;; Built-in
(use-package js
  :delight js-mode "JavaScript")


;;=============================================================================
;; Configure Tern
;;=============================================================================


;; Website: http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :defer t
  :config (add-hook 'js-mode-hook 'tern-mode))


(provide 'init-javascript)
;;; init-javascript.el ends here
