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
;; Configure JavaScript mode
;;=============================================================================


(require 'use-package)


;; Built-in
(use-package js
  :delight js-mode "JavaScript")


(provide 'init-javascript)
;;; init-javascript.el ends here
