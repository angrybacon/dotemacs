;;; init-python.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure Python mode.

;;; Code:


;;=============================================================================
;; Configure Python mode
;;=============================================================================


(require 'use-package)


;; Built-in
(use-package python
  :delight python-mode "Python")


(provide 'init-python)
;;; init-python.el ends here
