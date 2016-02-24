;;; init-html.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: abbrev, convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure HTML and Web modes.

;;; Code:


;;=============================================================================
;; Configure SGML Mode
;;=============================================================================


;; Built-in
(use-package sgml-mode
  :delight html-mode "HTML"

  :init
  (setq sgml-basic-offset 2))


(provide 'init-html)
;;; init-html.el ends here
