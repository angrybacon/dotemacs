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
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)


;;=============================================================================
;; Configure HTML mode
;;=============================================================================


;; Built-in
(use-package sgml-mode
  :delight html-mode "HTML"
  :init
  (setq sgml-basic-offset 2))


;;=============================================================================
;; Configure Web mode
;;=============================================================================


;; Website: http://web-mode.org/
(use-package web-mode
  :ensure t
  :delight web-mode "Web"
  :init
  (setq
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2)
  :bind
  ("M-;" . comment-dwim))


(provide 'init-html)
;;; init-html.el ends here
