;;; init-html.el --- All about HTML

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure sgml-mode
;;=============================================================================


;; Built-in
(use-package sgml-mode
  :ensure nil
  :delight html-mode "HTML"
  :init (setq-default sgml-basic-offset 2))


(provide 'init-html)
;;; init-html.el ends here
