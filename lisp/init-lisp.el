;;; init-lisp.el --- Configure Lisp mode and derived

;; Copyright (C) 2016  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 28 February 2016
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure lisp-mode
;;=============================================================================


;; Built-in
(use-package lisp-mode
  :ensure nil
  :config
  (delight
   '((emacs-lisp-mode "Emacs Lisp")
     (lisp-interaction-mode "Lisp Interaction"))))


(provide 'init-lisp)
;;; init-lisp.el ends here
