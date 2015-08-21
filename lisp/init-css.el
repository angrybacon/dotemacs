;;; init-css.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure CSS and SCSS modes.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)


;;=============================================================================
;; Configure CSS mode
;;=============================================================================


;; Built-in
(use-package css-mode
  :delight css-mode "CSS"
  :init
  (setq css-indent-offset 2))


;;=============================================================================
;; Configure SCSS mode
;;=============================================================================


;; Website: https://github.com/antonj/scss-mode/
(use-package scss-mode
  :ensure t
  :delight scss-mode "SCSS"
  :init
  (setq scss-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))


(provide 'init-css)
;;; init-css.el ends here
