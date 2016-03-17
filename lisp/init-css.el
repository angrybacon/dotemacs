;;; init-css.el --- All about CSS

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure css-mode
;;=============================================================================


;; Built-in
(use-package css-mode
  :delight css-mode "CSS"
  :init (setq-default css-indent-offset 2))


;;=============================================================================
;; Configure scss-mode
;;=============================================================================


;; https://github.com/antonj/scss-mode/
(use-package scss-mode
  :delight scss-mode "SCSS"
  :mode ("\\.css\\'" "\\.sass\\'" "\\.scss\\'")
  :init (setq-default scss-compile-at-save nil))


(provide 'init-css)
;;; init-css.el ends here
