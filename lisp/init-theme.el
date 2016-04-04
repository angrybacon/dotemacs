;;; init-theme.el --- Configure the theme

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure zenburn-theme
;;=============================================================================


;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme

  :defines
  (me/font-family
   me/font-size-default
   zenburn/bg+3
   zenburn/bg+0
   zenburn/bg
   zenburn/bg-1
   zenburn/blue
   zenburn/fg-1
   zenburn/green)

  :init (load-theme 'zenburn t)

  :config
  ;; (load-theme 'zenburn t)
  (set-face-attribute 'default nil :height me/font-size-default)
  (set-face-attribute 'font-lock-doc-face nil :italic t)
  (set-face-attribute 'font-lock-comment-face nil :italic t)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground zenburn/bg+3 :italic t)
  (set-face-attribute 'font-lock-function-name-face nil :foreground zenburn/blue)
  (set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/bg+3)
  (set-face-attribute 'hl-line nil :background zenburn/bg+0)
  (set-face-attribute 'region nil :foreground zenburn/green)
  (set-face-attribute 'vertical-border nil :foreground zenburn/bg-1)
  (when (member me/font-family (font-family-list))
    (set-face-attribute 'default nil :font me/font-family)))


(provide 'init-theme)
;;; init-theme.el ends here
