;;; init-theme.el --- Configure the theme

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Zenburn theme
;;=============================================================================


;; Website: https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme

  :defines
  (me/font-family-default
   me/font-size-default
   zenburn/bg+3
   zenburn/bg+0
   zenburn/bg
   zenburn/bg-1
   zenburn/blue
   zenburn/fg-1
   zenburn/green
   zenburn/green-1)

  :config
  (load-theme 'zenburn t)
  ;; FIXME: Invalid face
  ;; (set-face-attribute 'custom-group-tag nil
  ;;                     :font me/font-family-default :foreground zenburn/blue)
  ;; (set-face-attribute 'custom-state nil :foreground zenburn/green+4)
  ;; (set-face-attribute 'custom-variable-tag nil :foreground zenburn/blue)
  (set-face-attribute 'default nil :font me/font-family-default :height me/font-size-default)
  (set-face-attribute 'font-lock-doc-face nil :italic t)
  (set-face-attribute 'font-lock-comment-face nil :foreground zenburn/fg-1 :italic t)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground zenburn/green-1 :italic t)
  (set-face-attribute 'font-lock-function-name-face nil :foreground zenburn/blue)
  (set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/bg+3)
  (set-face-attribute 'hl-line nil :background zenburn/bg+0)
  (set-face-attribute 'region nil :foreground zenburn/green)
  (set-face-attribute 'vertical-border nil :foreground zenburn/bg-1))


(provide 'init-theme)
;;; init-theme.el ends here
