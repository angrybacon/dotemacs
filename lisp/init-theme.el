;;; init-theme.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure the theme.

;;; Code:


;;=============================================================================
;; Configure the theme
;;=============================================================================


;; Website: https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :ensure t

  :defines
  (me/font-family-default
   me/font-size-default
   zenburn/bg+2
   zenburn/bg+0
   zenburn/bg
   zenburn/bg-1
   zenburn/bg-2
   zenburn/blue
   zenburn/fg-1
   zenburn/green+2
   zenburn/green
   zenburn/green-1)

  :init
  (load-theme 'zenburn t)

  :config

  ;; Set font size
  (set-face-attribute 'default nil :height me/font-size-default)

  ;; Set font family
  (when (member me/font-family-default (font-family-list))
    (set-face-attribute 'default nil :font me/font-family-default)
    (set-face-attribute 'header-line nil :font me/font-family-default))

  ;; Set
  ;; TODO: Minimize those
  (set-face-attribute 'font-lock-doc-face nil :italic t)
  (set-face-attribute 'font-lock-comment-face nil :foreground zenburn/fg-1 :italic t)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground zenburn/green-1 :italic t)
  (set-face-attribute 'font-lock-function-name-face nil :foreground zenburn/blue)
  (set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/bg+2)
  (set-face-attribute 'header-line nil :background zenburn/bg+0 :box nil)
  (set-face-attribute 'isearch nil :foreground zenburn/green+2 :background zenburn/bg-2)
  (set-face-attribute 'lazy-highlight nil :foreground zenburn/green+2 :background zenburn/bg-1)
  (set-face-attribute 'region nil :foreground zenburn/green)
  (set-face-attribute 'show-paren-match nil :background 'unspecified)
  (set-face-attribute 'show-paren-mismatch nil :background 'unspecified)
  (set-face-attribute 'vertical-border nil :foreground zenburn/bg-1))


;;=============================================================================
;; Overwrite face attributes
;;=============================================================================




(provide 'init-theme)
;;; init-theme.el ends here
