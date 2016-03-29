;;; init-customize.el --- All about customize

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 29 March 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure cus-edit
;;=============================================================================


;; Built-in
(use-package cus-edit

  :ensure nil
  :defer t

  :defines
  (me/font-family
   me/font-size-header
   zenburn/blue
   zenburn/green+4
   zenburn/yellow)

  :config
  (set-face-attribute 'custom-group-tag nil :foreground zenburn/yellow :height me/font-size-header)
  (set-face-attribute 'custom-state nil :foreground zenburn/green+4)
  (set-face-attribute 'custom-variable-tag nil :foreground zenburn/blue)
  (when (member me/font-family (font-family-list))
    (set-face-attribute 'custom-group-tag nil :font me/font-family)))


(provide 'init-customize)
;;; init-customize.el ends here
