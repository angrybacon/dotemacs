;;; init-theme.el --- Configure the theme

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Add Zenburn palette: https://github.com/bbatsov/zenburn-emacs
;;=============================================================================


(defconst zenburn/bg+3      "#6F6F6F"  "Zenburn palette: #6F6F6F.")
(defconst zenburn/bg+2      "#5F5F5F"  "Zenburn palette: #5F5F5F.")
(defconst zenburn/bg+1      "#4F4F4F"  "Zenburn palette: #4F4F4F.")
(defconst zenburn/bg+0      "#494949"  "Zenburn palette: #494949.")
(defconst zenburn/bg        "#3F3F3F"  "Zenburn palette: #3F3F3F.")
(defconst zenburn/bg-0      "#383838"  "Zenburn palette: #383838.")
(defconst zenburn/bg-1      "#2B2B2B"  "Zenburn palette: #2B2B2B.")
(defconst zenburn/bg-2      "#000000"  "Zenburn palette: #000000.")
(defconst zenburn/blue+1    "#94BFF3"  "Zenburn palette: #94BFF3.")
(defconst zenburn/blue      "#8CD0D3"  "Zenburn palette: #8CD0D3.")
(defconst zenburn/blue-1    "#7CB8BB"  "Zenburn palette: #7CB8BB.")
(defconst zenburn/blue-2    "#6CA0A3"  "Zenburn palette: #6CA0A3.")
(defconst zenburn/blue-3    "#5C888B"  "Zenburn palette: #5C888B.")
(defconst zenburn/blue-4    "#4C7073"  "Zenburn palette: #4C7073.")
(defconst zenburn/blue-5    "#366060"  "Zenburn palette: #366060.")
(defconst zenburn/cyan      "#93E0E3"  "Zenburn palette: #93E0E3.")
(defconst zenburn/fg+1      "#FFFFEF"  "Zenburn palette: #FFFFEF.")
(defconst zenburn/fg        "#DCDCCC"  "Zenburn palette: #DCDCCC.")
(defconst zenburn/fg-1      "#656555"  "Zenburn palette: #656555.")
(defconst zenburn/green+4   "#BFEBBF"  "Zenburn palette: #BFEBBF.")
(defconst zenburn/green+3   "#AFD8AF"  "Zenburn palette: #AFD8AF.")
(defconst zenburn/green+2   "#9FC59F"  "Zenburn palette: #9FC59F.")
(defconst zenburn/green+1   "#8FB28F"  "Zenburn palette: #8FB28F.")
(defconst zenburn/green     "#7F9F7F"  "Zenburn palette: #7F9F7F.")
(defconst zenburn/green-1   "#5F7F5F"  "Zenburn palette: #5F7F5F.")
(defconst zenburn/magenta   "#DC8CC3"  "Zenburn palette: #DC8CC3.")
(defconst zenburn/orange    "#DFAF8F"  "Zenburn palette: #DFAF8F.")
(defconst zenburn/red+1     "#DCA3A3"  "Zenburn palette: #DCA3A3.")
(defconst zenburn/red       "#CC9393"  "Zenburn palette: #CC9393.")
(defconst zenburn/red-1     "#BC8383"  "Zenburn palette: #BC8383.")
(defconst zenburn/red-2     "#AC7373"  "Zenburn palette: #AC7373.")
(defconst zenburn/red-3     "#9C6363"  "Zenburn palette: #9C6363.")
(defconst zenburn/red-4     "#8C5353"  "Zenburn palette: #8C5353.")
(defconst zenburn/yellow    "#F0DFAF"  "Zenburn palette: #F0DFAF.")
(defconst zenburn/yellow-1  "#E0CF9F"  "Zenburn palette: #E0CF9F.")
(defconst zenburn/yellow-2  "#D0BF8F"  "Zenburn palette: #D0BF8F.")


;;=============================================================================
;; Configure rainbow-mode
;;=============================================================================


;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook 'rainbow-mode)
  :config (setq-default rainbow-x-colors-major-mode-list '()))


;;=============================================================================
;; Configure zenburn-theme
;;=============================================================================


;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme

  :defines
  (me/font-family
   me/font-size-default
   me/font-size-header
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
  (set-face-attribute 'header-line nil
                      :box `(:line-width 1 :color ,zenburn/bg-1) :height me/font-size-header)
  (set-face-attribute 'hl-line nil :background zenburn/bg+0)
  (set-face-attribute 'region nil :foreground zenburn/green)
  (set-face-attribute 'vertical-border nil :foreground zenburn/bg-1)
  (when (member me/font-family (font-family-list))
    (set-face-attribute 'default nil :font me/font-family)))


(provide 'init-theme)
;;; init-theme.el ends here
