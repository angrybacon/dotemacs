;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-theme.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)


(defvar me/font-family-default)
(defvar me/font-size-default)
(defvar zenburn/bg+2)
(defvar zenburn/bg+0)
(defvar zenburn/bg)
(defvar zenburn/bg-1)
(defvar zenburn/bg-2)
(defvar zenburn/fg-1)
(defvar zenburn/green+2)
(defvar zenburn/green)
(defvar zenburn/green-1)


;;─────────────────────────────────────────────────────────────────────────────
;; Set the color theme
;;─────────────────────────────────────────────────────────────────────────────


(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))


;;─────────────────────────────────────────────────────────────────────────────
;; Font settings
;;─────────────────────────────────────────────────────────────────────────────


(set-face-attribute 'default nil :height me/font-size-default)


(when (member me/font-family-default (font-family-list))
  (set-face-attribute 'default nil :font me/font-family-default)
  (set-face-attribute 'header-line nil :font me/font-family-default))


;;─────────────────────────────────────────────────────────────────────────────
;; Customize built-in faces
;;─────────────────────────────────────────────────────────────────────────────


(set-face-attribute 'font-lock-doc-face nil :italic t)
(set-face-attribute 'font-lock-constant-face nil :foreground zenburn/green-1)
(set-face-attribute 'font-lock-comment-face nil :foreground zenburn/fg-1 :italic t)
(set-face-attribute 'font-lock-comment-delimiter-face nil :italic t)
(set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/bg+2)
(set-face-attribute 'header-line nil :background zenburn/bg+0 :box `(:line-width 4 :color ,zenburn/bg+0))
(set-face-attribute 'isearch nil :foreground zenburn/green+2 :background zenburn/bg-2)
(set-face-attribute 'lazy-highlight nil :foreground zenburn/green+2 :background zenburn/bg-1)
(set-face-attribute 'region nil :foreground zenburn/green)
(set-face-attribute 'show-paren-match nil :background 'unspecified)
(set-face-attribute 'show-paren-mismatch nil :background 'unspecified)
(set-face-attribute 'vertical-border nil :foreground zenburn/bg-1)


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-theme.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-theme)
