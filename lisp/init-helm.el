;;; init-helm.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: abbrev, convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Enable incremental completion and selection narrowing framework.

;;; Code:


;;=============================================================================
;; Silence byte-compiler
;;=============================================================================


(defvar me/font-family-default)
(defvar me/font-family-header)
(defvar me/font-size-default)
(defvar me/font-size-header)
(defvar zenburn/bg)
(defvar zenburn/blue)
(defvar zenburn/green+2)
(defvar zenburn/red)


;;=============================================================================
;; Configure Helm
;;=============================================================================


(require 'use-package)


;; Website: https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :init
  (setq
   helm-google-suggest-use-curl-p t
   helm-mode-line-string "")
  :bind
  (("C-c h b" . helm-buffers-list)
   ("C-c h m" . helm-imenu)
   ("C-c h g" . helm-google-suggest))
  :config
  (when (member me/font-family-header (font-family-list))
    (set-face-attribute 'helm-header nil :font me/font-family-default :height me/font-size-default)
    (set-face-attribute 'helm-source-header nil :font me/font-family-header :height me/font-size-header))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil :foreground zenburn/blue :background zenburn/bg :box nil)
  (set-face-attribute 'helm-match nil :foreground zenburn/green+2 :weight 'normal)

  (use-package helm-command
    :bind
    ("M-x" . helm-M-x)
    :config
    (set-face-attribute 'helm-M-x-key nil :foreground zenburn/red :underline nil)))


(provide 'init-helm)
;;; init-helm.el ends here
