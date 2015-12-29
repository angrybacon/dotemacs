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
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)
(defvar me/font-family-default)
(defvar me/font-family-header)
(defvar me/font-size-default)
(defvar me/font-size-header)
(defvar zenburn/bg)
(defvar zenburn/green+2)
(defvar zenburn/red)
(defvar zenburn/yellow)


;;=============================================================================
;; Configure Helm
;;=============================================================================


;; Website: https://github.com/emacs-helm/helm
(use-package helm
  :ensure t

  :init
  (setq
   helm-always-two-windows t
   helm-display-header-line nil
   ;; helm-mode-line-string ""
   helm-net-prefer-curl t
   helm-split-window-default-side 'left)

  :bind
  (("C-c h b" . helm-buffers-list)
   ("C-c h m" . helm-imenu)
   ("C-c h g" . helm-google-suggest))

  :config
  (helm-mode 1)
  (helm-push-mark-mode 1)
  (when (member me/font-family-header (font-family-list))
    (set-face-attribute 'helm-header nil
                        :font me/font-family-default :height me/font-size-default)
    (set-face-attribute 'helm-source-header nil
                        :font me/font-family-header :height me/font-size-header))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil
                      :foreground zenburn/yellow :background zenburn/bg :box nil)
  (set-face-attribute 'helm-match nil :foreground zenburn/green+2 :weight 'normal)

  (use-package helm-ag
    :ensure t)

  (use-package helm-command
    :bind
    ("M-x" . helm-M-x)
    :config
    (set-face-attribute 'helm-M-x-key nil :foreground zenburn/red :underline nil)))


(provide 'init-helm)
;;; init-helm.el ends here
