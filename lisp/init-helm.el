;;; init-helm.el --- Enable incremental completion and selection narrowing framework

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure helm
;;=============================================================================


;; https://github.com/emacs-helm/helm
(use-package helm

  :defines
  (me/font-family-default
   me/font-family-header
   me/font-size-default
   me/font-size-header
   zenburn/bg+3
   zenburn/bg+0
   zenburn/bg
   zenburn/green+2
   zenburn/yellow)

  :bind
  (("C-c h k" . helm-show-kill-ring)
   ("C-c h g" . helm-google-suggest)
   ("C-c h m" . helm-imenu)
   ("C-x b" . helm-buffers-list))

  :config


  ;; Activate Helm
  (helm-mode 1)

  ;; Use better defaults
  (setq-default
   helm-always-two-windows t
   helm-display-header-line nil
   helm-M-x-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-mode-line-string nil
   helm-net-prefer-curl t
   helm-ff-skip-boring-files t
   helm-split-window-default-side 'left)

  ;; Customize faces
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :background 'unspecified :foreground zenburn/bg+3)
  (set-face-attribute 'helm-header nil
                      :font me/font-family-default :height me/font-size-default :italic t)
  (set-face-attribute 'helm-match nil :foreground zenburn/green+2 :weight 'normal)
  (set-face-attribute 'helm-selection nil :background zenburn/bg+0)
  (set-face-attribute 'helm-source-header nil
                      :box nil :background zenburn/bg :font me/font-family-header
                      :foreground zenburn/yellow :height me/font-size-header))


;;=============================================================================
;; Configure helm-ag
;;=============================================================================


(use-package helm-ag)


;;======================================
;; Configure helm-command
;;======================================


(use-package helm-command
  :ensure nil
  :defines zenburn/red
  :bind ("M-x" . helm-M-x)
  :config (set-face-attribute 'helm-M-x-key nil :foreground zenburn/red :underline nil))


;;======================================
;; Configure helm-descbinds
;;======================================


(use-package helm-descbinds
  :bind ("C-h k" . helm-descbinds)
  :config (setq-default helm-descbinds-window-style 'split-window))


;;======================================
;; Configure helm-describe-modes
;;======================================


(use-package helm-describe-modes
  :bind ("C-h m" . helm-describe-modes))


(provide 'init-helm)
;;; init-helm.el ends here
