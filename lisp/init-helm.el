;;; init-helm.el --- Enable incremental completion and selection narrowing framework

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure helm
;;=============================================================================


;; TODO: Make helm-list-faces-display


;; https://github.com/emacs-helm/helm
(use-package helm

  :defines
  (me/font-size-default
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
   ("C-c h r" . helm-resume))

  :config

  (defadvice helm-display-mode-line (after me/helm-display-mode-line activate)
    "Customize mode-line for helm buffers."
    ;; TODO: Add relevant information in the mode-line.
    )

  ;; Activate Helm
  (helm-mode 1)

  ;; Use better defaults
  (setq-default
   helm-always-two-windows t
   helm-display-header-line nil
   helm-mode-line-string nil
   helm-split-window-default-side 'left)

  ;; Customize faces
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :background 'unspecified :foreground zenburn/bg+3)
  (set-face-attribute 'helm-match nil :foreground zenburn/green+2 :weight 'normal)
  (set-face-attribute 'helm-source-header nil
                      :box nil :background 'unspecified :height me/font-size-header))


;; https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
(use-package helm-buffers
  :ensure helm
  :config (setq-default helm-buffers-fuzzy-matching t))


;; https://github.com/emacs-helm/helm/blob/master/helm-color.el
(use-package helm-color
  :ensure helm
  :bind ("C-c h c" . helm-colors))


;; https://github.com/emacs-helm/helm/blob/master/helm-command.el
(use-package helm-command
  :ensure helm
  :defines (zenburn/orange)
  :bind ([remap execute-extended-command] . helm-M-x)
  :config
  (setq-default helm-M-x-fuzzy-match t)
  (set-face-attribute 'helm-M-x-key nil :foreground zenburn/orange :underline nil))


;; https://github.com/emacs-helm/helm/blob/master/helm-grep.el
(use-package helm-grep
  :ensure helm
  :defines (zenburn/bg+3)
  :config (set-face-attribute 'helm-grep-lineno nil :foreground zenburn/bg+3))


;; https://github.com/emacs-helm/helm/blob/master/helm-misc.el
(use-package helm-misc
  :ensure helm
  :bind ([remap switch-to-buffer] . helm-buffers-list))


;; https://github.com/emacs-helm/helm/blob/master/helm-mode.el
(use-package helm-mode
  :ensure helm
  :config
  (setq-default
   helm-completion-in-region-fuzzy-match t
   helm-mode-fuzzy-match t))


;; https://github.com/emacs-helm/helm/blob/master/helm-net.el
(use-package helm-net
  :ensure helm
  :config
  (setq-default helm-net-prefer-curl t))


;; https://github.com/emacs-helm/helm/blob/master/helm-regexp.el
(use-package helm-regexp
  :ensure helm
  :defines (zenburn/blue)
  :config (set-face-attribute 'helm-moccur-buffer nil :foreground zenburn/blue))


;;=============================================================================
;; Configure helm-ag
;;=============================================================================


;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag)


;;=============================================================================
;; Configure helm-css-scss
;;=============================================================================


;; https://github.com/ShingoFukuyama/helm-css-scss
(use-package helm-css-scss
  ;; WARNING: https://github.com/ShingoFukuyama/helm-css-scss/issues/7
  ;; TODO: Fix Zenburn palette (ttps://github.com/bbatsov/zenburn-emacs/issues/220)
  :bind ("C-c h s" . helm-css-scss)
  :config (setq-default helm-css-scss-split-direction 'split-window-horizontally))


;;=============================================================================
;; Configure helm-descbinds
;;=============================================================================


;; https://github.com/emacs-helm/helm-descbinds
(use-package helm-descbinds
  :bind ("C-c h k" . helm-descbinds)
  :config (setq-default helm-descbinds-window-style 'split-window))


;;=============================================================================
;; Configure helm-describe-modes
;;=============================================================================


;; https://github.com/emacs-helm/helm-describe-modes
(use-package helm-describe-modes
  :bind ("C-c h m" . helm-describe-modes))


;;=============================================================================
;; Configure helm-flycheck
;;=============================================================================


;; https://github.com/yasuyk/helm-flycheck
(use-package helm-flycheck
  :bind ("C-c h f" . helm-flycheck))


;;=============================================================================
;; Configure helm-projectile
;;=============================================================================


;; https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))


(provide 'init-helm)
;;; init-helm.el ends here
