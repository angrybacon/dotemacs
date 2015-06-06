;;─────────────────────────────────────────────────────────────────────────────
;; Add incremental completion and selection narrowing
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :init
  (setq
   helm-mode-line-string ""
   helm-google-suggest-use-curl-p t)
  :bind
  (("C-c h b" . helm-buffers-list)
   ("C-c h m" . helm-imenu)
   ("C-c h g" . helm-google-suggest))
  :config

  ;; FIXME: Overwritten by Zenburn theme
  (when (member me/font-family-header (font-family-list))
    (set-face-attribute 'helm-header nil :font me/font-family-default :height me/font-size-default)
    (set-face-attribute 'helm-source-header nil :font me/font-family-header :height me/font-size-header))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil :foreground zenburn/blue :background zenburn/bg :box nil)
  (set-face-attribute 'helm-match nil :foreground zenburn/green+2)

  (use-package helm-command
    :bind
    ("M-x" . helm-M-x)
    :config
    (set-face-attribute 'helm-M-x-key nil :underline nil)))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-helm.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-helm)
