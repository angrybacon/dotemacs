;;─────────────────────────────────────────────────────────────────────────────
;; Add incremental completion and selection narrowing
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-line-string "")
  :bind
  (("C-c m" . helm-imenu)
   ("C-x b" . helm-buffers-list))
  :config

  ;; TOFIX: Will be overwritten by zenburn theme
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'helm-header nil :font "Monaco-12")
    (set-face-attribute 'helm-source-header nil :font "Monaco-14"))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil :foreground zenburn/blue :background zenburn/bg :box nil)
  (set-face-attribute 'helm-match nil :foreground "gold1")

  (use-package helm-command
    :bind
    ("M-x" . helm-M-x)
    :config
    (set-face-attribute 'helm-M-x-key nil :underline nil)))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-helm.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-helm)
