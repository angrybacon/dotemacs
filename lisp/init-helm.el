;;─────────────────────────────────────────────────────────────────────────────
;; Enable incremental completion and selection narrowing
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `helm' (https://github.com/emacs-helm/helm)

(with-eval-after-load 'helm
  (setq helm-mode-line-string "")
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (define-key global-map (kbd "C-c m") 'helm-imenu)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list))

;; (with-eval-after-load 'helm-command
(global-set-key (kbd "M-x") 'helm-M-x)
;; )

;; (with-eval-after-load 'helm-projectile
(helm-projectile-on)
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-helm.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-helm)
