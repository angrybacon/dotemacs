;;─────────────────────────────────────────────────────────────────────────────
;; Add support for Git operations
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :init
  (setq
   magit-show-child-count t
   magit-stage-all-confirm nil
   magit-unstage-all-confirm nil)
  :bind
  ("C-c g" . magit-status)
  :config
  (when (member "Monaco" (font-family-list)) (set-face-attribute 'magit-section-title nil :font "Monaco-14"))
  (set-face-attribute 'magit-section-title nil :weight 'unspecified :foreground zenburn/blue))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-git.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-git)
