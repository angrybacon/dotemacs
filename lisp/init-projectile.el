;;─────────────────────────────────────────────────────────────────────────────
;; Enable incremental completion and selection narrowing
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `projectile' (https://github.com/bbatsov/projectile)

;; (with-eval-after-load 'projectile
  (setq
   projectile-enable-caching t
   projectile-remember-window-configs t
   projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
(projectile-global-mode)
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-projectile.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-projectile)
