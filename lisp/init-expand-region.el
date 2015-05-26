;;─────────────────────────────────────────────────────────────────────────────
;; Increase region by semantic units
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  :config
  ;; TOFIX: Doesn't apply when restarting Emacs
  (pending-delete-mode t))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-expand-region.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-expand-region)
