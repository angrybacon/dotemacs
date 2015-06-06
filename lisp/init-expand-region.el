;;─────────────────────────────────────────────────────────────────────────────
;; Increase region by semantic units
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :init
  (pending-delete-mode t)
  :bind
  ("C-=" . er/expand-region))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-expand-region.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-expand-region)
