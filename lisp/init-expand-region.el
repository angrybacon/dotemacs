;;─────────────────────────────────────────────────────────────────────────────
;; Increase region by semantic units
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `expand-region' (https://github.com/magnars/expand-region.el)

;; (with-eval-after-load 'expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-expand-region.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-expand-region)
