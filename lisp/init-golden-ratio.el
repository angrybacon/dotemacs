;;─────────────────────────────────────────────────────────────────────────────
;; Add automatic resizing of buffers to the golden ratio
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (setq golden-ratio-adjust-factor .9)
  :config
  (golden-ratio-mode 1))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-golden-ratio.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-golden-ratio)
