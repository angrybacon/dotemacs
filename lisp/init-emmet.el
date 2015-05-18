;;─────────────────────────────────────────────────────────────────────────────
;; Add Emmet support
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `emmet-mode' (https://github.com/smihica/emmet-mode)

;; (with-eval-after-load 'emmet-mode
  (setq
   emmet-preview-default nil
   emmet-move-cursor-between-quotes t)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (global-set-key (kbd "<M-left>") 'emmet-prev-edit-point)
(global-set-key (kbd "<M-right>") 'emmet-next-edit-point)
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-emmet.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-emmet)
