;;─────────────────────────────────────────────────────────────────────────────
;; Add support for quick jump within buffer
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :init
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
  :bind
  (("C-f" . ace-jump-mode)
   ("C-c f" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))



;;─────────────────────────────────────────────────────────────────────────────
;; End init-cursor.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-cursor)
