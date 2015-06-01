;;─────────────────────────────────────────────────────────────────────────────
;; Show parent namespace at the top of the active buffer
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/tuhdo/semantic-stickyfunc-enhance
(use-package stickyfunc-enhance
  :ensure t
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  :config
  (semantic-mode 1))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-semantic-stickyfunc.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-semantic-stickyfunc)
