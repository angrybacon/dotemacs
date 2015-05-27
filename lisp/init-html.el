;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for HTML buffers
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package sgml-mode
  :delight html-mode "html"
  :init
  (setq sgml-basic-offset 2))


;; http://web-mode.org/
(use-package web-mode
  :ensure t
  :delight web-mode "web"
  :init
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :bind
  ("M-;" . comment-dwim))



;;─────────────────────────────────────────────────────────────────────────────
;; End init-html.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-html)
