;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Sass code
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `scss-mode' (https://github.com/antonj/scss-mode/)

;; (with-eval-after-load 'scss-mode
  (setq scss-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-css.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-css)
