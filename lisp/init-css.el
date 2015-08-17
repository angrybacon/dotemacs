(require 'use-package)


;; Built-in
(use-package css-mode
  :delight css-mode "CSS"
  :init
  (setq css-indent-offset 2))


;; https://github.com/antonj/scss-mode/
(use-package scss-mode
  :ensure t
  :delight scss-mode "SCSS"
  :init
  (setq scss-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))


(provide 'init-css)
