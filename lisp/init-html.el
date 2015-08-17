(require 'use-package)


;; Built-in
(use-package sgml-mode
  :delight html-mode "HTML"
  :init
  (setq sgml-basic-offset 2))


;; http://web-mode.org/
(use-package web-mode
  :ensure t
  :delight web-mode "Web"
  :init
  (setq
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2)
  :bind
  ("M-;" . comment-dwim))


(provide 'init-html)
