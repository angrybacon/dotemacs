(require 'use-package)


;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :ensure t
  :delight yaml-mode "YAML"
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(provide 'init-yaml)
