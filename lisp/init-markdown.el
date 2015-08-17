(require 'use-package)


;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :delight markdown-mode "Markdown"
  :init
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode)))


(provide 'init-markdown)
