;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-markdown.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)


;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Markdown buffers
;;─────────────────────────────────────────────────────────────────────────────


;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :delight markdown-mode "Markdown"
  :init
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode)))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-markdown.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-markdown)
