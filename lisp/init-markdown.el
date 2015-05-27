;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Markdown buffers
;;─────────────────────────────────────────────────────────────────────────────


;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :delight markdown-mode "markdown"
  :init
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode)))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-markdown.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-markdown)
