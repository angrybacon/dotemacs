;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for YAML buffers
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :delight yaml-mode "YAML Test"
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-yaml.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-yaml)
