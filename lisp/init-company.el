;;─────────────────────────────────────────────────────────────────────────────
;; Add support for autocompletion
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/company-mode/company-mode
(use-package company
  :ensure t
  :init
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.1)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'html-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'scss-mode-hook 'company-mode)
  :config
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-company.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-company)
