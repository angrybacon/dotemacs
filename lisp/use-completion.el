;;; use-completion.el --- Customize completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :functions consult-imenu consult-mark consult-outline
  :bind
  ([remap goto-line] . consult-goto-line)
  ([remap isearch-forward] . consult-line)
  ([remap switch-to-buffer] . consult-buffer)
  ("C-h M" . consult-minor-mode-menu)
  ("C-S-r" . isearch-backward-regexp)   ; Move `isearch' command
  ("C-S-s" . isearch-forward-regexp)    ; Move `isearch' command
  :custom
  (consult-line-start-from-top t)
  (consult-project-root-function #'me/project-root)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'motion "gm" #'consult-mark)
    (evil-global-set-key 'motion "gM" #'consult-imenu)
    (evil-global-set-key 'motion "go" #'consult-outline)))

(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay .5))

(use-package corfu-popupinfo
  :ensure nil
  :custom
  (corfu-popupinfo-delay '(1.0 . t))
  :hook
  (global-corfu-mode . corfu-popupinfo-mode))

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

(use-package orderless
  :custom
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-styles '(orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space))

(use-package vertico
  :bind
  ("M-R" . vertico-repeat)
  :custom
  (vertico-count-format '("%-5s " . "%2$s"))
  (vertico-resize nil)
  :hook
  (after-init . vertico-mode)
  (minibuffer-setup . vertico-repeat-save))

;;; use-completion.el ends here