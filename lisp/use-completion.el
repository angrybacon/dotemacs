;;; use-completion.el --- Customize completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :defines consult-narrow-map
  :functions consult-imenu consult-mark consult-outline
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap isearch-forward] . consult-line)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ("C-S-r" . isearch-backward-regexp)
   ("C-S-s" . isearch-forward-regexp)
   :map consult-narrow-map
   ("C-h" . consult-narrow-help)
   :map goto-map
   ("m" . consult-mark)
   ;; TODO Use `consult-org-heading' in `org-mode'
   ("o" . consult-outline)
   :map help-map
   ("M" . consult-minor-mode-menu))
  :custom
  (consult-line-start-from-top t)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref))

(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1))

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

(use-package wgrep)

;;; use-completion.el ends here
