;;; use-templates.el --- Expansible templates        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :defines emmet-mode-keymap
  :bind
  (:map emmet-mode-keymap
   ("C-<return>" . nil)
   ("C-S-<return>" . emmet-expand-line))
  :custom
  (emmet-insert-flash-time .1)
  (emmet-jsx-className-braces? t)
  (emmet-move-cursor-between-quotes t)
  :hook
  (css-base-mode . emmet-mode)
  (html-mode . emmet-mode)
  (tsx-ts-mode . emmet-mode))

(use-package tempel
  :defines tempel-map
  :bind
  ("C-<return>" . tempel-expand)
  (:map tempel-map
   ([backtab] . tempel-previous)
   ([tab] . tempel-next)))

;;; use-templates.el ends here
