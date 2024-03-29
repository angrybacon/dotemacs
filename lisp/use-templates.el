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

(declare-function tempel-expand "tempel")
(declare-function tempel-insert "tempel")

(defun me/tempel-dwim ()
  "Call `tempel-expand' or `tempel-insert' when region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively #'tempel-insert)
    (tempel-expand :interactive)))

(use-package tempel
  :defines tempel-map
  :bind
  ("C-<return>" . me/tempel-dwim)
  (:map tempel-map
   ([backtab] . tempel-previous)
   ([tab] . tempel-next)))

;;; use-templates.el ends here
