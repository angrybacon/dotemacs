;;; use-snippets.el --- Expansible snippets          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :defines emmet-mode-keymap
  :bind
  (:map emmet-mode-keymap
   ("C-<return>" . nil))
  :custom
  (emmet-insert-flash-time .1)
  (emmet-jsx-className-braces? t)
  (emmet-move-cursor-between-quote t)
  :hook
  (css-base-mode . emmet-mode)
  (html-mode . emmet-mode)
  (tsx-ts-mode . emmet-mode)
  :preface
  (defun me/emmet-try-expand (args)
    "Try `emmet-expand-line' if `emmet-mode' is active. Else, does nothing."
    (interactive "P")
    (when emmet-mode (emmet-expand-line args))))

(use-package hippie-exp
  :ensure nil
  :bind
  ("C-<return>" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(yas-hippie-try-expand me/emmet-try-expand))
  (hippie-expand-verbose nil))

(use-package yasnippet
  :defines yas-minor-mode-map
  :functions yas-reload-all
  :bind
  (:map yas-minor-mode-map
   ("TAB" . nil)
   ([tab] . nil))
  :custom
  (yas-verbosity 2)
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;;; use-snippets.el ends here
