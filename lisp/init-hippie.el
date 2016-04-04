;;; init-hippie.el --- Enable intelligent expansion at point

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure emmet-mode
;;=============================================================================


;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq-default emmet-move-cursor-between-quote t)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))


;;=============================================================================
;; Configure hippie-exp
;;=============================================================================


;; Built-in
(use-package hippie-exp
  :ensure nil
  :defer t
  :bind ("<C-return>" . hippie-expand)
  :config
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))


;;=============================================================================
;; Configure yasnippet
;;=============================================================================


;; https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'js-mode-hook 'yas-minor-mode)
  (add-hook 'sgml-mode-hook 'yas-minor-mode)
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))


(provide 'init-hippie)
;;; init-hippie.el ends here
