;;; init-yasnippet.el --- Enable interactive templates

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 29 December 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure yasnippet
;;=============================================================================


;; https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :defines (yas-minor-mode-map)
  :functions (yas-reload-all)
  ;; FIXME: This should override emmet trigger
  ;;        https://github.com/emacsmirror/emacswiki.org/blob/master/minor-mode-hack.el
  :bind* (:map yas-minor-mode-map
               ("<C-return>" . yas-expand))
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
