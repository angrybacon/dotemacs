;;; init-yasnippet.el --- Enable interactive templates

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 29 December 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure YASnippet
;;=============================================================================


;; Website: https://github.com/capitaomorte/yasnippet
(use-package yasnippet

  :functions (yas-reload-all)

  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<tab>" . nil)
        ("<C-return>" . yas-expand))

  :init
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  :config
  (yas-reload-all))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
