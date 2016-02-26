;;; init-yasnippet.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 29 December 2015
;; Keywords: abbrev, convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Enable incremental completion and selection narrowing framework.

;;; Code:


;;=============================================================================
;; Configure YASnippet
;;=============================================================================


;; Website: https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-return" . yas-expand))
  :init (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode 1))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
