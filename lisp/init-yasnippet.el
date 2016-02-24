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
  :ensure t

  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  :config
  (yas-global-mode 1)
  ;; FIXME: Use :bindmap?
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-return>") 'yas-expand))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
