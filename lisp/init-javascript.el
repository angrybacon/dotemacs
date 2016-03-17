;;; init-javascript.el --- All about JavaScript

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure js
;;=============================================================================


;; Built-in
(use-package js
  :defer t
  :delight js-mode "JavaScript")


;;=============================================================================
;; Configure json-mode
;;=============================================================================


;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode "\\.json\\'"
  :delight json-mode "JSON"
  :config
  ;; FIXME: This will be fixed with https://github.com/joshwnj/json-mode/issues/32.
  ;; (setq-default json-reformat:indent-width 2)
  (add-hook 'json-mode-hook (lambda () (setq-local js-indent-level 2))))


;;=============================================================================
;; Configure tern
;;=============================================================================


;; http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :defer t
  :config (add-hook 'js-mode-hook 'tern-mode))


(provide 'init-javascript)
;;; init-javascript.el ends here
