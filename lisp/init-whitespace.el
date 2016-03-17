;;; init-whitespace.el --- Highlight whitespace-like characters

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 23 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure whitespace
;;=============================================================================


;; Built-in
(use-package whitespace
  :defer t
  :defines (zenburn/red-1)
  :init (global-whitespace-mode 1)
  :config
  (setq-default whitespace-style '(face empty tab trailing))
  (set-face-attribute 'whitespace-empty nil :background zenburn/red-1)
  (set-face-attribute 'whitespace-tab nil :background zenburn/red-1)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/red-1))


(provide 'init-whitespace)
;;; init-whitespace.el ends here
