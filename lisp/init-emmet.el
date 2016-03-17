;;; init-emmet.el --- Add integration for Emmet within Emacs

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
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq-default
   emmet-move-cursor-between-quotes t
   emmet-preview-default nil)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))


(provide 'init-emmet)
;;; init-emmet.el ends here
