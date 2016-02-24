;;; init-emmet.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Add integration for Emmet within Emacs.

;;; Code:


;;=============================================================================
;; Configure Emmet Mode
;;=============================================================================


;; Website: https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t

  :init
  (setq
   emmet-move-cursor-between-quotes t
   emmet-preview-default nil)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)

  :config
  (define-key emmet-mode-keymap (kbd "C-M-<left>") nil)
  (define-key emmet-mode-keymap (kbd "C-M-<right>") nil))


(provide 'init-emmet)
;;; init-emmet.el ends here
