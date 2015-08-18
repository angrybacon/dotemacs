;;; init-company.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Enable code completion.

;;; Code:


;;=============================================================================
;; Configure Company mode
;;=============================================================================


(require 'use-package)


;; Website: https://github.com/company-mode/company-mode
(use-package company
  :ensure t
  :init
  (setq
   company-idle-delay 0.1
   company-minimum-prefix-length 1)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'html-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'scss-mode-hook 'company-mode)
  :config
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection))


(provide 'init-company)
;;; init-company.el ends here
