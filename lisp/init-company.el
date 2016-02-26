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
;; Configure Company Mode
;;=============================================================================


;; Website: https://github.com/company-mode/company-mode
(use-package company
  :init
  (setq-default
   company-idle-delay .1
   company-minimum-prefix-length 1)
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection))


;;=============================================================================
;; Configure Company Tern
;;=============================================================================


(use-package company-tern
  :defer t
  :init
  (setq-default
   company-tern-meta-as-single-line t
   company-tern-property-marker " *"
   company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-tern))


(provide 'init-company)
;;; init-company.el ends here
