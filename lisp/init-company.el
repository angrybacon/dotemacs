;;; init-company.el --- Enable code completion

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure company
;;=============================================================================


;; https://github.com/company-mode/company-mode
(use-package company
  :init (global-company-mode)
  :config
  (setq-default
   company-idle-delay .2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t))


;;=============================================================================
;; Configure company-tern
;;=============================================================================


;; https://github.com/proofit404/company-tern
(use-package company-tern
  :init (add-to-list 'company-backends 'company-tern)
  :config
  (setq-default
   company-tern-meta-as-single-line t
   company-tern-property-marker " *"))


(provide 'init-company)
;;; init-company.el ends here
