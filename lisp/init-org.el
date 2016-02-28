;;; init-org.el --- Enhance note-taking workflow

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Org
;;=============================================================================


;; Website: http://orgmode.org/
(use-package org
  :delight org-mode "Org"
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b" . org-iswitchb)
   ("C-c o l" . org-store-link))
  :init (setq-default org-support-shift-select t))


(provide 'init-org)
;;; init-org.el ends here
