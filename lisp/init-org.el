;;; init-org.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: abbrev, convenience, outlines
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Enhance note-taking workflow.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)


;;=============================================================================
;; Configure Org mode
;;=============================================================================


;; Website: http://orgmode.org/
(use-package org
  :ensure t
  :delight org-mode "Org"
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b" . org-iswitchb)
   ("C-c o l" . org-store-link)))


(provide 'init-org)
;;; init-org.el ends here
