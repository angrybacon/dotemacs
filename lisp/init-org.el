;;; init-org.el --- Enhance note-taking workflow

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure org
;;=============================================================================


;; http://orgmode.org/
(use-package org
  :defer t
  :delight org-mode "Org"
  :bind
  (("C-c o a" . org-agenda-list)
   ("C-c o b" . org-iswitchb)
   ("C-c o c" . org-captur)
   ("C-c o f" . org-cycle-agenda-files)
   ("C-c o l" . org-store-link)
   ("C-c o s" . org-search-view)
   ("C-c o t" . org-todo-list))
  :config (setq-default org-support-shift-select t))


(provide 'init-org)
;;; init-org.el ends here
