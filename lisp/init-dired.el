;;; init-dired.el --- All about Dired

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure dired
;;=============================================================================


;; Built-in
(use-package dired
  :ensure nil
  :defer t
  :delight dired-mode "Dired"
  :config
  (setq-default
   dired-auto-revert-buffer t
   dired-listing-switches "-alh"
   dired-ls-F-marks-symlinks nil
   dired-recursive-copies 'always))


(provide 'init-dired)
;;; init-dired.el ends here
