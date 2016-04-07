;;; init-eww.el --- All about Emacs Web Wowser

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 04 April 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure eww
;;=============================================================================


;; Built-in
(use-package eww
  :ensure nil
  :delight eww-mode "Emacs Web Wowser"
  :config (setq eww-header-line-format " %t: %u"))


(provide 'init-eww)
;;; init-eww.el ends here
