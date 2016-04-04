;;; init-projectile.el --- Add integration for project-related features within Emacs

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure projectile
;;=============================================================================


;; https://github.com/bbatsov/projectile
(use-package projectile

  :demand t

  :defines
  (projectile-enable-caching
   projectile-mode-line)

  :functions (me/projectile-project-name)

  :config
  (defvar me/project-name nil)
  (defun me/projectile-project-name (orig-fun &rest args)
    "Prefer `me/project-name' over default Projectile project string."
    (or me/project-name (apply orig-fun args)))
  (projectile-global-mode)
  (put 'me/project-name 'safe-local-variable #'stringp)
  (advice-add 'projectile-project-name :around #'me/projectile-project-name)
  (setq-default
   projectile-completion-system 'helm
   projectile-enable-caching t
   projectile-mode-line '(:eval (projectile-project-name))))


(provide 'init-projectile)
;;; init-projectile.el ends here
