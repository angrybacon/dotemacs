;;; init-projectile.el --- Add integration for project-related features within Emacs

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created:  May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Projectile
;;=============================================================================


;; Website: https://github.com/bbatsov/projectile
(use-package projectile

  :preface (defvar project-name nil)

  :defines
  (projectile-enable-caching
   projectile-mode-line)

  :functions (projectile-project-name--prefer-mine)

  :config
  (setq-default
   projectile-enable-caching t
   projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
  (put 'project-name 'safe-local-variable #'stringp)
  (advice-add 'projectile-project-name :around #'projectile-project-name--prefer-mine)
  (defun projectile-project-name--prefer-mine (orig-fun &rest args)
    "Prefer `project-name' over default Projectile project string."
    (or project-name (apply orig-fun args)))
  (projectile-global-mode)

  ;;======================================
  ;; Configure Helm Projectile
  ;;======================================

  (use-package helm-projectile
    :config (helm-projectile-on)))


(provide 'init-projectile)
;;; init-projectile.el ends here
