;;; init-projectile.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created:  May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Add integration for project-related features within Emacs.

;;; Code:


;;=============================================================================
;; Silence byte-compiler
;;=============================================================================


(defvar project-name nil)
(put 'project-name 'safe-local-variable #'stringp)


;;=============================================================================
;; Configure Projectile
;;=============================================================================


(require 'use-package)


;; Website: https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :init
  (advice-add 'projectile-project-name :around #'projectile-project-name--prefer-mine)
  (setq
   projectile-enable-caching t
   projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
  :config

  (defun projectile-project-name--prefer-mine (orig-fun &rest args)
    "Prefer `project-name' over default Projectile project string."
    (or project-name (apply orig-fun args)))

  (use-package helm-projectile
    :init
    (require 'grep)
    :config
    (helm-projectile-on))

  (projectile-global-mode))


(provide 'init-projectile)
;;; init-projectile.el ends here
