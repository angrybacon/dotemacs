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
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)
(defvar project-name nil)
(put 'project-name 'safe-local-variable #'stringp)


;;=============================================================================
;; Configure Projectile
;;=============================================================================


;; Website: https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t

  :init
  (setq
   projectile-enable-caching t
   projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
  (advice-add 'projectile-project-name :around #'projectile-project-name--prefer-mine)

  :config

  (defun projectile-project-name--prefer-mine (orig-fun &rest args)
    "Prefer `project-name' over default Projectile project string."
    (or project-name (apply orig-fun args)))

  (projectile-global-mode)

  ;; Configure Helm Projectile
  ;; =====================================

  (use-package helm-projectile
    :ensure t

    :init
    (require 'grep)

    :config
    (helm-projectile-on)))


(provide 'init-projectile)
;;; init-projectile.el ends here
