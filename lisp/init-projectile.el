(require 'use-package)


(defvar project-name nil)
(put 'project-name 'safe-local-variable #'stringp)


(defun projectile-project-name--prefer-mine (orig-fun &rest args)
  "Prefer `project-name' over default Projectile project string."

  (or project-name (apply orig-fun args)))


;;─────────────────────────────────────────────────────────────────────────────
;; Add support for project management
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :init
  (advice-add 'projectile-project-name :around #'projectile-project-name--prefer-mine)
  (setq
   projectile-enable-caching t
   projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
  :config

  (use-package helm-projectile
    :init
    (require 'grep)
    :config
    (helm-projectile-on))

  (projectile-global-mode))


(provide 'init-projectile)
