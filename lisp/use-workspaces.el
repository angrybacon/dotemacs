;;; use-workspaces.el --- Saving groups of windows   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Persistence

(use-package desktop
  :ensure nil
  :defer 1
  :config
  (desktop-read)
  (desktop-save-mode)
  :custom
  (desktop-base-file-name (shelldock "desktop"))
  (desktop-base-lock-name (shelldock "desktop.lock"))
  (desktop-restore-eager 4))

;;;; Workspaces

(defun me/eyebrowse-switch (n)
  "Switch to configuration N or to the last visited."
  (if (eq (eyebrowse--get 'current-slot) n)
      (eyebrowse-last-window-config)
    (funcall (intern (format "eyebrowse-switch-to-window-config-%s" n)))))

(dotimes (n 9)
  (let* ((n (1+ n))
         (name (intern (format "me/eyebrowse-switch-%s" n)))
         (documentation
          (format "Switch to configuration %s or to the last visited." n)))
    (eval `(defun ,name ()
             ,documentation
             (interactive)
             (me/eyebrowse-switch ,n))
          t)))

;; TODO Replace with built-in tabs
(use-package eyebrowse
  :bind
  ("M-1" . me/eyebrowse-switch-1)
  ("M-2" . me/eyebrowse-switch-2)
  ("M-3" . me/eyebrowse-switch-3)
  ("M-4" . me/eyebrowse-switch-4)
  ("M-5" . me/eyebrowse-switch-5)
  ("M-6" . me/eyebrowse-switch-6)
  ("M-7" . me/eyebrowse-switch-7)
  ("M-8" . me/eyebrowse-switch-8)
  ("M-9" . me/eyebrowse-switch-9)
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-mode-line-style nil)
  (eyebrowse-new-workspace t))

;;; use-workspaces.el ends here
