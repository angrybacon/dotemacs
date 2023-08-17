;;; use-dired.el --- Customize the file explorer     -*- lexical-binding: t; -*-
;;; Commentary:

;; The Dire package serves as a repository for all my Dired commands and
;; helpers. Some of them will be bound directly to =dired-mode-map=, others will
;; remain unbound interactive commands.

;;; Code:

(use-package dire
  :load-path "lisp/dire"
  :bind
  (:map dired-mode-map
   ("C-<return>" . dire-open-externally)))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Aghov --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode))

;;; use-dired.el ends here
