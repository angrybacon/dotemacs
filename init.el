;; Packages
;; ─────────────────────────────────────────────────────────────────────────────

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


;; Configuration files
;; ─────────────────────────────────────────────────────────────────────────────

(load "~/.emacs.d/init/interface.el")
(load "~/.emacs.d/init/aliases.el")
(load "~/.emacs.d/init/modes.el")
(load "~/.emacs.d/init/shortcuts.el")
(load "~/.emacs.d/init/theme.el")
