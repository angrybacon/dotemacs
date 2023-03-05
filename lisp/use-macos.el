;;; use-macos.el --- Augment MacOS experience        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)

  (use-package exec-path-from-shell
    :hook
    (after-init . exec-path-from-shell-initialize))

  (setq-default
   ns-alternate-modifier 'super         ; Map Super to the Alt key
   ns-command-modifier 'meta            ; Map Meta to the Cmd key
   ns-pop-up-frames nil))               ; Always re-use the same frame

;;; use-macos.el ends here
