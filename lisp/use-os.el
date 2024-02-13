;;; use-os.el --- Augment Os experience              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)

  (use-package exec-path-from-shell
    :custom
    (exec-path-from-shell-variables '("ANDROID_HOME" "MANPATH" "PATH"))
    :hook
    (after-init . exec-path-from-shell-initialize))

  ;; TODO https://github.com/KaratasFurkan/.emacs.d/tree/emacs-29#pixel-scroll

  (use-package pixel-scroll
    :ensure nil
    :custom
    (pixel-scroll-precision-use-momentum t)
    :hook
    (after-init . pixel-scroll-precision-mode))

  (setq-default
   ns-alternate-modifier 'super         ; Map Super to the Alt key
   ns-command-modifier 'meta            ; Map Meta to the Cmd key
   ns-pop-up-frames nil))               ; Always re-use the same frame

;;; use-os.el ends here
