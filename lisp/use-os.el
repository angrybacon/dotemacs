;;; use-os.el --- Customize OS-specific behavior     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)

  (setq-default
   ns-alternate-modifier 'super         ; Map Super to the Alt key
   ns-command-modifier 'meta            ; Map Meta to the Cmd key
   ns-pop-up-frames nil)                ; Always re-use the same frame

  ;; TODO See https://github.com/purcell/exec-path-from-shell?tab=readme-ov-file#making-exec-path-from-shell-faster
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))

  (use-package pixel-scroll
    ;; NOTE A bug in `pixel-scroll-mode' from Emacs 29 sometimes halts the
    ;;      current scroll motion.
    :ensure nil
    :custom
    (pixel-scroll-precision-use-momentum t)
    :hook
    (after-init . pixel-scroll-precision-mode)))

;;; use-os.el ends here
