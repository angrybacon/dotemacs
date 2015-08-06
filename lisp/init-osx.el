;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-osx.el
;;─────────────────────────────────────────────────────────────────────────────


(defvar ns-command-modifier)
(defvar ns-option-modifier)


;;─────────────────────────────────────────────────────────────────────────────
;; Settings for OSX builds only
;;─────────────────────────────────────────────────────────────────────────────


(when (eq system-type 'darwin)
  (setq
   exec-path (append exec-path '("/usr/local/bin"))  ; Add path to binaries installed with Homebrew
   ns-command-modifier 'meta                         ; Map the Meta key to the `cmd' key
   ns-option-modifier nil))                          ; Disable the `alt' key


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-osx.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-osx)
