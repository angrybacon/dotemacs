;;─────────────────────────────────────────────────────────────────────────────
;; Initialize Melpa packages, see `elpa/'
;;─────────────────────────────────────────────────────────────────────────────


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-melpa.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-melpa)
