;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-elpa.el
;;─────────────────────────────────────────────────────────────────────────────


;;─────────────────────────────────────────────────────────────────────────────
;; Initialize packages, see `elpa/'
;;─────────────────────────────────────────────────────────────────────────────


(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))


;; (eval-when-compile
;;   (require 'use-package))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-elpa.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-elpa)
