;;─────────────────────────────────────────────────────────────────────────────
;; Define skeletons for different types of buffer
;;─────────────────────────────────────────────────────────────────────────────


(define-skeleton me/header-for-python
  "Prompt the user for package information and build a header with it."
  ""
  > "# -*- coding: utf-8 -*-" \n \n \n
  > "__author__ = " user-full-name \n
  > "__email__ = " (setq email (skeleton-read (concat "Email (" user-mail-address "): "))) | me/email \n
  > "__copyright__ = Copyright (C) 2015 " user-full-name \n
  > "__license__ = " (setq license (skeleton-read "License (MIT): ")) "MIT" \n
  > "__version__ = " (setq version (skeleton-read "Version (0.1): ")) | "0.1" \n
  > \n)


(define-auto-insert "\\.py\\'" 'me/header-for-python)


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-skeletons.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-skeletons)
