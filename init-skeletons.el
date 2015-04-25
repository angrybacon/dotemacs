;; Constants
(defconst me/name   "Mathieu Marques"             "My full name.")
(defconst me/email  "mathieumarques78@gmail.com"  "My email address.")


;; Header for Python files
(define-skeleton me/header-for-python
  "Prompt the user for package information and build a header with it."
  ""
  > "# -*- coding: utf-8 -*-" \n \n \n
  > "__author__ = " me/name \n
  > "__email__ = " (setq email (skeleton-read (concat "Email (" me/email "): "))) | me/email \n
  > "__copyright__ = Copyright (C) 2015 " me/name \n
  > "__license__ = " (setq license (skeleton-read "License (MIT): ")) "MIT" \n
  > "__version__ = " (setq version (skeleton-read "Version (0.1): ")) | "0.1" \n
  > \n)
(define-auto-insert "\\.py\\'" 'me/header-for-python)
