;;; init-elpa.el --- Initialize packages, see elpa/

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Initialize packages from Melpa
;;=============================================================================


(when (>= emacs-major-version 24)
  (setq-default package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (unless (package-installed-p 'delight)
    (package-refresh-contents)
    (package-install 'delight))
  (setq-default use-package-always-ensure t))
(eval-when-compile (require 'use-package))


(provide 'init-elpa)
;;; init-elpa.el ends here
