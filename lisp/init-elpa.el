;;; init-elpa.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Initialize packages, see elpa/.

;;; Code:


;;=============================================================================
;; Initialize packages from Melpa
;;=============================================================================


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


(provide 'init-elpa)
;;; init-elpa.el ends here
