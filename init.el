;;; init.el --- My Emacs configuration

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 Oct 2014
;; Keywords: abbrev, convenience, faces, maint, outlines, vc
;; Homepage: https://github.com/angrybacon/dotemacs

;; This program is free software. It comes without any warranty, to the extent
;; permitted by applicable law. You can redistribute it and/or modify it under the
;; terms of the Do What The Fuck You Want To Public License, Version 2, as published
;; by Sam Hocevar. See http://www.wtfpl.net/ for more details.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it. This
;; process is known as tangling.
;;
;; See README.md for more details.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  ;; Mark directory-local variables as safe
  (defvar me/project-name nil)
  (put 'me/project-name 'safe-local-variable #'stringp)

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Install dependencies
  (unless (and (package-installed-p 'delight)
               (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'delight t)
    (package-install 'use-package t))
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t)

  ;; Use latest Org
  (use-package org
    :pin org
    :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
