;;; init.el --- My Emacs configuration

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: October 16, 2014
;; Homepage: https://github.com/angrybacon/dotemacs
;; Keywords: abbrev, convenience, faces, maint, outlines, vc

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

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
    :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
