;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: October 16, 2014
;; Homepage: https://github.com/angrybacon/dotemacs

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

(require 'org)

(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Tangle and compile if necessary only, then load the configuration
  (let* ((.org "dotemacs.org")
         (.el (concat (file-name-sans-extension .org) ".el"))
         (modification-time
          (file-attribute-modification-time (file-attributes .org))))
    (unless (org-file-newer-than-p .el modification-time)
      (org-babel-tangle-file .org .el "emacs-lisp")
      (byte-compile-file .el))
    (load-file .el))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))

;;; init.el ends here
