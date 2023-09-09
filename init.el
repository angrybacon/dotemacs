;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

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

(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  ;; Reduce fanfare
  ;; TODO See https://yrh.dev/blog/rant-obfuscation-in-emacs/
  (setq inhibit-startup-echo-area-message "angrybacon")

  ;; Set up packages
  (add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
  (load "use-packages")

  ;; Load these first to avoid flickering
  (load "use-core")
  (load "use-defaults")
  (load "use-interface")
  (load "use-macos")

  ;; Load partials
  (load "use-completion")
  (load "use-dired")
  (load "use-display")
  (load "use-evil")
  (load "use-git")
  (load "use-help")
  (load "use-http")
  (load "use-hydra")
  (load "use-languages")
  (load "use-lint")
  (load "use-lsp")
  (load "use-mouse")
  (load "use-parens")
  (load "use-presentation")
  (load "use-project")
  (load "use-qol")
  (load "use-templates")
  (load "use-terminal")
  (load "use-workspaces")

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect)

  ;; Log the start-up time
  (add-hook 'emacs-startup-hook #'(lambda () (message "%s" (emacs-init-time)))))

;;; init.el ends here
