;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) Mathieu Marques

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

(defun me/load (file)
  "Load FILE, prefixing any error with FILE's name."
  (condition-case message
      (load file)
    (error (error "[%s] %s" file (error-message-string message)))))

(let ((file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (mode-line-format nil)
      (read-process-output-max (* 1024 1024)))

  ;; Reduce fanfare
  ;; TODO See https://yrh.dev/blog/rant-obfuscation-in-emacs/
  (setq inhibit-startup-echo-area-message "angrybacon")

  ;; Set up packages
  (add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
  (me/load "use-doctor")
  (me/load "use-packages")

  ;; Load these first to avoid flickering
  (me/load "use-core")
  (me/load "use-defaults")
  (me/load "use-interface")
  (me/load "use-os")

  ;; Load partials
  (me/load "use-agent")
  (me/load "use-applications")
  (me/load "use-completion")
  (me/load "use-dired")
  (me/load "use-display")
  (me/load "use-evil")
  (me/load "use-git")
  (me/load "use-help")
  (me/load "use-languages")
  (me/load "use-lint")
  (me/load "use-lsp")
  (me/load "use-mouse")
  (me/load "use-parentheses")
  (me/load "use-presentation")
  (me/load "use-project")
  (me/load "use-qol")
  (me/load "use-templates")
  (me/load "use-workspaces")

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect)

  ;; Log the start-up time
  (add-hook 'emacs-startup-hook #'(lambda () (message "%s" (emacs-init-time)))))

;;; init.el ends here
