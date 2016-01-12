;;; init-whitespace.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 23 May 2015
;; Keywords: convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Highlight whitespace-like characters.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)
(defvar zenburn/red-1)


;;=============================================================================
;; Configure Whitespace Mode
;;=============================================================================


;; Built-in
(use-package whitespace

  :init
  (setq whitespace-style '(face empty tab trailing))

  :config
  (set-face-attribute 'whitespace-empty nil :background zenburn/red-1)
  (set-face-attribute 'whitespace-tab nil :background zenburn/red-1)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/red-1)
  (global-whitespace-mode 1))


(provide 'init-whitespace)
;;; init-whitespace.el ends here
