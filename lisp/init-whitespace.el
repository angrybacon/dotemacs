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
;; Silence byte-compiler
;;=============================================================================


(defvar zenburn/bg+2)
(defvar zenburn/bg+1)
(defvar zenburn/red)


;;=============================================================================
;; Configure Whitespace mode
;;=============================================================================


(require 'use-package)


;; Built-in
(use-package whitespace
  :config
  (set-face-attribute 'whitespace-empty nil :background zenburn/bg+1)
  (set-face-attribute 'whitespace-indentation nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-after-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-before-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space nil :background 'unspecified :foreground zenburn/bg+2))


(provide 'init-whitespace)
;;; init-whitespace.el ends here
