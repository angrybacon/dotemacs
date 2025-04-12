;;; early-init.el --- Early Emacs configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: January 05, 2021
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

;; Since Emacs 27, an early configuration file early-init.el can be provided to
;; handle initialization to be done before init.el is loaded.

;;; Code:

(when (getenv-internal "DEBUG")
  (setq-default
   debug-on-error t
   init-file-debug t))

(setq-default
 default-frame-alist
 '((background-color . "#3F3F3F")       ; Default background color
   (bottom-divider-width . 4)           ; Thin horizontal window divider
   (foreground-color . "#DCDCCC")       ; Default foreground color
   (fullscreen . maximized)             ; Maximize the window by default
   (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
   (left-fringe . 8)                    ; Thin left fringe
   (menu-bar-lines . 0)                 ; No menu bar
   (right-divider-width . 4)            ; Thin vertical window divider
   (right-fringe . 8)                   ; Thin right fringe
   (tool-bar-lines . 0)                 ; No tool bar
   (undecorated-round . t)              ; Enable round corners
   (vertical-scroll-bars . nil))        ; No vertical scroll-bars
 load-prefer-newer t)                   ; Avoid old byte-compiled dependencies

;;; early-init.el ends here
