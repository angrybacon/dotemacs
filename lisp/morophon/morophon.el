;;; morophon.el --- Interact with themes             -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: November 29, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/morophon
;; Package-Requires: ((emacs "29.1"))

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

;; A collection of functions that interact with themes.

;;; Code:

(declare-function szadek-get "szadek")

(defgroup morophon nil
  "Interact with themes."
  :group 'convenience)

;;;; Alpha

(defcustom morophon-alpha-range '(5 . 100)
  "Accepted range for the frame alpha level."
  :type '(cons number number))

(defvar morophon--alpha nil
  "Variable to hold the current value for the alpha opacity in effect.
If nil, the opacity has not been modified yet. See `default-frame-alist'.")

(defun morophon-alpha-change (delta &optional frame)
  "Offset the the opacity level for FRAME by DELTA.
By default consider the current frame."
  (let* ((target-frame (or frame (selected-frame)))
         (minimum (car morophon-alpha-range))
         (maximum (cdr morophon-alpha-range))
         (old (frame-parameter target-frame 'alpha))
         (old (cond ((floatp old) (truncate (* old 100)))
                    ((numberp old) old)
                    (t maximum)))
         (new (+ old delta))
         (new (max minimum (min maximum new))))
    (set-frame-parameter target-frame 'alpha new)
    (setq morophon--alpha new)))

;;;###autoload
(defun morophon-alpha-less (&optional frame)
  "Decrease the opacity level for FRAME.
By default consider the current frame."
  (interactive)
  (morophon-alpha-change -5 frame))

;;;###autoload
(defun morophon-alpha-more (&optional frame)
  "Increase the opacity level for FRAME.
By default consider the current frame."
  (interactive)
  (morophon-alpha-change 5 frame))

;;;; Themes

(defvar morophon-after-load-theme-hook nil
  "Hook run after a theme is loaded with `load-theme'.")

(advice-add 'load-theme :after
  (defun morophon-after-load-theme-run-hooks (&rest _)
    "Run hooks set in `morophon-after-load-theme-hook'.
Inhibit `custom--inhibit-theme-enable' temporarily for the theme customization
that might happen within the hook."
    (let ((custom--inhibit-theme-enable nil))
      (run-hooks 'morophon-after-load-theme-hook))))

(defcustom morophon-known-themes '(modus-operandi modus-vivendi)
  "List of themes to take into account with `morophon-cycle'.
See `custom-available-themes'.")

;;;###autoload
(defun morophon-disable-themes ()
  "Disable all themes found in `custom-enable-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;;;###autoload
(defun morophon-cycle ()
  "Cycle through themes from `morophon-known-themes' in succession."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (next (or (cadr (memq current morophon-known-themes))
                   (car morophon-known-themes))))
    (morophon-disable-themes)
    (when next
      (load-theme next t))
    (message "%s" next)))

;;;; Typography

;;;###autoload
(defun morophon-typography-reset ()
  "Set the base settings for typography faces."
  (interactive)
  (let ((font-fixed (szadek-get 'font-fixed "monospace"))
        (font-size (szadek-get 'font-size 120))
        (font-variable (szadek-get 'font-variable "sans-serif")))
    (set-face-attribute 'default nil :font font-fixed :height font-size)
    (set-face-attribute 'fixed-pitch nil :font font-fixed :height font-size)
    (set-face-attribute 'mode-line nil :height font-size :inherit 'default)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
    (set-face-attribute 'variable-pitch nil :font font-variable)))

(provide 'morophon)

;;; morophon.el ends here
