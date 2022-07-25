;;; morophon.el --- Interact with themes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

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

(defgroup morophon nil
  "Interact with themes."
  :group 'convenience)

;;;; Alpha

(defvar morophon--alpha nil
  "Variable to hold the current value for the alpha opacity in effect.
If nil, the opacity has not been modified yet. See `default-frame-alist'.")

(defun morophon-alpha-change (delta &optional frame)
  "Offset the the opacity level for FRAME by DELTA.
By default consider the current frame."
  (let* ((target-frame (or frame (selected-frame)))
         (old (frame-parameter target-frame 'alpha))
         (old (cond ((floatp old) (truncate (* old 100)))
                    ((numberp old) old)
                    (t 100)))
         (new (+ old delta)))
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

(provide 'morophon)

;;; morophon.el ends here
