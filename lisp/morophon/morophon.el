;;; morophon.el --- Collection of theme functions -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mathieu Marques

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

(defcustom morophon-known-themes '(modus-operandi modus-vivendi)
  "List of themes to take into account with `morophon-cycle'.
See `custom-available-themes'.")


(defun morophon-disable-themes ()
  "Disable all themes found in `custom-enable-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

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
