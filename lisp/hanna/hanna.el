;;; hanna.el --- Collection of navigation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: November 29, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/hanna

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

;; All aboard the Weatherlight! Summon Hanna to help you navigate through the
;; mist of your buffers.

;;; Code:

(declare-function dired-move-to-filename "dired")

;;;; Jump

;;;###autoload
(defun hanna-beginning-of-line ()
  "Move point to first non-whitespace character, or beginning of line."
  (interactive "^")
  (if (eq major-mode 'dired-mode)
      (dired-move-to-filename)
    (if (bolp)
        (back-to-indentation)
      (beginning-of-line))) )

;;;###autoload
(defun hanna-paragraph-backward ()
  "Move backward to start of paragraph."
  (interactive "^")
  (skip-chars-backward "\n")
  (unless (search-backward-regexp "\n[[:blank:]]*\n" nil t)
    (goto-char (point-min)))
  (skip-chars-forward "\n"))

;;;###autoload
(defun hanna-paragraph-forward ()
  "Move forward to start of next paragraph."
  (interactive "^")
  (skip-chars-forward "\n")
  (unless (search-forward-regexp "\n[[:blank:]]*\n" nil t)
    (goto-char (point-max)))
  (skip-chars-forward "\n"))

;;;; Scroll

(defun hanna-scroll-horizontal (amount)
  "Scroll the window horizontall by AMOUNT increment in columns.
A positive value means to scroll towards the right, as opposed to negative value
which scroll the content leftwards."
  (interactive)
  (let* ((position (mouse-position))
         (window (window-at (cadr position) (cddr position))))
    (with-selected-window window
      (if truncate-lines
          (scroll-left amount)
        (message "[Hanna] Nothing to scroll")))))

(mapc (lambda (n)
        (let ((documentation (format "Scroll horizontally by %+d columns." n))
              (name (intern (format "hanna-scroll-horizontal%+d" n))))
          (eval `(defun ,name ()
                   ,documentation
                   (interactive)
                   (hanna-scroll-horizontal ,n)))))
      '(2 4 8 -2 -4 -8))

(provide 'hanna)

;;; hanna.el ends here
