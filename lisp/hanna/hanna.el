;;; hanna.el --- Collection of navigation functions -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

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

;; Functions to move around in buffers. Mostly redefine over Emacs' opinions.

;;; Code:

(defun hanna-beginning-of-line ()
  "Move point to first non-whitespace character, or beginning of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))

(defun hanna-paragraph-backward ()
  "Move backward to start of paragraph."
  (interactive "^")
  (skip-chars-backward "\n")
  (unless (search-backward-regexp "\n[[:blank:]]*\n" nil t)
    (goto-char (point-min)))
  (skip-chars-forward "\n"))

(defun hanna-paragraph-forward ()
  "Move forward to start of next paragraph."
  (interactive "^")
  (skip-chars-forward "\n")
  (unless (search-forward-regexp "\n[[:blank:]]*\n" nil t)
    (goto-char (point-max)))
  (skip-chars-forward "\n"))

(provide 'hanna)

;;; hanna.el ends here
