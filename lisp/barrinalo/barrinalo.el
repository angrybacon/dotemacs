;;; barrinalo.el --- Collection of text transformers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: November 29, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/barrinalo

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

;; Barrinalo was a powerful blue mage from Dominaria and scribe at the Tolarian
;; Academy. Comparably, barrinalo.el is a collection of small interactive
;; functions that help you change text at point or in region.

;;; Code:

(require 'seq)                          ; `seq-sort-by'
(require 'simple)                       ; `count-lines' `cycle-spacing'
                                        ; `delete-blank-lines'
                                        ; `move-end-of-line' `newline'
                                        ; `push-mark' `transpose-line'
                                        ; `use-region-p'
(require 'sort)                         ; `reverse-region' `sort-regexp-fields'

;;;; Date commands

;;;###autoload
(defun barrinalo-date-iso ()
  "Insert the current date, ISO format eg. 2016-12-09."
  (interactive "*")
  (insert (format-time-string "%F")))

;;;###autoload
(defun barrinalo-date-iso-with-time ()
  "Insert the current date, ISO format with time eg. 2016-12-09T14:34:54+0100."
  (interactive "*")
  (insert (format-time-string "%FT%T%z")))

;;;###autoload
(defun barrinalo-date-long ()
  "Insert the current date, long format eg. December 09, 2016."
  (interactive "*")
  (insert (format-time-string "%B %d, %Y")))

;;;###autoload
(defun barrinalo-date-long-with-time ()
  "Insert the current date, long format eg. December 09, 2016 - 14:34."
  (interactive "*")
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

;;;###autoload
(defun barrinalo-date-short ()
  "Insert the current date, short format eg. 2016.12.09."
  (interactive "*")
  (insert (format-time-string "%Y.%m.%d")))

;;;###autoload
(defun barrinalo-date-short-with-time ()
  "Insert the current date, short format with time eg. 2016.12.09 14:34."
  (interactive "*")
  (insert (format-time-string "%Y.%m.%d %H:%M")))

;;;; Duplicate functions

(defun barrinalo--duplicate-line (&optional stay)
  "Duplicate current line.
With optional argument STAY true, leave point where it was."
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (pos-bol) (pos-eol))))
    (newline))
  (unless stay
    (let ((column (current-column)))
      (forward-line)
      (forward-char column))))

;;;###autoload
(defun barrinalo-duplicate-backward ()
  "Duplicate current line upward or region backward.
If region was active, keep it so that the command can be repeated."
  (interactive "*")
  (if (use-region-p)
      (let (deactivate-mark)
        (save-excursion
          (insert (buffer-substring (region-beginning) (region-end)))))
    (barrinalo--duplicate-line t)))

;;;###autoload
(defun barrinalo-duplicate-forward ()
  "Duplicate current line downward or region forward.
If region was active, keep it so that the command can be repeated."
  (interactive "*")
  (if (use-region-p)
      (let (deactivate-mark (point (point)))
        (insert (buffer-substring (region-beginning) (region-end)))
        (push-mark point))
    (barrinalo--duplicate-line)))

;;;; Indent functions

;;;###autoload
(defun barrinalo-cycle-spacing ()
  "If within blank lines delete all but one or cycle spacing around point."
  (interactive)
  (if (looking-at "[[:blank:]]*$")
      (delete-blank-lines)
    (cycle-spacing)))

;;;###autoload
(defun barrinalo-shift-left (begin end)
  "Shift lines between BEGIN and END leftward by one space and keep mark."
  (interactive "*r")
  (indent-rigidly-left begin end)
  (setq deactivate-mark nil))

;;;###autoload
(defun barrinalo-shift-left-tab (begin end)
  "Indent lines between BEGIN and END leftward to a tab stop and keep mark."
  (interactive "*r")
  (indent-rigidly-left-to-tab-stop begin end)
  (setq deactivate-mark nil))

;;;###autoload
(defun barrinalo-shift-right (begin end)
  "Shift lines between BEGIN and END rightward by one space and keep mark."
  (interactive "*r")
  (indent-rigidly-right begin end)
  (setq deactivate-mark nil))

;;;###autoload
(defun barrinalo-shift-right-tab (begin end)
  "Indent lines between BEGIN and END rightward to a tab stop and keep mark."
  (interactive "*r")
  (indent-rigidly-right-to-tab-stop begin end)
  (setq deactivate-mark nil))

;;;; Transpose functions

;;;###autoload
(defun barrinalo-reverse (begin end)
  "Reverse region.
Reverse lines between BEGIN and END. If region only has one line, reverse
characters in region instead."
  (interactive "*r")
  (if (= (count-lines begin end) 1)
      (insert (nreverse (delete-and-extract-region begin end)))
    (reverse-region begin end)))

;;;###autoload
(defun barrinalo-sort-numbers (reverse begin end)
  "Sort numbers in region by natural order.
Sort numbers between BEGIN and END, or region if called interactively.
When prefixed with \\[universal-agument], sort in REVERSE order instead."
  (interactive "*P\nr")
  (let* ((text (delete-and-extract-region begin end))
         (separators (split-string text (rx (+ digit))))
         (sorter (if reverse #'> #'<))
         (numbers (split-string text (rx (+ (not digit))) t))
         (numbers (seq-sort-by #'string-to-number sorter numbers)))
    (goto-char begin)
    (dotimes (i (length separators))
      (insert (concat (nth i separators) (nth i numbers))))))

;;;###autoload
(defun barrinalo-sort-words (reverse begin end)
  "Sort words in region alphabetically.
Sort words between BEGIN and END, or region is called interactively.
When prefixed with \\[universal-argument], sort in REVERSE order instead.

The variable `sort-fold-case' determines whether the case affects the sort. See
`sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" begin end))

;;;###autoload
(defun barrinalo-swap-down ()
  "Move down the line under point."
  (interactive "*")
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun barrinalo-swap-up ()
  "Move up the line under point."
  (interactive "*")
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(provide 'barrinalo)

;;; barrinalo.el ends here
