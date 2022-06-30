;;; ruric.el --- Pair-programming features -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: June 30, 2022
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/ruric
;; Package-Requires: ((emacs "29.0.50"))

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

;; Summon the two-headed ogre to enhance settings with regards to
;; pair-programming.

;;; Code:

(require 'cl-seq)

(defgroup ruric nil
  "Pair-programming features."
  :group 'convenience
  :prefix "ruric-")

;;;; Line numbers

;;;###autoload
(defun ruric-toggle-line-numbers ()
  "Cycle through the possible values of `display-line-numbers'.
Cycle between nil, t and 'relative."
  (interactive)
  (let* ((range '(nil t relative))
         (position (1+ (cl-position display-line-numbers range)))
         (position (if (= position (length range)) 0 position)))
    (setq-local display-line-numbers (nth position range))))

;;;###autoload
(defun ruric-toggle-line-numbers-absolute ()
  "Toggle the value of `display-line-numbers-current-absolute'."
  (interactive)
  (let ((value display-line-numbers-current-absolute))
    (setq-local display-line-numbers-current-absolute (not value))))

(defvar ruric--initial-display-line-numbers-type nil
  "Previous value for `display-line-numbers' to reset to.")

(provide 'ruric)

;;; ruric.el ends here
