;;; ruric.el --- Pair-programming features           -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

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

;; Collection of functions to enhance settings with regards to pair-programming.
;; Provide a minor mode and its global variant as a convenient shortcut.

;;; Code:

(require 'cl-seq)

(defgroup ruric nil
  "Pair-programming features."
  :group 'convenience)

(defcustom ruric-blacklist-modes nil
  "List of modes that should not be considered for `ruric-global-mode'."
  :type '(repeat symbol))

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

;;;; Modes

;;;###autoload
(define-minor-mode ruric-mode
  "Better pair-programming environment."
  :global nil
  :group 'ruric
  (cond
   (ruric-mode
    (setq-local ruric--initial-display-line-numbers-type display-line-numbers)
    (setq-local display-line-numbers t))
   (t
    (setq-local display-line-numbers ruric--initial-display-line-numbers-type)
    (setq-local ruric--initial-display-line-numbers-type nil))))

(defun ruric--on ()
  "Turn `ruric-mode' on."
  (unless (or noninteractive (memq major-mode ruric-blacklist-modes))
    (ruric-mode 1)))

;;;###autoload
(define-globalized-minor-mode ruric-global-mode ruric-mode ruric--on
  :group 'ruric)

(provide 'ruric)

;;; ruric.el ends here
