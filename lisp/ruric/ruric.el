;;; ruric.el --- Pair-programming features           -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Mathieu Marques

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
;; Provide a global minor mode to enable the feature in all buffers.

;;; Code:

(declare-function cl-position "cl-seq")

(defgroup ruric nil
  "Pair-programming features."
  :group 'convenience)

(defcustom ruric-blacklist-modes nil
  "List of modes that should not be considered for `ruric-mode'."
  :type '(repeat symbol))

;;;; Line numbers

;;;###autoload
(defun ruric-toggle-line-numbers ()
  "Cycle through the possible values of `display-line-numbers'.
Cycle between nil, t and `'relative'."
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

(define-minor-mode ruric-local-mode
  "Better pair-programming environment."
  :init-value nil
  (if ruric-local-mode
      (when display-line-numbers-mode
        (setq-local
         ruric--initial-display-line-numbers-type display-line-numbers
         display-line-numbers t))
    (when display-line-numbers-mode
      (setq-local
       display-line-numbers ruric--initial-display-line-numbers-type
       ruric--initial-display-line-numbers-type nil))))

(defun ruric--off ()
  "Turn `ruric-local-mode' off."
  (ruric-local-mode -1))

(defun ruric--on ()
  "Turn `ruric-local-mode' on."
  (unless (or noninteractive (memq major-mode ruric-blacklist-modes))
    (ruric-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode ruric-mode ruric-local-mode ruric--on
  :group 'ruric)

(provide 'ruric)

;;; ruric.el ends here
