;;; dire.el --- Helpers for Dired                    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: July 29, 2022
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/dire

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

;; Collection of helpers to use in Dired buffers.

;;; Code:

(declare-function dired-get-file-for-visit "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-get-marked-files "dired")
(declare-function dired-toggle-marks "dired")
(declare-function dired-unmark-all-marks "dired")

(defgroup dire nil
  "Helpers for Dired."
  :group 'dired)

;;;; Helpers

(defun dire--propertize-directory (directory)
  "Return a propertize string representing DIRECTORY."
  (propertize directory 'face '(:inherit dired-directory)))

(defun dire--propertize-file (file)
  "Return a propertize string representing FILE."
  (propertize file 'face '(:inherit dired-marked :inverse-video t)))

;;;; Commands

;;;###autoload
(defun dire-open-externally (argument)
  "Open file under point with an external program.
With positive ARGUMENT, prompt for the command to use."
  (interactive "P")
  (let* ((file (dired-get-file-for-visit))
         (command (and (not argument)
                       (pcase system-type
                         ('darwin "open")
                         ((or 'gnu 'gnu/kfreebsd 'gnu/linux) "xdg-open"))))
         (command (or command (read-shell-command "[Dire] Open with? "))))
    (call-process command nil 0 nil file)))

;;;; Renaming

(defcustom dire-movie-re (rx bos
                             (group (+ alpha) (* (and "." (+ alpha))))
                             "."
                             (group digit digit digit digit)
                             "."
                             (* not-newline)
                             eos)
  "Regular expression used to identify parts of a movie file name.
The first group should capture the name of the movie and the second group should
capture the year of release."
  :type 'regexp)

(defvar dire--rename-history nil
  "Keep the last rename operations around for reverting purposes.
Each entry in the list should be a tuple representing the old and new names.")

(defun dire-movies-get-compliant-name (input)
  "Return a compliant movie file name from INPUT."
  (let ((name (if (string-match dire-movie-re input)
                  (replace-regexp-in-string dire-movie-re "\\1 (\\2)" input)
                input)))
    ;; TODO Enforce proper title casing
    (subst-char-in-string ?. ?\s name :in-place)))

(defun dire-movies-rename-movie (file)
  "Rename the provided FILE to be compliant with most media servers.
If FILE is not a directory, rename it accordingly.
If FILE is a directory, do nothing.

Files are renamed to be compliant with most media servers:

  Movie Title in Title Case (2022)/
  ├── Movie Title in Title Case (2022).en.srt
  ├── Movie Title in Title Case (2022).fr.srt
  └── Movie Title in Title Case (2022).mp4

If a movie title cannot be guessed from FILE, print a warning and do nothing.
On successful rename, populate `dire--rename-history'."
  ;; TODO Handle directories
  (unless (file-directory-p file)
    (if-let* ((new (dire-movies-get-compliant-name (file-name-base file))))
        (let ((old (file-name-nondirectory file))
              (new (file-name-with-extension new (file-name-extension file))))
          (when (yes-or-no-p (format "[Dire] Rename %s to %s?"
                                     (dire--propertize-file old)
                                     (dire--propertize-file new)))
            (condition-case nil
                (progn
                  (rename-file old new 0)
                  (message "[Dire] Renamed `%s' to `%s'" old new)
                  (push (cons old new) dire--rename-history))
              ('file-already-exists (message "[Dire] Ignored `%s'" old)))))
      (user-error "[Dire] Could not guess compliant name from `%s'" file))))

;;;###autoload
(defun dire-movies-rename-dwim ()
  "Clean up movie files in the current `dired-mode' buffer.
Apply changes on marked entries only or currently selected entry."
  (interactive)
  (mapc #'dire-movies-rename-movie (dired-get-marked-files)))

;;;###autoload
(defun dire-rename-undo ()
  "Read `dire--rename-history' and revert all items contained within."
  (interactive)
  (let ((count (length dire--rename-history)))
    (if (zerop count)
        (message "[Dire] Nothing to revert")
      (dolist (entry dire--rename-history)
        (pcase-let ((`(,old . ,new) entry))
          (rename-file new old 0)))
      (message "[Dire] Reverted %d entr%s" count (if (> count 1) "ies" "y"))))
  ;; TODO Clean history progressively in case of errors
  (setq dire--rename-history nil))

(provide 'dire)

;;; dire.el ends here
