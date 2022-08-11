;;; dire.el --- Helpers for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

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

(require 'dired)
(require 'subr-x)

(defgroup dire nil
  "Helpers for Dired."
  :group 'dired)

;;;; Helpers

(defun dire--propertize-directory (directory)
  "Return a propertize string representing DIRECTORY."
  (propertize directory 'face '(:inherit dired-directory)))

(defun dire--propertize-file (file)
  "Return a propertize string representing FILE."
  (propertize file 'face '(:inherit default)))

(defun dire-marked-files ()
  "Return the currently marked files."
  (let* ((files (dired-get-marked-files nil nil nil :distinguish-one-marked))
         (count (length files)))
    (pcase count
      (1 nil)
      (2 (if (eq (car files) t)
             (cdr files)
           files))
      (_ files))))

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
  "Regular expression used to identify parts of a movie file name."
  :type 'regexp)

(defvar dire--rename-list nil
  "Keep the last rename operations around for reverting purposes.
Each entry in the list should be a tuple representing the old and new names.")

(defun dire-movies-add-extension (movie extension)
  "Return a new string representing MOVIE with EXTENSION.
If EXTENSION is nil, don't add a dot separator."
  (if extension
      (concat movie "." extension)
    movie))

(defun dire-movies-get-compliant-name (name)
  "Return a compliant movie file name from NAME or nil."
  (when (string-match dire-movie-re name)
    ;; TODO Enforce proper title casing
    (replace-regexp-in-string dire-movie-re "\\1 (\\2)" name)))

(defun dire--movies-rename-movie-file (old new extension)
  "Sub-routine for `dire-movies-rename-movie' to handle non-directory files.
OLD is the current filename and NEW is the new name to use both without
extension. EXTENSION is the extension to consider.
See `dire--movies-rename-movie-directory' for the directory counterpart."
  (when-let* ((left (dire--propertize-file old))
              (right (dire--propertize-file new))
              (confirm (yes-or-no-p
                        (format "[Dire] Rename `%s' to `%s'?" left right)))
              (old (dire-movies-add-extension old extension))
              (new (dire-movies-add-extension new extension)))
    (condition-case nil
        (progn
          (rename-file old new 0)
          (message "[Dire] Renamed `%s' to `%s'" old new)
          (push (cons old new) dire--rename-list))
      ('file-already-exists (message "[Dire] Ignored `%s'" old)))))

(defun dire-movies-rename-movie (file)
  "Rename the provided FILE to be compliant with most media servers.
If FILE is a directory, rename it, enter it and rename the expected files that
are contained there.

Files are renamed to be compliant with most media servers:

  Movie Title in Title Case (2022)/
  ├── Movie Title in Title Case (2022).en.srt
  ├── Movie Title in Title Case (2022).fr.srt
  └── Movie Title in Title Case (2022).mp4

If a movie title cannot be guessed from FILE, do nothing."
  (if-let* ((file (or file (dired-get-filename)))
            (base (file-name-base file)))
      (when-let ((compliant-name (dire-movies-get-compliant-name base)))
        (if (file-directory-p file)
            (progn
              ;; TODO Handle directories
              )
          (dire--movies-rename-movie-file base
                                          compliant-name
                                          (file-name-extension file))))
    (user-error "[Dire] No file selected")))

;;;###autoload
(defun dire-movies-rename-dwim ()
  "Clean up movie files and directories in the current `dired-mode' buffer.
If no entries are marked, mark all files beforehand. If some are marked,
consider those only."
  (interactive)
  (let ((should-mark-all (not (dire-marked-files))))
    (when should-mark-all
      (dired-unmark-all-marks)
      (dired-toggle-marks))
    (mapc #'dire-movies-rename-movie (dire-marked-files))
    (when should-mark-all
      (dired-unmark-all-marks))))

;;;###autoload
(defun dire-rename-undo ()
  "Read `dire--rename-list' and revert all items contained within."
  (interactive)
  (let ((count (length dire--rename-list)))
    (if (zerop count)
        (user-error "[Dire] Nothing to revert")
      (dolist (entry dire--rename-list)
        (pcase-let ((`(,old . ,new) entry))
          (rename-file new old 0)))
      (message "[Dire] Reverted %d %s"
               count
               (if (> count 1) "entries" "entry"))))
  (setq dire--rename-list nil))

(provide 'dire)

;;; dire.el ends here
