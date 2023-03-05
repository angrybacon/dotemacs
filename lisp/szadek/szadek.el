;;; szadek.el --- Framework to retrieve secrets      -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: December 5, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/szadek
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

;; A small framework to read secrets out of a machine-local file.
;;
;; This can be used to keep sensible things out of version control, or
;; machine-specific settings.
;;
;; Usage:
;;
;;   (szadek-get)                          ; ((one .  1) (two . 2))
;;   (szadek-get 'one)                     ; 1
;;   (szadek-get 'three)                   ; nil
;;   (szadek-get 'three 'fallback)         ; 'fallback

;;; Code:

(require 'cl-seq)
(require 'pp)

(defgroup szadek nil
  "Framework to retrieve secrets."
  :group 'convenience)

(defcustom szadek-file (expand-file-name ".szadek.eld" user-emacs-directory)
  "File used to store secrets in `lisp-data-mode' format."
  :type 'file)

(defcustom szadek-fix-missing nil
  "Whether getting missing secrets should fix them on disk.
Set this to t to automatically set the missing secrets to their fallback value
permanently."
  :type 'boolean)

(defvar szadek-on-save-hook '()
  "Hook run after the secret file has been saved.")

(defun szadek--on-save ()
  "Run hooks on save in the secret file buffer."
  (let ((file szadek-file))
    (if (file-exists-p file)
        (when (equal (buffer-file-name) file)
          (message "[Szadek] Running %d hooks" (length szadek-on-save-hook))
          (run-hooks 'szadek-on-save-hook))
      (error "[Szadek] Missing secret file `%S'" file))))

(add-hook 'after-save-hook #'szadek--on-save)

;;;###autoload
(defun szadek-register (function &optional immediate)
  "Register FUNCTION under `szadek-on-save-hook'.
It will be called every time the secret file `szadek-file' is updated.
With IMMEDIATE, call FUNCTION immediately too."
  (add-hook 'szadek-on-save-hook function)
  (when immediate
    (funcall function)))

(defun szadek--read-file ()
  "Return the Lisp data found in the secret file.
The data should be a list."
  (with-temp-buffer
    (insert-file-contents szadek-file)
    (let ((data (read (buffer-string))))
      (if (listp data)
          data
        (user-error "[Szadek] Secrets should be a list")))))

(defun szadek--write-secret (secret)
  "Write SECRET in the secret file.
If the secret file does not exist, create it in the process."
  (let* ((file szadek-file)
         (initial (when (file-exists-p file) (szadek--read-file)))
         (data (append (list secret) (or initial '()))))
    (delete-dups data)
    (with-temp-file file
      (insert (let ((print-escape-newlines t)
                    print-length
                    print-level)
                (pp-to-string data))))))

(defun szadek--set-value (name value)
  "Set VALUE under NAME in the secret file."
  (let ((file szadek-file))
    (if (file-writable-p file)
        (szadek--write-secret `(,name . ,value))
      (error "[Szadek] `%S' is not writeable" file))))

(defun szadek--fallback (value &optional name)
  "Return fallback VALUE and optionally set it permanently.
Set the value in the secret file under NAME when it is provided."
  (when name
    (szadek--set-value name value))
  value)

(defun szadek-get (&optional name fallback)
  "Read the Lisp structure found in the secret file.
When NAME is provided, return the value associated to this key only. If no value
was found for NAME or if the secret file was not found, return FALLBACK instead
which defaults to nil.
When `szadek-fix-missing' is non nil, save the fallback value to the secret file
after creating it if necessary. If the fallback value was not provided or nil,
do nothing and return nil."
  (if-let* ((file szadek-file)
            (exists (file-exists-p file))
            (data (szadek--read-file))
            (match (cl-member-if #'(lambda (it) (eq (car it) name)) data)))
      (cdar match)
    (szadek--fallback fallback (when (and szadek-fix-missing fallback)
                                 name))))

(provide 'szadek)

;;; szadek.el ends here
