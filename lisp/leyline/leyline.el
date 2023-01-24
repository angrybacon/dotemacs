;;; leyline.el --- Mode-line utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: January 23, 2023
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/leyline
;; Package-Requires: ((emacs "29.0.60"))

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

;; Utilities to customize the mode-line.

;;; Code:

(defgroup leyline nil
  "Mode-line utilities."
  :group 'mode-line)

(defcustom leyline-rules '((tsx-ts-mode "TSX")
                           (typescript-ts-mode "TS"))
  "Rules for major-mode names to be renamed."
  :type '(alist :key-type symbol :value-type string))

(defun leyline--rename ()
  "Rename the current major-mode name as per `leyline-rules'."
  (when-let ((name (alist-get major-mode leyline-rules)))
    (setq mode-name name)))

(add-hook 'after-change-major-mode-hook #'leyline--rename)

(provide 'leyline)

;;; leyline.el ends here
