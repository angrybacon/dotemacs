;;; hydra-plus.el --- Hydra augments -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: November 29, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/hydra-plus

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

;; Augments and helpers for hydra.el.

;;; Code:

(defvar-local hydra-plus--super-body nil)

(defun hydra-plus-heading (&rest headings)
  "Format HEADINGS to look pretty in a hydra docstring."
  (concat "\n "
          (mapconcat (lambda (heading)
                       (propertize (format "%-18s" heading) 'face 'shadow))
                     headings
                     nil)))

(defun hydra-plus-set-super ()
  (when-let* ((suffix "-mode")
              (position (- (length suffix)))
              (mode (symbol-name major-mode))
              (name (if (string= suffix (substring mode position))
                        (substring mode 0 position)
                      mode))
              (body (intern (format "hydra-%s/body" name))))
    (when (functionp body)
      (setq hydra-plus-super-body body))))

(defun hydra-plus-super-maybe ()
  (interactive)
  (if hydra-plus--super-body
      (funcall hydra-plus--super-body)
    (user-error "hydra-plus-super: hydra-plus--super-body is not set")))

(provide 'hydra-plus)

;;; hydra-plus.el ends here
