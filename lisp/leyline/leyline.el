;;; leyline.el --- Yet another minimal mode-line     -*- lexical-binding: t; -*-

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

(declare-function cl-struct-slot-value "cl-macs" (struct-type slot-name inst))
(declare-function flymake--handle-report "flymake" ())
(declare-function flymake-reporting-backends "flymake" ())
(declare-function flymake-running-backends "flymake" ())
(declare-function flymake-start "flymake" ())

(defgroup leyline nil
  "Yet another minimal mode-line."
  :group 'mode-line)

;;;; Rename major modes

(defcustom leyline-rules '((tsx-ts-mode "TSX")
                           (typescript-ts-mode "TS"))
  "Rules for major-mode names to be renamed."
  :type '(alist :key-type symbol :value-type string))

;;;###autoload
(defun leyline-rename ()
  "Rename the current major-mode name as per `leyline-rules'."
  (when-let ((name (alist-get major-mode leyline-rules)))
    (setq mode-name name)))

;;;; Faces

(defgroup leyline-faces nil
  "Faces for `leyline'."
  :group 'faces
  :group 'leyline)

(defface leyline-buffer
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying the value of `buffer-name'."
  :group 'leyline-faces)

(defface leyline-error
  '((t (:inherit error :weight normal)))
  "Face for error status indicators."
  :group 'leyline-faces)

(defface leyline-major
  '((t (:inherit bold)))
  "Face used for the major mode indicator."
  :group 'leyline-faces)

(defface leyline-note
  '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face used for neutral status indicators."
  :group 'leyline-faces)

(defface leyline-secondary
  '((t (:inherit shadow :weight normal)))
  "Face used for less important mode-line content."
  :group 'leyline-faces)

(defface leyline-success
  '((t (:inherit success :weight normal)))
  "Face used for successful status indicators."
  :group 'leyline-faces)

(defface leyline-warning
  '((t (:inherit warning :weight normal)))
  "Face for warning status indicators."
  :group 'leyline-faces)

;;;; Evil

(defcustom leyline-evil-alist
  '((emacs    . ("<E>" . font-lock-builtin-face))
    (insert   . ("<I>" . font-lock-string-face))
    (motion   . ("<M>" . font-lock-constant-face))
    (normal   . ("<N>" . font-lock-variable-name-face))
    (operator . ("<O>" . font-lock-function-name-face))
    (replace  . ("<R>" . font-lock-type-face))
    (visual   . ("<V>" . font-lock-keyword-face)))
  "Configure the display text and face for all `evil-mode' states."
  :group 'leyline
  :type '(alist :key-type symbol
                :value-type (cons (string :tag "Display text")
                                  (choice :tag "Face" face plist))))

;;;; Flymake

(defvar-local leyline--flymake-text nil)

(defun leyline--flymake-count (type)
  "Return the count of Flymake diagnostics of the given TYPE."
  (let ((count 0))
    (dolist (diagnostic (flymake-diagnostics))
      (when (eq (cl-struct-slot-value 'flymake--diag 'type diagnostic) type)
        (cl-incf count)))
    count))

(defun leyline--flymake-diagnostics ()
  "Return an alist with counts for all the current Flymake diagnostic reports."
  (let ((notes (leyline--flymake-count :note))
        (errors (leyline--flymake-count :error))
        (warnings (leyline--flymake-count :warning)))
    `((all . ,(+ notes warnings errors))
      (errors . ,errors)
      (issues . ,(+ warnings errors))
      (notes . ,notes)
      (warnings . ,warnings))))

(defun leyline--update-flymake (&rest _)
  "Update `leyline--flymake-text' with the current state of Flymake."
  (setq leyline--flymake-text
        (when (and (fboundp 'flymake-is-running)
                   (flymake-is-running))
          (let-alist (leyline--flymake-diagnostics)
            (cond
             ((seq-difference (flymake-running-backends)
                              (flymake-reporting-backends))
              (propertize "Checking..." 'face 'leyline-note))
             ((> .errors 0)
              (propertize (number-to-string .all) 'face 'leyline-error))
             ((> .warnings 0)
              (propertize (number-to-string .all) 'face 'leyline-warning))
             ((> .notes 0)
              (propertize (number-to-string .all) 'face 'leyline-note)))))))

;;;; Version control

(defvar-local leyline--vc-text nil)

(defun leyline--update-vc (&rest _)
  "Update `leyline--vc-text' with the current version control branch."
  (setq leyline--vc-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (offset (+ (if (eq backend 'Hg) 2 3) 2)))
            (substring-no-properties vc-mode offset)))))

;;;; Segments

(defun leyline-segment-buffer ()
  "Return the name of the current buffer for the mode-line."
  (propertize " %b " 'face 'leyline-buffer))

(defun leyline-segment-evil ()
  "Return the current `evil-mode' state for the mode-line."
  (when (and (bound-and-true-p evil-mode)
             (boundp 'evil-state))
    (let ((state (alist-get evil-state leyline-evil-alist)))
      (propertize (format " %s " (car state)) 'face (cdr state)))))

(defun leyline-segment-flymake ()
  "Return the current Flymake status for the mode-line."
  (when (and (bound-and-true-p flymake-mode)
             leyline--flymake-text
             (not (string-blank-p leyline--flymake-text)))
    (format " %s " leyline--flymake-text)))

(defun leyline-segment-major ()
  "Return the current major mode for the mode-line."
  (let ((text (substring-no-properties (format-mode-line mode-name))))
    (propertize (format " %s " text) 'face 'leyline-major)))

(defun leyline-segment-miscellaneous ()
  "Return the current value of `mode-line-misc-info' for the mode-line."
  (let ((text (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p text)
      (propertize (format " %s " (string-trim text))
                  'face 'leyline-secondary))))

(defun leyline-segment-position ()
  "Return the current cursor position for the mode-line."
  (concat
   " %l:%c"
   (propertize " %p%% " 'face 'leyline-secondary)))

(defun leyline-segment-process ()
  "Return current value of `mode-line-process' for the mode-line."
  (let ((text (format-mode-line mode-line-process)))
    (unless (string-blank-p text)
      (format " %s " (string-trim text)))))

(defun leyline-segment-vc ()
  "Return the version control details for the mode-line."
  (unless (or (not leyline--vc-text)
              (string-blank-p leyline--vc-text))
    (format " :%s " leyline--vc-text)))

;;;; Mode

(defun leyline--format (left right)
  "Format a mode line with LEFT and RIGHT justified segments."
  (concat
   left
   (propertize " " 'display `((space :align-to (- right
                                                  (- 0 right-fringe)
                                                  (- 0 right-margin)
                                                  ,(length right)))))
   right))

(defun leyline--make ()
  "Return the mode-line format."
  '((:eval
     (leyline--format
      (format-mode-line
       '((:eval (leyline-segment-evil))
         (:eval (leyline-segment-buffer))
         (:eval (leyline-segment-position))))
      (format-mode-line
       '((:eval (leyline-segment-miscellaneous))
         (:eval (leyline-segment-flymake))
         (:eval (leyline-segment-process))
         (:eval (leyline-segment-vc))
         (:eval (leyline-segment-major))))))))

(defvar-local leyline--default-format mode-line-format)

;;;###autoload
(define-minor-mode leyline-mode
  "Toggle `leyline-mode' on or off."
  :group 'leyline
  :global t
  (if leyline-mode
      (progn
        (advice-add #'flymake-start :after #'leyline--update-flymake)
        (advice-add #'flymake--handle-report :after #'leyline--update-flymake)
        (add-hook 'after-save-hook #'leyline--update-vc)
        (add-hook 'find-file-hook #'leyline--update-vc)
        (advice-add #'vc-refresh-state :after #'leyline--update-vc)
        (setq leyline--default-format mode-line-format)
        (setq-default mode-line-format (leyline--make)))
    (advice-remove #'flymake-start #'leyline--update-flymake)
    (advice-remove #'flymake--handle-report #'leyline--update-flymake)
    (remove-hook 'after-save-hook #'leyline--update-vc)
    (remove-hook 'file-find-hook #'leyline--update-vc)
    (advice-remove #'vc-refresh-state #'leyline--update-vc)
    (setq-default mode-line-format leyline--default-format)))

(provide 'leyline)

;;; leyline.el ends here
