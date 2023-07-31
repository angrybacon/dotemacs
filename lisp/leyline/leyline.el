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

(declare-function cl-map "cl-extra")
(declare-function cl-struct-slot-value "cl-macs")
(declare-function eglot-current-server "eglot")
(declare-function eglot-project-nickname "eglot")
(declare-function eglot--managed-mode "eglot")
(declare-function eyebrowse--get "eyebrowse")
(declare-function flymake--handle-report "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function flymake-start "flymake")
(declare-function flymake-start "flymake")
(declare-function jsonrpc-last-error "jsonrpc")
(declare-function jsonrpc--request-continuations "jsonrpc")

(defgroup leyline nil
  "Yet another minimal mode-line."
  :group 'mode-line)

;;;; Rename major modes

(defcustom leyline-rules '((emacs-lisp-mode "ELisp")
                           (tsx-ts-mode "TSX")
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

(defface leyline-buffer-modified
  '((t (:inherit (warning leyline-buffer))))
  "Face used for displaying the value of `buffer-name' when it is modified."
  :group 'leyline-faces)

(defface leyline-error
  '((t (:inherit error)))
  "Face for error status indicators."
  :group 'leyline-faces)

(defface leyline-evil-emacs
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-insert
  '((t (:inherit font-lock-warning-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-motion
  '((t (:inherit leyline-evil-normal-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-normal
  '((t ()))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-operator
  '((t (:inherit font-lock-constant-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-replace
  '((t (:inherit font-lock-type-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-evil-visual
  '((t (:inherit font-lock-function-name-face)))
  "Face used for the evil state segment."
  :group 'leyline-faces)

(defface leyline-note
  '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face used for neutral status indicators."
  :group 'leyline-faces)

(defface leyline-pending
  '((t (:inherit mode-line-highlight)))
  "Face used for transient segments."
  :group 'leyline-faces)

(defface leyline-secondary
  '((t (:inherit shadow)))
  "Face used for less important mode-line content."
  :group 'leyline-faces)

(defface leyline-success
  '((t (:inherit success)))
  "Face used for successful status indicators."
  :group 'leyline-faces)

(defface leyline-warning
  '((t (:inherit warning)))
  "Face for warning status indicators."
  :group 'leyline-faces)

;;;; Evil

(defcustom leyline-evil-alist
  '((emacs    . ("EMACS" . leyline-evil-emacs))
    (insert   . ("INSERT" . leyline-evil-insert))
    (motion   . ("MOTION" . leyline-evil-motion))
    (normal   . ("NORMAL" . leyline-evil-normal))
    (operator . ("OPERATOR" . leyline-evil-operator))
    (replace  . ("REPLACE" . leyline-evil-replace))
    (visual   . ("VISUAL" . leyline-evil-visual)))
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
              (propertize " Checking " 'face 'leyline-pending))
             ((> .errors 0)
              (propertize (format " %s " .all) 'face 'leyline-error))
             ((> .warnings 0)
              (propertize (format " %s " .all) 'face 'leyline-warning))
             ((> .notes 0)
              (propertize (format " %s " .all) 'face 'leyline-note)))))))

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
  (propertize " %b " 'face (if (buffer-modified-p)
                               'leyline-buffer-modified
                             'leyline-buffer)))

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
    (format "%s" leyline--flymake-text)))

(defun leyline-segment-lsp ()
  "Return the current LSP status for the mode-line."
  (when (bound-and-true-p eglot--managed-mode)
    (when-let* ((server (eglot-current-server))
                (name (eglot-project-nickname server)))
      (let* ((pending (hash-table-count (jsonrpc--request-continuations server)))
             (last-error (jsonrpc-last-error server))
             (result `(,@(when (cl-plusp pending) (list (format "%d" pending)))
                       ,@(when last-error (list last-error))
                       ,name)))
        (propertize
         (format " %s " (string-join result ":")) 'face 'leyline-secondary)))))

(defun leyline-segment-major ()
  "Return the current major mode for the mode-line."
  (let ((text (substring-no-properties (format-mode-line mode-name))))
    (format " %s " text)))

(defun leyline-segment-miscellaneous ()
  "Return the current value of `mode-line-misc-info' for the mode-line."
  (let ((text (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p text)
      (propertize (format " %s " (string-trim text))
                  'face 'leyline-secondary))))

(defun leyline-segment-position ()
  "Return the current cursor position for the mode-line."
  (propertize " %p%% " 'face 'leyline-secondary))

(defun leyline-segment-process ()
  "Return current value of `mode-line-process' for the mode-line."
  (let ((text (format-mode-line mode-line-process)))
    (unless (string-blank-p text)
      (propertize (format " %s " (string-trim text)) 'face 'leyline-pending))))

(defun leyline-segment-vc ()
  "Return the version control details for the mode-line."
  (unless (or (not leyline--vc-text)
              (string-blank-p leyline--vc-text))
    (format " :%s " leyline--vc-text)))

(defun leyline-segment-workspace ()
  "Return the current workspace name for the mode-line."
  (when (and (bound-and-true-p eyebrowse-mode)
             (length> (eyebrowse--get 'window-configs) 1))
    (when-let*
        ((slot (eyebrowse--get 'current-slot))
         (tag (nth 2 (assoc slot (eyebrowse--get 'window-configs))))
         (name (if (length> tag 0) tag slot)))
      (propertize (format " %s " name) 'face 'leyline-secondary))))

;;;; Mode

(defun leyline--format-segments (segments)
  "Format SEGMENTS for a mode-line construct."
  (format-mode-line
   (cl-map 'list (lambda (it) `(:eval ,it)) segments)))

(defun leyline--format (left-segments right-segments)
  "Format a mode line with LEFT-SEGMENTS and RIGHT-SEGMENT.
LEFT-SEGMENT and RIGHT-SEGMENTS should be lists and the result value is a list
starting with `:eval' in order to form a valid mode-line format string."
  (let* ((left (leyline--format-segments left-segments))
         (right (leyline--format-segments right-segments))
         ;; TODO Check out `mode-line-right-align-edge'
         (offset `(- right (- 0 right-fringe right-margin) ,(length right)))
         (spacing (propertize " " 'display `((space :align-to ,offset)))))
    (concat left spacing right)))

(defun leyline--make ()
  "Return the new mode-line format."
  '((:eval
     (leyline--format
      '((leyline-segment-evil)
        (leyline-segment-buffer)
        (leyline-segment-position))
      '((when (mode-line-window-selected-p)
          (leyline-segment-flymake))
        (leyline-segment-process)
        (leyline-segment-miscellaneous)
        (leyline-segment-lsp)
        (when (mode-line-window-selected-p)
          (leyline-segment-vc))
        (leyline-segment-major)
        (leyline-segment-workspace))))))

(defvar leyline--default-format nil
  "Remember the previous format for when `leyline-mode' is turned off.")

(defvar leyline--default-miscellaneous nil
  "Remember the previous segment format for when `leyline-mode' is turned off.")

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
        (setq
         leyline--default-format mode-line-format
         leyline--default-miscellaneous mode-line-misc-info)
        (setq-default
         mode-line-format (leyline--make)
         mode-line-misc-info (assq-delete-all
                              'eglot--managed-mode mode-line-misc-info)
         mode-line-misc-info (assq-delete-all
                              'eyebrowse-mode mode-line-misc-info)))
    (advice-remove #'flymake-start #'leyline--update-flymake)
    (advice-remove #'flymake--handle-report #'leyline--update-flymake)
    (remove-hook 'after-save-hook #'leyline--update-vc)
    (remove-hook 'file-find-hook #'leyline--update-vc)
    (advice-remove #'vc-refresh-state #'leyline--update-vc)
    (setq-default
     mode-line-format leyline--default-format
     mode-line-misc-info leyline--default-miscellaneous)))

(provide 'leyline)

;;; leyline.el ends here
