;;; leyline.el --- Yet another minimal mode-line     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: January 23, 2023
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/leyline
;; Package-Requires: ((emacs "30.0.50"))

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
  "Yet another minimal mode-line."
  :group 'mode-line)

;;;; Rename major modes

(defcustom leyline-rules '((emacs-lisp-mode "ELisp")
                           (js-ts-mode "JS")
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
  '((t (:inherit error :inverse-video t)))
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
  '((t (:inherit font-lock-keyword-face :inverse-video t :weight normal)))
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
  '((t (:inherit warning :inverse-video t)))
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

(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function flymake-running-backends "flymake")
(defvar-local leyline--flymake-text nil)

(defun leyline--flymake-count (type)
  "Return the count of Flymake diagnostics of the given TYPE."
  (let ((count 0))
    (dolist (diagnostic (flymake-diagnostics))
      (when (eq (flymake-diagnostic-type diagnostic) type)
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

(defvar-local leyline--revision-text nil)

(defun leyline--update-revision (&rest _)
  "Update `leyline--revision-text' with the current version control branch."
  (setq leyline--revision-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (offset (+ (if (eq backend 'Hg) 2 3) 2)))
            (substring-no-properties vc-mode offset)))))

;;;; Segments

(declare-function eglot-current-server "eglot")
(declare-function eglot-project-nickname "eglot")
(declare-function eyebrowse--get "eyebrowse")
(declare-function jsonrpc-continuation-count "jsonrpc")
(declare-function jsonrpc-last-error "jsonrpc")

(defun leyline-segment-buffer ()
  "Format the name of the current buffer for the mode-line."
  (propertize "%b" 'face (if (buffer-modified-p)
                               'leyline-buffer-modified
                             'leyline-buffer)))

(defun leyline-segment-evil ()
  "Format the current `evil-mode' state for the mode-line."
  (when (and (bound-and-true-p evil-mode)
             (boundp 'evil-state))
    (let ((state (alist-get evil-state leyline-evil-alist)))
      (propertize (format "%s" (car state)) 'face (cdr state)))))

(defun leyline-segment-flymake ()
  "Format the current Flymake status for the mode-line."
  (when (and (bound-and-true-p flymake-mode)
             leyline--flymake-text
             (not (string-blank-p leyline--flymake-text)))
    leyline--flymake-text))

(defun leyline-segment-lsp ()
  "Format the current LSP status for the mode-line."
  (when (bound-and-true-p eglot--managed-mode)
    (when-let* ((server (eglot-current-server))
                (name (eglot-project-nickname server)))
      (let* ((pending (jsonrpc-continuation-count server))
             (text (jsonrpc-last-error server))
             (result `(,@(when (cl-plusp pending) (list (format "%d" pending)))
                       ,@(when text (list text))
                       ,name)))
        (propertize
         (format "%s" (string-join result ":")) 'face 'leyline-secondary)))))

(defun leyline-segment-major ()
  "Format the current major mode for the mode-line."
  (let ((text (substring-no-properties (format-mode-line mode-name))))
    (format "%s" text)))

(defun leyline-segment-miscellaneous ()
  "Format the current value of `mode-line-misc-info' for the mode-line."
  (let ((text (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p text)
      (propertize (string-trim text) 'face 'leyline-secondary))))

(defun leyline-segment-position ()
  "Format the current cursor position for the mode-line."
  (propertize "%p%%" 'face 'leyline-secondary))

(defun leyline-segment-process ()
  "Format current value of `mode-line-process' for the mode-line."
  (let ((text (format-mode-line mode-line-process)))
    (unless (string-blank-p text)
      (propertize (format " %s " (string-trim text)) 'face 'leyline-pending))))

(defun leyline-segment-revision ()
  "Format the version control details for the mode-line."
  (unless (or (not leyline--revision-text)
              (string-blank-p leyline--revision-text))
    (format ":%s" leyline--revision-text)))

(defun leyline-segment-workspace ()
  "Format the current workspace name for the mode-line."
  (when (and (bound-and-true-p eyebrowse-mode)
             (length> (eyebrowse--get 'window-configs) 1))
    (when-let*
        ((slot (eyebrowse--get 'current-slot))
         (tag (nth 2 (assoc slot (eyebrowse--get 'window-configs))))
         (name (if (length> tag 0) tag slot)))
      (propertize (format "%s" name) 'face 'leyline-secondary))))

;;;; Mode

(declare-function flymake--handle-report "flymake")
(declare-function flymake-start "flymake")

(defcustom leyline-separator "  "
  "Separator to insert between mode-line segments."
  :type 'string)

(defun leyline--format (&rest inputs)
  "Format INPUTS for the mode-line.
Each item in INPUTS can either be a segment or a list of segments."
  (format-mode-line (string-join (flatten-tree inputs) leyline-separator)))

(defun leyline--make ()
  "Return the new mode-line format."
  '((:eval
     (leyline--format
      (leyline-segment-evil)
      (leyline-segment-buffer)))
    ;; TODO Add segment for repeat-echo-mode-line-string
    mode-line-format-right-align
    (:eval
     (leyline--format
      (leyline-segment-process)
      (when (mode-line-window-selected-p)
        (list
         (leyline-segment-flymake)
         (leyline-segment-miscellaneous)
         (leyline-segment-lsp)
         (leyline-segment-revision)
         (leyline-segment-major)))
      (leyline-segment-workspace)))))

(defvar leyline--previous-format nil
  "Previous `mode-line-format' for when `leyline-mode' is turned off.")

(defvar leyline--previous-miscellaneous nil
  "Previous `mode-line-misc-info' for when `leyline-mode' is turned off.")

(defun leyline--prune-miscellaneous (name)
  "Pull NAME out of `mode-line-misc-info'.
Remember the previous value beforehand so that it can be restored later."
  (setq-default
   leyline--previous-miscellaneous mode-line-misc-info
   mode-line-misc-info (assq-delete-all name mode-line-misc-info)))

(defun leyline--remember-previous ()
  "Save previous formats for when `leyline-mode' is turned off."
  (setq-default
   leyline--previous-format mode-line-format
   leyline--previous-miscellaneous mode-line-misc-info))

(defun leyline--restore-previous ()
  "Restore previous formats after `leyline-mode' is turned off."
  (setq-default
   mode-line-format leyline--previous-format
   mode-line-misc-info leyline--previous-miscellaneous))

;;;###autoload
(define-minor-mode leyline-mode
  "Toggle `leyline-mode' on or off."
  :group 'leyline
  :global t
  (if leyline-mode
      (progn
        (advice-add 'flymake-start :after #'leyline--update-flymake)
        (advice-add 'flymake--handle-report :after #'leyline--update-flymake)
        (add-hook 'after-save-hook #'leyline--update-revision)
        (add-hook 'find-file-hook #'leyline--update-revision)
        (advice-add 'vc-refresh-state :after #'leyline--update-revision)
        (leyline--remember-previous)
        (setq-default mode-line-format (leyline--make))
        (with-eval-after-load 'eglot
          (leyline--prune-miscellaneous 'eglot--managed-mode))
        (with-eval-after-load 'eyebrowse
          (leyline--prune-miscellaneous 'eyebrowse-mode)))
    (advice-remove 'flymake-start #'leyline--update-flymake)
    (advice-remove 'flymake--handle-report #'leyline--update-flymake)
    (remove-hook 'after-save-hook #'leyline--update-revision)
    (remove-hook 'file-find-hook #'leyline--update-revision)
    (advice-remove 'vc-refresh-state #'leyline--update-revision)
    (leyline--restore-previous)))

(provide 'leyline)

;;; leyline.el ends here
