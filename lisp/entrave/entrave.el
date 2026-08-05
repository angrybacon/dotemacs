;;; entrave.el --- Layer for display buffer rules    -*- lexical-binding: t; -*-

;; Copyright (C) Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: July 15, 2026
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/entrave
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

;; Attempt at making display buffer rules more manageable.

;;; Code:

(defgroup entrave nil
  "Layer for display buffer rules."
  :group 'windows)

(defcustom entrave-height .25
  "Height for bottom windows.
An integer specifies rows, a float specifies a ratio."
  :type 'number)

(defcustom entrave-width 80
  "Width for left windows.
An integer specifies columns, a float specifies a ratio."
  :type 'number)

(defcustom entrave-rules nil
  "Rules for displaying buffers.

Each element looks like (MATCHER SIDE &rest FLAGS) where MATCHER is either a
major mode symbol or a buffer name regular expression, SIDE is required and can
either be `:bottom', `:left' or `:right'. FLAGS is an optional list of keywords.
First match wins.

- `:select' requests a newly opened window to be selected automatically"
  :type '(repeat (list (choice string symbol)
                       (choice (const :bottom)
                               (const :left)
                               (const :right))
                       (set :inline t (const :select)))))

(defvar entrave--entry '(entrave-match-p (entrave-display))
  "The `display-buffer-alist' entry managed by entrave.")

(defvar entrave--select nil
  "Whether the last matched rule requests window selection.")

(defvar entrave--side nil
  "Side found by the last `entrave-match-p' call.")

(defun entrave--reset-margins ()
  "Reset window margins on all windows.
Wide margins can make windows too small for `display-buffer-in-side-window' to
split."
  (walk-windows
   (lambda (window)
     (set-window-margins window nil nil))))

(defun entrave-display (buffer alist)
  "Display BUFFER according to entrave rules.
ALIST is the action alist passed by `display-buffer'."
  (when-let* ((side entrave--side)
              (extra (pcase side
                       (:bottom
                        `((side . bottom)
                          (window-height . ,entrave-height)))
                       (:left
                        `((side . left)
                          (window-width . ,entrave-width)))
                       (:right
                        `((side . right)
                          (window-width . ,entrave-width))))))
    (entrave--reset-margins)
    (when-let* ((window (display-buffer-in-side-window buffer
                                                       (append extra alist))))
      (when entrave--select
        (select-window window))
      window)))

(defun entrave--match-rule-p (name rule)
  "Return non-nil if buffer NAME matches RULE."
  (if (stringp rule)
      (string-match-p rule name)
    (with-current-buffer name
      (derived-mode-p rule))))

(defun entrave-match-p (name &rest _)
  "Return non-nil if NAME matches any entrave rule."
  (when-let* ((rule (seq-find (lambda (entry)
                                (entrave--match-rule-p name (car entry)))
                              entrave-rules)))
    (setq
     entrave--select (memq :select (cddr rule))
     entrave--side (cadr rule))
    entrave--side))

;;;###autoload
(define-minor-mode entrave-mode
  "Toggle display buffer rules."
  :global t
  (setq display-buffer-alist (delete entrave--entry display-buffer-alist))
  (when entrave-mode
    (push entrave--entry display-buffer-alist)))

(provide 'entrave)

;;; entrave.el ends here
