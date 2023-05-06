;;; widowmaker.el --- Manage windows                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: December 19, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/widowmaker
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

;; Collection of window management features and customization for window
;; management packages. Configure window navigation but also pop-up management.

;;; Code:

(require 'cl-lib)    ; `cl-incf'
(require 'cl-macs)   ; `cl-destructuring-bind'
(require 'cl-seq)    ; `cl-position'
(require 'frame)     ; `display-monitor-attributes-list' `display-pixel-height'
                     ; `display-pixel-width' `frame-inner-width'
                     ; `frame-monitor-attributes' `set-frame-parameter'
(require 'nadvice)   ; `advice-add'
(require 'olivetti)
(require 'pcase)     ; `pcase-let'
(require 'project)   ; `project-current' `project-root'
(require 'seq)       ; `seq-filter'
(require 'window)    ; `window-edges'

(defgroup widowmaker nil
  "Manage windows and buffers."
  :group 'convenience)

;;;; Common

;;;###autoload
(defun widowmaker-kill-buffer-with-process (&optional buffer-or-name)
  "Kill BUFFER-OR-NAME with no confirmation.
If the buffer is not specified, kill the current buffer instead.
Inhibit `kill-buffer-query-functions' to bypass the modal prompt for process
buffers."
  (interactive)
  (let ((kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                           kill-buffer-query-functions)))
    (kill-buffer (or buffer-or-name (current-buffer)))))

;;;; Olivetti

(defcustom widowmaker-olivetti-automatic t
  "Whether `olivetti-mode' should be enabled automatically.
See `widowmaker-olivetti-mode-maybe' for the heuristics used and details of
implementation."
  :type 'boolean)

(defcustom widowmaker-olivetti-blacklist-buffers '()
  "Buffers for which `olivetti-mode' should not be enabled automatically."
  :type '(repeat string))

(defcustom widowmaker-olivetti-blacklist-modes '(magit-status-mode
                                                 messages-buffer-mode
                                                 minibuffer-mode
                                                 minibuffer-inactive-mode
                                                 tabulated-list-mode
                                                 vterm-mode)
  "Modes for which `olivetti-mode' should not be enabled automatically."
  :type '(repeat symbol))

(defcustom widowmaker-olivetti-body-width (+ fill-column 6)
  "Default width to use for the body.
Add a safe margin to account for line numbers."
  :type 'number)

(setq-default olivetti-body-width widowmaker-olivetti-body-width)

(defmacro with-widowmaker-olivetti (&rest body)
  "Protect BODY with a check for `olivetti-mode' active status."
  (declare (indent defun))
  `(if olivetti-mode
       ,@body
     (user-error "[Widowmaker] `olivetti-mode' must be active")))

;;;###autoload
(defun widowmaker-olivetti-automatic-toggle ()
  "Toggle `widowmaker-olivetti-automatic'.
When it is disabled that way, turn `olivetti-mode' off in all windows for the
current frame."
  (interactive)
  (setq widowmaker-olivetti-automatic (not widowmaker-olivetti-automatic))
  (unless widowmaker-olivetti-automatic
    (dolist (window (window-list nil))
      (with-selected-window window
        (olivetti-mode 0)))))

;;;###autoload
(defun widowmaker-olivetti-body-less ()
  "Narrow the body width by 4 columns."
  (interactive)
  (with-widowmaker-olivetti
    (cl-incf olivetti-body-width -4)))

;;;###autoload
(defun widowmaker-olivetti-body-more ()
  "Expand the body width by 4 columns."
  (interactive)
  (with-widowmaker-olivetti
    (cl-incf olivetti-body-width 4)))

;;;###autoload
(defun widowmaker-olivetti-body-reset (&optional width)
  "Set the body width to WIDTH columns.
When the value is not provided, reset the width to its initial value."
  (interactive)
  (with-widowmaker-olivetti
    (setq-local olivetti-body-width (or width widowmaker-olivetti-body-width))))

(defun widowmaker-olivetti--maybe-predicate (window)
  "Predicate to run against WINDOW in `widowmaker-olivetti-maybe'.
Return t for all windows that pass the following tests:
  - The window is not protected with the `no-other-window' parameter
  - The window does not have a `window-side' parameter set
  - The buffer name is not listed in `widowmaker-olivetti-blacklist-buffers'
  - The major mode is not and does not derive from a mode listed in
    `widowmaker-olivetti-blacklist-modes'
If any test fails, return nil."
  (with-selected-window window
    (and (not (window-parameter window 'no-other-window))
         (not (window-parameter window 'window-side))
         (not (member (buffer-name) widowmaker-olivetti-blacklist-buffers))
         (not (apply 'derived-mode-p widowmaker-olivetti-blacklist-modes)))))

(defun widowmaker-olivetti-maybe (&optional frame)
  "Turn on `olivetti-mode' for lone windows in FRAME.
When FRAME is not provided, use the current frame instead.

A window is considered lone when it has no neighbour to its left nor to its
right. A list of modes and buffer names can be configured to ignore this
geometry heuristic. See `widowmaker-olivetti-blacklist-buffers' to ignore
specific buffer names and `widowmaker-olivetti-blacklist-modes' to ignore
specific major modes or modes derived from them. This is useful for modes that
behave better in wider windows by design like modes that display tabulated data.

If `widowmaker-olivetti-automatic' is nil, do nothing."
  (when widowmaker-olivetti-automatic
    (let ((windows (seq-filter #'widowmaker-olivetti--maybe-predicate
                               (window-list frame)))
          (columns (frame-inner-width frame)))
      (dolist (window windows)
        (with-selected-window window
          (pcase-let ((`(,l ,_t ,r ,_b) (window-edges nil nil nil :pixelwise)))
            (if (equal (- r l) columns)
                (olivetti-mode 1)
              (olivetti-mode 0))))))))

;;;; Placement

;;;###autoload
(defun widowmaker-placement-center ()
  "Set window geometry to be centered within the main display."
  (interactive)
  (set-frame-height nil 25)
  (set-frame-width nil 90)
  (let* ((x (/ (- (display-pixel-width) (frame-pixel-width)) 2))
         (y (/ (- (display-pixel-height) (frame-pixel-height)) 2)))
    (set-frame-position nil x y)))

;;;###autoload
(defun widowmaker-placement-cycle (&optional frame)
  "Cycle FRAME between the available displays.
If FRAME is nil, consider the current frame only."
  (interactive)
  (let* ((geometry (assq 'geometry (frame-monitor-attributes)))
         (geometries (mapcar #'(lambda (display) (assq 'geometry display))
                             (display-monitor-attributes-list)))
         (index (cl-position geometry geometries :test #'equal)))
    (when (equal (length geometries) 1)
      (user-error "[Widowmaker] Found only one display"))
    (unless (integerp index)
      (user-error "[Widowmaker] Could not guess current display"))
    (let* ((next (if (equal index (1- (length geometries))) 0 (1+ index)))
           (geometry (nth next geometries)))
      (cl-destructuring-bind (x y width height) (cdr geometry)
        (set-frame-position frame x y)
        (set-frame-size frame width height :pixelwise)
        (set-frame-parameter frame 'fullscreen 'maximized)))))

;;;; Shackle

;;;###autoload
(defun widowmaker-shackle-set-window-side (_buffer _alist plist)
  "Set window side parameter for `shackle-last-window' according to PLIST.
This allows features to filter or select windows based on their function ie. a
side window."
  (with-selected-window shackle-last-window
    (when-let ((window shackle-last-window)
               (alignment (plist-get plist :align)))
      (set-window-parameter window 'window-side t))))

;;;; Terminal

(defun widowmaker-terminal-vterm (&optional buffer-name)
  "Invoke `vterm'.
Use BUFFER-NAME as name for the new terminal buffer when it is provided."
  (if (require 'vterm nil :noerror)
      (vterm buffer-name)
    (error "[Widowmaker] Package `vterm' not found")))

(defcustom widowmaker-terminal-function 'widowmaker-terminal-vterm
  "Terminal emulator to use."
  :type '(choice (const :tag "Vterm" widowmaker-terminal-vterm)))

;;;###autoload
(defun widowmaker-terminal-dwim (force)
  "Spawn a terminal window using `widowmaker-terminal-function'.
If a project root is found using `project-current', customize the current
directory to this location before spawning the terminal.
When the appropriate terminal window is already open, switch to it.
When prefixed with \\[universal-argument], FORCE create a new terminal session."
  (interactive "P")
  (if-let ((root (ignore-errors (project-root (project-current)))))
      (let* ((default-directory root)
             (name (file-name-nondirectory (directory-file-name root)))
             (buffer (format "*terminal: %s*" name))
             (buffer (if force (generate-new-buffer-name buffer) buffer)))
       (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (funcall widowmaker-terminal-function buffer)))
    (funcall widowmaker-terminal-function)))

(provide 'widowmaker)

;;; widowmaker.el ends here
