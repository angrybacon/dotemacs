;;; widowmaker.el --- Manage windows -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

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

(require 'cl-lib)
(require 'menu-bar)
(require 'olivetti)
(require 'project)
(require 'shackle)
(require 'window)
(require 'windmove)
(require 'winner)

(defgroup widowmaker nil
  "Manage windows."
  :group 'convenience
  :prefix "widowmaker-")

;;;; Olivetti

(defcustom widowmaker-olivetti-automatic t
  "Whether `olivetti-mode' should be enabled automatically.
See `widowmaker-olivetti-mode-maybe' for the heuristics used and details of
implementation."
  :type 'boolean)

(defcustom widowmaker-olivetti-blacklist-buffers '()
  "Buffers for which `olivetti-mode' should not be enabled automatically."
  :type '(repeat string))

(defcustom widowmaker-olivetti-blacklist-modes '(minibuffer-mode
                                                 minibuffer-inactive-mode
                                                 tabulated-list-mode)
  "Modes for which `olivetti-mode' should not be enabled automatically."
  :type '(repeat symbol))

(defmacro with-widowmaker-olivetti (&rest body)
  "Protect BODY with a check for `olivetti-mode' active status."
  (declare (indent defun))
  `(if olivetti-mode
       ,@body
     (user-error "[Widowmaker] `olivetti-mode' must be active")))

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

(defun widowmaker-olivetti-body-less ()
  "Narrow the body width by 4 columns."
  (interactive)
  (with-widowmaker-olivetti
    (cl-incf olivetti-body-width -4)))

(defun widowmaker-olivetti-body-more ()
  "Expand the body width by 4 columns."
  (interactive)
  (with-widowmaker-olivetti
    (cl-incf olivetti-body-width 4)))

(defun widowmaker-olivetti-body-reset ()
  "Reset the body width to its initial value."
  (interactive)
  (with-widowmaker-olivetti
    (setq-local olivetti-body-width widowmaker-olivetti-body-width)))

(defun widowmaker-olivetti-maybe--predicate (window)
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
    (let ((windows (seq-filter #'widowmaker-olivetti-maybe--predicate
                               (window-list frame)))
          (columns (frame-total-cols frame)))
      (dolist (window windows)
        (with-selected-window window
          (pcase-let ((`(,l ,_t ,r ,_b) (window-edges window)))
            (if (and (equal l 0) (equal r columns))
                (olivetti-mode 1)
              (olivetti-mode 0))))))))

;;;; Shackle

(defun widowmaker-shackle-set-window-side (_buffer _alist plist)
  "Set window side parameter for `shackle-last-window' according to PLIST.
This allows features to filter or select windows based on their function ie. a
side window."
  (with-selected-window shackle-last-window
    (when-let ((window shackle-last-window)
               (alignment (plist-get plist :align)))
      (set-window-parameter window 'window-side t))))

(advice-add 'shackle--display-buffer-aligned-window :after
  #'widowmaker-shackle-set-window-side)

;;;; Terminal

(defcustom widowmaker-terminal-function 'widowmaker-terminal-vterm
  "Terminal emulator to use."
  :type '(choice (const :tag "Vterm" widowmaker-terminal-vterm)))

(defun widowmaker-terminal-vterm (&optional buffer)
  "Invoke `vterm'.
Use BUFFER for the terminal window when it is provided."
  (if (require 'vterm nil :noerror)
      (vterm buffer)
    (error "[Widowmaker] Package 'vterm' not found")))

(defun widowmaker-terminal-dwim ()
  "Spawn a terminal window using `widowmaker-terminal' command.
This can either raise a pop-up or invoke in place depending on context."
  (interactive)
  (let ((project (ignore-errors (project-root (project-current)))))
    (if-let* ((project)
              (project-name (file-name-nondirectory
                             (directory-file-name
                              (file-name-directory project))))
              (buffer (format "*terminal: %s*" project-name)))
        (funcall widowmaker-terminal-function buffer)
      (funcall widowmaker-terminal-function))))

(provide 'widowmaker)

;;; widowmaker.el ends here
