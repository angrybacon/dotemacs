;;; manticore.el --- Core functions                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: August 10, 2022
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/manticore

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

;; Container for core and miscellaneous features that other libraries from lisp/
;; might rely on.

;;; Code:

(defgroup manticore nil
  "Core functions and commands."
  :group 'convenience)

;;;###autoload
(defun manticore-delete-compiled ()
  "Find all byte-compiled files under the current directory and delete them."
  (interactive)
  (shell-command "find . -name \"*.elc\" -type f | xargs rm -f"))

;;;###autoload
(defun manticore-eval-region-dwim ()
  "When region is active, evaluate it and kill the mark.
With no region, evaluate the whole buffer."
  (interactive)
  (if (not (use-region-p))
      (eval-buffer)
    (eval-region (region-beginning) (region-end))
    (setq-local deactivate-mark t)))

;;;###autoload
(defun manticore-kill-terminal ()
  "Clear byte-compiled libraries, desktop backups and then quit Emacs."
  (interactive)
  (let ((desktop-save nil))
    (manticore-delete-compiled)
    (desktop-remove)
    (save-buffers-kill-terminal)))

;;;###autoload
(defun manticore-revert-buffer-immediately ()
  "Revert the current buffer with no confirmation."
  (interactive)
  (revert-buffer nil t))

(defcustom manticore-scroll-margin-minimum 0
  "Minimum value possible for the scroll margin when it should be inhibited."
  :type 'number)

(defun manticore-scroll-margin-disable ()
  "Disable `scroll-margin' locally."
  (setq-local scroll-margin (max manticore-scroll-margin-minimum 0)))

(provide 'manticore)

;;; manticore.el ends here
