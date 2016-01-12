;;; init-cursor.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Keywords: abbrev, convenience, maint
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Add helpers to move the cursor around.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)


;;=============================================================================
;; Define key bindings
;;=============================================================================


(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "<C-up>") 'me/goto-previous-block)
(global-set-key (kbd "<C-down>") 'me/goto-next-block)
(global-set-key (kbd "<M-up>") 'me/move-line-up)
(global-set-key (kbd "<M-down>") 'me/move-line-down)


;;=============================================================================
;; Define helpers
;;=============================================================================


(defun me/goto-next-block ()
  "Jump to next paragraph."
  (interactive)
  (skip-chars-forward "\n")
  (when (not (search-forward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-max)))
  (skip-chars-forward "\n"))


(defun me/goto-previous-block ()
  "Jump to previous paragraph."
  (interactive)
  (skip-chars-backward "\n")
  (when (not (search-backward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-min)))
  (skip-chars-forward "\n"))


(defun me/move-line-down ()
  "Move down the current line under point."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun me/move-line-up ()
  "Move up the current line under point."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))


;;=============================================================================
;; Configure Expand Region
;;=============================================================================


;; Website: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t

  :init
  (pending-delete-mode t)

  :bind
  ("C-=" . er/expand-region))


;;=============================================================================
;; Configure Multiple Cursors
;;=============================================================================


;; Website: https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t

  :init
  (setq mc/list-file (expand-file-name ".multiple-cursors-lists.el" user-emacs-directory))

  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)))


(provide 'init-cursor)
;;; init-cursor.el ends here
