;;; use-presentation.el --- Make it pretty for all   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Pair-Programming Mode

;; TODO Remove the non-global mode

(use-package ruric
  :load-path "lisp/ruric"
  :commands
  ruric-global-mode
  ruric-mode
  ruric-toggle-line-numbers
  ruric-toggle-line-numbers-absolute)

;;;; Presentation Mode

(defvar-local me/org-present-face-remap-cookies nil
  "Keep the face remappings around to revert them.")

(defun me/org-present-end ()
  "Configurations to run when `org-present-mode' ends."
  (setq
   header-line-format nil
   org-hide-emphasis-markers nil)
  (mapc #'face-remap-remove-relative me/org-present-face-remap-cookies)
  (setq face-remap-remove-relative nil)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (org-remove-inline-images)
  (evil-initialize-state)
  (widowmaker-olivetti-body-reset))

(defun me/org-present-begin ()
  "Configurations to run when `org-present-mode' begins."
  (goto-char (point-min))
  (setq
   header-line-format " "
   org-hide-emphasis-markers t)
  (push (face-remap-add-relative 'default 'variable-pitch)
        me/org-present-face-remap-cookies)
  (push (face-remap-add-relative 'header-line '(:height 4.0))
        me/org-present-face-remap-cookies)
  (push (face-remap-add-relative 'org-block 'fixed-pitch)
        me/org-present-face-remap-cookies)
  (push (face-remap-add-relative 'org-block 'hl-line)
        me/org-present-face-remap-cookies)
  (push (face-remap-add-relative 'org-block-begin-line 'fixed-pitch)
        me/org-present-face-remap-cookies)
  (push (face-remap-add-relative 'org-block-begin-line 'hl-line)
        me/org-present-face-remap-cookies)
  (display-line-numbers-mode 0)
  (hl-line-mode 0)
  (org-display-inline-images)
  (org-present-big)
  (evil-emacs-state)
  (widowmaker-olivetti-body-reset 50))

(use-package org-present
  :hook
  (org-present-mode . me/org-present-begin)
  (org-present-mode-quit . me/org-present-end))

;;; use-presentation.el ends here
