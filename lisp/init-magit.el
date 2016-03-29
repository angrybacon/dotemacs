;;; init-magit.el --- Add integration for Git within Emacs

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure gitattributes-mode, gitconfig-mode, gitignore-mode
;;=============================================================================


;; https://github.com/magit/git-modes
(use-package gitattributes-mode
  :defer t
  :delight gitattributes-mode "Git Attributes")
(use-package gitconfig-mode
  :defer t
  :delight gitconfig-mode "Git Config")
(use-package gitignore-mode
  :defer t
  :delight gitignore-mode "Git Ignore")


;;=============================================================================
;; Configure magit
;;=============================================================================


;; https://github.com/magit/magit
(use-package magit

  :defer t

  :defines
  (me/font-size-header
   zenburn/red
   zenburn/red-4
   zenburn/green+2
   zenburn/green-1)

  :bind
  (("C-c g b" . magit-blame)
   ("C-c g l" . magit-log)
   ("C-c g p" . magit-pull)
   ("C-c g s" . magit-status))

  :config

  (defun me/magit-display-buffer-function (buffer)
    "Render some magit modes in the currently selected buffer."
    (display-buffer
     buffer
     (cond ((and (derived-mode-p 'magit-mode)
                 (eq (with-current-buffer buffer major-mode)
                     'magit-status-mode))
            nil)
           ((memq (with-current-buffer buffer major-mode)
                  '(magit-process-mode
                    magit-revision-mode
                    magit-diff-mode
                    magit-stash-mode))
            nil)
           (t
            '(display-buffer-same-window)))))

  ;; Use better defaults
  (setq-default
   magit-display-buffer-function 'me/magit-display-buffer-function
   magit-refs-show-commit-count (quote all)
   magit-section-show-child-count t
   magit-set-upstream-on-push 'askifnotset
   magit-revision-show-gravatars t)

  ;; Customize lighters
  (delight
   '((magit-diff-mode "Magit Diff")
     (magit-log-mode "Magit Log")
     (magit-popup-mode "Magit Popup")
     (magit-status-mode "Magit Status")))

  ;; Customize faces
  (set-face-attribute 'magit-diff-added nil
                      :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-added-highlight nil
                      :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-removed nil
                      :background zenburn/red-4 :foreground zenburn/red)
  (set-face-attribute 'magit-diff-removed-highlight nil
                      :background zenburn/red-4 :foreground zenburn/red)
  (set-face-attribute 'magit-popup-heading nil :height me/font-size-header)
  (set-face-attribute 'magit-section-heading nil :height me/font-size-header))


(provide 'init-magit)
;;; init-magit.el ends here
