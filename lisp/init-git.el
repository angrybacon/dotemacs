;;; init-git.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: abbrev, convenience, faces, vc
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Add integration for Git within Emacs.

;;; Code:


;;=============================================================================
;; Configure Magit
;;=============================================================================


;; Website: https://github.com/magit/magit
(use-package magit

  :defer t

  :defines
  (me/font-family-header
   me/font-size-header
   zenburn/red
   zenburn/red-4
   zenburn/green+2
   zenburn/green-1)

  :bind ("C-c g" . magit-status)

  :init
  (setq-default
   magit-refs-show-commit-count (quote all)
   magit-section-show-child-count t)

  :config
  (set-face-attribute 'magit-diff-added nil
                      :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-added-highlight nil
                      :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-removed nil
                      :background zenburn/red-4 :foreground zenburn/red)
  (set-face-attribute 'magit-diff-removed-highlight nil
                      :background zenburn/red-4 :foreground zenburn/red)
  (set-face-attribute 'magit-popup-heading nil
                      :font me/font-family-header :height me/font-size-header)
  (set-face-attribute 'magit-section-heading nil
                      :font me/font-family-header :height me/font-size-header))


;;=============================================================================
;; Configure Git modes
;;=============================================================================


;; Website: https://github.com/magit/git-modes
(use-package gitattributes-mode
  :delight gitattributes-mode "Git Attributes")
(use-package gitconfig-mode
  :delight gitconfig-mode "Git Config")
(use-package gitignore-mode
  :delight gitignore-mode "Git Ignore")


(provide 'init-git)
;;; init-git.el ends here
