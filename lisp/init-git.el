;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-git.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)


(defvar me/font-family-header)
(defvar me/font-size-header)
(defvar zenburn/red)
(defvar zenburn/red-4)
(defvar zenburn/green+2)
(defvar zenburn/green-1)


;;─────────────────────────────────────────────────────────────────────────────
;; Add support for Git operations
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :init
  (setq
   magit-refs-show-commit-count (quote all)
   magit-section-show-child-count t)
  :bind
  ("C-c g" . magit-status)
  :config
  (when (member me/font-family-header (font-family-list))
    (set-face-attribute 'magit-popup-heading nil :font me/font-family-header :height me/font-size-header)
    (set-face-attribute 'magit-section-heading nil :font me/font-family-header :height me/font-size-header))
  (set-face-attribute 'magit-diff-added nil :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-added-highlight nil :background zenburn/green-1 :foreground zenburn/green+2)
  (set-face-attribute 'magit-diff-removed nil :background zenburn/red-4 :foreground zenburn/red)
  (set-face-attribute 'magit-diff-removed-highlight nil :background zenburn/red-4 :foreground zenburn/red))


;; https://github.com/magit/git-modes
(use-package gitattributes-mode
  :delight gitattributes-mode "Git Attributes")
(use-package gitconfig-mode
  :delight gitconfig-mode "Git Config")
(use-package gitignore-mode
  :delight gitignore-mode "Git Ignore")


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-git.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-git)
