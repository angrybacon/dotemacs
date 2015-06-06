;;─────────────────────────────────────────────────────────────────────────────
;; Add support for Git operations
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magit/magit
(use-package magit
  :ensure t

  ;; FIXME: I can't make multiple call to :delight
  ;;        See https://github.com/jwiegley/use-package/pull/206
  :delight magit-status-mode "magit-status"

  ;; '((magit-commit-mode "magit-commit")
  ;;   (magit-log-mode "magit-log")
  ;;   (magit-status-mode "magit-status"))

  :init
  (setq
   magit-show-child-count t
   magit-stage-all-confirm nil
   magit-unstage-all-confirm nil)
  :bind
  ("C-c g" . magit-status)
  :config
  (when (member me/font-family-header (font-family-list))
    (set-face-attribute 'magit-section-title nil :font me/font-family-header :height me/font-size-header))
  (set-face-attribute 'magit-section-title nil :weight 'unspecified :foreground zenburn/blue))


;; https://github.com/magit/git-modes
(use-package gitattributes-mode
  :delight gitattributes-mode "gitattributes")
(use-package gitconfig-mode
  :delight gitconfig-mode "gitconfig")
(use-package gitignore-mode
  :delight gitignore-mode "gitignore")


;;─────────────────────────────────────────────────────────────────────────────
;; End init-git.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-git)
