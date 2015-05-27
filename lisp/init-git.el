;;─────────────────────────────────────────────────────────────────────────────
;; Add support for Git operations
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magit/magit
(use-package magit
  :ensure t
  ;; TOFIX: I can't make multiple call to :delight
  :delight magit-status-mode "magit-status"
  ;; magit-commit-mode "magit-commit"
  ;; magit-log-mode "magit-log"
  :init
  (setq
   magit-show-child-count t
   magit-stage-all-confirm nil
   magit-unstage-all-confirm nil)
  :bind
  ("C-c g" . magit-status)
  :config
  (when (member "Monaco" (font-family-list)) (set-face-attribute 'magit-section-title nil :font "Monaco-14"))
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
