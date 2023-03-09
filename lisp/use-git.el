;;; use-git.el --- Git facilities                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :config
  (define-fringe-bitmap 'me/diff-hl-insert [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-change [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-delete (make-vector 6 240) nil nil 'top)
  ;; (with-eval-after-load 'magit
  ;;   (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  ;;   (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  :custom
  (diff-hl-fringe-bmp-function #'me/diff-hl-fringe-bitmap)
  (diff-hl-show-staged-changes nil)
  :hook
  ((prog-mode text-mode) . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  :preface
  (defun me/diff-hl-fringe-bitmap (type _position)
    "Return the name of the bitmap to use for a given change TYPE."
    (intern (format "me/diff-hl-%s" type))))

(use-package ediff-wind
  :ensure nil
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package git-modes)

(use-package magit
  :defines
  (magit-file-section-map
   magit-hunk-section-map
   magit-section-mode-map
   magit-status-mode-map)
  :functions magit-add-section-hook magit-diff-highlight
  :bind
  (:map magit-file-section-map
   ("<return>" . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("<return>" . magit-diff-visit-file-other-window)
   :map magit-section-mode-map
   ("M-1" . nil)
   ("M-2" . nil)
   ("M-3" . nil)
   ("M-4" . nil)
   :map magit-status-mode-map
   ("M-1" . nil)
   ("M-2" . nil)
   ("M-3" . nil)
   ("M-4" . nil))
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face))
  (magit-diff-refine-hunk 'all)
  (magit-section-initial-visibility-alist '((unpushed . show)))
  :config
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-modules-overview
   'magit-insert-merge-log)
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight))

;; NOTE Start a pinentry service automatically in order for Emacs to be able to
;;      prompt passphrases from the minibuffer. If Emacs doesn't redirect
;;      prompts regardless of the value for `epg-pinentry-mode', add
;;      "allow-emacs-pinentry" to ~/.gnupg/gpg-agent.conf.

(use-package pinentry
  :hook
  (after-init . pinentry-start))

(use-package transient
  :init
  (setq-default
   transient-history-file (shelldock "transient/history.el")
   transient-levels-file (shelldock "transient/levels.el")
   transient-values-file (shelldock "transient/values.el"))
  :custom
  (transient-default-level 5)
  (transient-show-popup nil))

;;; use-git.el ends here
