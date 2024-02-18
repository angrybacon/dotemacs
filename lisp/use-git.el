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

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package git-modes)

(use-package magit
  :defines
  magit-file-section-map
  magit-hunk-section-map
  magit-section-mode-map
  :functions
  magit-diff-highlight
  :bind
  (:map magit-file-section-map
   ("RET" . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("RET" . magit-diff-visit-file-other-window))
  :config
  (define-key magit-section-mode-map (kbd "M-1") nil :remove)
  (define-key magit-section-mode-map (kbd "M-2") nil :remove)
  (define-key magit-section-mode-map (kbd "M-3") nil :remove)
  (define-key magit-section-mode-map (kbd "M-4") nil :remove)
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face))
  (magit-diff-refine-hunk 'all)
  (magit-section-initial-visibility-alist '((unpushed . show)))
  (magit-section-visibility-indicator '("…" . nil)))

;; NOTE Start a pinentry service automatically in order for Emacs to be able to
;;      prompt passphrases from the minibuffer. If Emacs doesn't redirect
;;      prompts regardless of the value for `epg-pinentry-mode', add
;;      "allow-emacs-pinentry" to ~/.gnupg/gpg-agent.conf.

(use-package pinentry
  :hook
  (after-init . pinentry-start))

;;; use-git.el ends here
