;;; use-git.el --- Git facilities                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :disabled
  :config
  (define-fringe-bitmap 'me/diff-hl-insert [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-change [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-delete (make-vector 4 240) nil nil 'top)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  :custom
  (diff-hl-fringe-bmp-function #'me/diff-hl-fringe-bitmap)
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  :hook
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  (prog-mode . diff-hl-mode)
  (text-mode . diff-hl-mode)
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
  (with-eval-after-load 'marginalia
    (dolist (f '(magit-read-branch
                 magit-read-other-branch
                 magit-read-other-branch-or-commit
                 magit-read-branch-prefer-other))
      (advice-add f :around #'me/magit-branch-category))
    (add-to-list
     'marginalia-annotators
     '(magit-branch me/marginalia-annotate-magit-branch builtin none)))
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face))
  (magit-diff-refine-hunk 'all)
  (magit-refs-show-branch-descriptions t)
  (magit-section-initial-visibility-alist
   '((unpulled . show) (unpushed . show) (untracked . show)))
  (magit-section-visibility-indicators nil)
  :preface
  (defun me/magit-branch-category (reader &rest arguments)
    "Augment current completion properties with the `magit-branch' category."
    (let ((completion-extra-properties '(:category magit-branch)))
      (apply reader arguments)))
  (defun me/marginalia-annotate-magit-branch (branch)
    "Return description from BRANCH for marginalia, or nil if unset."
    (when-let* ((label (magit-get "branch" branch "description"))
                ((not (string-empty-p label))))
      (marginalia--fields
       ((string-trim label) :face 'marginalia-documentation :truncate 1.0 )))))

;; NOTE Start a pinentry service automatically in order for Emacs to be able to
;;      prompt passphrases from the minibuffer. If Emacs doesn't redirect
;;      prompts regardless of the value for `epg-pinentry-mode', add
;;      "allow-emacs-pinentry" to ~/.gnupg/gpg-agent.conf.

(use-package pinentry
  :hook
  (after-init . pinentry-start))

;;; use-git.el ends here
