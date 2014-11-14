;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))


;; Set color theme
(load-theme 'zenburn t)


;; Face customization
(set-face-attribute 'dired-header nil
                    :background 'unspecified
                    :foreground "#268BD2"
                    :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)
(set-face-attribute 'font-lock-warning-face nil :italic nil)
(set-face-attribute 'fringe nil :foreground "#384E55")
(set-face-attribute 'match nil :background 'unspecified :foreground "#B58900" :weight 'normal)
(set-face-attribute 'show-paren-match nil :weight 'normal)


;; Company face customization
(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip-common nil
                      :background 'unspecified
                      :foreground "#B58900"
                      :inherit 'company-tooltip
                      :underline nil
                      :weight 'normal)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :background 'unspecified
                      :foreground "#B58900"
                      :inherit 'company-tooltip-selection
                      :underline nil
                      :weight 'normal)
  )


;; Helm face customization
(with-eval-after-load 'helm
  (set-face-attribute 'helm-action nil :underline t :weight 'normal)
  (set-face-attribute 'helm-grep-file nil :underline nil :weight 'normal)
  (set-face-attribute 'helm-moccur-buffer nil :underline t :weight 'normal)
  (set-face-attribute 'helm-selection nil :underline 'unspecified)
  (set-face-attribute 'helm-source-header nil
                      :background 'unspecified
                      :foreground "#268BD2"
                      :weight 'bold)
  (set-face-attribute 'helm-header nil :inherit 'font-lock-comment-face)
  )
