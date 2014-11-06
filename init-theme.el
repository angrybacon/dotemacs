;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))


;; Set color theme
(load-theme 'solarized-dark t)


;; Face customization
(set-face-attribute 'show-paren-match nil :weight 'normal)
(set-face-italic-p 'font-lock-string-face 1)
(set-face-italic-p 'font-lock-comment-face 1)
(set-face-foreground 'fringe "#384E55")


;; Helm face customization (M-x helm-colors)
(set-face-underline-p 'helm-selection nil)
(set-face-underline-p 'helm-grep-file t)
(set-face-background 'helm-match "#002b36")
(set-face-foreground 'helm-match "#CB4B16")
(set-face-background 'helm-grep-match "#002b36")
(set-face-foreground 'helm-grep-match "#CB4B16")
