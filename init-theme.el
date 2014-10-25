;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))


;; Set color theme
(load-theme 'solarized-dark t)


;; Face customization
(set-face-italic-p 'font-lock-string-face 1)
(set-face-italic-p 'font-lock-comment-face 1)
(set-face-background 'linum "#073642")
