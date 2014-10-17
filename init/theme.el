;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))


;; Set color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark 1)


;; Face customization
(set-face-italic-p 'font-lock-string-face 1)
(set-face-italic-p 'font-lock-comment-face 1)
(set-face-background 'linum "#073642")


;; Customize the mode line
(add-to-list 'load-path "~/.emacs.d/packages/delight/")
(require 'delight)
(delight 'indent-guide-mode " ig" "indent-guide")
(delight 'emmet-mode " emmet" "emmet-mode")
(delight 'emacs-lisp-mode "el" :major)
