;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))


;; Set color theme
(load-theme 'solarized-dark t)


;; Face customization
(set-face-italic-p 'font-lock-string-face 1)
(set-face-italic-p 'font-lock-comment-face 1)
(set-face-background 'linum "#073642")


;; Customize the mode line
(add-to-list 'load-path "~/.emacs.d/packages/delight/")
(require 'delight)
(delight 'auto-complete-mode " autocomplete" "auto-complete")
(delight 'autopair-mode " autopair" "autopair")
(delight 'emmet-mode " emmet" "emmet-mode")
(delight 'indent-guide-mode " indent" "indent-guide")
(delight 'css-mode "css" :major)
(delight 'html-mode "html" :major)
(delight 'scss-mode "scss" :major)
(delight 'emacs-lisp-mode "elisp" :major)
