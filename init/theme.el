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
(delight 'css-mode               "css"       :major)
(delight 'emacs-lisp-mode        "elisp"     :major)
(delight 'html-mode              "html"      :major)
(delight 'lisp-interaction-mode  "lisp"      :major)
(delight 'python-mode            "python"    :major)
(delight 'scss-mode              "scss"      :major)
(delight 'auto-complete-mode     " ac"       "auto-complete")
(delight 'autopair-mode          " ap"       "autopair")
(delight 'emmet-mode             " emmet"    "emmet-mode")
(delight 'indent-guide-mode      " ig"       "indent-guide")
(delight 'subword-mode           " subword"  "subword-mode")
