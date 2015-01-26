;; Set font
(when (member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12"))


;; Set color theme
(load-theme 'jazz t)


;; Jazz palette (https://github.com/donderom/jazz-theme)
(defconst jazz/bg-1     "#101010" "Jazz palette: bg-1.")
(defconst jazz/bg       "#151515" "Jazz palette: bg.")
(defconst jazz/bg+1     "#202020" "Jazz palette: bg+1.")
(defconst jazz/bg+2     "#505050" "Jazz palette: bg+2.")
(defconst jazz/bg+3     "#606060" "Jazz palette: bg+3.")
(defconst jazz/blue     "#385E6B" "Jazz palette: blue.")
(defconst jazz/blue+1   "#5C737C" "Jazz palette: blue+1.")
(defconst jazz/cyan     "#34676F" "Jazz palette: cyan.")
(defconst jazz/fg       "#C6A57B" "Jazz palette: fg.")
(defconst jazz/green    "#546A29" "Jazz palette: green.")
(defconst jazz/green+1  "#7E9960" "Jazz palette: green+1.")
(defconst jazz/magenta  "#7F355E" "Jazz palette: magenta.")
(defconst jazz/orange   "#BA5B34" "Jazz palette: orange.")
(defconst jazz/red      "#953331" "Jazz palette: red.")
(defconst jazz/red+1    "#8D4A4A" "Jazz palette: red+1.")
(defconst jazz/yellow   "#909737" "Jazz palette: yellow.")
(defconst jazz/yellow+1 "#96A62D" "Jazz palette: yellow+.")


;; Solarized palette (http://ethanschoonover.com/solarized#the-values)
(defconst solarized/base03  "#002B36" "Solarized palette: base03.")
(defconst solarized/base02  "#073642" "Solarized palette: base02.")
(defconst solarized/base01  "#586E75" "Solarized palette: base01.")
(defconst solarized/base00  "#657B83" "Solarized palette: base00.")
(defconst solarized/base0   "#839496" "Solarized palette: base0.")
(defconst solarized/base1   "#93A1A1" "Solarized palette: base1.")
(defconst solarized/base2   "#EEE8D5" "Solarized palette: base2.")
(defconst solarized/base3   "#FDF6E3" "Solarized palette: base3.")
(defconst solarized/yellow  "#B58900" "Solarized palette: yellow.")
(defconst solarized/orange  "#CB4B16" "Solarized palette: orange.")
(defconst solarized/red     "#DC322F" "Solarized palette: red.")
(defconst solarized/magenta "#D33682" "Solarized palette: magenta.")
(defconst solarized/violet  "#6C71C4" "Solarized palette: violet.")
(defconst solarized/blue    "#268BD2" "Solarized palette: blue.")
(defconst solarized/cyan    "#2AA198" "Solarized palette: cyan.")
(defconst solarized/green   "#859900" "Solarized palette: green.")


;; Color constants for further use
(defconst me/background-color  jazz/bg        "Custom palette: background.")
(defconst me/border-color      jazz/bg+1      "Custom palette: vertical border color.")
(defconst me/comment-color     jazz/bg+2      "Custom palette: comments.")
(defconst me/company-bg-color  jazz/blue+1    "Custom palette: company background.")
(defconst me/company-fg-color  jazz/bg-1      "Custom palette: company foreground.")
(defconst me/company-sb-color  jazz/bg+2      "Custom palette: company scrollbar background.")
(defconst me/company-sf-color  jazz/bg-1      "Custom palette: company scrollbar foreground.")
(defconst me/company-sl-color  jazz/red+1     "Custom palette: company selection.")
(defconst me/header-color      jazz/green+1   "Custom palette: headers.")
(defconst me/highlight-color   jazz/bg+1      "Custom palette: highlight.")
(defconst me/line-ab-color     jazz/bg+1    "Custom palette: active mode line background.")
(defconst me/line-af-color     jazz/green     "Custom palette: active mode line foreground.")
(defconst me/line-ib-color     jazz/bg+1    "Custom palette: inactive mode line background.")
(defconst me/line-if-color     jazz/bg+2     "Custom palette: inactive mode line foreground.")
(defconst me/match-color       jazz/yellow+1  "Custom palette: matching strings.")
(defconst me/whitespace-color  jazz/red       "Custom palette: trailingv whitespaces.")


;; Base faces for further use
(make-face 'me/comment-face)
(make-face 'me/header-face)
(make-face 'me/match-face)
(set-face-attribute 'me/comment-face nil
                    :background 'unspecified :box nil :foreground me/comment-color
                    :italic t :weight 'normal)
(set-face-attribute 'me/header-face nil
                    :background 'unspecified :box nil :foreground me/header-color
                    :italic nil :weight 'bold)
(set-face-attribute 'me/match-face nil
                    :background 'unspecified :box nil :foreground me/match-color
                    :italic nil :weight 'normal)


;; Copy a face's attributes onto one another
(defun me/set-face-attribute (target inherit)
  (let ((attributes(face-all-attributes inherit (selected-frame))))
    (mapcar (lambda (x) (set-face-attribute target nil (car x) (cdr x))) attributes)))


;; Face customization
(me/set-face-attribute 'dired-header 'me/header-face)
(me/set-face-attribute 'font-lock-comment-face 'me/comment-face)
(me/set-face-attribute 'match 'me/match-face)
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground me/comment-color)
(set-face-attribute 'fringe nil :foreground me/comment-color)
(set-face-attribute 'highlight nil :background me/highlight-color)
(set-face-attribute 'show-paren-match nil :weight 'normal)
(set-face-attribute 'trailing-whitespace nil :background me/whitespace-color)


;; Company face customization
(with-eval-after-load 'company
  (me/set-face-attribute 'company-tooltip-common 'me/match-face)
  (me/set-face-attribute 'company-tooltip-common-selection 'me/match-face)
  (set-face-attribute 'company-tooltip nil :background me/company-bg-color)
  (set-face-attribute 'company-tooltip nil :foreground me/company-fg-color)
  (set-face-attribute 'company-scrollbar-bg nil :background me/company-sb-color)
  (set-face-attribute 'company-scrollbar-fg nil :background me/company-sf-color)
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection)
  (set-face-attribute 'company-tooltip-search nil :background me/company-sl-color)
  (set-face-attribute 'company-tooltip-selection nil :background me/company-sl-color)
  )


;; Helm face customization
(with-eval-after-load 'helm
  (me/set-face-attribute 'helm-header 'me/comment-face)
  (me/set-face-attribute 'helm-source-header 'me/header-face)
  (set-face-attribute 'helm-action nil :underline t :weight 'normal)
  (set-face-attribute 'helm-grep-file nil :underline nil :weight 'normal)
  (set-face-attribute 'helm-header nil :background me/background-color)
  (set-face-attribute 'helm-match nil :foreground me/match-color)
  (set-face-attribute 'helm-moccur-buffer nil :underline t :weight 'normal)
  (set-face-attribute 'helm-selection nil :background me/highlight-color :underline 'unspecified)
  )


;; Ace Jump Mode face customization
(with-eval-after-load 'ace-jump-mode
  (set-face-attribute 'ace-jump-face-foreground nil :foreground me/match-color))


;; What's this ?
;; (set-face-attribute 'font-lock-doc-face nil :italic t)
;; (set-face-attribute 'font-lock-warning-face nil :italic nil)
