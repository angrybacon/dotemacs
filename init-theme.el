;; Set color theme
(load-theme 'zenburn t)


;; Set font
(when (member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12"))


;; Copy a face's attributes onto one another
(defun me/set-face-attribute (target inherit)
  (let ((attributes(face-all-attributes inherit (selected-frame))))
    (mapcar (lambda (x) (set-face-attribute target nil (car x) (cdr x))) attributes)))


;; Face customization
(set-face-attribute 'font-lock-doc-face nil :italic t)
(set-face-attribute 'font-lock-constant-face nil :foreground zenburn/green-1)
(set-face-attribute 'font-lock-comment-face nil :foreground zenburn/fg-1 :italic t)
(set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/fg-1)
(set-face-attribute 'header-line nil :background 'unspecified :foreground zenburn/blue :bold t :box nil)
(set-face-attribute 'helm-match nil :foreground "gold1")
(set-face-attribute 'isearch nil :foreground "gold1" :background 'unspecified)
(set-face-attribute 'lazy-highlight nil :foreground "gold3" :background 'unspecified)
(set-face-attribute 'show-paren-match nil :background 'unspecified)
(set-face-attribute 'show-paren-mismatch nil :background 'unspecified)
(set-face-attribute 'region nil :foreground zenburn/green)
(set-face-attribute 'vertical-border nil :foreground zenburn/bg-1)


;; Company face customization
(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection))


;; Helm face customization
(with-eval-after-load 'helm
  (when (member "Monaco" (font-family-list)) (set-face-attribute 'helm-source-header nil :font "Monaco-14"))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil :foreground zenburn/blue :background zenburn/bg :box nil))
(with-eval-after-load 'helm-command
  (set-face-attribute 'helm-M-x-key nil :underline nil))


;; Magit face customization
(with-eval-after-load 'magit
  (when (member "Monaco" (font-family-list)) (set-face-attribute 'magit-section-title nil :font "Monaco-14"))
  (set-face-attribute 'magit-section-title nil :weight 'unspecified :foreground zenburn/blue))
