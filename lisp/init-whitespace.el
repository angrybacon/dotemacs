;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-whitespace.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)
(defvar zenburn/bg+2)
(defvar zenburn/bg+1)
(defvar zenburn/red)


;;─────────────────────────────────────────────────────────────────────────────
;; Visualize blank characters
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package whitespace
  :config
  (set-face-attribute 'whitespace-empty nil :background zenburn/bg+1)
  (set-face-attribute 'whitespace-indentation nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-after-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-before-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space nil :background 'unspecified :foreground zenburn/bg+2))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-whitespace.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-whitespace)
