;;─────────────────────────────────────────────────────────────────────────────
;; Visualize blank characters
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package whitespace
  :init
  (setq whitespace-style
        '(face tabs spaces trailing space-before-tab indentation empty space-after-tab space-mark tab-mark))
  :config

  ;; FIXME: Overwritten by Zenburn theme
  (set-face-attribute 'whitespace-empty nil :background zenburn/bg+1)
  (set-face-attribute 'whitespace-indentation nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-after-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-before-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space nil :background 'unspecified :foreground zenburn/bg+2)

  )



;;─────────────────────────────────────────────────────────────────────────────
;; End init-whitespace.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-whitespace)
