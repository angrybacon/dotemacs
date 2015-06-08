;;─────────────────────────────────────────────────────────────────────────────
;; Add incremental completion and selection narrowing
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/antonio/delight.el
(use-package delight
  :init
  (delight 'emacs-lisp-mode "Emacs Lisp")
  (delight 'lisp-interaction-mode "Lisp Interaction"))


(use-package smart-mode-line
  :init
  (setq
   display-time-24hr-format t
   display-time-default-load-average nil
   display-time-format "%H:%M"
   display-time-load-average-threshold 3.0
   rm-whitelist '("")
   sml/projectile-replacement-format "[%s] "
   sml/use-projectile-p 'before-prefixes
   sml/battery-format "%p%% "
   sml/show-remote t
   sml/vc-mode-show-backend nil
   sml/theme 'dark)
  :config
  (sml/setup)

  ;; Customize built-in faces
  (when (member me/font-family-mode-line (font-family-list))
    (set-face-attribute 'mode-line nil :font me/font-family-mode-line :height me/font-size-mode-line)
    (set-face-attribute 'mode-line-inactive nil :font me/font-family-mode-line :height me/font-size-mode-line))
  (set-face-attribute 'mode-line nil :background zenburn/bg-1 :box `(:line-width 4 :color ,zenburn/bg-1))
  (set-face-attribute 'mode-line-inactive nil :background zenburn/bg-0 :slant 'unspecified
                      :box `(:line-width 4 :color ,zenburn/bg-0))

  ;; Customize smart-mode-line faces
  (set-face-attribute 'sml/charging nil :foreground zenburn/green)
  (set-face-attribute 'sml/discharging nil :foreground zenburn/red)
  (set-face-attribute 'sml/filename nil :foreground zenburn/yellow)
  (set-face-attribute 'sml/git nil :foreground zenburn/blue-1)
  (set-face-attribute 'sml/global nil :foreground zenburn/bg+3)
  (set-face-attribute 'sml/line-number nil :foreground zenburn/fg :weight 'unspecified)
  (set-face-attribute 'sml/modes nil :foreground 'unspecified)
  (set-face-attribute 'sml/modified nil :foreground zenburn/red :weight 'unspecified)
  (set-face-attribute 'sml/prefix nil :foreground zenburn/orange)
  (set-face-attribute 'sml/read-only nil :foreground zenburn/blue)
  (set-face-attribute 'sml/vc-edited nil :foreground zenburn/yellow))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-mode-line.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-mode-line)
