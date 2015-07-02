;;─────────────────────────────────────────────────────────────────────────────
;; Customize the mode line
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/antonio/delight.el
(use-package delight
  :init
  (delight '((emacs-lisp-mode "Emacs Lisp")
             (lisp-interaction-mode "Lisp Interaction")
             (magit-commit-mode "Magit Commit")
             (magit-log-mode "Magit Log")
             (magit-mode "Magit")
             (magit-status-mode "Magit Status"))))


;; https://github.com/milkypostman/powerline
(use-package powerline
  :init
  (setq
   powerline-height 18
   powerline-default-separator 'wave)

  ;; Define new faces for elements on an active powerline
  (defface my/pl-battery-face '((t (:inherit powerline-active1)))
    "Face used for the battery load.")
  (defface my/pl-battery-charging-face '((t (:inherit powerline-active1)))
    "Face used for the battery load when charging.")
  (defface my/pl-battery-discharging-face '((t (:inherit powerline-active1)))
    "Face used for the battery load when discharging.")
  (defface my/pl-buffer-name-face '((t (:inherit powerline-active2)))
    "Face used for the buffer name.")
  (defface my/pl-buffer-status-face '((t (:inherit powerline-active1)))
    "Face used for the buffer status.")
  (defface my/pl-major-mode-face '((t (:inherit nil)))
    "Face used for the major mode.")
  (defface my/pl-line-number-face '((t (:inherit powerline-active2)))
    "Face used for the line number.")
  (defface my/pl-line-separator-face '((t (:inherit powerline-active2)))
    "Face used for the line separator.")
  (defface my/pl-project-delimiter-face '((t (:inherit powerline-active1)))
    "Face used for the project delimiters.")
  (defface my/pl-project-name-face '((t (:inherit powerline-active1)))
    "Face used for the project name.")
  (defface my/pl-time-face '((t (:inherit nil)))
    "Face used for the time.")
  (defface my/pl-vc-branch-face '((t (:inherit powerline-active1)))
    "Face used for the version control branch.")

  ;; Define new faces for elements on an inactive powerline
  (defface my/pl-battery-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load, in an inactive powerline.")
  (defface my/pl-battery-charging-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load when charging, in an inactive powerline.")
  (defface my/pl-battery-discharging-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load when discharging, in an inactive powerline.")
  (defface my/pl-buffer-name-inactive-face '((t (:inherit powerline-inactive2)))
    "Face used for the buffer name, in an inactive powerline.")
  (defface my/pl-buffer-status-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the buffer status, in an inactive powerline.")
  (defface my/pl-major-mode-inactive-face '((t (:inherit nil)))
    "Face used for the major mode, in an inactive powerline.")
  (defface my/pl-line-number-inactive-face '((t (:inherit powerline-inactive2)))
    "Face used for the line number, in an inactive powerline.")
  (defface my/pl-line-separator-inactive-face '((t (:inherit powerline-inactive2)))
    "Face used for the line separator, in an inactive powerline.")
  (defface my/pl-project-delimiter-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the project delimiters, in an inactive powerline.")
  (defface my/pl-project-name-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the project name, in an inactive powerline.")
  (defface my/pl-time-inactive-face '((t (:inherit nil)))
    "Face used for the time, in an inactive powerline.")
  (defface my/pl-vc-branch-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the version control branch, in an inactive powerline.")

  ;; Define the mode-line format
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))

             ;; NOTE: Will cripple Emacs
             ;; (charging (string-equal "AC" (cdr (assoc 76 (funcall battery-status-function)))))

             ;; Define faces for mode-line elements
             (mode-line-face (if active 'mode-line 'mode-line-inactive))
             (mode-line-major-face (if active 'powerline-active1 'powerline-inactive1))
             (mode-line-minor-face (if active 'powerline-active2 'powerline-inactive2))
             (battery-face (if active 'my/pl-battery-face 'my/pl-battery-inactive-face))
             (buffer-name-face (if active 'my/pl-buffer-name-face 'my/pl-buffer-name-inactive-face))
             (buffer-status-face (if active 'my/pl-buffer-status-face 'my/pl-buffer-status-inactive-face))
             (major-mode-face (if active 'my/pl-major-mode-face 'my/pl-major-mode-inactive-face))
             (line-number-face (if active 'my/pl-line-number-face 'my/pl-line-number-inactive-face))
             (line-separator-face (if active 'my/pl-line-separator-face 'my/pl-line-separator-inactive-face))
             (project-delimiter-face
              (if active 'my/pl-project-delimiter-face 'my/pl-project-delimiter-inactive-face))
             (project-name-face (if active 'my/pl-project-name-face 'my/pl-project-name-inactive-face))
             (time-face (if active 'my/pl-time-face 'my/pl-time-inactive-face))
             (vc-branch-face (if active 'my/pl-vc-branch-face 'my/pl-vc-branch-inactive-face))

             ;; Define faces for separators
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))

             ;; List all the elements on the left
             (lhs (list
                   (powerline-major-mode major-mode-face 'l)
                   (powerline-raw " " nil)
                   (funcall separator-left mode-line-face mode-line-major-face)
                   (powerline-raw "[" project-delimiter-face 'l)
                   (powerline-raw (projectile-project-name) project-name-face)

                   ;; TODO: Remove version control backend
                   (powerline-vc vc-branch-face)

                   (powerline-raw "]" project-delimiter-face)
                   (powerline-raw "%+" buffer-status-face 'l)
                   (powerline-raw " " mode-line-major-face)
                   (funcall separator-left mode-line-major-face mode-line-minor-face)

                   ;; TODO: Add parent directory
                   (powerline-raw "%b" buffer-name-face 'l)

                   (powerline-raw ":" line-separator-face)
                   (powerline-raw "%l" line-number-face)
                   (when (and (boundp 'which-func-mode) which-func-mode)
                     (powerline-raw which-func-format mode-line-minor-face 'l))))

             ;; List all the elements on the right
             (rhs (list
                   (funcall separator-right mode-line-minor-face mode-line-major-face)
                   (powerline-raw " " mode-line-major-face)

                   ;; FIXME: I cannot add a percent character with `battery-mode-line-format'
                   ;; (powerline-raw (concat battery-mode-line-string "%%") battery-discharging-face 'r)
                   (powerline-raw (concat battery-mode-line-string "%%") battery-face 'r)

                   (funcall separator-right mode-line-major-face mode-line-face)
                   (powerline-raw " " nil)
                   (powerline-raw display-time-string time-face 'r))))

        ;; Build the result
        (concat (powerline-render lhs)
                (powerline-fill mode-line-minor-face (powerline-width rhs))
                (powerline-render rhs))))))

  :config

  ;; TODO: This should set faces for the inactive powerline as well
  ;; Colorize the battery load string on update
  (defadvice battery-update (before my/pl-colorize-battery-advice activate)
    "Colorize the battery load string depending on its status (dis/charging)."
    (if (string-equal "AC" (cdr (assoc 76 (funcall battery-status-function))))
        (copy-face 'my/pl-battery-charging-face 'my/pl-battery-face)
      (copy-face 'my/pl-battery-discharging-face 'my/pl-battery-face)))

  ;; FIXME: The mode-line is slightly thinner when using Helm. How to make Helm use powerline?

  ;; Customize fonts within a mode-line
  (when (member me/font-family-mode-line (font-family-list))
    (set-face-attribute 'mode-line nil :font me/font-family-mode-line :height me/font-size-mode-line)
    (set-face-attribute 'mode-line-inactive nil :font me/font-family-mode-line :height me/font-size-mode-line))

  ;; Customize faces for an active mode-line
  (set-face-attribute 'mode-line nil :background zenburn/bg-1 :box `(:line-width 2 :color ,zenburn/bg-1))
  (set-face-attribute 'powerline-active1 nil :background zenburn/bg-0)
  (set-face-attribute 'powerline-active2 nil :background zenburn/bg+0)
  (set-face-attribute 'my/pl-battery-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-battery-charging-face nil :foreground zenburn/green)
  (set-face-attribute 'my/pl-battery-discharging-face nil :foreground zenburn/red)
  (set-face-attribute 'my/pl-buffer-name-face nil :foreground zenburn/green+1)
  (set-face-attribute 'my/pl-buffer-status-face nil :foreground zenburn/red)
  (set-face-attribute 'my/pl-major-mode-face nil :foreground zenburn/fg)
  (set-face-attribute 'my/pl-line-number-face nil :foreground zenburn/fg)
  (set-face-attribute 'my/pl-line-separator-face nil :foreground zenburn/fg)
  (set-face-attribute 'my/pl-project-delimiter-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-project-name-face nil :foreground zenburn/blue-1)
  (set-face-attribute 'my/pl-time-face nil :foreground zenburn/fg)
  (set-face-attribute 'my/pl-vc-branch-face nil :foreground zenburn/bg+3)

  ;; Customize faces for an inactive mode-line
  (set-face-attribute 'mode-line-inactive nil :background zenburn/bg-0
                      :box `(:line-width 2 :color ,zenburn/bg-0))
  (set-face-attribute 'powerline-inactive1 nil :background zenburn/bg+0)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn/bg+1)
  (set-face-attribute 'my/pl-battery-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-battery-charging-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-battery-discharging-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-buffer-name-inactive-face nil :foreground zenburn/fg)
  (set-face-attribute 'my/pl-buffer-status-inactive-face nil :foreground zenburn/red)
  (set-face-attribute 'my/pl-major-mode-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-line-number-inactive-face nil :foreground zenburn/bg)
  (set-face-attribute 'my/pl-line-separator-inactive-face nil :foreground zenburn/bg)
  (set-face-attribute 'my/pl-project-delimiter-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-project-name-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-time-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'my/pl-vc-branch-inactive-face nil :foreground zenburn/bg+3))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-mode-line.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-mode-line)
