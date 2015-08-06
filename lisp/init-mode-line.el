;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-mode-line.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)
(defvar me/font-family-mode-line)
(defvar me/font-size-mode-line)
(defvar zenburn/bg+3)
(defvar zenburn/bg+1)
(defvar zenburn/bg+0)
(defvar zenburn/bg-0)
(defvar zenburn/bg-1)
(defvar zenburn/blue+1)
(defvar zenburn/blue-1)
(defvar zenburn/fg)
(defvar zenburn/green+1)
(defvar zenburn/green)
(defvar zenburn/orange)
(defvar zenburn/red+1)
(defvar zenburn/red)


;;─────────────────────────────────────────────────────────────────────────────
;; Customize the mode line
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/antonio/delight.el
(use-package delight
  :init
  (delight '((emacs-lisp-mode "Emacs Lisp")
             (lisp-interaction-mode "Lisp Interaction")
             (magit-mode "Magit")
             (magit-commit-mode "Magit Commit")
             (magit-log-mode "Magit Log")
             (magit-popup-mode "Magit Popup")
             (magit-status-mode "Magit Status"))))


;; https://github.com/milkypostman/powerline
(use-package powerline
  :init
  (setq
   powerline-default-separator 'wave
   powerline-height 18)

  ;; Define new faces for elements on an active powerline
  (defface me/pl-battery-face '((t (:inherit powerline-active1)))
    "Face used for the battery load."
    :group 'me/powerline)
  (defface me/pl-battery-charging-face '((t (:inherit powerline-active1)))
    "Face used for the battery load when charging."
    :group 'me/powerline)
  (defface me/pl-battery-discharging-face '((t (:inherit powerline-active1)))
    "Face used for the battery load when discharging."
    :group 'me/powerline)
  (defface me/pl-buffer-name-face '((t (:inherit powerline-active1)))
    "Face used for the buffer name."
    :group 'me/powerline)
  (defface me/pl-buffer-name-modified-face '((t (:inherit me/pl-buffer-name-face)))
    "Face used for the buffer name, modified."
    :group 'me/powerline)
  (defface me/pl-buffer-name-readonly-face '((t (:inherit me/pl-buffer-name-face)))
    "Face used for the buffer name, readonly."
    :group 'me/powerline)
  (defface me/pl-buffer-status-face '((t (:inherit powerline-active1)))
    "Face used for the buffer status."
    :group 'me/powerline)
  (defface me/pl-fc-error-face '((t (:inherit powerline-active1)))
    "Face used for the error count."
    :group 'me/powerline)
  (defface me/pl-fc-info-face '((t (:inherit powerline-active1)))
    "Face used for the info count."
    :group 'me/powerline)
  (defface me/pl-fc-warning-face '((t (:inherit powerline-active1)))
    "Face used for the warning count."
    :group 'me/powerline)
  (defface me/pl-major-mode-face '((t (:inherit nil)))
    "Face used for the major mode."
    :group 'me/powerline)
  (defface me/pl-line-number-face '((t (:inherit powerline-active1)))
    "Face used for the line number."
    :group 'me/powerline)
  (defface me/pl-line-separator-face '((t (:inherit powerline-active1)))
    "Face used for the line separator."
    :group 'me/powerline)
  (defface me/pl-project-delimiter-face '((t (:inherit powerline-active1)))
    "Face used for the project delimiters."
    :group 'me/powerline)
  (defface me/pl-project-name-face '((t (:inherit powerline-active1)))
    "Face used for the project name."
    :group 'me/powerline)
  (defface me/pl-time-face '((t (:inherit nil)))
    "Face used for the time."
    :group 'me/powerline)
  (defface me/pl-vc-branch-face '((t (:inherit powerline-active1)))
    "Face used for the version control branch."
    :group 'me/powerline)

  ;; Define new faces for elements on an inactive powerline
  (defface me/pl-battery-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-battery-charging-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load when charging, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-battery-discharging-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the battery load when discharging, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-buffer-name-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the buffer name, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-buffer-name-modified-inactive-face '((t (:inherit me/pl-buffer-name-inactive-face)))
    "Face used for the buffer name, modified and in an inactive powerlin."
    :group 'me/powerline)
  (defface me/pl-buffer-name-readonly-inactive-face '((t (:inherit me/pl-buffer-name-inactive-face)))
    "Face used for the buffer name, readonly and in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-buffer-status-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the buffer status, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-fc-error-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the error count, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-fc-info-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the info count, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-fc-warning-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the warning count, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-major-mode-inactive-face '((t (:inherit nil)))
    "Face used for the major mode, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-line-number-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the line number, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-line-separator-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the line separator, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-project-delimiter-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the project delimiters, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-project-name-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the project name, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-time-inactive-face '((t (:inherit nil)))
    "Face used for the time, in an inactive powerline."
    :group 'me/powerline)
  (defface me/pl-vc-branch-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the version control branch, in an inactive powerline."
    :group 'me/powerline)

  ;; Shamelessly taken from spacemacs
  (defmacro me/flycheck-lighter (error)
    "Return a formatted string describing the ERROR (error, warning, info) count."
    `(let* ((error-counts (flycheck-count-errors flycheck-current-errors))
            (errorp (flycheck-has-current-errors-p ',error))
            (count (or (cdr (assq ',error error-counts)) "?"))
            (running (eq 'running flycheck-last-status-change)))
       (if (or errorp running) (format "• %s" count))))
       ;; (format "%s" count)))

  ;; Define the mode-line format
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))

             ;; Define faces for mode-line elements
             (mode-line-face (if active 'mode-line 'mode-line-inactive))
             (mode-line-major-face (if active 'powerline-active1 'powerline-inactive1))
             (mode-line-minor-face (if active 'powerline-active2 'powerline-inactive2))
             (battery-face (if active 'me/pl-battery-face 'me/pl-battery-inactive-face))
             (buffer-name-face (if active 'me/pl-buffer-name-face 'me/pl-buffer-name-inactive-face))
             (buffer-status-face (if active 'me/pl-buffer-status-face 'me/pl-buffer-status-inactive-face))
             (flycheck-error-face (if active 'me/pl-fc-error-face 'me/pl-fc-error-inactive-face))
             (flycheck-info-face (if active 'me/pl-fc-info-face 'me/pl-fc-info-inactive-face))
             (flycheck-warning-face (if active 'me/pl-fc-warning-face 'me/pl-fc-warning-inactive-face))
             (major-mode-face (if active 'me/pl-major-mode-face 'me/pl-major-mode-inactive-face))
             (line-number-face (if active 'me/pl-line-number-face 'me/pl-line-number-inactive-face))
             (line-separator-face (if active 'me/pl-line-separator-face 'me/pl-line-separator-inactive-face))
             (project-delimiter-face (if active 'me/pl-project-delimiter-face 'me/pl-project-delimiter-inactive-face))
             (project-name-face (if active 'me/pl-project-name-face 'me/pl-project-name-inactive-face))
             (time-face (if active 'me/pl-time-face 'me/pl-time-inactive-face))
             (vc-branch-face (if active 'me/pl-vc-branch-face 'me/pl-vc-branch-inactive-face))

             ;; Define faces for separators
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))

             ;; List all the elements on the left
             (lhs (append
                   (list
                    (powerline-major-mode major-mode-face 'l)
                    (powerline-raw " " nil)
                    (funcall separator-left mode-line-face mode-line-major-face)
                    )
                   (list
                    (powerline-raw "[" project-delimiter-face 'l)
                    (powerline-raw (projectile-project-name) project-name-face)
                    ;; TODO: Remove version control backend
                    (powerline-vc vc-branch-face)
                    (powerline-raw "]" project-delimiter-face)
                    (powerline-raw "%+" buffer-status-face 'l)
                    ;; TODO: Add parent directory
                    (powerline-raw "%b" buffer-name-face 'l)
                    (powerline-raw ":" line-separator-face)
                    (powerline-raw "%l" line-number-face)
                    (powerline-raw " " mode-line-major-face)
                    (funcall separator-left mode-line-major-face mode-line-minor-face)
                    )
                   ))

             ;; List all the elements on the right
             (rhs (append
                   (when (and (bound-and-true-p flycheck-mode)
                              (or flycheck-current-errors (eq 'running flycheck-last-status-change)))
                     (list
                      (funcall separator-right mode-line-minor-face mode-line-major-face)
                      (powerline-raw " " mode-line-major-face)
                      (powerline-raw (me/flycheck-lighter error) flycheck-error-face 'r)
                      (powerline-raw (me/flycheck-lighter warning) flycheck-warning-face 'r)
                      (powerline-raw (me/flycheck-lighter info) flycheck-info-face'r)
                      (funcall separator-right mode-line-major-face mode-line-minor-face)
                      ))
                   (list
                    (funcall separator-right mode-line-minor-face mode-line-major-face)
                    (powerline-raw " " mode-line-major-face)
                    ;; FIXME: I cannot add a percent character with `battery-mode-line-format'
                    ;; FIXME: Some red color is automatically added under 11% battery load
                    (powerline-raw (concat battery-mode-line-string "%%") battery-face 'r)
                    )
                   (list
                    (funcall separator-right mode-line-major-face mode-line-face)
                    (powerline-raw " " nil)
                    (powerline-raw display-time-string time-face 'r)
                    ))))

        ;; Build the result
        (concat (powerline-render lhs)
                (powerline-fill mode-line-minor-face (powerline-width rhs))
                (powerline-render rhs))))))

  :config

  ;; TODO: This should set faces for the inactive powerline as well
  ;; Colorize the battery load string on update
  (defadvice battery-update (before me/pl-colorize-battery-advice activate)
    "Colorize the battery load string depending on its status (dis/charging)."
    (if (string-equal "AC" (cdr (assoc 76 (funcall battery-status-function))))
        (copy-face 'me/pl-battery-charging-face 'me/pl-battery-face)
      (copy-face 'me/pl-battery-discharging-face 'me/pl-battery-face)))

  ;; FIXME: The mode-line is slightly thinner when using Helm. How to make Helm use powerline?

  ;; Customize fonts within a mode-line
  (when (member me/font-family-mode-line (font-family-list))
    (set-face-attribute 'mode-line nil :font me/font-family-mode-line :height me/font-size-mode-line)
    (set-face-attribute 'mode-line-inactive nil :font me/font-family-mode-line :height me/font-size-mode-line))

  ;; Customize faces for an active mode-line
  (set-face-attribute 'mode-line nil :background zenburn/bg-1 :box `(:line-width 2 :color ,zenburn/bg-1))
  (set-face-attribute 'powerline-active1 nil :background zenburn/bg-0)
  (set-face-attribute 'powerline-active2 nil :background zenburn/bg+0)
  (set-face-attribute 'me/pl-battery-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-battery-charging-face nil :foreground zenburn/green)
  (set-face-attribute 'me/pl-battery-discharging-face nil :foreground zenburn/red)
  (set-face-attribute 'me/pl-buffer-name-face nil :foreground zenburn/green+1)
  (set-face-attribute 'me/pl-buffer-status-face nil :foreground zenburn/red)
  (set-face-attribute 'me/pl-fc-error-face nil :foreground zenburn/red+1)
  (set-face-attribute 'me/pl-fc-info-face nil :foreground zenburn/blue+1)
  (set-face-attribute 'me/pl-fc-warning-face nil :foreground zenburn/orange)
  (set-face-attribute 'me/pl-major-mode-face nil :foreground zenburn/fg)
  (set-face-attribute 'me/pl-line-number-face nil :foreground zenburn/fg)
  (set-face-attribute 'me/pl-line-separator-face nil :foreground zenburn/fg)
  (set-face-attribute 'me/pl-project-delimiter-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-project-name-face nil :foreground zenburn/blue-1)
  (set-face-attribute 'me/pl-time-face nil :foreground zenburn/fg)
  (set-face-attribute 'me/pl-vc-branch-face nil :foreground zenburn/bg+3)

  ;; Customize faces for an inactive mode-line
  (set-face-attribute 'mode-line-inactive nil :background zenburn/bg-0
                      :box `(:line-width 2 :color ,zenburn/bg-0))
  (set-face-attribute 'powerline-inactive1 nil :background zenburn/bg+0)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn/bg+1)
  (set-face-attribute 'me/pl-battery-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-battery-charging-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-battery-discharging-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-buffer-name-inactive-face nil :foreground zenburn/fg)
  (set-face-attribute 'me/pl-buffer-status-inactive-face nil :foreground zenburn/red)
  (set-face-attribute 'me/pl-fc-error-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-fc-info-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-fc-warning-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-major-mode-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-line-number-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-line-separator-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-project-delimiter-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-project-name-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-time-inactive-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/pl-vc-branch-inactive-face nil :foreground zenburn/bg+3))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-mode-line.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-mode-line)
