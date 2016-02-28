;;; init-mode-line.el --- Prettify the mode-line

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 1 Jun 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Delight
;;=============================================================================


;; Website: https://github.com/antonio/delight.el
(use-package delight

  ;; NOTE: There are conflicts between `delight' and `powerline'.
  ;;       Following code is copied and adapted from
  ;;       http://emacs.stackexchange.com/q/20605/2397.

  :preface (defvar inhibit-mode-name-delight)
  :init (setq-default inhibit-mode-name-delight t)
  :config
  (ad-disable-advice 'format-mode-line 'around 'delighted-modes-are-glum)
  (ad-activate 'format-mode-line)
  (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
    (let ((inhibit-mode-name-delight nil))
      ad-do-it))
  (defadvice powerline-minor-modes (around delight-powerline-minor-modes activate)
    (let ((inhibit-mode-name-delight nil))
      ad-do-it)))


;;=============================================================================
;; Configure Powerline
;;=============================================================================


;; Website: https://github.com/milkypostman/powerline
(use-package powerline

  :defines
  (me/font-family-mode-line
   me/font-size-mode-line
   zenburn/bg+3
   zenburn/bg+1
   zenburn/bg-0
   zenburn/bg-1
   zenburn/blue+1
   zenburn/blue-1
   zenburn/fg
   zenburn/green
   zenburn/orange
   zenburn/red)

  :preface

  ;; Define new faces for elements on an active buffer
  (defface me/fc-error-face '((t (:inherit powerline-active1)))
    "Face used for the error count."
    :group 'me/powerline)
  (defface me/fc-info-face '((t (:inherit powerline-active1)))
    "Face used for the info count."
    :group 'me/powerline)
  (defface me/fc-warning-face '((t (:inherit powerline-active1)))
    "Face used for the warning count."
    :group 'me/powerline)
  (defface me/projectile-face '((t (:inherit powerline-active1)))
    "Face used for the projectile string."
    :group 'me/powerline)
  (defface me/vc-face '((t (:inherit powerline-active1)))
    "Face used for the version control string."
    :group 'me/powerline)

  ;; Define new faces for elements on an inactive buffer
  (defface me/fc-error-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the error count, in an inactive buffer."
    :group 'me/powerline)
  (defface me/fc-info-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the info count, in an inactive buffer."
    :group 'me/powerline)
  (defface me/fc-warning-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the warning count, in an inactive buffer."
    :group 'me/powerline)
  (defface me/projectile-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the projectile string, in an inactive buffer."
    :group 'me/powerline)
  (defface me/vc-inactive-face '((t (:inherit powerline-inactive1)))
    "Face used for the version control string, in an inactive buffer."
    :group 'me/powerline)

  ;; Light flycheck indicators
  (defmacro me/flycheck-lighter (error)
    "Return a formatted string describing the ERROR (error, warning, info) count."
    ;; NOTE: Shamelessly taken from spacemacs
    `(let* ((error-counts (flycheck-count-errors flycheck-current-errors))
            (errorp (flycheck-has-current-errors-p ',error))
            (count (or (cdr (assq ',error error-counts)) "?"))
            (running (eq 'running flycheck-last-status-change)))
       (if (or errorp running) (format "• %s" count))))

  :init

  ;; Customize appearance
  (setq-default
   powerline-default-separator 'wave
   powerline-height 18)

  ;; Define the mode-line format
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))

             ;; Define faces for mode-line elements
             (fc-error-face (if active 'me/fc-error-face 'me/fc-error-inactive-face))
             (fc-info-face (if active 'me/fc-info-face 'me/fc-info-inactive-face))
             (fc-warning-face (if active 'me/fc-warning-face 'me/fc-warning-inactive-face))
             (mode-line-1-face (if active 'mode-line 'mode-line-inactive))
             (mode-line-2-face (if active 'powerline-active1 'powerline-inactive1))
             (mode-line-3-face (if active 'powerline-active2 'powerline-inactive2))
             (projectile-face (if active 'me/projectile-face 'me/projectile-inactive-face))
             (vc-face (if active 'me/vc-face 'me/vc-inactive-face))

             ;; Define faces for separators
             (separator-left
              (intern
               (format
                "powerline-%s-%s"
                (powerline-current-separator) (car powerline-default-separator-dir))))
             (separator-right
              (intern
               (format
                "powerline-%s-%s"
                (powerline-current-separator) (cdr powerline-default-separator-dir))))

             ;; List left elements
             (lhs
              (append
               (list
                (powerline-major-mode mode-line-1-face 'l)
                (powerline-raw " " mode-line-1-face)
                (funcall separator-left mode-line-1-face mode-line-2-face))
               (list
                (powerline-raw "%b" mode-line-2-face 'l)
                (powerline-raw ":" mode-line-2-face)
                (powerline-raw "%l" mode-line-2-face)
                ;; TODO: Use color semantics rather than a character for bufffer status
                (powerline-raw "%*" mode-line-2-face 'l)
                (powerline-raw " " mode-line-2-face)
                (funcall separator-left mode-line-2-face mode-line-3-face))))

             ;; List right elements
             (rhs
              (append
               (when (and
                      (bound-and-true-p flycheck-mode)
                      (or flycheck-current-errors (eq 'running flycheck-last-status-change)))
                 (list
                  (funcall separator-right mode-line-3-face mode-line-2-face)
                  (powerline-raw " " mode-line-2-face)
                  (powerline-raw (me/flycheck-lighter error) fc-error-face 'r)
                  (powerline-raw (me/flycheck-lighter warning) fc-warning-face 'r)
                  (powerline-raw (me/flycheck-lighter info) fc-info-face 'r)
                  (funcall separator-left mode-line-2-face mode-line-3-face)
                  (powerline-raw "  " mode-line-3-face)))
               (list
                (funcall separator-right mode-line-3-face mode-line-2-face)
                (powerline-raw " " mode-line-2-face)
                (powerline-raw (projectile-project-name) projectile-face)
                ;; TODO: Remove VC backend
                (powerline-vc vc-face)
                (powerline-raw " " mode-line-2-face))
               (list
                (funcall separator-right mode-line-2-face mode-line-1-face)
                (powerline-raw " " mode-line-1-face)
                (powerline-raw display-time-string mode-line-1-face 'r)))))

        ;; Build the result
        (concat
         (powerline-render lhs)
         (powerline-fill mode-line-3-face (powerline-width rhs))
         (powerline-render rhs))))))

  :config

  ;; Colorize the battery load string on update
  ;; (defadvice battery-update (before me/colorize-battery-advice activate)
  ;;   "Colorize the battery load string depending on its status (dis/charging)."
  ;;   (if (string-equal "AC" (cdr (assoc 76 (funcall battery-status-function))))
  ;;       (progn
  ;;         (copy-face 'me/battery-charging-face 'me/battery-face)
  ;;         (copy-face 'me/battery-charging-inactive-face 'me/battery-inactive-face))
  ;;     (progn
  ;;       (copy-face 'me/battery-discharging-face 'me/battery-face)
  ;;       (copy-face 'me/battery-charging-inactive-face 'me/battery-inactive-face))))

  ;; Customize faces for active buffers
  (set-face-attribute 'mode-line nil
                      :box nil :background zenburn/bg-1 :font me/font-family-mode-line
                      :foreground zenburn/green :height me/font-size-mode-line)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil :background zenburn/bg-1 :font me/font-family-mode-line
                      :foreground zenburn/bg+3 :height me/font-size-mode-line)
  (set-face-attribute 'powerline-active1 nil :background zenburn/bg-0 :foreground zenburn/fg)
  (set-face-attribute 'powerline-active2 nil :background zenburn/bg+1)
  (set-face-attribute 'me/fc-error-face nil :foreground zenburn/red)
  (set-face-attribute 'me/fc-info-face nil :foreground zenburn/blue+1)
  (set-face-attribute 'me/fc-warning-face nil :foreground zenburn/orange)
  (set-face-attribute 'me/projectile-face nil :foreground zenburn/blue-1)
  (set-face-attribute 'me/vc-face nil :foreground zenburn/bg+3)

  ;; Customize faces for inactive buffers
  (set-face-attribute 'powerline-inactive1 nil :background zenburn/bg-0)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn/bg+1))


(provide 'init-mode-line)
;;; init-mode-line.el ends here
