;;; init-mode-line.el --- Prettify the mode-line

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 1 June 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Delight
;;=============================================================================


;; Website: https://github.com/antonio/delight.el
(use-package delight
  :config
  ;; NOTE: Or use https://www.emacswiki.org/emacs/delight-powerline.el?
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
   zenburn/blue
   zenburn/fg
   zenburn/fg-1
   zenburn/green+2
   zenburn/green
   zenburn/green-1
   zenburn/magenta
   zenburn/orange
   zenburn/red
   zenburn/red-1)

  :preface

  ;; Configure the the mode-line
  (defvar me/powerline-hud nil)

  ;; Define new faces for elements
  (defface me/buffer-clean-face '((t (:inherit powerline-active1)))
    "Face used for the buffer string: clean."
    :group 'me/powerline)
  (defface me/buffer-read-only-face '((t (:inherit powerline-active1)))
    "Face used for the buffer string: read only."
    :group 'me/powerline)
  (defface me/buffer-modified-face '((t (:inherit powerline-active1)))
    "Face used for the buffer string: modified."
    :group 'me/powerline)
  (defface me/fc-error-face '((t (:inherit powerline-active1)))
    "Face used for the error count."
    :group 'me/powerline)
  (defface me/fc-info-face '((t (:inherit powerline-active1)))
    "Face used for the info count."
    :group 'me/powerline)
  (defface me/fc-warning-face '((t (:inherit powerline-active1)))
    "Face used for the warning count."
    :group 'me/powerline)
  (defface me/hud-face '((t (:inherit powerline-active1)))
    "Face used for the XPM of relative buffer location."
    :group 'me/powerline)
  (defface me/line-number-face '((t (:inherit powerline-active1)))
    "Face used for the line number string."
    :group 'me/powerline)
  (defface me/projectile-face '((t (:inherit powerline-active1)))
    "Face used for the projectile string."
    :group 'me/powerline)
  (defface me/vc-face '((t (:inherit powerline-active1)))
    "Face used for the version control string."
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

  :config

  ;; Customize appearance
  (setq-default
   powerline-default-separator 'wave
   powerline-height 20
   me/powerline-hud nil)

  ;; Define the mode-line format
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))

             ;; Define faces for mode-line elements
             (buffer-face
              (if active
                  (cond
                   (buffer-read-only 'me/buffer-read-only-face)
                   ((buffer-modified-p) 'me/buffer-modified-face)
                   (t 'me/buffer-clean-face))
                (cond
                 ((buffer-modified-p) 'me/buffer-modified-face)
                 (t ''powerline-inactive1))))
             (fc-error-face (if active 'me/fc-error-face 'powerline-inactive1))
             (fc-info-face (if active 'me/fc-info-face 'powerline-inactive1))
             (fc-warning-face (if active 'me/fc-warning-face 'powerline-inactive1))
             (hud-face 'me/hud-face)
             (line-number-face (if active 'me/line-number-face 'powerline-inactive1))
             (mode-line-1-face (if active 'mode-line 'mode-line-inactive))
             (mode-line-2-face (if active 'powerline-active1 'powerline-inactive1))
             (mode-line-3-face (if active 'powerline-active2 'powerline-inactive2))
             (projectile-face (if active 'me/projectile-face 'powerline-inactive1))
             (vc-face (if active 'me/vc-face 'powerline-inactive1))

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
                (powerline-raw "%b" buffer-face 'l)
                (powerline-raw ":%l" line-number-face)
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
                (powerline-raw display-time-string mode-line-1-face 'r)
                (if me/powerline-hud (powerline-hud hud-face mode-line-2-face 2))))))

        ;; Build the result
        (concat
         (powerline-render lhs)
         (powerline-fill mode-line-3-face (powerline-width rhs))
         (powerline-render rhs))))))

  ;; :config

  (defadvice vc-mode-line (after strip-backend () activate)
    "Strip backend from the VC information."
    (when (stringp vc-mode)
      (let ((vc-text (replace-regexp-in-string "^ Git." ":" vc-mode)))
        (setq vc-mode vc-text))))

  ;; Customize faces
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 1 :color ,zenburn/bg-1)
                      :background zenburn/green-1 :font me/font-family-mode-line
                      :foreground zenburn/green+2 :height me/font-size-mode-line)
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 1 :color ,zenburn/bg-1)
                      :background zenburn/bg-1 :font me/font-family-mode-line
                      :foreground zenburn/bg+3 :height me/font-size-mode-line)
  (set-face-attribute 'powerline-active1 nil :background zenburn/bg-0 :foreground zenburn/fg)
  (set-face-attribute 'powerline-active2 nil :background zenburn/bg+1)
  (set-face-attribute 'powerline-inactive1 nil :background zenburn/bg-0)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn/bg+1)
  (set-face-attribute 'me/buffer-clean-face nil :foreground zenburn/green)
  (set-face-attribute 'me/buffer-modified-face nil :foreground zenburn/red)
  (set-face-attribute 'me/buffer-read-only-face nil :foreground zenburn/magenta)
  (set-face-attribute 'me/fc-error-face nil :foreground zenburn/red-1)
  (set-face-attribute 'me/fc-info-face nil :foreground zenburn/blue+1)
  (set-face-attribute 'me/fc-warning-face nil :foreground zenburn/orange)
  (set-face-attribute 'me/hud-face nil :background zenburn/fg-1)
  (set-face-attribute 'me/line-number-face nil :foreground zenburn/bg+3)
  (set-face-attribute 'me/projectile-face nil :foreground zenburn/blue)
  (set-face-attribute 'me/vc-face nil :foreground zenburn/bg+3))


(provide 'init-mode-line)
;;; init-mode-line.el ends here
