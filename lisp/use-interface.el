;;; use-interface.el --- Prettify all the things     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Breadcrumbs

(use-package breadcrumb)

;;;; Current Line

(use-package hl-line
  :ensure nil
  :custom
  (hl-line-sticky-flag nil)
  :hook
  (conf-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  (fundamental-mode . hl-line-mode)
  (grep-mode . hl-line-mode)
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

;;;; Indent Guides

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook
  (css-base-mode . highlight-indent-guides-mode)
  (json-ts-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . highlight-indent-guides-mode))

;;;; Line Numbers

(use-package display-line-numbers
  :ensure nil
  :config
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  :hook
  (conf-mode . display-line-numbers-mode)
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))

;;;; Mode-Line

(use-package leyline
  :load-path "lisp/leyline"
  :hook
  (after-change-major-mode . leyline-rename)
  (after-init . leyline-mode))

;;;; Pulse

(use-package pulsar
  :defines pulsar-pulse-functions
  :config
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'goto-char)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions-other-window)
  :hook
  (after-init . pulsar-global-mode))

;;;; Text Size

(global-set-key [remap text-scale-adjust] #'global-text-scale-adjust)

;;;; Themes

(declare-function szadek-register "szadek")

(use-package morophon
  :load-path "lisp/morophon"
  :commands
  morophon-alpha-less
  morophon-alpha-more
  morophon-cycle
  morophon-disable-themes
  morophon-typography-reset
  :custom
  (morophon-known-themes '(zenmelt modus-operandi))
  :init
  (szadek-register #'morophon-typography-reset :immediate))

;; Customize line-continuation indicator bitmaps
(define-fringe-bitmap 'left-curly-arrow [16 48 112 240 240 112 48 16])
(define-fringe-bitmap 'right-curly-arrow [8 12 14 15 15 14 12 8])

;; And mute their color
(set-fringe-bitmap-face 'left-curly-arrow 'shadow)
(set-fringe-bitmap-face 'right-curly-arrow 'shadow)

(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-diffs 'fg-only)
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-no-mixed-fonts t)
  (modus-themes-operandi-color-overrides
   '((bg-main . "#FAFAFA")
     (fg-main . "#101010")
     (fg-window-divider-inner . "#FAFAFA")))
  (modus-themes-org-blocks 'tinted-background))

(use-package zenmelt-theme
  :demand
  :load-path "lisp/zenmelt"
  :config
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(zenmelt--reset t))))
  (load-theme 'zenmelt :noconfirm))

;;; use-interface.el ends here
