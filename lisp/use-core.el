;;; use-core.el --- Load core re-usable features     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package barrinalo
  :load-path "lisp/barrinalo"
  :commands
  barrinalo-cycle-spacing
  barrinalo-date-iso
  barrinalo-date-iso-with-time
  barrinalo-date-long
  barrinalo-date-long-with-time
  barrinalo-date-short
  barrinalo-date-short-with-time
  barrinalo-duplicate-backward
  barrinalo-duplicate-forward
  barrinalo-shift-left
  barrinalo-shift-left-tab
  barrinalo-shift-right
  barrinalo-shift-right-tab
  barrinalo-reverse
  barrinalo-sort-numbers
  barrinalo-sort-words
  barrinalo-swap-down
  barrinalo-swap-up
  :bind
  ([remap delete-horizontal-space] . barrinalo-cycle-spacing)
  ("M-p" . barrinalo-swap-up)
  ("M-n" . barrinalo-swap-down)
  ("M-P" . barrinalo-duplicate-backward)
  ("M-N" . barrinalo-duplicate-forward))

(use-package hanna
  :load-path "lisp/hanna"
  :bind
  ([remap move-beginning-of-line] . hanna-beginning-of-line)
  ([remap backward-paragraph] . hanna-paragraph-backward)
  ([remap forward-paragraph] . hanna-paragraph-forward))

(use-package manticore
  :load-path "lisp/manticore"
  :commands
  manticore-delete-compiled
  manticore-eval-region-dwim
  manticore-kill-terminal
  manticore-revert-buffer-immediately)

(use-package shelldock
  :demand
  :load-path "lisp/shelldock")

(use-package szadek
  :load-path "lisp/szadek"
  :commands
  szadek-get
  szadek-register
  :custom
  (szadek-file (shelldock "szadek.eld"))
  (szadek-fix-missing t))

;;; use-core.el ends here
