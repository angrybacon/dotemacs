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

(declare-function shelldock "shelldock")

(defun me/visit--make-visiter (target)
  "Create a visiter function for TARGET.
TARGET should be a cons whose car is the name for the visiter, and whose cdr is
either an absolute file path or a function to call on visit."
  (cl-destructuring-bind (name . action) target
    (eval
     `(defun ,(intern (format "me/visit-%s" name)) ()
        (interactive)
        ,(cl-typecase action
           (function `(,action))
           (string `(find-file ,action))
           (symbol `(find-file ,(eval action :lexical)))
           (t `(user-error "[Visit] Unsupported type `%s'" ,action))))
     :lexical)))

(mapc #'me/visit--make-visiter
      '((compositor    . "~/Workspace/dot/config/picom.org")
        (desktop       . "~/Workspace/dot/config/qtile.org")
        (emacs         . user-init-file)
        (linux         . "~/Workspace/dot/LINUX.org")
        (macos         . "~/Workspace/dot/MACOS.org")
        (notifications . "~/Workspace/dot/config/dunst.org")
        (scratch       . scratch-buffer)
        (secrets       . "~/.config/emacs/.cache/szadek.eld")
        (shell         . "~/Workspace/dot/config/zsh.org")
        (terminal      . "~/Workspace/dot/config/kitty.org")
        (vim           . "~/Workspace/dot/config/vim.org")))

(use-package transient
  :ensure nil
  :bind
  ("<leader>i" . me/transient-interface)
  ("<leader>v" . me/transient-visit)
  :init
  (setq-default
   transient-history-file (shelldock "transient/history.el")
   transient-levels-file (shelldock "transient/levels.el")
   transient-values-file (shelldock "transient/values.el"))
  :config
  (transient-define-prefix me/transient-interface ()
    "Visit configuration files."
    ["Frame"
     ("m" "Maximize"      toggle-frame-maximized)
     ("M" "Cycle display" widowmaker-placement-cycle)]
    ["Olivetti"
     ("o" "Toggle"        widowmaker-olivetti-automatic-toggle)
     ("O" "Toggle"        widowmaker-olivetti-body-reset)]
    ["Pair-programming"
     ("n" "Cycle line numbers" ruric-toggle-line-numbers)
     ("r" "Toggle pair-programming mode" ruric-mode)]
    ["Themes"
     ("t" "Cycle themes"  morophon-cycle)])
  (transient-define-prefix me/transient-visit ()
    "Visit configuration files."
    ["Applications"
     ("c" "Picom"         me/visit-compositor)
     ("d" "Qtile"         me/visit-desktop)
     ("i" "Vim"           me/visit-vim)
     ("n" "Dunst"         me/visit-notifications)
     ("s" "Zsh"           me/visit-shell)
     ("t" "Kitty"         me/visit-terminal)]
    ["Emacs"
     ("." "Secrets"       me/visit-secrets)
     ("e" "Configuration" me/visit-emacs)
     ("v" "Scratch"       me/visit-scratch)]
    ["OS"
     ("l" "Linux"         me/visit-linux)
     ("m" "macOS"         me/visit-macos)])
  :custom
  (transient-show-popup nil))


;;; use-core.el ends here
