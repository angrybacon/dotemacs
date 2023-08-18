;;; use-evil.el --- Set up VIM motions               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function barrinalo-reverse "barrinalo")
(declare-function barrinalo-shift-left "barrinalo")
(declare-function barrinalo-shift-left-tab "barrinalo")
(declare-function barrinalo-shift-right "barrinalo")
(declare-function barrinalo-shift-right-tab "barrinalo")
(declare-function barrinalo-sort-numbers "barrinalo")
(declare-function barrinalo-sort-words "barrinalo")
(declare-function consult-imenu "consult")
(declare-function consult-mark "consult")
(declare-function consult-outline "consult")
(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(declare-function manticore-revert-buffer-immediately "manticore")
(declare-function webpaste-paste-region "webpaste")

(use-package avy
  :custom
  (avy-background t)
  (avy-timeout-seconds .3))

(defun me/evil-define-bindings ()
  "Add personal bindings to the global maps."
  (evil-define-key* 'motion 'global
    (kbd "gm") #'consult-imenu
    (kbd "gM") #'consult-mark
    (kbd "go") #'consult-outline
    (kbd "g'") #'flymake-goto-next-error
    (kbd "g\"") #'flymake-goto-prev-error
    (kbd "C-S-d") #'evil-scroll-up)
  ;; NOTE Use `global-map' instead of `'global' to bypass Magit overwrites
  (evil-define-key* 'normal global-map
    (kbd "gb") #'switch-to-buffer
    (kbd "gB") #'project-switch-to-buffer
    (kbd "gC") #'describe-face
    (kbd "gd") #'project-find-dir
    (kbd "gD") #'project-dired
    (kbd "gf") #'me/project-find-file
    (kbd "gF") #'me/project-search
    (kbd "gp") #'project-switch-project
    (kbd "gP") #'me/project-todo
    (kbd "gr") #'manticore-revert-buffer-immediately
    (kbd "gs") #'evil-avy-goto-char-timer
    (kbd "g/") #'me/project-kill-path)
  (evil-define-key* 'visual 'global
    (kbd "p") #'webpaste-paste-region
    (kbd "sn") #'barrinalo-sort-numbers
    (kbd "sr") #'barrinalo-reverse
    (kbd "ss") #'sort-lines
    (kbd "sw") #'barrinalo-sort-words
    (kbd "<") #'barrinalo-shift-left
    (kbd ">") #'barrinalo-shift-right
    (kbd "c-<") #'barrinalo-shift-left-tab
    (kbd "C->") #'barrinalo-shift-right-tab))

(use-package evil
  :defines
  evil-inner-text-objects-map
  evil-insert-state-map
  evil-motion-state-map
  evil-normal-state-map
  :functions
  evil-avy-goto-char-timer
  evil-define-key*
  evil-define-text-object
  evil-range
  evil-scroll-up
  evil-select-search-module
  :bind
  (:map evil-inner-text-objects-map
   ("g" . me/evil-inner-buffer)
   :map evil-insert-state-map
   ("C-a" . nil)                        ; Free Readline key
   ("C-e" . nil)                        ; Free Readline key
   ("C-w" . nil)                        ; Free kill command
   ("S-<left>" . nil)                   ; Free `left-char' command
   ("S-<right>" . nil)                  ; Free `right-char' command
   :map evil-motion-state-map
   ("C-e" . nil)                        ; Free Readline key
   ("C-]" . nil)                        ; Free abort edit command
   :map evil-normal-state-map
   ("M-." . nil))                       ; Free xref command
  :custom
  (evil-emacs-state-cursor (default-value 'cursor-type))
  (evil-shift-round nil)
  (evil-undo-system 'undo-redo)
  (evil-visual-state-cursor 'hollow)
  (evil-want-keybinding nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-define-text-object me/evil-inner-buffer
    (_count &optional _begin _end type)
    "Text object to represent the whole buffer."
    (evil-range (point-min) (point-max) type))
  :hook
  (after-init . evil-mode)
  (after-save . evil-normal-state)
  (evil-mode . me/evil-define-bindings))

(use-package evil-collection
  :custom
  (evil-collection-key-blacklist
   '("gd" "gf" "gfp" "gfu" "gpp" "gpu"  ; Free project commands
     "gs"                               ; Free avy command
     "M-1" "M-2" "M-3" "M-4"))          ; Free workspace command
  (evil-collection-want-find-usages-bindings nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

(use-package evil-goggles
  :custom
  (evil-goggles-duration .1)
  (evil-goggles-pulse nil)
  :hook
  (evil-mode . evil-goggles-mode))

(use-package evil-lion
  :hook
  (evil-mode . evil-lion-mode))

(declare-function evil-multiedit-match-all "evil-multiedit")
(declare-function evil-multiedit-match-and-next "evil-multiedit")
(declare-function evil-multiedit-match-and-prev "evil-multiedit")
(declare-function evil-multiedit-match-symbol-and-next "evil-multiedit")
(declare-function evil-multiedit-match-symbol-and-prev "evil-multiedit")

(defun me/evil-multiedit-define-bindings ()
    "Add personal bindings to the global maps."
    (evil-define-key* '(normal visual) 'global
      (kbd "C-M-d") #'evil-multiedit-match-all)
    (evil-define-key* 'normal 'global
      (kbd "M-d") #'evil-multiedit-match-symbol-and-next
      (kbd "M-D") #'evil-multiedit-match-symbol-and-prev)
    (evil-define-key* 'visual 'global
      (kbd "M-d") #'evil-multiedit-match-and-next
      (kbd "M-D") #'evil-multiedit-match-and-prev))

(use-package evil-multiedit
  :hook
  (evil-mode . me/evil-multiedit-define-bindings))

(use-package evil-surround
  :hook
  (evil-mode . global-evil-surround-mode))

;;; use-evil.el ends here
