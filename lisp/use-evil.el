;;; use-evil.el --- Set up VIM motions               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package avy
  :custom
  (avy-background t)
  (avy-style 'at-full)
  (avy-timeout-seconds .3))

;; TODO Vilify all selected.el commands

(use-package selected
  :defines selected-keymap
  :bind
  (:map selected-keymap
   ("C-c c"       . capitalize-region)
   ("C-c k"       . barrinalo-kebab)
   ("C-q"         . selected-off)
   ("C-s n"       . barrinalo-sort-numbers)
   ("C-s r"       . barrinalo-reverse)
   ("C-s s"       . sort-lines)
   ("C-s w"       . barrinalo-sort-words)
   ("M-<left>"    . barrinalo-indent-leftward)
   ("M-<right>"   . barrinalo-indent-rightward)
   ("M-S-<left>"  . barrinalo-indent-leftward-tab)
   ("M-S-<right>" . barrinalo-indent-rightward-tab))
  :hook
  (after-init . selected-global-mode)
  :config
  (require 'barrinalo)
  :custom
  (selected-minor-mode-override t))

(use-package evil
  :defines
  evil-inner-text-objects-map
  evil-insert-state-map
  evil-motion-state-map
  evil-normal-state-map
  :functions
  evil-avy-goto-char-timer
  evil-define-key
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
   :map evil-motion-state-map
   ("C-e" . nil)                        ; Free Readline key
   ("C-]" . nil)                        ; Free abort edit command
   :map evil-normal-state-map
   ("M-." . nil))                       ; Free xref command
  :custom
  (evil-emacs-state-cursor (default-value 'cursor-type))
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
  (evil-mode . me/evil-define-bindings)
  :preface
  (defun me/evil-define-bindings ()
    "Add personal bindings to the global maps."
    (evil-define-key 'motion global-map
      (kbd "gs")    #'evil-avy-goto-char-timer
      (kbd "C-S-d") #'evil-scroll-up)
    (evil-define-key 'normal global-map
      (kbd "gb") #'switch-to-buffer
      (kbd "gB") #'project-switch-to-buffer
      (kbd "gC") #'describe-face
      (kbd "gd") #'project-find-dir
      (kbd "gD") #'project-dired
      (kbd "gf") #'me/project-find-file
      (kbd "gF") #'me/project-search
      (kbd "gp") #'project-switch-project
      (kbd "gP") #'me/project-todo
      (kbd "gr") #'manticore-revert-buffer-immediately)))

(use-package evil-collection
  :custom
  (evil-collection-key-blacklist '("gfp" "gfu" "gpp" "gpu"))
  (evil-collection-magit-section-use-z-for-folds t)
  (evil-collection-want-find-usages-bindings nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

(use-package evil-lion
  :hook
  (evil-mode . evil-lion-mode))

(use-package evil-multiedit
  :hook
  (evil-mode . me/evil-multiedit-define-bindings)
  :preface
  (defun me/evil-multiedit-define-bindings ()
    "Add personal bindings to the global maps."
    (evil-define-key 'normal global-map
      (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
      (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev
      (kbd "C-M-d") #'evil-multiedit-match-all)
    (evil-define-key 'visual global-map
      (kbd "M-d")   #'evil-multiedit-match-and-next
      (kbd "M-D")   #'evil-multiedit-match-and-prev
      (kbd "C-M-d") #'evil-multiedit-match-all)))

(use-package evil-snipe
  :hook
  (evil-mode . evil-snipe-mode)
  (evil-mode . evil-snipe-override-mode)
  :custom
  (evil-snipe-char-fold t)
  (evil-snipe-repeat-scope 'visible)
  (evil-snipe-smart-case t))

(use-package evil-surround
  :hook
  (after-init . global-evil-surround-mode))

;; TODO Use `repeat-mode' instead
(defun me/evil-window-resize-continue (&optional _count)
  "Activate a sparse keymap for repeated resizing routines."
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "-") #'evil-window-decrease-height)
     (define-key map (kbd "+") #'evil-window-increase-height)
     (define-key map (kbd "<") #'evil-window-decrease-width)
     (define-key map (kbd ">") #'evil-window-increase-width)
     map)))

(advice-add 'evil-window-decrease-height :after #'me/evil-window-resize-continue)
(advice-add 'evil-window-increase-height :after #'me/evil-window-resize-continue)
(advice-add 'evil-window-decrease-width :after #'me/evil-window-resize-continue)
(advice-add 'evil-window-increase-width :after #'me/evil-window-resize-continue)

;;; use-evil.el ends here
