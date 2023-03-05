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

(defun me/evil-record-macro-or-quit ()
  "Quit the current window or record a macro when the buffer is writeable."
  (interactive)
  (if buffer-read-only
      (quit-window)
    (call-interactively #'evil-record-macro)))

(use-package evil
  :bind
  (:map evil-inner-text-objects-map
   ("g" . me/evil-buffer)
   :map evil-outer-text-objects-map
   ("g" . me/evil-buffer)
   :map evil-insert-state-map
   ("C-a" . nil)                        ; Free Readline key
   ("C-e" . nil)                        ; Free Readline key
   ("C-w" . nil)                        ; Free kill command
   ("S-<left>" . nil)                   ; Free motion command
   ("S-<right>" . nil)                  ; Free motion command
   :map evil-motion-state-map
   ("RET" . nil)                        ; Free return command
   ("gb" . switch-to-buffer)
   ("gB" . project-switch-to-buffer)
   ("gC" . describe-face)
   ("gr" . manticore-revert-buffer-immediately)
   ("gs" . avy-goto-char-timer)
   ("gS" . avy-goto-char)
   ("C-e" . nil)                        ; Free Readline key
   ("C-]" . nil)                        ; Free abort edit command
   ("C-S-d" . evil-scroll-up)
   :map evil-normal-state-map
   ("q" . me/evil-record-macro-or-quit)
   ("gd" . project-find-dir)
   ("gD" . project-dired)
   ("gf" . me/project-find-file)
   ("gp" . project-switch-project)
   ("M-." . nil)                        ; Free xref command
   :map evil-visual-state-map
   ("f" . fill-region)
   :map evil-window-map
   ("u" . winner-undo)
   ("C-r" . winner-redo))
  :custom
  (evil-echo-state nil)
  (evil-emacs-state-cursor (default-value 'cursor-type))
  (evil-undo-system 'undo-redo)
  (evil-visual-state-cursor 'hollow)
  (evil-want-keybinding nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (add-to-list 'evil-emacs-state-modes 'exwm-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'process-menu-mode)
  (add-to-list 'evil-emacs-state-modes 'profiler-report-mode)
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-insert-state-modes 'with-editor-mode)
  (add-to-list 'evil-motion-state-modes 'helpful-mode)
  (evil-define-text-object me/evil-buffer (_count &optional _begin _end type)
    "Text object to represent the whole buffer."
    (evil-range (point-min) (point-max) type))
  (advice-add 'evil-indent :around #'manticore-save-excursion)
  :hook
  (after-init . evil-mode)
  (after-save . evil-normal-state))

(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

(use-package evil-lion
  :hook
  (evil-mode . evil-lion-mode))

(use-package evil-multiedit
  :after evil
  :bind
  (:map evil-normal-state-map
   ("M-d". evil-multiedit-match-symbol-and-next)
   ("M-D". evil-multiedit-match-symbol-and-prev)
   ("C-M-d". evil-multiedit-match-all)
   :map evil-visual-state-map
   ("M-d". evil-multiedit-match-and-next)
   ("M-D". evil-multiedit-match-and-prev)
   ("C-M-d". evil-multiedit-match-all)))

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
