;;; use-evil.el --- Set up VIM motions               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package avy
  :custom
  (avy-background t)
  (avy-timeout-seconds .3))

(declare-function magit-blame "magit-blame")
(declare-function magit-clone "magit-clone")
(declare-function magit-status "magit-status")
(declare-function diff-hl-revert-hunk "diff-hl")
(declare-function diff-hl-stage-current-hunk "diff-hl")

(defvar-keymap me/magit-map
  "b" #'magit-blame
  "c" #'magit-clone
  "g" #'magit-status
  "r" #'diff-hl-revert-hunk
  "s" #'diff-hl-stage-current-hunk)

(declare-function manticore-kill-terminal "manticore")
(declare-function manticore-revert-buffer-immediately "manticore")

(defvar-keymap me/system-map
  "d" #'desktop-remove
  "g" #'toggle-debug-on-error
  "l" #'list-processes
  "p" #'package-list-packages
  "q" #'manticore-kill-terminal
  "s" #'manticore-revert-buffer-immediately)

(declare-function barrinalo-reverse "barrinalo")
(declare-function barrinalo-shift-left "barrinalo")
(declare-function barrinalo-shift-left-tab "barrinalo")
(declare-function barrinalo-shift-right "barrinalo")
(declare-function barrinalo-shift-right-tab "barrinalo")
(declare-function barrinalo-sort-numbers "barrinalo")
(declare-function barrinalo-sort-words "barrinalo")
(declare-function evil-scroll-up "evil-commands")
(declare-function evil-define-key "evil-core")
(declare-function evil-avy-goto-char-timer "evil-integration")
(declare-function webpaste-paste-region "webpaste")

(defun me/evil-define-bindings ()
  "Add personal bindings to the global maps."
  (evil-define-key 'motion 'global
    (kbd "C-S-d") #'evil-scroll-up)
  (evil-define-key 'normal 'global
    ;; TODO Those are shadowed by Magit's own keybindings
    (kbd "<leader>g") me/magit-map
    (kbd "<leader>h") help-map
    (kbd "<leader>p") project-prefix-map
    (kbd "<leader>s") me/system-map
    (kbd "g/") #'me/project-kill-path
    (kbd "gb") #'switch-to-buffer
    (kbd "gs") #'evil-avy-goto-char-timer)
  (evil-define-key 'visual 'global
    (kbd "<") #'barrinalo-shift-left-tab
    (kbd ">") #'barrinalo-shift-right-tab
    (kbd "C-<") #'barrinalo-shift-left
    (kbd "C->") #'barrinalo-shift-right
    (kbd "g+") #'me/evil-sum
    (kbd "p") #'webpaste-paste-region
    (kbd "sn") #'barrinalo-sort-numbers
    (kbd "sr") #'barrinalo-reverse
    (kbd "ss") #'sort-lines
    (kbd "sw") #'barrinalo-sort-words))

(defun me/evil-define-operators ()
  "Create custom operators."
  (evil-define-operator me/evil-sum (begin end type)
    "Count the sum of all numbers in block."
    (unless (eq type 'block)
      (user-error "[Evil] Expected a block to work on"))
    (let* ((items (mapcar #'string-to-number (extract-rectangle begin end)))
           (result (apply #'+ items)))
      (message "Count: %d" result))))

(use-package evil
  :defines
  evil-inner-text-objects-map
  evil-insert-state-map
  evil-motion-state-map
  evil-window-map
  :functions
  evil-define-text-object
  evil-range
  evil-select-search-module
  evil-set-leader
  :config
  (evil-define-text-object me/evil-buffer-object (_count &optional _begin _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))
  (define-key evil-inner-text-objects-map (kbd "g") #'me/evil-buffer-object)
  (define-key evil-insert-state-map (kbd "C-a") nil :remove)
  (define-key evil-insert-state-map (kbd "C-e") nil :remove)
  (define-key evil-insert-state-map (kbd "C-w") nil :remove)
  (define-key evil-insert-state-map (kbd "S-<left>") nil :remove)
  (define-key evil-insert-state-map (kbd "S-<right>") nil :remove)
  (define-key evil-motion-state-map (kbd "C-]") nil :remove)
  (define-key evil-motion-state-map (kbd "C-e") nil :remove)
  (define-key evil-window-map (kbd "C-h") nil :remove)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-leader 'normal (kbd "SPC"))
  :custom
  (evil-echo-state nil)
  (evil-emacs-state-cursor (default-value 'cursor-type))
  (evil-move-cursor-back nil)
  (evil-shift-round nil)
  (evil-undo-system 'undo-redo)
  (evil-visual-state-cursor 'hollow)
  (evil-want-keybinding nil)
  :hook
  (after-init . evil-mode)
  (after-save . evil-normal-state)
  (evil-mode . me/evil-define-bindings)
  (evil-mode . me/evil-define-operators))

;; TODO Repeat'ize the unimpaired bindings from evil-collection

(use-package evil-collection
  :custom
  (evil-collection-key-blacklist '("M-1" "M-2" "M-3" "M-4"))
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
  :custom
  (evil-lion-squeeze-spaces nil)
  :hook
  (evil-mode . evil-lion-mode))

(declare-function evil-multiedit-match-all "evil-multiedit")
(declare-function evil-multiedit-match-and-next "evil-multiedit")
(declare-function evil-multiedit-match-and-prev "evil-multiedit")
(declare-function evil-multiedit-match-symbol-and-next "evil-multiedit")
(declare-function evil-multiedit-match-symbol-and-prev "evil-multiedit")

(defun me/evil-multiedit-define-bindings ()
  "Add personal bindings to the global maps."
  (evil-define-key '(normal visual) 'global
    (kbd "C-M-d") #'evil-multiedit-match-all)
  (evil-define-key 'normal 'global
    (kbd "M-d") #'evil-multiedit-match-symbol-and-next
    (kbd "M-D") #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    (kbd "M-d") #'evil-multiedit-match-and-next
    (kbd "M-D") #'evil-multiedit-match-and-prev))

(use-package evil-multiedit
  :hook
  (evil-mode . me/evil-multiedit-define-bindings))

(use-package evil-surround
  :hook
  (evil-mode . global-evil-surround-mode))

;;; use-evil.el ends here
