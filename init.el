;; Packages
;; ─────────────────────────────────────────────────────────────────────────────

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Interface and syntax
;; ─────────────────────────────────────────────────────────────────────────────

(setq inhibit-startup-screen 1)            ; Remove start-up screen
(scroll-bar-mode -1)                       ; Remove scroll bar
(menu-bar-mode -1)                         ; Remove menu bar
(tool-bar-mode -1)                         ; Remove the toolbar
(set-fringe-mode 0)                        ; Remove left and right fringes
(line-number-mode 1)                       ; Display line number according cursor current position
(column-number-mode 1)                     ; Display column number according cursor current position
(setq scroll-step 1)                       ; Line by line scrolling
(setq default-tab-width 4)                 ; Set width for tabs
(global-hl-line-mode 1)                    ; Hightlight current line
(setq x-select-enable-clipboard 1)         ; Merge both system's Emacs'clipboard
(setq-default show-trailing-whitespace 1)  ; Display trailing whitespaces
(setq-default indent-tabs-mode nil)        ; No tabs
(setq sgml-basic-offset 4)                 ; Set indent to 4 spaces
(global-subword-mode 1)                    ; Move through CamelCase words
(setq mouse-yank-at-point 1)               ; Yank at mouse cursor rather than click location
(mouse-avoidance-mode 'animate)            ; Move the cursor to the corner when typing

;; Add prefixes to buffer titles (in case of duplicates)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Sort directories first while in dired mode
(defun list-directories-first ()
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))
(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  (list-directories-first))

;; Consistent behavior when jumping paragraphs backward and forward
(defun backward-block ()
  (interactive)
  (skip-chars-backward "\n")
  (when (not (search-backward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-min)))
  (skip-chars-forward "\n"))
(global-set-key (kbd "<C-up>") 'backward-block)
(defun forward-block ()
  (interactive)
  (skip-chars-forward "\n")
  (when (not (search-forward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-max)))
  (skip-chars-forward "\n"))
(global-set-key (kbd "<C-down>") 'forward-block)

;; Color the background of strings representing a hex color code
(defun color-hex-code-strings ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer)
  )
(add-hook 'emmet-mode-hook 'color-hex-code-strings)

;; Highlight parent parenthesis
(show-paren-mode t)             ; Turn paren-mode on
(setq show-paren-delay 0)       ; How long to wait ?
(setq show-paren-style 'mixed)  ; Alternatives are 'parenthesis' and 'mixed'

;; Linum mode
(global-linum-mode 1)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format " %%%dd  " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;; Multiple cursors (https://github.com/magnars/multiple-cursors.el)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(setq mc/list-file "~/.emacs.d/multiple-cursors/mc-lists.el")

;; Expand region (https://github.com/magnars/expand-region.el)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)

;; Aliases
;; ────────────────────────────────────────────────────────────────────────────

(defalias 'yes-or-no-p 'y-or-n-p)  ; Always use 'y' or 'n'
(defalias 'replace-string 'rp)     ; Alias for replace-string

;; Key bindings
;; ────────────────────────────────────────────────────────────────────────────

;; Inhibit the mouse events
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]))
  (global-unset-key k))

;; Shortcut for goto-line function
(global-set-key (kbd "C-f") 'goto-line)

;; Shortcut to jump to previous buffer
(defun goto-left-buffer ()
  (interactive)
  (other-window -1))
(define-key global-map (kbd "C-x p") 'goto-left-buffer)

;; Modes
;; ────────────────────────────────────────────────────────────────────────────

;; CSS mode
(add-to-list 'auto-mode-alist '("\\.less?\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass?\\'" . css-mode))

;; Auto complete (https://github.com/auto-complete/auto-complete)
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict/")
(ac-config-default)

;; Autopair (https://github.com/capitaomorte/autopair)
(add-to-list 'load-path "~/.emacs.d/autopair/")
(require 'autopair)
(autopair-global-mode)

;; Emmet (https://github.com/smihica/emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)                     ; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)                     ; Enable Emmet's css abbreviation
(setq emmet-preview-default nil)                           ; Disable preview before expanding
(setq emmet-move-cursor-between-quotes t)                  ; Position the cursor on first empty quotes
(global-set-key (kbd "<M-left>") 'emmet-prev-edit-point)   ; Jump to previous empty edit point
(global-set-key (kbd "<M-right>") 'emmet-next-edit-point)  ; Jump to next empty edit point
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))

;; Indent guide (https://github.com/zk-phi/indent-guide)
(add-to-list 'load-path "~/.emacs.d/indent-guide/")
(require 'indent-guide)
(set-face-foreground 'indent-guide-face "#586E75")
(setq indent-guide-char "│")
(setq indent-guide-recursive 1)
(indent-guide-global-mode)

;; SCSS mode (https://github.com/antonj/scss-mode/)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode/"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(eval-after-load "scss-mode"
  '(define-key emmet-mode-keymap (kbd "C-S-c C-S-c") nil))

;; Theme
;; ────────────────────────────────────────────────────────────────────────────

;; Set default font
(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-9"))

;; Set color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark 1)

;; Face customization
(set-face-italic-p 'font-lock-string-face 1)
(set-face-italic-p 'font-lock-comment-face 1)
(set-face-background 'linum "#073642")

;; Customize the mode line
(add-to-list 'load-path "~/.emacs.d/delight/")
(require 'delight)
(delight 'indent-guide-mode " ig" "indent-guide")
(delight 'emmet-mode " emmet" "emmet-mode")
(delight 'emacs-lisp-mode "el" :major)
