;; Linum mode
(global-linum-mode 1)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format " %%%dd  " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)


;; CSS mode
(add-to-list 'auto-mode-alist '("\\.less?\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass?\\'" . css-mode))


;; Auto complete (https://github.com/auto-complete/auto-complete)
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict/")
(ac-config-default)


;; Autopair (https://github.com/capitaomorte/autopair)
(add-to-list 'load-path "~/.emacs.d/packages/autopair/")
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
(add-to-list 'load-path "~/.emacs.d/packages/indent-guide/")
(require 'indent-guide)
(set-face-foreground 'indent-guide-face "#586E75")
(setq indent-guide-char "│")
(setq indent-guide-recursive 1)
(indent-guide-global-mode)


;; SCSS mode (https://github.com/antonj/scss-mode/)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/scss-mode/"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(eval-after-load "scss-mode"
  '(define-key emmet-mode-keymap (kbd "C-S-c C-S-c") nil))

;; Multiple cursors (https://github.com/magnars/multiple-cursors.el)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(setq mc/list-file "~/.emacs.d/packages/multiple-cursors/mc-lists.el")


;; Expand region (https://github.com/magnars/expand-region.el)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)
