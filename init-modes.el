;; Smooth scroll (http://www.emacswiki.org/emacs/smooth-scroll.el)
(add-to-list 'load-path "~/.emacs.d/vendor/smooth-scroll/")
(require 'smooth-scroll)
(smooth-scroll-mode t)


;; Autopair (https://github.com/capitaomorte/autopair)
;; (add-to-list 'load-path "~/.emacs.d/vendor/autopair/")
;; (require 'autopair)
;; (autopair-global-mode)


;; SCSS mode (https://github.com/antonj/scss-mode/)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/scss-mode/"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)


;; Indent guide (https://github.com/zk-phi/indent-guide)
(add-to-list 'load-path "~/.emacs.d/vendor/indent-guide/")
(require 'indent-guide)
(set-face-foreground 'indent-guide-face "#586E75")
(setq indent-guide-char "·")
(setq indent-guide-recursive nil)
(indent-guide-global-mode)


;; Linum mode
;; (global-linum-mode 1)
;; (defun linum-format-func (line)
;;   (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
;;      (propertize (format (format " %%%dd  " w) line) 'face 'linum)))
;; (setq linum-format 'linum-format-func)


;; Emmet (https://github.com/smihica/emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)                     ; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)                     ; Enable Emmet's css abbreviation
(setq emmet-preview-default nil)                           ; Disable preview before expanding
(setq emmet-move-cursor-between-quotes t)                  ; Position the cursor on first empty quotes
(global-set-key (kbd "<M-left>") 'emmet-prev-edit-point)   ; Jump to previous empty edit point
(global-set-key (kbd "<M-right>") 'emmet-next-edit-point)  ; Jump to next empty edit point
(eval-after-load "scss-mode"
  '(define-key emmet-mode-keymap (kbd "C-S-c C-S-c") nil))
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))


;; Multiple cursors (https://github.com/magnars/multiple-cursors.el)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(setq mc/list-file "~/.emacs.d/vendor/multiple-cursors/mc-lists.el")


;; Expand region (https://github.com/magnars/expand-region.el)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)


;; Projectile (https://github.com/bbatsov/projectile)
(setq projectile-enable-caching t)
(setq projectile-remember-window-configs t)
(setq projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "M-x") 'helm-M-x)


;; Company (https://github.com/company-mode/company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.1)


;; Anaconda backend for Company (https://github.com/proofit404/company-anaconda)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))


;; Ace Jump Mode (https://github.com/winterTTr/ace-jump-mode)
(define-key global-map (kbd "C-f") 'ace-jump-mode)
;; Enable a more powerful jump back function from ace jump mode
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c f") 'ace-jump-mode-pop-mark)
