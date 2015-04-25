;; SCSS mode (https://github.com/antonj/scss-mode/)
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; Emmet (https://github.com/smihica/emmet-mode)
(setq emmet-preview-default nil
      emmet-move-cursor-between-quotes t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(global-set-key (kbd "<M-left>") 'emmet-prev-edit-point)
(global-set-key (kbd "<M-right>") 'emmet-next-edit-point)
;; (eval-after-load "scss-mode"
;;   '(define-key emmet-mode-keymap (kbd "C-S-c C-S-c") nil))
;; (eval-after-load "emmet-mode"
;;   (lambda ()
;;     (define-key emmet-mode-keymap (kbd "C-m") 'emmet-expand-line)
;;     (define-key emmet-mode-keymap (kbd "C-j") nil)))


;; Multiple cursors (https://github.com/magnars/multiple-cursors.el)
(setq mc/list-file "~/.emacs.d/vendor/multiple-cursors/mc-lists.el")
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)


;; Expand region (https://github.com/magnars/expand-region.el)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)


;; Helm (https://github.com/emacs-helm/helm)
(setq helm-mode-line-string "")


;; Projectile (https://github.com/bbatsov/projectile)
(setq projectile-enable-caching t
      projectile-remember-window-configs t
      projectile-mode-line '(:eval (format " %s" (projectile-project-name))))
(projectile-global-mode)
(helm-projectile-on)
(global-set-key (kbd "M-x") 'helm-M-x)


;; Company (https://github.com/company-mode/company-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.1)
(add-hook 'after-init-hook 'global-company-mode)


;; Ace Jump Mode (https://github.com/winterTTr/ace-jump-mode)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-f") 'ace-jump-mode)
(define-key global-map (kbd "C-c f") 'ace-jump-mode-pop-mark)


;; Hightlight Parentheses (http://www.emacswiki.org/emacs/HighlightParentheses)
(setq hl-paren-colors `(,zenburn/orange ,zenburn/red+1 ,zenburn/red-1 ,zenburn/red-2))
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)


;; Autopair (https://github.com/capitaomorte/autopair)
(autopair-global-mode)


;; Org (http://orgmode.org/)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;; Web Mode (http://web-mode.org/)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)


;; Golden Ratio (https://github.com/roman/golden-ratio.el)
(setq golden-ratio-adjust-factor .9)
(golden-ratio-mode 1)


;; Magit (https://github.com/magit/magit)
(setq magit-show-child-count t
      magit-stage-all-confirm nil
      magit-unstage-all-confirm nil)
(global-set-key (kbd "C-c g") 'magit-status)


;; Python Mode (built-in)
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--colors=Linux"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))


;; Dockerfile Mode (https://github.com/spotify/dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
