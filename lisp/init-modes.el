


;; Company (https://github.com/company-mode/company-mode)
(setq
 company-minimum-prefix-length 1
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


;; Smartparent (https://github.com/Fuco1/smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)


;; Org (http://orgmode.org/)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;; Web Mode (http://web-mode.org/)
(with-eval-after-load 'web-mode
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (define-key web-mode-map (kbd "M-;") 'comment-dwim))



;; Golden Ratio (https://github.com/roman/golden-ratio.el)
(setq golden-ratio-adjust-factor .9)
(golden-ratio-mode 1)


;; Magit (https://github.com/magit/magit)
(setq
 magit-show-child-count t
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


;; Whitespace Mode (built-in)
(setq whitespace-style
      '(face tabs spaces trailing space-before-tab indentation empty space-after-tab space-mark tab-mark))


;; HTML Mode (built-in)
(setq sgml-basic-offset 2)


;; CSS Mode (buit-in)
(setq css-indent-offset 2)


;; Markdown Mode (http://jblevins.org/projects/markdown-mode/)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))


;; JSON reformat (https://github.com/gongo/json-reformat)
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jshintrc$" . json-mode) auto-mode-alist))
;; NOTE: This will be fixed with https://github.com/joshwnj/json-mode/issues/32.
(setq json-reformat:indent-width 2)




(provide 'init-modes)
