;;; use-languages.el --- Language-specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Tree-Sitter

(use-package treesit
  :ensure nil
  :config
  (dolist (source '((graphql "https://github.com/bkegley/tree-sitter-graphql")
                    (kotlin "https://github.com/fwcd/tree-sitter-kotlin")))
    (add-to-list 'treesit-language-source-alist source))
  :custom
  (treesit-enabled-modes t)
  (treesit-extra-load-path (list (shelldock "treesit/")))
  (treesit-font-lock-level 4))

;;;; CSS

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

;;;; CSV

(use-package csv-mode)

;;;; Configuration Files

(use-package conf-mode
  :ensure nil
  :mode (rx (or "CODEOWNERS"
                (and ".env" (* (and "." (+ word))))
                (and "." (+ word) "rc"))
            eos))

;;;; GraphQL

(use-package graphql-ts-mode
  :config
  ;; NOTE The mode itself should probably be doing this
  (treesit-ensure-installed 'graphql))

;;;; HTML

(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

;;;; Haskell

(use-package haskell-mode)

;;;; JavaScript

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2)
  :init
  ;; NOTE Shebang mapping uses `js-mode' regardless of `treesit'
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  :mode ((rx "." (or "cjs" "mjs") eos) . js-ts-mode))

(use-package typescript-ts-mode
  :ensure nil
  :mode ((rx "." (or "cts" "mts") eos) . typescript-ts-mode))

;;;; Kotlin

(use-package kotlin-ts-mode
  :config
  ;; NOTE The mode itself should probably be doing this
  (treesit-ensure-installed 'kotlin)
  :mode (rx "." (or "kt" "kts") eos))

;;;; Lisp

(use-package emacs-lisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
   ("C-c C-c" . manticore-eval-region-dwim)
   ("C-x C-S-e" . eval-print-last-sexp)
   :map lisp-interaction-mode-map
   ("C-c C-c" . manticore-eval-region-dwim)
   ("C-x C-S-e" . eval-print-last-sexp))
  :custom
  (emacs-lisp-docstring-fill-column nil)
  :hook
  (emacs-lisp-mode . flymake-mode)
  (emacs-lisp-mode . outline-minor-mode))

;;;; Lua

(use-package lua-ts-mode
  :ensure nil
  :custom
  (lua-ts-indent-offset 2))

;;;; Markdown

(use-package markdown-ts-mode
  :ensure nil
  :mode (rx (or ".md" ".mdx" "CONTRIBUTORS" "INSTALL" "LICENSE" "README") eos))

;;;; Org

(use-package gnuplot)

(declare-function org-at-heading-p "org")
(declare-function org-previous-visible-heading "org")
(declare-function outline-up-heading "outline")

(defun me/org-src-buffer (name &rest _)
  "Format a simple buffer NAME."
  (format "*%s*" name))

(defun me/org-cycle-parent (argument)
  "Go to the nearest parent heading and run `org-cycle'.
With ARGUMENT move up that amount."
  (interactive "p")
  (if (org-at-heading-p)
      (outline-up-heading argument)
    (org-previous-visible-heading argument))
  (org-cycle))

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
   ("C-<return>" . nil)                 ; Free expand command
   ("C-<tab>" . me/org-cycle-parent)
   ("C-S-<down>" . nil)                 ; Free paragraph command
   ("C-S-<up>" . nil))                  ; Free paragraph command
  :config
  (add-to-list 'org-src-lang-modes (cons "json" 'json-ts))
  (require 'ob-shell)
  (org-babel-do-load-languages
   'org-babel-load-languages '((gnuplot . t) (python . t) (shell . t)))
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  (advice-add 'org-src--construct-edit-buffer-name :override #'me/org-src-buffer)
  :custom
  (org-babel-default-header-args:gnuplot
   '((:results . "file")
     (:session . "none")))
  (org-babel-default-header-args:sh '((:results . "silent")))
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-return-follows-link t)
  (org-src-window-setup 'current-window)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :hook
  (org-mode . buffer-face-mode)
  (org-mode . (lambda () (setq-local comment-auto-fill-only-comments nil)))
  :init
  (add-to-list 'safe-local-eval-forms
               '(add-hook 'after-save-hook #'org-babel-tangle nil t))
  (add-to-list 'safe-local-eval-forms
               '(when (eq system-type 'darwin)
                  (add-hook 'after-save-hook #'org-babel-tangle nil t)))
  (add-to-list 'safe-local-eval-forms
               '(when (eq system-type 'gnu/linux)
                  (add-hook 'after-save-hook #'org-babel-tangle nil t)))
  (add-to-list 'safe-local-eval-forms
               '(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images nil t))
  (add-to-list 'safe-local-eval-forms
               '(add-hook 'org-babel-after-execute-hook #'save-buffer nil t)))

;;;; Terraform

(use-package terraform-mode)

;;;; VIMrc

(use-package vimrc-mode)

;;; use-languages.el ends here
