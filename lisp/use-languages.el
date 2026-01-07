;;; use-languages.el --- Language-specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Tree-sitter

(use-package pendelhaven
  :load-path "lisp/pendelhaven"
  :commands
  pendelhaven-configure
  pendelhaven-install
  :custom
  (pendelhaven-directory (shelldock "pendelhaven/"))
  :hook
  (after-init . pendelhaven-configure))

(use-package treesit
  :ensure nil
  :config
  (setq-default
   treesit-language-source-alist
   '((css        . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
     (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
     (graphql    . ("https://github.com/bkegley/tree-sitter-graphql"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
     (kotlin     . ("https://github.com/fwcd/tree-sitter-kotlin"))
     (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
     (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript"
                    "master"
                    "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                    "master"
                    "typescript/src"))
     (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :custom
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

(use-package graphql-ts-mode)

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
  :mode ((rx ".mjs" eos) . js-ts-mode))

(declare-function json-pretty-print-ordered "json")
(declare-function json-pretty-print-buffer-ordered "json")

(defun me/json-prettify-dwim ()
  "Prettify region if active or entire buffer accordingly."
  (interactive)
  (if (use-region-p)
      (json-pretty-print-ordered (use-region-beginning) (use-region-end))
    (json-pretty-print-buffer-ordered)))

(use-package json-ts-mode
  :ensure nil
  :bind
  (:map json-ts-mode-map
   ([remap fill-paragraph] . me/json-prettify-dwim)))

;;;; Kotlin

(use-package kotlin-ts-mode)

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

;;;; Markdown

(use-package markdown-mode
  :defines markdown-code-lang-modes markdown-mode-map
  :bind
  (:map markdown-mode-map
   ("M-n" . nil)
   ("M-p" . nil))
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-list-indent-width 2)
  (markdown-split-window-direction 'right)
  :config
  (add-to-list 'markdown-code-lang-modes '("tsx" . tsx-ts-mode))
  :mode (rx (or "INSTALL" "CONTRIBUTORS" "LICENSE" "README" ".mdx") eos))

;;;; Org

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
  :custom
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-return-follows-link t)
  (org-src-window-setup 'current-window)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :config
  (add-to-list 'org-src-lang-modes (cons "ts" 'typescript-ts))
  (add-to-list 'org-src-lang-modes (cons "tsx" 'tsx-ts))
  (add-to-list
   'safe-local-variable-values '(after-save-hook . (org-babel-tangle t)))
  (require 'ob-shell)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (shell . t)))
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  (advice-add 'org-src--construct-edit-buffer-name :override #'me/org-src-buffer)
  :hook
  (org-mode . buffer-face-mode)
  (org-mode . (lambda () (setq-local comment-auto-fill-only-comments nil))))

;;;; Terraform

(use-package terraform-mode)

;;;; VIMrc

(use-package vimrc-mode)

;;; use-languages.el ends here
