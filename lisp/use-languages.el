;;; use-languages.el --- Language-specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Tree-Sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-extra-load-path
   `(,(expand-file-name "elpa/tree-sitter-module/dist/" user-emacs-directory)))
  (treesit-font-lock-level 4)
  :init
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))

(unless (package-installed-p 'tree-sitter-module)
  (package-vc-install "https://github.com/casouri/tree-sitter-module"))

;;;; CSV

(use-package csv-mode)

;;;; Configuration Files

(use-package conf-mode
  :ensure nil
  :mode (rx (or "CODEOWNERS"
                (and ".env" (* (and "." (+ word))))
                (and "." (+ word) "rc"))
            eos))

;;;; HTML

(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

;;;; Haskell

(use-package haskell-mode)

;;;; JavaScript

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package typescript-ts-mode
  :ensure nil
  :hook
  (tsx-ts-mode . sgml-electric-tag-pair-mode)
  :mode
  ((rx ".ts" eos) . typescript-ts-mode)
  ((rx ".tsx" eos) . tsx-ts-mode))

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
