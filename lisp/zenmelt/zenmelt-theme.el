;;; zenmelt-theme.el --- A Zenburn clone -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: February 08, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/zenmelt

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; An opinionated take on the ever popular Zenburn theme.
;;
;; The present theme is not meant to be exhaustive. Packages that I don't use
;; are left un-themed on purpose.

;;; Code:

(deftheme zenmelt "The Zenmelt color theme.")

(defvar zenmelt-colors-alist
  '(("bg-4"     . "#000000")
    ("bg-3"     . "#2B2B2B")
    ("bg-2"     . "#303030")
    ("bg-1"     . "#383838")
    ("bg"       . "#3F3F3F")
    ("bg+1"     . "#494949")
    ("bg+2"     . "#4F4F4F")
    ("bg+3"     . "#5F5F5F")
    ("bg+4"     . "#6F6F6F")
    ("blue-5"   . "#366060")
    ("blue-4"   . "#4C7073")
    ("blue-3"   . "#5C888B")
    ("blue-2"   . "#6CA0A3")
    ("blue-1"   . "#7CB8BB")
    ("blue"     . "#8CD0D3")
    ("blue+1"   . "#94BFF3")
    ("blue+2"   . "#ACE0E3")
    ("blue+3"   . "#BDE0F3")
    ("cyan"     . "#93E0E3")
    ("fg-2"     . "#656555")
    ("fg-1"     . "#989890")
    ("fg"       . "#DCDCCC")
    ("fg+1"     . "#FFFFEF")
    ("fg+2"     . "#FFFFFD")
    ("green-5"  . "#2F4F2F")
    ("green-4"  . "#3F5F3F")
    ("green-3"  . "#4F6F4F")
    ("green-2"  . "#5F7F5F")
    ("green-1"  . "#6F8F6F")
    ("green"    . "#7F9F7F")
    ("green+1"  . "#8FB28F")
    ("green+2"  . "#9FC59F")
    ("green+3"  . "#AFD8AF")
    ("green+4"  . "#BFEBBF")
    ("magenta"  . "#DC8CC3")
    ("orange"   . "#DFAF8F")
    ("red-6"    . "#6C3333")
    ("red-5"    . "#7C4343")
    ("red-4"    . "#8C5353")
    ("red-3"    . "#9C6363")
    ("red-2"    . "#AC7373")
    ("red-1"    . "#BC8383")
    ("red"      . "#CC9393")
    ("red+1"    . "#DCA3A3")
    ("red+2"    . "#ECB3B3")
    ("yellow-2" . "#D0BF8F")
    ("yellow-1" . "#E0CF9F")
    ("yellow"   . "#F0DFAF"))
  "List of Zenmelt colors.")

(defmacro zenmelt-with-colors (immediate &rest body)
  "Let-bind all colors defined in `zenmelt-colors-alist' around BODY.
With IMMEDIATE non nil, allow style changes immediately within the body.
See `custom--inhibit-theme-enable'."
  (declare (indent defun))
  `(let ((class '((class color) (min-colors 89)))
         (custom--inhibit-theme-enable (if ,immediate
                                           nil
                                         custom--inhibit-theme-enable))
         ,@(mapcar (lambda (cons) (list (intern (car cons)) (cdr cons)))
                   zenmelt-colors-alist))
     ,@body))

;;; Theme Faces
(zenmelt-with-colors nil
  (custom-theme-set-faces
   'zenmelt
;;;; Built-in
;;;;; Base
   `(bold                               ((t :foreground ,fg+1)))
   `(bold-italic                        ((t :inherit (italic bold))))
   `(default                            ((t :background ,bg :foreground ,fg)))
   `(error                              ((t :foreground ,red)))
   `(escape-glyph                       ((t :foreground ,yellow)))
   `(font-lock-builtin-face             ((t :foreground ,blue)))
   `(font-lock-comment-delimiter-face   ((t :foreground ,green-2)))
   `(font-lock-comment-face             ((t :foreground ,green)))
   `(font-lock-constant-face            ((t :foreground ,green+4)))
   `(font-lock-doc-face                 ((t :foreground ,green+2)))
   `(font-lock-function-name-face       ((t :foreground ,cyan)))
   `(font-lock-keyword-face             ((t :foreground ,yellow)))
   `(font-lock-negation-char-face       ((t :foreground ,yellow)))
   `(font-lock-preprocessor-face        ((t :foreground ,blue+1)))
   `(font-lock-regexp-grouping-backslash ((t :foreground ,green)))
   `(font-lock-regexp-grouping-construct ((t :foreground ,yellow)))
   `(font-lock-string-face              ((t :foreground ,red)))
   `(font-lock-type-face                ((t :foreground ,blue-1)))
   `(font-lock-variable-name-face       ((t :foreground ,orange)))
   `(font-lock-warning-face             ((t :foreground ,yellow-2)))
   `(highlight                          ((t :background ,blue-5
                                            :foreground ,blue)))
   `(italic                             ((t :foreground ,fg+1 :slant italic)))
   `(link                               ((t :foreground ,yellow :underline t)))
   `(link-visited                       ((t :foreground ,yellow-2 :underline t)))
   `(shadow                             ((t :foreground ,fg-1)))
   `(success                            ((t :foreground ,green)))
   `(warning                            ((t :foreground ,orange)))
;;;;; Applications
   `(eww-invalid-certificate            ((t :inherit error)))
   `(eww-valid-certificate              ((t :inherit success)))
;;;;; Compilation
   `(compilation-column-face            ((t :foreground ,yellow)))
   `(compilation-enter-directory-face   ((t :foreground ,green)))
   `(compilation-error-face             ((t :foreground ,red-1 :underline t)))
   `(compilation-face                   ((t :foreground ,fg)))
   `(compilation-info                   ((t :foreground ,green+4 :underline t)))
   `(compilation-info-face              ((t :foreground ,blue)))
   `(compilation-leave-directory-face   ((t :foreground ,green)))
   `(compilation-line-face              ((t :foreground ,yellow)))
   `(compilation-line-number            ((t :foreground ,yellow)))
   `(compilation-message-face           ((t :foreground ,blue)))
   `(compilation-mode-line-exit         ((t :foreground ,green+2)))
   `(compilation-mode-line-fail         ((t :foreground ,red)))
   `(compilation-mode-line-run          ((t :foreground ,yellow)))
   `(compilation-warning-face           ((t :foreground ,orange :underline t)))
;;;;; Customize
   `(custom-group-tag                   ((t :foreground ,blue)))
   `(custom-state                       ((t :foreground ,green+4)))
   `(custom-variable-tag                ((t :foreground ,blue)))
;;;;; Help
   `(Info-quoted                        ((t :inherit font-lock-constant-face)))
   `(eldoc-box-body                     ((t :inherit tooltip)))
   `(eldoc-box-border                   ((t :background ,red)))
   `(eldoc-highlight-function-argument  ((t :inherit highlight)))
;;;;; Interface
   `(button                             ((t :underline t)))
   `(cursor                             ((t :background ,fg+1 :foreground ,fg)))
   `(fringe                             ((t :foreground ,fg)))
   `(header-line                        ((t :background ,bg-3
                                            :box (:color ,bg-3 :line-width 4)
                                            :foreground ,yellow)))
   `(help-key-binding                   ((t :background ,bg+1
                                            :box
                                            (:color ,fg-2 :line-width (-2 . -2))
                                            :foreground ,orange)))
   `(hl-line                            ((t :background ,bg+2)))
   `(line-number                        ((t :inherit shadow)))
   `(line-number-current-line           ((t :foreground ,yellow-2
                                            :inherit hl-line)))
   `(menu                               ((t :inherit default)))
   `(minibuffer-prompt                  ((t :foreground ,yellow)))
   `(mode-line                          ((t :background ,bg-3
                                            :foreground ,green)))
   `(mode-line-buffer-id                ((t :foreground ,green+4)))
   `(mode-line-emphasis                 ((t :inherit mode-line)))
   `(mode-line-highlight                ((t :inherit highlight :box (-2 . -2))))
   `(mode-line-inactive                 ((t :background ,bg-1 :inherit shadow)))
   `(region                             ((,class :background ,blue-5)
                                         (t :inverse-video t)))
   `(secondary-selection                ((t :background ,bg+3)))
   `(tooltip                            ((t :background ,bg-3)))
   `(trailing-whitespace                ((t :background ,red)))
   `(vertical-border                    ((t :background ,bg-1)))
   `(widget-field                       ((t :background ,bg+4 :foreground ,fg)))
   `(window-divider                     ((t :inherit vertical-border
                                            :inverse-video t)))
   `(window-divider-first-pixel         ((t :inherit window-divider)))
   `(window-divider-last-pixel          ((t :inherit window-divider)))
;;;;; Language
   `(flymake-error                      ((t :background ,red-5
                                            :foreground ,red+1
                                            :underline t)))
   `(flymake-note                       ((t :background ,blue-5
                                            :foreground ,blue+1
                                            :underline t)))
   `(flymake-warning                    ((t :background ,fg-2
                                            :foreground ,orange
                                            :underline t)))
;;;;; Org
   `(org-agenda-date-today              ((t :foreground ,fg+1 :slant italic)))
   `(org-agenda-structure               ((t :inherit font-lock-comment-face)))
   `(org-archived                       ((t :foreground ,fg)))
   `(org-block                          ((t :background ,bg+1
                                            :extend t
                                            :foreground ,fg)))
   `(org-block-begin-line               ((t :inherit shadow)))
   `(org-checkbox                       ((t :inherit org-verbatim)))
   `(org-column                         ((t :background ,bg-3)))
   `(org-column-title                   ((t :background ,bg-3 :underline t)))
   `(org-date                           ((t :foreground ,blue :underline t)))
   `(org-deadline-announce              ((t :foreground ,red-1)))
   `(org-default                        ((t :foreground ,fg-1)))
   `(org-document-info                  ((t :foreground ,blue)))
   `(org-document-title                 ((t :foreground ,blue)))
   `(org-done                           ((t :foreground ,green+3)))
   `(org-ellipsis                       ((t :foreground ,yellow-1 :underline t)))
   `(org-footnote                       ((t :foreground ,cyan :underline t)))
   `(org-formula                        ((t :foreground ,yellow-2)))
   `(org-habit-alert-face               ((t :background ,yellow-1
                                            :foreground ,bg)))
   `(org-habit-alert-future-face        ((t :background ,yellow-2
                                            :foreground ,bg)))
   `(org-habit-clear-face               ((t :background ,blue-3)))
   `(org-habit-clear-future-face        ((t :background ,blue-4)))
   `(org-habit-overdue-face             ((t :background ,red-3)))
   `(org-habit-overdue-future-face      ((t :background ,red-4)))
   `(org-habit-ready-face               ((t :background ,green)))
   `(org-habit-ready-future-face        ((t :background ,green-2)))
   `(org-headline-done                  ((t :foreground ,green+3)))
   `(org-hide                           ((t :foreground ,bg)))
   `(org-level-1                        ((t :background ,bg+1
                                            :foreground ,orange
                                            :height 1.2
                                            :overline t
                                            :underline t)))
   `(org-level-2                        ((t :foreground ,green+4
                                            :inherit org-level-1)))
   `(org-level-3                        ((t :background ,bg
                                            :foreground ,blue-1
                                            :inherit org-level-2
                                            :overline nil
                                            :underline nil)))
   `(org-level-4                        ((t :foreground ,yellow-2
                                            :inherit org-level-3)))
   `(org-level-5                        ((t :foreground ,cyan
                                            :inherit org-level-4)))
   `(org-level-6                        ((t :foreground ,green+2
                                            :inherit org-level-5)))
   `(org-level-7                        ((t :foreground ,red-4
                                            :inherit org-level-6)))
   `(org-level-8                        ((t :foreground ,blue-4
                                            :inherit org-level-7)))
   `(org-link                           ((t :foreground ,yellow-2 :underline t)))
   `(org-mode-line-clock                ((t :foreground ,fg :background ,bg-3)))
   `(org-mode-line-clock-overrun        ((t :background ,red-1 :foreground ,bg)))
   `(org-quote                          ((t :foreground ,fg-1 :italic t)))
   `(org-scheduled                      ((t :foreground ,green+4)))
   `(org-scheduled-previously           ((t :foreground ,red)))
   `(org-scheduled-today                ((t :foreground ,blue+1)))
   `(org-sexp-date                      ((t :foreground ,blue+1 :underline t)))
   `(org-special-keyword                ((t :inherit font-lock-comment-face)))
   `(org-table                          ((t :foreground ,green+2)))
   `(org-tag                            ((t :foreground ,fg+1)))
   `(org-time-grid                      ((t :foreground ,orange)))
   `(org-todo                           ((t :foreground ,red)))
   `(org-upcoming-deadline              ((t :inherit font-lock-keyword-face)))
   `(org-verbatim                       ((t :inherit help-key-binding)))
   `(org-warning                        ((t :foreground ,red)))
;;;;; Search
   `(grep-context-face                  ((t :foreground ,fg)))
   `(grep-error-face                    ((t :foreground ,red-1 :underline t)))
   `(grep-hit-face                      ((t :foreground ,blue)))
   `(grep-match-face                    ((t :foreground ,orange)))
   `(isearch                            ((t :background ,blue
                                            :box (-1 . -1)
                                            :foreground ,blue-5)))
   `(isearch-fail                       ((t :foreground ,red)))
   `(lazy-highlight                     ((t :background ,blue-5
                                            :foreground ,blue
                                            :inherit isearch)))
   `(match                              ((t :background ,bg-3
                                            :foreground ,orange)))
;;;;; Syntax
   `(whitespace-empty                   ((t :background ,yellow-2 :extend t)))
   `(whitespace-hspace                  ((t :inherit whitespace-space)))
   `(whitespace-indentation             ((t :background ,bg+1 :foreground ,red)))
   `(whitespace-line                    ((t :foreground ,magenta)))
   `(whitespace-newline                 ((t :foreground ,bg+2)))
   `(whitespace-space                   ((t :inherit shadow :inverse-video t)))
   `(whitespace-space-after-tab         ((t :inherit whitespace-space)))
   `(whitespace-space-before-tab        ((t :inherit
                                            whitespace-space-after-tab)))
   `(whitespace-tab                     ((t :background ,red-1)))
   `(whitespace-trailing                ((t :inherit trailing-whitespace)))
;;;;; Version Control
;;;;;; Diff
   `(diff-added                         ((t :foreground ,green)))
   `(diff-changed                       ((t :foreground ,yellow-2)))
   `(diff-context                       ((t :foreground ,fg-2)))
   `(diff-file-header                   ((t :inherit diff-header)))
   `(diff-function                      ((t :foreground ,blue
                                            :inherit diff-hunk-header)))
   `(diff-header                        ((t :background ,bg+1
                                            :foreground ,yellow)))
   `(diff-hunk-header                   ((t :foreground ,yellow-2
                                            :inherit diff-header)))
   `(diff-indicator-added               ((t :inherit diff-added)))
   `(diff-indicator-removed             ((t :inherit diff-removed)))
   `(diff-refine-added                  ((t :foreground ,green+4)))
   `(diff-refine-changed                ((t :inherit diff-refine-added
                                            :foreground ,yellow)))
   `(diff-refine-removed                ((t :inherit diff-refine-added
                                            :foreground ,red+2)))
   `(diff-removed                       ((t :foreground ,red)))
;;;;;; Ediff
   `(ediff-current-diff-A               ((t :background ,red-6)))
   `(ediff-current-diff-Ancestor        ((t :inherit ediff-current-diff-A)))
   `(ediff-current-diff-B               ((t :background ,green-5)))
   `(ediff-current-diff-C               ((t :background ,blue-5)))
   `(ediff-even-diff-A                  ((t :background ,bg+2)))
   `(ediff-even-diff-Ancestor           ((t :inherit ediff-even-diff-A)))
   `(ediff-even-diff-B                  ((t :inherit ediff-even-diff-A)))
   `(ediff-even-diff-C                  ((t :inherit ediff-even-diff-A)))
   `(ediff-fine-diff-A                  ((t :background ,red-5)))
   `(ediff-fine-diff-Ancestor           ((t :inherit ediff-fine-diff-A)))
   `(ediff-fine-diff-B                  ((t :background ,green-4)))
   `(ediff-fine-diff-C                  ((t :background ,blue-4)))
   `(ediff-odd-diff-A                   ((t :background ,bg+1)))
   `(ediff-odd-diff-Ancestor            ((t :inherit ediff-odd-diff-A)))
   `(ediff-odd-diff-B                   ((t :inherit ediff-odd-diff-A)))
   `(ediff-odd-diff-C                   ((t :inherit ediff-odd-diff-A)))
;;;;;; Smerge
   `(smerge-lower                       ((t :background ,bg+1
                                            :inherit diff-added)))
   `(smerge-markers                     ((t :background ,bg+1 :inherit shadow)))
   `(smerge-refined-added               ((t :inherit diff-refine-added)))
   `(smerge-refined-removed             ((t :inherit diff-refine-removed)))
   `(smerge-upper                       ((t :background ,bg+1
                                            :inherit diff-removed)))
;;;; Third-party
;;;;; Completion
   `(completions-annotations            ((t :foreground ,fg-1)))
   `(consult-preview-line               ((t :extend t :inherit highlight)))
   `(consult-preview-match              ((t :background nil)))
   `(corfu-current                      ((t :inherit 'highlight)))
   `(corfu-default                      ((t :background ,bg-3)))
   `(orderless-match-face-0             ((t :background ,fg-2
                                            :box (-1 . -1)
                                            :foreground ,yellow-2)))
   `(orderless-match-face-1             ((t :background ,green-3
                                            :box (-1 . -1)
                                            :foreground ,green+2)))
   `(orderless-match-face-2             ((t :background ,red-3
                                            :box (-1 . -1)
                                            :foreground ,red+2)))
   `(orderless-match-face-3             ((t :background ,blue-4
                                            :box (-1 . -1)
                                            :foreground ,blue+1)))
;;;;; Help
   `(helpful-heading                    ((t :foreground ,orange
                                            :height 1.4
                                            :inherit variable-pitch)))
;;;;; Language Servers
   `(eglot-highlight-symbol-face        ((t :background ,fg-2
                                            :box (-1 . -1)
                                            :foreground ,yellow)))
;;;;; Mode-Line
   `(doom-modeline-bar                  ((t :inherit mode-line)))
   `(doom-modeline-bar-inactive         ((t :inherit doom-modeline-bar)))
   `(doom-modeline-evil-emacs-state     ((t :foreground ,magenta)))
   `(doom-modeline-evil-insert-state    ((t :foreground ,blue)))
   `(doom-modeline-info                 ((t :foreground ,green+4)))
   `(doom-modeline-project-dir          ((t :inherit dired-directory)))
   `(doom-modeline-project-parent-dir   ((t :inherit shadow)))
;;;;; Version Control
;;;;;; Magit: bisect
   `(magit-bisect-bad                   ((t :foreground ,red)))
   `(magit-bisect-good                  ((t :foreground ,green)))
   `(magit-bisect-skip                  ((t :foreground ,yellow)))
;;;;;; Magit: blame
   `(magit-blame-date                   ((t :foreground ,fg)))
   `(magit-blame-hash                   ((t :foreground ,blue)))
   `(magit-blame-heading                ((t :background ,bg-3 :inherit shadow)))
   `(magit-blame-name                   ((t :foreground ,orange)))
;;;;;; Magit: diff
   `(magit-diff-added                   ((t :inherit diff-added)))
   `(magit-diff-added-highlight         ((t :foreground ,green+1)))
   `(magit-diff-context                 ((t :inherit magit-dimmed)))
   `(magit-diff-context-highlight       ((t :background ,bg)))
   `(magit-diff-file-heading            ((t :weight normal)))
   `(magit-diff-file-heading-highlight  ((t :foreground ,fg+1)))
   `(magit-diff-file-heading-selection  ((t :foreground ,orange)))
   `(magit-diff-hunk-heading            ((t :inherit diff-hunk-header)))
   `(magit-diff-hunk-heading-highlight  ((t :foreground ,fg+1)))
   `(magit-diff-hunk-heading-selection  ((t :foreground ,orange)))
   `(magit-diff-hunk-region             ((t :weight unspecified)))
   `(magit-diff-lines-heading           ((t :foreground ,orange)))
   `(magit-diff-removed                 ((t :inherit diff-removed)))
   `(magit-diff-removed-highlight       ((t :foreground ,red+1)))
   `(magit-diffstat-added               ((t :foreground ,green+4)))
   `(magit-diffstat-removed             ((t :foreground ,red)))
;;;;;; Magit: section
   `(magit-section-heading              ((t :foreground ,yellow)))
   `(magit-section-heading-selection    ((t :foreground ,orange)))
   `(magit-section-highlight            ((t :inherit hl-line)))
;;;;;; Magit: log
   `(magit-log-author                   ((t :foreground ,orange)))
   `(magit-log-date                     ((t :inherit magit-dimmed)))
   `(magit-log-graph                    ((t :foreground ,yellow)))
;;;;;; Magit: process
   `(magit-process-ng                   ((t :foreground ,red)))
   `(magit-process-ok                   ((t :foreground ,green)))
;;;;;; Magit: references
   `(magit-branch-current               ((t :box (-1 . -1)
                                            :inherit magit-branch-local)))
   `(magit-branch-local                 ((t :foreground ,blue)))
   `(magit-branch-remote                ((t :foreground ,green+4)))
   `(magit-branch-remote-head           ((t :box (-1 . -1)
                                            :inherit magit-branch-remote)))
   `(magit-cherry-equivalent            ((t :foreground ,magenta)))
   `(magit-cherry-unmatched             ((t :foreground ,cyan)))
   `(magit-dimmed                       ((t :inherit shadow)))
   `(magit-hash                         ((t :inherit magit-dimmed)))
   `(magit-reflog-amend                 ((t :foreground ,orange)))
   `(magit-reflog-checkout              ((t :foreground ,blue+1)))
   `(magit-reflog-cherry-pick           ((t :foreground ,red+2)))
   `(magit-reflog-commit                ((t :foreground ,green)))
   `(magit-reflog-merge                 ((t :foreground ,green+4)))
   `(magit-reflog-other                 ((t :foreground ,magenta)))
   `(magit-reflog-rebase                ((t :foreground ,yellow)))
   `(magit-reflog-remote                ((t :foreground ,cyan)))
   `(magit-reflog-reset                 ((t :foreground ,red)))
   `(magit-refname                      ((t :inherit magit-dimmed)))
   `(magit-signature-bad                ((t :foreground ,red)))
   `(magit-signature-error              ((t :foreground ,red-1)))
   `(magit-signature-expired            ((t :foreground ,orange)))
   `(magit-signature-good               ((t :foreground ,green)))
   `(magit-signature-revoked            ((t :foreground ,magenta)))
   `(magit-signature-untrusted          ((t :foreground ,yellow)))
   `(magit-tag                          ((t :foreground ,orange)))
;;;;;; Magit: sequence
   `(magit-sequence-done                ((t :inherit magit-dimmed)))
   `(magit-sequence-drop                ((t :foreground ,red)))
   `(magit-sequence-head                ((t :foreground ,blue)))
   `(magit-sequence-onto                ((t :inherit magit-dimmed)))
   `(magit-sequence-part                ((t :foreground ,yellow)))
   `(magit-sequence-pick                ((t :foreground ,yellow-2)))
   `(magit-sequence-stop                ((t :foreground ,green+4)))
;;;;; Markdown
   `(markdown-inline-code-face          ((t :inherit org-verbatim)))

;;;; TODO Prettify useful faces and clean up the rest
;;;;; git-annex
   `(git-annex-dired-annexed-available  ((t :inherit success)))
   `(git-annex-dired-annexed-unavailable ((t :inherit error)))
;;;;; git-commit
   `(git-commit-comment-action          ((,class :foreground ,green+1)))
   `(git-commit-comment-branch          ((,class :foreground ,blue+1))) ; Obsolete
   `(git-commit-comment-branch-local    ((,class :foreground ,blue+1)))
   `(git-commit-comment-branch-remote   ((,class :foreground ,green)))
   `(git-commit-comment-heading         ((,class :foreground ,yellow)))
;;;;; git-gutter
   `(git-gutter:added                   ((t :foreground ,green :inverse-video t)))
   `(git-gutter:deleted                 ((t :foreground ,red :inverse-video t)))
   `(git-gutter:modified                ((t :foreground ,blue-2
                                            :inverse-video t)))
   `(git-gutter:unchanged               ((t :foreground ,fg :inverse-video t)))
;;;;; git-gutter-fr
   `(git-gutter-fr:added                ((t :foreground ,green)))
   `(git-gutter-fr:deleted              ((t :foreground ,red)))
   `(git-gutter-fr:modified             ((t :foreground ,blue-2)))
;;;;; git-rebase
   `(git-rebase-hash                    ((t :foreground, orange)))
;;;;; diff-hl
   `(diff-hl-change                     ((,class :background ,blue-2
                                                 :foreground ,blue)))
   `(diff-hl-delete                     ((,class :background ,red-1
                                                 :foreground ,red+1)))
   `(diff-hl-insert                     ((,class :background ,green-2
                                                 :foreground ,green+1)))
;;;;; ace-jump
   `(ace-jump-face-background           ((t :background ,bg
                                            :foreground ,fg-2
                                            :inverse-video nil)))
   `(ace-jump-face-foreground           ((t :background ,bg
                                            :foreground ,green+2
                                            :inverse-video nil)))
;;;;; anzu
   `(anzu-mode-line                     ((t :foreground ,cyan)))
   `(anzu-mode-line-no-match            ((t :foreground ,red)))
   `(anzu-match-1                       ((t :background ,green
                                            :foreground ,bg)))
   `(anzu-match-2                       ((t :background ,orange
                                            :foreground ,bg)))
   `(anzu-match-3                       ((t :background ,blue
                                            :foreground ,bg)))
   `(anzu-replace-to                    ((t :foreground ,yellow
                                            :inherit anzu-replace-highlight)))
;;;;; avy
   `(avy-background-face                ((t :foreground ,fg-2
                                            :inverse-video nil)))
   `(avy-lead-face                      ((t :foreground ,cyan
                                            :inverse-video nil)))
   `(avy-lead-face-0                    ((t :foreground ,green+3
                                            :inverse-video nil)))
   `(avy-lead-face-1                    ((t :foreground ,yellow
                                            :inverse-video nil)))
   `(avy-lead-face-2                    ((t :foreground ,red+1
                                            :inverse-video nil)))
;;;;; bm
   `(bm-face                            ((t :background ,yellow-1
                                            :foreground ,bg)))
   `(bm-fringe-face                     ((t :background ,yellow-1
                                            :foreground ,bg)))
   `(bm-fringe-persistent-face          ((t :background ,green-2
                                            :foreground ,bg)))
   `(bm-persistent-face                 ((t :background ,green-2
                                            :foreground ,bg)))
;;;;; cider
   `(cider-result-overlay-face          ((t :background unspecified)))
   `(cider-enlightened-face             ((t :box
                                            (:color ,orange :line-width -1))))
   `(cider-enlightened-local-face       ((t :foreground ,green+1)))
   `(cider-deprecated-face              ((t :background ,yellow-2)))
   `(cider-instrumented-face            ((t :box (:color ,red :line-width -1))))
   `(cider-traced-face                  ((t :box (:color ,cyan :line-width -1))))
   `(cider-test-failure-face            ((t :background ,red-4)))
   `(cider-test-error-face              ((t :background ,magenta)))
   `(cider-test-success-face            ((t :background ,green-2)))
   `(cider-fringe-good-face             ((t :foreground ,green+4)))
;;;;; context-coloring
   `(context-coloring-level-0-face      ((t :foreground ,fg)))
   `(context-coloring-level-1-face      ((t :foreground ,cyan)))
   `(context-coloring-level-2-face      ((t :foreground ,green+4)))
   `(context-coloring-level-3-face      ((t :foreground ,yellow)))
   `(context-coloring-level-4-face      ((t :foreground ,orange)))
   `(context-coloring-level-5-face      ((t :foreground ,magenta)))
   `(context-coloring-level-6-face      ((t :foreground ,blue+1)))
   `(context-coloring-level-7-face      ((t :foreground ,green+2)))
   `(context-coloring-level-8-face      ((t :foreground ,yellow-2)))
   `(context-coloring-level-9-face      ((t :foreground ,red+1)))
;;;;; coq
   `(coq-solve-tactics-face             ((t :foreground nil
                                            :inherit font-lock-constant-face)))
;;;;; ctable
   `(ctbl:face-cell-select              ((t :background ,blue :foreground ,bg)))
   `(ctbl:face-continue-bar             ((t :background ,bg-1 :foreground ,bg)))
   `(ctbl:face-row-select               ((t :background ,cyan :foreground ,bg)))
;;;;; debbugs
   `(debbugs-gnu-done                   ((t :foreground ,fg-2)))
   `(debbugs-gnu-handled                ((t :foreground ,green)))
   `(debbugs-gnu-new                    ((t :foreground ,red)))
   `(debbugs-gnu-pending                ((t :foreground ,blue)))
   `(debbugs-gnu-stale                  ((t :foreground ,orange)))
   `(debbugs-gnu-tagged                 ((t :foreground ,red)))
;;;;; dim-autoload
   `(dim-autoload-cookie-line           ((t :foreground ,bg+2)))
;;;;; diredfl
   `(diredfl-compressed-file-suffix     ((t :foreground ,orange)))
   `(diredfl-date-time                  ((t :foreground ,magenta)))
   `(diredfl-deletion                   ((t :foreground ,yellow)))
   `(diredfl-deletion-file-name         ((t :foreground ,red)))
   `(diredfl-dir-heading                ((t :background ,bg-3 :foreground ,blue)))
   `(diredfl-dir-priv                   ((t :foreground ,cyan)))
   `(diredfl-exec-priv                  ((t :foreground ,red)))
   `(diredfl-executable-tag             ((t :foreground ,green+1)))
   `(diredfl-file-name                  ((t :foreground ,blue)))
   `(diredfl-file-suffix                ((t :foreground ,green)))
   `(diredfl-flag-mark                  ((t :foreground ,yellow)))
   `(diredfl-flag-mark-line             ((t :foreground ,orange)))
   `(diredfl-ignored-file-name          ((t :foreground ,red)))
   `(diredfl-link-priv                  ((t :foreground ,yellow)))
   `(diredfl-no-priv                    ((t :foreground ,fg)))
   `(diredfl-number                     ((t :foreground ,green+1)))
   `(diredfl-other-priv                 ((t :foreground ,yellow-1)))
   `(diredfl-rare-priv                  ((t :foreground ,red-1)))
   `(diredfl-read-priv                  ((t :foreground ,green-1)))
   `(diredfl-symlink                    ((t :foreground ,yellow)))
   `(diredfl-write-priv                 ((t :foreground ,magenta)))
;;;;; egg
   `(egg-text-base                      ((t :foreground ,fg)))
   `(egg-help-header-1                  ((t :foreground ,yellow)))
   `(egg-help-header-2                  ((t :foreground ,green+3)))
   `(egg-branch                         ((t :foreground ,yellow)))
   `(egg-branch-mono                    ((t :foreground ,yellow)))
   `(egg-term                           ((t :foreground ,yellow)))
   `(egg-diff-add                       ((t :foreground ,green+4)))
   `(egg-diff-del                       ((t :foreground ,red+1)))
   `(egg-diff-file-header               ((t :foreground ,yellow-2)))
   `(egg-section-title                  ((t :foreground ,yellow)))
   `(egg-stash-mono                     ((t :foreground ,green+4)))
;;;;; elfeed
   `(elfeed-log-error-level-face        ((t :foreground ,red)))
   `(elfeed-log-info-level-face         ((t :foreground ,blue)))
   `(elfeed-log-warn-level-face         ((t :foreground ,yellow)))
   `(elfeed-search-date-face            ((t :foreground ,yellow-1
                                            :underline t)))
   `(elfeed-search-tag-face             ((t :foreground ,green)))
   `(elfeed-search-feed-face            ((t :foreground ,cyan)))
   `(elfeed-search-title-face           ((t :foreground ,fg-1)))
   `(elfeed-search-unread-title-face    ((t :foreground ,fg)))
;;;;; eros
   `(eros-result-overlay-face           ((t :background unspecified)))
;;;;; ert
   `(ert-test-result-expected           ((t :background ,bg
                                            :foreground ,green+4)))
   `(ert-test-result-unexpected         ((t :foreground ,red)))
;;;;; eshell
   `(eshell-prompt                      ((t :foreground ,yellow)))
   `(eshell-ls-archive                  ((t :foreground ,red-1)))
   `(eshell-ls-backup                   ((t :inherit font-lock-comment-face)))
   `(eshell-ls-clutter                  ((t :inherit font-lock-comment-face)))
   `(eshell-ls-directory                ((t :foreground ,blue+1)))
   `(eshell-ls-executable               ((t :foreground ,red+1)))
   `(eshell-ls-unreadable               ((t :foreground ,fg)))
   `(eshell-ls-missing                  ((t :inherit font-lock-warning-face)))
   `(eshell-ls-product                  ((t :inherit font-lock-doc-face)))
   `(eshell-ls-special                  ((t :foreground ,yellow)))
   `(eshell-ls-symlink                  ((t :foreground ,cyan)))
;;;;; flx
   `(flx-highlight-face                 ((t :foreground ,green+2)))
;;;;; full-ack
   `(ack-separator                      ((t :foreground ,fg)))
   `(ack-file                           ((t :foreground ,blue)))
   `(ack-line                           ((t :foreground ,yellow)))
   `(ack-match                          ((t  :background ,bg-3
                                             :foreground ,orange)))
;;;;; guide-key
   `(guide-key/highlight-command-face   ((t :foreground ,blue)))
   `(guide-key/key-face                 ((t :foreground ,green)))
   `(guide-key/prefix-command-face      ((t :foreground ,green+1)))
;;;;; highlight-numbers
   `(highlight-numbers-number           ((t :foreground ,blue)))
;;;;; highlight-symbol
   `(highlight-symbol-face              ((t :background ,bg+3)))
;;;;; highlight-thing
   `(highlight-thing                    ((t :background ,bg+3)))
;;;;; hl-sexp
   `(hl-sexp-face                       ((,class :background ,bg+2) (t)))
;;;;; hydra
   `(hydra-face-red                     ((t :foreground ,red-1)))
   `(hydra-face-amaranth                ((t :foreground ,red-3)))
   `(hydra-face-blue                    ((t :foreground ,blue)))
   `(hydra-face-pink                    ((t :background ,bg
                                            :foreground ,magenta)))
   `(hydra-face-teal                    ((t :foreground ,cyan)))
;;;;; info+
   `(info-command-ref-item              ((t :background ,bg-3
                                            :foreground ,orange)))
   `(info-constant-ref-item             ((t :background ,bg-3
                                            :foreground ,magenta)))
   `(info-double-quoted-name            ((t :inherit font-lock-comment-face)))
   `(info-file                          ((t :background ,bg-3
                                            :foreground ,yellow)))
   `(info-function-ref-item             ((t :background ,bg-3
                                            :inherit
                                            font-lock-function-name-face)))
   `(info-macro-ref-item                ((t :background ,bg-3
                                            :foreground ,yellow)))
   `(info-menu                          ((t :foreground ,yellow)))
   `(info-quoted-name                   ((t :inherit font-lock-constant-face)))
   `(info-reference-item                ((t :background ,bg-3)))
   `(info-single-quote                  ((t :inherit font-lock-keyword-face)))
   `(info-special-form-ref-item         ((t :background ,bg-3
                                            :foreground ,yellow)))
   `(info-string                        ((t :inherit font-lock-string-face)))
   `(info-syntax-class-item             ((t :background ,bg-3
                                            :foreground ,blue+1)))
   `(info-user-option-ref-item          ((t :background ,bg-3
                                            :foreground ,red)))
   `(info-variable-ref-item             ((t :background ,bg-3
                                            :foreground ,orange)))
;;;;; irfc
   `(irfc-head-name-face                ((t :foreground ,red)))
   `(irfc-head-number-face              ((t :foreground ,red)))
   `(irfc-reference-face                ((t :foreground ,blue-1)))
   `(irfc-requirement-keyword-face      ((t :inherit font-lock-keyword-face)))
   `(irfc-rfc-link-face                 ((t :inherit link)))
   `(irfc-rfc-number-face               ((t :foreground ,cyan)))
   `(irfc-std-number-face               ((t :foreground ,green+4)))
   `(irfc-table-item-face               ((t :foreground ,green+3)))
   `(irfc-title-face                    ((t :foreground ,yellow :underline t)))
;;;;; iedit-mode
   `(iedit-occurrence                   ((t :background ,bg+3)))
;;;;; js2-mode
   `(js2-warning                        ((t :underline ,orange)))
   `(js2-error                          ((t :foreground ,red)))
   `(js2-jsdoc-tag                      ((t :foreground ,green-2)))
   `(js2-jsdoc-type                     ((t :foreground ,green+2)))
   `(js2-jsdoc-value                    ((t :foreground ,green+3)))
   `(js2-function-param                 ((t :foreground, orange)))
   `(js2-external-variable              ((t :foreground ,orange)))
   `(js2-instance-member                ((t :foreground ,green-2)))
   `(js2-jsdoc-html-tag-delimiter       ((t :foreground ,orange)))
   `(js2-jsdoc-html-tag-name            ((t :foreground ,red-1)))
   `(js2-object-property                ((t :foreground ,blue+1)))
   `(js2-magic-paren                    ((t :foreground ,blue-5)))
   `(js2-private-function-call          ((t :foreground ,cyan)))
   `(js2-function-call                  ((t :foreground ,cyan)))
   `(js2-private-member                 ((t :foreground ,blue-1)))
   `(js2-keywords                       ((t :foreground ,magenta)))
;;;;; lispy
   `(lispy-command-name-face            ((t :background ,bg-1
                                            :inherit
                                            ,font-lock-function-name-face)))
   `(lispy-cursor-face                  ((t :background ,fg :foreground ,bg)))
   `(lispy-face-hint                    ((t :foreground ,yellow
                                            :inherit highlight)))
;;;;; lui
   `(lui-time-stamp-face                ((t :foreground ,blue-1)))
   `(lui-hilight-face                   ((t :background ,bg
                                            :foreground ,green+2)))
   `(lui-button-face                    ((t :inherit hover-highlight)))
;;;;; macrostep
   `(macrostep-gensym-1                 ((t :background ,bg-3
                                            :foreground ,green+2)))
   `(macrostep-gensym-2                 ((t :background ,bg-3
                                            :foreground ,red+1)))
   `(macrostep-gensym-3                 ((t :background ,bg-3
                                            :foreground ,blue+1)))
   `(macrostep-gensym-4                 ((t :background ,bg-3
                                            :foreground ,magenta)))
   `(macrostep-gensym-5                 ((t :background ,bg-3
                                            :foreground ,yellow)))
   `(macrostep-expansion-highlight-face ((t :inherit highlight)))
   `(macrostep-macro-face               ((t :underline t)))
;;;;; markup-faces
   `(markup-anchor-face                 ((t :foreground ,blue+1)))
   `(markup-code-face                   ((t :inherit font-lock-constant-face)))
   `(markup-command-face                ((t :foreground ,yellow)))
   `(markup-emphasis-face               ((t :inherit bold)))
   `(markup-internal-reference-face     ((t :foreground ,yellow-2 :underline t)))
   `(markup-list-face                   ((t :foreground ,fg+1)))
   `(markup-meta-face                   ((t :foreground ,yellow)))
   `(markup-meta-hide-face              ((t :foreground ,yellow)))
   `(markup-secondary-text-face         ((t :foreground ,yellow-1)))
   `(markup-title-0-face                ((t :inherit
                                            font-lock-function-name-face)))
   `(markup-title-1-face                ((t :inherit markup-title-0-face)))
   `(markup-title-2-face                ((t :inherit markup-title-0-face)))
   `(markup-title-3-face                ((t :inherit markup-title-0-face)))
   `(markup-title-4-face                ((t :inherit markup-title-0-face)))
   `(markup-typewriter-face             ((t :inherit font-lock-constant-face)))
   `(markup-verbatim-face               ((t :inherit font-lock-constant-face)))
   `(markup-value-face                  ((t :foreground ,yellow)))
;;;;; message-mode
   `(message-cited-text                 ((t :inherit font-lock-comment-face)))
   `(message-header-name                ((t :foreground ,green+1)))
   `(message-header-other               ((t :foreground ,green)))
   `(message-header-to                  ((t :foreground ,yellow)))
   `(message-header-cc                  ((t :foreground ,yellow)))
   `(message-header-newsgroups          ((t :foreground ,yellow)))
   `(message-header-subject             ((t :foreground ,orange)))
   `(message-header-xheader             ((t :foreground ,green)))
   `(message-mml                        ((t :foreground ,yellow)))
   `(message-separator                  ((t :inherit font-lock-comment-face)))
;;;;; mew
   `(mew-face-header-subject            ((t :foreground ,orange)))
   `(mew-face-header-from               ((t :foreground ,yellow)))
   `(mew-face-header-date               ((t :foreground ,green)))
   `(mew-face-header-to                 ((t :foreground ,red)))
   `(mew-face-header-key                ((t :foreground ,green)))
   `(mew-face-header-private            ((t :foreground ,green)))
   `(mew-face-header-important          ((t :foreground ,blue)))
   `(mew-face-header-marginal           ((t :foreground ,fg+1)))
   `(mew-face-header-warning            ((t :foreground ,red)))
   `(mew-face-header-xmew               ((t :foreground ,green)))
   `(mew-face-header-xmew-bad           ((t :foreground ,red)))
   `(mew-face-body-url                  ((t :foreground ,orange)))
   `(mew-face-body-comment              ((t :foreground ,fg :slant italic)))
   `(mew-face-body-cite1                ((t :foreground ,green)))
   `(mew-face-body-cite2                ((t :foreground ,blue)))
   `(mew-face-body-cite3                ((t :foreground ,orange)))
   `(mew-face-body-cite4                ((t :foreground ,yellow)))
   `(mew-face-body-cite5                ((t :foreground ,red)))
   `(mew-face-mark-review               ((t :foreground ,blue)))
   `(mew-face-mark-escape               ((t :foreground ,green)))
   `(mew-face-mark-delete               ((t :foreground ,red)))
   `(mew-face-mark-unlink               ((t :foreground ,yellow)))
   `(mew-face-mark-refile               ((t :foreground ,green)))
   `(mew-face-mark-unread               ((t :foreground ,red-2)))
   `(mew-face-eof-message               ((t :foreground ,green)))
   `(mew-face-eof-part                  ((t :foreground ,yellow)))
;;;;; mic-paren
   `(paren-face-match                   ((t :foreground ,cyan)))
   `(paren-face-mismatch                ((t :background ,magenta
                                            :foreground ,bg)))
   `(paren-face-no-match                ((t :background ,red :foreground ,bg)))
;;;;; mingus
   `(mingus-directory-face              ((t :foreground ,blue)))
   `(mingus-pausing-face                ((t :foreground ,magenta)))
   `(mingus-playing-face                ((t :foreground ,cyan)))
   `(mingus-playlist-face               ((t :foreground ,cyan )))
   `(mingus-mark-face                   ((t :foreground ,magenta)))
   `(mingus-song-file-face              ((t :foreground ,yellow)))
   `(mingus-artist-face                 ((t :foreground ,cyan)))
   `(mingus-album-face                  ((t :foreground ,red+1 :underline t)))
   `(mingus-album-stale-face            ((t :foreground ,red+1)))
   `(mingus-stopped-face                ((t :foreground ,red)))
;;;;; nav
   `(nav-face-heading                   ((t :foreground ,yellow)))
   `(nav-face-button-num                ((t :foreground ,cyan)))
   `(nav-face-dir                       ((t :foreground ,green)))
   `(nav-face-hdir                      ((t :foreground ,red)))
   `(nav-face-file                      ((t :foreground ,fg)))
   `(nav-face-hfile                     ((t :foreground ,red-4)))
;;;;; mumamo
   `(mumamo-background-chunk-major      ((t :background nil)))
   `(mumamo-background-chunk-submode1   ((t :background ,bg-3)))
   `(mumamo-background-chunk-submode2   ((t :background ,bg+3)))
   `(mumamo-background-chunk-submode3   ((t :background ,bg+4)))
   `(mumamo-background-chunk-submode4   ((t :background ,bg+2)))
;;;;; org-ref
   `(org-ref-ref-face                   ((t :underline t)))
   `(org-ref-label-face                 ((t :underline t)))
   `(org-ref-cite-face                  ((t :underline t)))
   `(org-ref-glossary-face              ((t :underline t)))
   `(org-ref-acronym-face               ((t :underline t)))
;;;;; outline
   `(outline-1                          ((t :foreground ,orange))) ; 1.3
   `(outline-2                          ((t :foreground ,green+4))) ; 1.2
   `(outline-3                          ((t :foreground ,blue-1))) ; 1.15
   `(outline-4                          ((t :foreground ,yellow-2))) ; 1.1
   `(outline-5                          ((t :foreground ,cyan)))
   `(outline-6                          ((t :foreground ,green+2)))
   `(outline-7                          ((t :foreground ,red-4)))
   `(outline-8                          ((t :foreground ,blue-4)))
;;;;; p4
   `(p4-depot-added-face                ((t :inherit diff-added)))
   `(p4-depot-branch-op-face            ((t :inherit diff-changed)))
   `(p4-depot-deleted-face              ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face             ((t :inherit diff-changed)))
   `(p4-diff-change-face                ((t :inherit diff-changed)))
   `(p4-diff-del-face                   ((t :inherit diff-removed)))
   `(p4-diff-file-face                  ((t :inherit diff-file-header)))
   `(p4-diff-head-face                  ((t :inherit diff-header)))
   `(p4-diff-ins-face                   ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face          ((t :foreground ,magenta)))
   `(cperl-array-face                   ((t :foreground ,yellow)))
   `(cperl-hash-face                    ((t :foreground ,yellow-1)))
;;;;; paren-face
   `(parenthesis                        ((t :foreground ,fg-2)))
;;;;; racket-mode
   `(racket-keyword-argument-face       ((t :inherit font-lock-constant-face)))
   `(racket-selfeval-face               ((t :inherit font-lock-type-face)))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face    ((t :foreground ,fg)))
   `(rainbow-delimiters-depth-2-face    ((t :foreground ,green+4)))
   `(rainbow-delimiters-depth-3-face    ((t :foreground ,yellow-2)))
   `(rainbow-delimiters-depth-4-face    ((t :foreground ,cyan)))
   `(rainbow-delimiters-depth-5-face    ((t :foreground ,green+2)))
   `(rainbow-delimiters-depth-6-face    ((t :foreground ,blue+1)))
   `(rainbow-delimiters-depth-7-face    ((t :foreground ,yellow-1)))
   `(rainbow-delimiters-depth-8-face    ((t :foreground ,green+1)))
   `(rainbow-delimiters-depth-9-face    ((t :foreground ,blue-2)))
   `(rainbow-delimiters-depth-10-face   ((t :foreground ,orange)))
   `(rainbow-delimiters-depth-11-face   ((t :foreground ,green)))
   `(rainbow-delimiters-depth-12-face   ((t :foreground ,blue-5)))
;;;;; rcirc
   `(rcirc-my-nick                      ((t :foreground ,blue)))
   `(rcirc-other-nick                   ((t :foreground ,orange)))
   `(rcirc-bright-nick                  ((t :foreground ,blue+1)))
   `(rcirc-dim-nick                     ((t :foreground ,blue-2)))
   `(rcirc-server                       ((t :foreground ,green)))
   `(rcirc-server-prefix                ((t :foreground ,green+1)))
   `(rcirc-timestamp                    ((t :foreground ,green+2)))
   `(rcirc-nick-in-message              ((t :foreground ,yellow)))
   `(rcirc-nick-in-message-full-line    ((t :foreground ,fg+1)))
   `(rcirc-prompt                       ((t :foreground ,yellow)))
   `(rcirc-track-nick                   ((t :inverse-video t)))
   `(rcirc-track-keyword                ((t :foreground ,fg+1)))
   `(rcirc-url                          ((t :foreground ,fg+1)))
   `(rcirc-keyword                      ((t :foreground ,yellow)))
;;;;; re-builder
   `(reb-match-0                        ((t :background ,magenta
                                            :foreground ,bg)))
   `(reb-match-1                        ((t :background ,blue :foreground ,bg)))
   `(reb-match-2                        ((t :background ,orange
                                            :foreground ,bg)))
   `(reb-match-3                        ((t :background ,red :foreground ,bg)))
;;;;; regex-tool
   `(regex-tool-matched-face            ((t :background ,blue-4)))
;;;;; rpm-mode
   `(rpm-spec-dir-face                  ((t :foreground ,green)))
   `(rpm-spec-doc-face                  ((t :foreground ,green)))
   `(rpm-spec-ghost-face                ((t :foreground ,red)))
   `(rpm-spec-macro-face                ((t :foreground ,yellow)))
   `(rpm-spec-obsolete-tag-face         ((t :foreground ,red)))
   `(rpm-spec-package-face              ((t :foreground ,red)))
   `(rpm-spec-section-face              ((t :foreground ,yellow)))
   `(rpm-spec-tag-face                  ((t :foreground ,blue)))
   `(rpm-spec-var-face                  ((t :foreground ,red)))
;;;;; rst-mode
   `(rst-level-1-face                   ((t :foreground ,orange)))
   `(rst-level-2-face                   ((t :foreground ,green+1)))
   `(rst-level-3-face                   ((t :foreground ,blue-1)))
   `(rst-level-4-face                   ((t :foreground ,yellow-2)))
   `(rst-level-5-face                   ((t :foreground ,cyan)))
   `(rst-level-6-face                   ((t :foreground ,green-2)))
;;;;; sh-mode
   `(sh-heredoc                         ((t :foreground ,yellow)))
   `(sh-quoted-exec                     ((t :foreground ,red)))
;;;;; show-paren
   `(show-paren-mismatch                ((t :background ,bg+4
                                            :foreground ,red+1)))
   `(show-paren-match                   ((t :background ,bg+4
                                            :foreground ,fg)))
;;;;; smartparens
   `(sp-show-pair-mismatch-face         ((t :background ,bg+4
                                            :foreground ,red+1)))
   `(sp-show-pair-match-face            ((t :background ,bg+4)))
;;;;; term
   `(term-color-black                   ((t :background ,bg-3 :foreground ,bg)))
   `(term-color-red                     ((t :background ,red-4
                                            :foreground ,red-2)))
   `(term-color-green                   ((t :background ,green-5
                                            :foreground ,green)))
   `(term-color-yellow                  ((t :background ,yellow
                                            :foreground ,orange)))
   `(term-color-blue                    ((t :background ,blue-4
                                            :foreground ,blue-1)))
   `(term-color-magenta                 ((t :background ,red
                                            :foreground ,magenta)))
   `(term-color-cyan                    ((t :background ,blue
                                            :foreground ,cyan)))
   `(term-color-white                   ((t :background ,fg-2 :foreground ,fg)))
   `(term-default-fg-color              ((t :inherit term-color-white)))
   `(term-default-bg-color              ((t :inherit term-color-black)))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t :foreground ,fg+1)))
   `(undo-tree-visualizer-current-face  ((t :foreground ,red-1)))
   `(undo-tree-visualizer-default-face  ((t :foreground ,fg)))
   `(undo-tree-visualizer-register-face ((t :foreground ,yellow)))
   `(undo-tree-visualizer-unmodified-face ((t :foreground ,cyan)))
;;;;; visual-regexp
   `(vr/group-0                         ((t :background ,green :foreground ,bg)))
   `(vr/group-1                         ((t :background ,orange
                                            :foreground ,bg)))
   `(vr/group-2                         ((t :background ,blue :foreground ,bg)))
   `(vr/match-0                         ((t :inherit isearch)))
   `(vr/match-1                         ((t :background ,bg-3
                                            :foreground ,yellow-2)))
   `(vr/match-separator-face            ((t :foreground ,red)))
;;;;; volatile-highlights
   `(vhl/default-face                   ((t :background ,bg-1)))
;;;;; web-mode
   `(web-mode-builtin-face              ((t :inherit ,font-lock-builtin-face)))
   `(web-mode-comment-face              ((t :inherit ,font-lock-comment-face)))
   `(web-mode-constant-face             ((t :inherit ,font-lock-constant-face)))
   `(web-mode-css-at-rule-face          ((t :foreground ,orange )))
   `(web-mode-css-prop-face             ((t :foreground ,orange)))
   `(web-mode-css-pseudo-class-face     ((t :foreground ,green+3)))
   `(web-mode-css-rule-face             ((t :foreground ,blue)))
   `(web-mode-doctype-face              ((t :inherit ,font-lock-comment-face)))
   `(web-mode-folded-face               ((t :underline t)))
   `(web-mode-function-name-face        ((t :foreground ,blue)))
   `(web-mode-html-attr-name-face       ((t :foreground ,orange)))
   `(web-mode-html-attr-value-face      ((t :inherit ,font-lock-string-face)))
   `(web-mode-html-tag-face             ((t :foreground ,cyan)))
   `(web-mode-keyword-face              ((t :inherit ,font-lock-keyword-face)))
   `(web-mode-preprocessor-face         ((t :inherit
                                            ,font-lock-preprocessor-face)))
   `(web-mode-string-face               ((t :inherit ,font-lock-string-face)))
   `(web-mode-type-face                 ((t :inherit ,font-lock-type-face)))
   `(web-mode-variable-name-face        ((t :inherit
                                            ,font-lock-variable-name-face)))
   `(web-mode-server-comment-face       ((t :inherit web-mode-comment-face)))
   `(web-mode-server-string-face        ((t :inherit web-mode-string-face)))
   `(web-mode-symbol-face               ((t :inherit font-lock-constant-face)))
   `(web-mode-warning-face              ((t :inherit font-lock-warning-face)))
   `(web-mode-whitespaces-face          ((t :background ,red)))))

;;; Theme Variables
(zenmelt-with-colors nil
  (custom-theme-set-variables
   'zenmelt
;;;;; ansi-color
   `(ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,bg-1)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,red ,orange ,yellow ,green ,green+4 ,cyan ,blue+1 ,magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,fg . ,bg-1))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,red-1)
       ( 40. . ,red)
       ( 60. . ,orange)
       ( 80. . ,yellow-2)
       (100. . ,yellow-1)
       (120. . ,yellow)
       (140. . ,green-2)
       (160. . ,green)
       (180. . ,green+1)
       (200. . ,green+2)
       (220. . ,green+3)
       (240. . ,green+4)
       (260. . ,cyan)
       (280. . ,blue-2)
       (300. . ,blue-1)
       (320. . ,blue)
       (340. . ,blue+1)
       (360. . ,magenta)))
   `(vc-annotate-very-old-color ,magenta)
   `(vc-annotate-background ,bg-3)))

(defun zenmelt--reset ()
  "Evaluate Zenmelt with the last saved face properties and variable values.
Apply properties and values immediately by disabling
`custom--inhibit-theme-enable' temporarily.

This should be used together with a file-local variable that appends this
function to `after-save-hook'."
  (let ((custom--inhibit-theme-enable nil))
    (eval-buffer)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zenmelt)

;;; zenmelt-theme.el ends here

;; Local Variables:
;; after-save-hook: (zenmelt--reset t)
;; End:
