;;; zenmelt-theme.el --- A Zenburn clone -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mathieu Marques

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

(defgroup zenmelt nil
  "A Zenburn clone."
  :group 'faces)

(deftheme zenmelt "The Zenmelt color theme.")

;;;; Palette

(defconst zenmelt-colors-alist
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

;;;; Boxes

(defcustom zenmelt-box-colors-alist
  (zenmelt-with-colors nil
    `((blue    . (,blue-4  . ,blue+1))
      (cyan    . (,blue-5  . ,blue))
      (cyan+   . (,blue    . ,blue-5))
      (default . (,bg+1    . ,bg+4))
      (green   . (,green-3 . ,green+2))
      (red     . (,red-3   . ,red+2))
      (yellow  . (,fg-2    . ,yellow-1))))
  "List color tuples for boxes.
The car gives the background color while the cdr gives the border.
The cdr is also used for the text foreground color."
  :type '(alist :key-type symbol :value-type (cons (list color variable)
                                                   (list color variable))))

(defun zenmelt--box (&optional color)
  "Return an plist containing properties for a colored box.
When the optional COLOR is not provided, use `'default' by default which yields
a gray box. It should return a valid plist to be used in the specifications
provided to `custom-theme-set-faces'.

See `zenmelt-box-colors-alist' for a complete list of available colors."
  (pcase-let* ((color (or color 'default))
               (`(,b . ,f) (alist-get color zenmelt-box-colors-alist)))
    `(:background ,b :box (:color ,f :line-width (-1 . -1)) :foreground ,f)))

;;;; Theme Faces

(zenmelt-with-colors nil
  (custom-theme-set-faces
   'zenmelt
;;;;; Built-in
;;;;;; Base
   `(bold                               ((t :weight bold)))
   `(bold-italic                        ((t :inherit (bold italic))))
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
   `(font-lock-negation-char-face       ((t :foreground ,magenta)))
   `(font-lock-preprocessor-face        ((t :foreground ,blue+1)))
   `(font-lock-regexp-grouping-backslash ((t :foreground ,green)))
   `(font-lock-regexp-grouping-construct ((t :foreground ,yellow)))
   `(font-lock-string-face              ((t :foreground ,red)))
   `(font-lock-type-face                ((t :foreground ,blue-2)))
   `(font-lock-variable-name-face       ((t :foreground ,orange)))
   `(font-lock-warning-face             ((t :inherit warning)))
   `(highlight                          ((t :background ,blue-5
                                            :foreground ,blue)))
   `(italic                             ((t :slant italic)))
   `(shadow                             ((t :foreground ,fg-1)))
   `(success                            ((t :foreground ,green)))
   `(warning                            ((t :foreground ,orange)))
;;;;;; Applications
   `(eww-invalid-certificate            ((t :inherit error)))
   `(eww-valid-certificate              ((t :inherit success)))
;;;;;; Compilation
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
;;;;;; Customize
   `(custom-button                      ((t :inherit button)))
   `(custom-button-mouse                ((t :foreground ,yellow
                                            :inherit custom-button)))
   `(custom-button-pressed              ((t :foreground ,fg-1
                                            :inherit custom-button-mouse)))
   `(custom-button-pressed-unraised     ((t :inherit custom-button-pressed)))
   `(custom-button-unraised             ((t :inherit custom-button-pressed)))
   `(custom-group-tag                   ((t :foreground ,blue)))
   `(custom-state                       ((t :foreground ,green+4)))
   `(custom-variable-tag                ((t :foreground ,blue)))
   `(custom-visibility                  ((t :inherit link)))
;;;;;; Dired
   `(dired-broken-symlink               ((t :background ,red-4
                                            :foreground ,fg)))
;;;;;; Help
   `(Info-quoted                        ((t :inherit font-lock-constant-face)))
   `(eldoc-box-body                     ((t :inherit tooltip)))
   `(eldoc-box-border                   ((t :background ,red)))
   `(eldoc-highlight-function-argument  ((t :inherit highlight)))
;;;;;; Interface
   `(button                             ((t ,@(zenmelt--box 'yellow))))
   `(cursor                             ((t :background ,fg+1 :foreground ,fg)))
   `(fringe                             ((t :foreground ,fg)))
   `(header-line                        ((t :foreground ,cyan)))
   `(help-key-binding                   ((t ,@(zenmelt--box)
                                            :foreground ,orange)))
   `(hl-line                            ((t :background ,bg+2)))
   `(line-number                        ((t :inherit shadow)))
   `(line-number-current-line           ((t :foreground ,yellow-2
                                            :inherit hl-line)))
   `(link                               ((t :foreground ,yellow :underline t)))
   `(link-visited                       ((t :foreground ,yellow-2 :underline t)))
   `(menu                               ((t :inherit default)))
   `(minibuffer-prompt                  ((t :foreground ,yellow)))
   `(mode-line                          ((t :background ,bg-3
                                            :foreground ,green)))
   `(mode-line-buffer-id                ((t :foreground ,green+4)))
   `(mode-line-emphasis                 ((t :inherit mode-line)))
   `(mode-line-highlight                ((t :box (-2 . -2) :inherit highlight)))
   `(mode-line-inactive                 ((t :background ,bg-1 :inherit shadow)))
   `(region                             ((,class :background ,blue-5)
                                         (t :inverse-video t)))
   `(secondary-selection                ((t :background ,bg+3)))
   `(tooltip                            ((t :background ,bg-3)))
   `(trailing-whitespace                ((t :background ,red)))
   `(vertical-border                    ((t :background ,bg+1)))
   `(widget-button                      ((t :inherit button)))
   `(widget-button-pressed              ((t :inherit widget-button)))
   `(widget-documentation               ((t :inherit font-lock-doc-face)))
   `(widget-field                       ((t :background ,bg+4 :foreground ,fg+2)))
   `(widget-inactive                    ((t :background ,bg+2 :foreground ,bg+3)))
   `(widget-single-line-field           ((t :inherit widget-field)))
   `(window-divider                     ((t :inherit vertical-border
                                            :inverse-video t)))
   `(window-divider-first-pixel         ((t :inherit window-divider)))
   `(window-divider-last-pixel          ((t :inherit window-divider)))
;;;;;; Language
   `(flymake-error                      ((t :background ,red-5
                                            :foreground ,red+1
                                            :underline t)))
   `(flymake-note                       ((t :background ,blue-5
                                            :foreground ,blue+1
                                            :underline t)))
   `(flymake-warning                    ((t :background ,fg-2
                                            :foreground ,orange
                                            :underline t)))
;;;;;; Org
   `(org-agenda-date-today              ((t :foreground ,fg+1 :slant italic)))
   `(org-agenda-structure               ((t :inherit font-lock-comment-face)))
   `(org-archived                       ((t :foreground ,fg)))
   `(org-block                          ((t :extend t :foreground ,fg)))
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
                                            :extend t
                                            :inherit outline-1)))
   `(org-level-2                        ((t :inherit (outline-2 org-level-1))))
   `(org-level-3                        ((t :inherit (outline-3 org-level-2))))
   `(org-level-4                        ((t :inherit (outline-4 org-level-3))))
   `(org-level-5                        ((t :inherit (outline-5 org-level-4))))
   `(org-level-6                        ((t :inherit (outline-6 org-level-5))))
   `(org-level-7                        ((t :inherit (outline-7 org-level-6))))
   `(org-level-8                        ((t :inherit (outline-8 org-level-7))))
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
;;;;;; Outline
   `(outline-1                          ((t :foreground ,orange)))
   `(outline-2                          ((t :foreground ,cyan)))
   `(outline-3                          ((t :foreground ,yellow)))
   `(outline-4                          ((t :foreground ,blue-1)))
   `(outline-5                          ((t :foreground ,yellow-2)))
   `(outline-6                          ((t :foreground ,blue-1)))
   `(outline-7                          ((t :foreground ,green+2)))
   `(outline-8                          ((t :foreground ,fg-1)))
;;;;;; Pulse
   `(pulse-highlight-start-face         ((t :background ,blue-3)))
;;;;;; Search
   `(grep-context-face                  ((t :foreground ,fg)))
   `(grep-error-face                    ((t :foreground ,red-1 :underline t)))
   `(grep-hit-face                      ((t :foreground ,blue)))
   `(grep-match-face                    ((t :foreground ,orange)))
   `(isearch                            ((t ,@(zenmelt--box 'cyan+))))
   `(isearch-fail                       ((t :foreground ,red)))
   `(lazy-highlight                     ((t ,@(zenmelt--box 'cyan))))
   `(match                              ((t ,@(zenmelt--box 'yellow))))
   `(reb-match-0                        ((t :background ,blue-5)))
   `(reb-match-1                        ((t ,@(zenmelt--box 'yellow))))
   `(reb-match-2                        ((t ,@(zenmelt--box 'green))))
   `(reb-match-3                        ((t ,@(zenmelt--box 'red))))
;;;;;; Syntax
   `(whitespace-empty                   ((t :background ,yellow-2 :extend t)))
   `(whitespace-hspace                  ((t :inherit whitespace-space)))
   `(whitespace-indentation             ((t :background ,bg+1 :foreground ,red)))
   `(whitespace-line                    ((t :foreground ,magenta)))
   `(whitespace-newline                 ((t :foreground ,bg+2)))
   `(whitespace-space                   ((t :inherit shadow :inverse-video t)))
   `(whitespace-space-after-tab         ((t :inherit whitespace-space)))
   `(whitespace-space-before-tab        ((t :inherit whitespace-space-after-tab)))
   `(whitespace-tab                     ((t :background ,red-1)))
   `(whitespace-trailing                ((t :inherit trailing-whitespace)))
;;;;;; Terminal
   `(ansi-color-black                   ((t :background ,bg+4 :foreground ,bg)))
   `(ansi-color-blue                    ((t :background ,blue+2
                                            :foreground ,blue)))
   `(ansi-color-cyan                    ((t :background ,cyan
                                            :foreground ,cyan)))
   `(ansi-color-green                   ((t :background ,green+2
                                            :foreground ,green)))
   `(ansi-color-magenta                 ((t :background ,red+2
                                            :foreground ,magenta)))
   `(ansi-color-red                     ((t :background ,red :foreground ,red)))
   `(ansi-color-white                   ((t :background ,fg :foreground ,fg)))
   `(ansi-color-yellow                  ((t :background ,yellow-1
                                            :foreground ,yellow-1)))
   `(ansi-color-bright-black            ((t :background ,fg-2
                                            :foreground ,fg-2)))
   `(ansi-color-bright-blue             ((t :background ,blue+1
                                            :foreground ,blue+1)))
   `(ansi-color-bright-cyan             ((t :background ,blue+3
                                            :foreground ,blue+3)))
   `(ansi-color-bright-green            ((t :background ,green+2
                                            :foreground ,green+2)))
   `(ansi-color-bright-magenta          ((t :background ,magenta
                                            :foreground ,magenta)))
   `(ansi-color-bright-red              ((t :background ,red+2
                                            :foreground ,red+2)))
   `(ansi-color-bright-white            ((t :background ,fg+2
                                            :foreground ,fg+2)))
   `(ansi-color-bright-yellow           ((t :background ,yellow
                                            :foreground ,yellow)))
;;;;;; Version Control
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
   `(diff-removed                       ((t :foreground ,red-1)))
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
   `(smerge-lower                       ((t :background ,bg+1
                                            :inherit diff-added)))
   `(smerge-markers                     ((t :background ,bg+1 :inherit shadow)))
   `(smerge-refined-added               ((t :inherit diff-refine-added)))
   `(smerge-refined-removed             ((t :inherit diff-refine-removed)))
   `(smerge-upper                       ((t :background ,bg+1
                                            :inherit diff-removed)))
;;;;; Third-party
;;;;;; Completion
   `(completions-annotations            ((t :inherit shadow)))
   `(completions-common-part            ((t :inherit match)))
   `(completions-first-difference       ((t :weight normal)))
   `(completions-group-title            ((t :inherit shadow :slant italic)))
   `(consult-preview-line               ((t :extend t :inherit highlight)))
   `(consult-preview-match              ((t :background nil)))
   `(corfu-current                      ((t :inherit 'highlight)))
   `(corfu-default                      ((t :background ,bg-3)))
   `(orderless-match-face-0             ((t ,@(zenmelt--box 'yellow))))
   `(orderless-match-face-1             ((t ,@(zenmelt--box 'green))))
   `(orderless-match-face-2             ((t ,@(zenmelt--box 'red))))
   `(orderless-match-face-3             ((t ,@(zenmelt--box 'blue))))
;;;;;; Help
   `(helpful-heading                    ((t :foreground ,cyan
                                            :height 2.0
                                            :inherit variable-pitch)))
;;;;;; Language Servers
   `(eglot-highlight-symbol-face        ((t ,@(zenmelt--box 'yellow))))
;;;;;; Mode-Line
   `(doom-modeline-bar                  ((t :inherit mode-line)))
   `(doom-modeline-bar-inactive         ((t :inherit mode-line-inactive)))
   `(doom-modeline-evil-emacs-state     ((t :foreground ,magenta)))
   `(doom-modeline-evil-insert-state    ((t :foreground ,blue)))
   `(doom-modeline-info                 ((t :foreground ,green+4)))
   `(doom-modeline-project-dir          ((t :inherit dired-directory)))
   `(doom-modeline-project-parent-dir   ((t :inherit shadow)))
;;;;;; Navigation
   `(avy-background-face                ((t :inherit shadow)))
   `(avy-lead-face                      ((t :foreground ,cyan)))
   `(avy-lead-face-0                    ((t :foreground ,green+4)))
   `(avy-lead-face-1                    ((t :foreground ,magenta)))
   `(avy-lead-face-2                    ((t :foreground ,yellow)))
;;;;;; Popup
   `(hydra-face-amaranth                ((t :foreground ,red-2)))
   `(hydra-face-blue                    ((t :foreground ,blue+1)))
   `(hydra-face-pink                    ((t :foreground ,magenta)))
   `(hydra-face-red                     ((t :foreground ,red+1)))
   `(hydra-face-teal                    ((t :foreground ,cyan)))
   `(popper-echo-area                   ((t :inherit match)))
   `(popper-echo-dispatch-hint          ((t :foreground ,orange)))
   `(transient-amaranth                 ((t :foreground ,red-2)))
   `(transient-blue                     ((t :foreground ,blue+1)))
   `(transient-disabled-suffix          ((t :foreground ,red)))
   `(transient-enabled-suffix           ((t :foreground ,green)))
   `(transient-pink                     ((t :foreground ,magenta)))
   `(transient-red                      ((t :foreground ,red+1)))
   `(transient-separator                ((t :foreground ,fg-2)))
   `(transient-teal                     ((t :foreground ,cyan)))
;;;;;; Replace
   `(anzu-match-1                       ((t ,@(zenmelt--box 'green))))
   `(anzu-match-2                       ((t ,@(zenmelt--box 'red))))
   `(anzu-match-3                       ((t ,@(zenmelt--box 'blue))))
   `(anzu-mode-line                     ((t :foreground ,magenta)))
   `(anzu-mode-line-no-match            ((t :foreground ,red)))
   `(anzu-replace-highlight             ((t ,@(zenmelt--box 'yellow))))
   `(anzu-replace-to                    ((t ,@(zenmelt--box 'green))))
;;;;;; Version Control
   `(git-commit-comment-action          ((t :inherit font-lock-comment-face)))
   `(git-commit-comment-branch-local    ((t :inherit magit-branch-local)))
   `(git-commit-comment-branch-remote   ((t :inherit magit-branch-remote)))
   `(git-gutter:added                   ((t :foreground ,green+4)))
   `(git-gutter:deleted                 ((t :foreground ,red+2)))
   `(git-gutter:modified                ((t :foreground ,blue)))
   `(git-gutter:unchanged               ((t :inherit shadow )))
   `(git-gutter-fr:added                ((t :foreground ,green)))
   `(git-gutter-fr:deleted              ((t :foreground ,red)))
   `(git-gutter-fr:modified             ((t :foreground ,blue-1)))
   `(magit-bisect-bad                   ((t :foreground ,red)))
   `(magit-bisect-good                  ((t :foreground ,green)))
   `(magit-bisect-skip                  ((t :foreground ,yellow)))
   `(magit-blame-date                   ((t :foreground ,fg)))
   `(magit-blame-hash                   ((t :foreground ,blue)))
   `(magit-blame-heading                ((t :background ,bg-3 :inherit shadow)))
   `(magit-blame-name                   ((t :foreground ,orange)))
   `(magit-branch-current               ((t :box (-1 . -1)
                                            :inherit magit-branch-local)))
   `(magit-branch-local                 ((t :foreground ,blue)))
   `(magit-branch-remote                ((t :foreground ,green+4)))
   `(magit-branch-remote-head           ((t :box (-1 . -1)
                                            :inherit magit-branch-remote)))
   `(magit-cherry-equivalent            ((t :foreground ,magenta)))
   `(magit-cherry-unmatched             ((t :foreground ,cyan)))
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
   `(magit-dimmed                       ((t :inherit shadow)))
   `(magit-hash                         ((t :inherit magit-dimmed)))
   `(magit-log-author                   ((t :foreground ,orange)))
   `(magit-log-date                     ((t :inherit magit-dimmed)))
   `(magit-log-graph                    ((t :foreground ,yellow)))
   `(magit-process-ng                   ((t :foreground ,red)))
   `(magit-process-ok                   ((t :foreground ,green)))
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
   `(magit-section-heading              ((t :foreground ,cyan)))
   `(magit-section-heading-selection    ((t :foreground ,orange)))
   `(magit-section-highlight            ((t :inherit hl-line)))
   `(magit-sequence-done                ((t :inherit magit-dimmed)))
   `(magit-sequence-drop                ((t :foreground ,red)))
   `(magit-sequence-head                ((t :foreground ,blue)))
   `(magit-sequence-onto                ((t :inherit magit-dimmed)))
   `(magit-sequence-part                ((t :foreground ,yellow)))
   `(magit-sequence-pick                ((t :foreground ,yellow-2)))
   `(magit-sequence-stop                ((t :foreground ,green+4)))
   `(magit-signature-bad                ((t :foreground ,red)))
   `(magit-signature-error              ((t :foreground ,red-1)))
   `(magit-signature-expired            ((t :foreground ,orange)))
   `(magit-signature-good               ((t :foreground ,green)))
   `(magit-signature-revoked            ((t :foreground ,magenta)))
   `(magit-signature-untrusted          ((t :foreground ,yellow)))
   `(magit-tag                          ((t :foreground ,orange)))
;;;;;; Markdown
   `(markdown-inline-code-face          ((t :inherit org-verbatim)))
   `(markdown-table-face                ((t :height .9
                                            :inherit markdown-code-face)))
;;;;;; Pairs
   `(rainbow-delimiters-depth-1-face    ((t :foreground ,fg)))
   `(rainbow-delimiters-depth-2-face    ((t :foreground ,blue-1)))
   `(rainbow-delimiters-depth-3-face    ((t :foreground ,yellow-2)))
   `(rainbow-delimiters-depth-4-face    ((t :foreground ,green+1)))
   `(rainbow-delimiters-depth-5-face    ((t :foreground ,orange)))
   `(rainbow-delimiters-depth-6-face    ((t :foreground ,cyan)))
   `(rainbow-delimiters-depth-7-face    ((t :foreground ,yellow)))
   `(rainbow-delimiters-depth-8-face    ((t :foreground ,green+4)))
   `(rainbow-delimiters-depth-9-face    ((t :foreground ,red+2)))))

;;;; Helpers

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
