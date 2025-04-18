;;; use-defaults.el --- Subjectively better defaults -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinitions
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 backup-by-copying t                    ; Backups never overwrite original
 comment-multi-line t                   ; Continue comments when filling
 create-lockfiles nil                   ; Locks are more nuisance than blessing
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 cursor-type '(hbar . 2)                ; Underline-shaped cursor
 custom-file null-device                ; Prevent littering
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 delete-by-moving-to-trash t            ; Delete files to trash
 delete-old-versions t                  ; Delete extra backups silently
 epg-pinentry-mode 'loopback            ; Redirect passphrase prompts to self
 frame-resize-pixelwise t               ; Play nice with tiling window managers
 fill-column 80                         ; Set width for automatic line breaks
 gc-cons-threshold (* 8 1024 1024)      ; We're not using Game Boys anymore
 help-window-select t                   ; Focus new help windows when opened
 indent-tabs-mode nil                   ; Prefer spaces over tabs to indent
 inhibit-startup-screen t               ; Disable start-up screen
 initial-major-mode #'org-mode          ; Prefer `org-mode' for *scratch*
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 isearch-allow-scroll t                 ; Allow scroll commands while isearching
 isearch-lazy-count t                   ; Display match count while searching
 lazy-highlight-buffer t                ; Highlight the entire buffer
 ;; TODO Clean up highlights after replace operations
 lazy-highlight-cleanup nil             ; Keep isearch highlights around
 lazy-highlight-initial-delay 0         ; Remove highlight delay
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 native-comp-async-report-warnings-errors 'silent ; Skip error buffers
 read-process-output-max (* 1024 1024)  ; Increase read size for data chunks
 recenter-positions '(5 bottom)         ; Set re-centering positions
 ring-bell-function 'ignore             ; Silence error bells
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 1                        ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; Use a single space after dots
 show-help-function nil                 ; Disable help text everywhere
 tab-always-indent 'complete            ; Indent first then try completions
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 use-short-answers t                    ; Replace yes/no prompts with y/n
 vc-follow-symlinks t                   ; Never prompt when visiting symlinks
 version-control t                      ; Use numeric versions for backups
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(blink-cursor-mode 0)                   ; Prefer a still cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(repeat-mode 1)                         ; Allow common repeated commands
(put 'downcase-region 'disabled nil)    ; Enable `downcase-region'
(put 'scroll-left 'disabled nil)        ; Enable `scroll-left'
(put 'upcase-region 'disabled nil)      ; Enable `upcase-region'
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(global-unset-key (kbd "<pinch>"))      ; Unbind the `text-scale-pinch' command
(global-unset-key (kbd "C-x C-z"))      ; Unbind the `suspend-frame' command

(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)

;;; use-defaults.el ends here
