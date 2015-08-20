;;; init-interface.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Keywords: convenience, faces
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Enable or disable several interface components.

;;; Code:


;;=============================================================================
;; Silence byte-compiler
;;=============================================================================


(defvar me/initial-buffer)


;;=============================================================================
;; Configure interface components
;;=============================================================================


;; Global default settings
(setq-default
 battery-mode-line-format "%p"                   ; Format the battery level string
 display-time-default-load-average nil           ; Hide the time load
 display-time-format "%H:%M"                     ; Format the time string
 indent-tabs-mode nil                            ; Stop using tabs to indent
 indicate-empty-lines t                          ; Indicate empty line in the fringe
 show-trailing-whitespace t)                     ; Display trailing whitespaces


;; Buffer local settings
(setq
 ad-redefinition-action 'accept                  ; Turn off the warnings due to functions being redefined
 inhibit-startup-screen t                        ; Remove start-up screen
 initial-buffer-choice me/initial-buffer         ; Open specified file on start-up
 mouse-yank-at-point t                           ; Yank at mouse cursor rather than click location
 scroll-step 1                                   ; Line by line scrolling
 ;; sgml-basic-offset 4                             ; Set indent to 4 spaces
 tab-width 4                                     ; Set width for tabs
 x-select-enable-clipboard t)                    ; Merge both system's and Emacs' clipboard


;; Toggle interface elements
(column-number-mode nil)                         ; Hide column number of the cursor current position
(display-battery-mode t)                         ; Display battery level in the mode-line
(display-time-mode t)                            ; Enable the time representation
(fringe-mode '(12 . 0))                          ; Display left fringe
(global-hl-line-mode t)                          ; Hightlight current line
(global-whitespace-mode 0)                       ; Hightlight blank characters
(line-number-mode t)                             ; Display line number of the cursor current position
(menu-bar-mode nil)                              ; Disable menu bar
(scroll-bar-mode -1)                             ; Disable scroll bar
(tool-bar-mode -1)                               ; Disable the toolbar


;; Goodies
(global-subword-mode t)                          ; Iterate through CamelCase words
(mouse-avoidance-mode 'animate)                  ; Move pointer when point reaches cursor location


;; FIXME: This messes up on Yamamoto Mitsuharu's build
;; (set-frame-parameter nil 'fullscreen 'fullboth)  ; Pseudo fullscreen (only tested with OS X)


;;=============================================================================
;; Define helpers
;;=============================================================================


(add-hook 'emacs-lisp-mode-hook 'me/highlight-hex-strings)
(add-hook 'emmet-mode-hook 'me/highlight-hex-strings)
(add-hook 'json-mode-hook 'me/highlight-hex-strings)
(add-hook 'markdown-mode-hook 'me/highlight-hex-strings)
(add-hook 'python-mode-hook 'me/highlight-hex-strings)


(defun me/list-directories-first ()
  "Sort by directory first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))


(defadvice dired-readin (after dired-after-updating-hook first () activate)
  "List directories first in `dired-mode'."
  (me/list-directories-first))


(defun me/highlight-hex-strings ()
  "Find and highlight hexadecimal color strings with a colored background."
  (interactive)
  (font-lock-add-keywords nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))


(provide 'init-interface)
;;; init-interface.el ends here
