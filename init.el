;;; init.el --- My Emacs configuration

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 Oct 2014
;; Keywords: abbrev, convenience, faces, maint, outlines, vc
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To install, clone dotemacs/ into ~/.emacs.d/.
;;
;; Following lines load several packages to configure my Emacs experience.
;; I work quite often with Python, HTML, SCSS and JavaScript code.
;;
;; See readme.md for more details.

;;; Code:


;;=============================================================================
;; Bootstrap configuration
;;=============================================================================


;; Better default
(setq-default
 ad-redefinition-action 'accept                  ; Silence warnings for redefined functions
 confirm-kill-emacs 'y-or-n-p                    ; Confirm before exiting Emacs
 delete-by-moving-to-trash t                     ; Delete files to trash
 display-time-default-load-average nil           ; Don't display load average
 display-time-format "%H:%M"                     ; Format the time string
 gc-cons-threshold 100000000                     ; Higher garbage collector treshold
 indent-tabs-mode nil                            ; Stop using tabs to indent
 indicate-empty-lines t                          ; Indicate unused lines in the fringe
 inhibit-startup-screen t                        ; Remove start-up screen
 initial-scratch-message ""                      ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                           ; Yank at point rather than cursor
 require-final-newline 'visit                    ; Add a newline at EOF on visit
 scroll-step 1                                   ; Line by line scrolling
 show-trailing-whitespace nil                    ; Display trailing whitespaces
 tab-width 4                                     ; Set width for tabs
 x-select-enable-clipboard t)                    ; Merge system's and Emacs' clipboard

;; Toggle UI components
(tool-bar-mode 0)                                ; Disable/enable the toolbar
(menu-bar-mode 0)                                ; Disable/enable menu bar
(scroll-bar-mode 0)                              ; Disable/enable scroll bar
(fringe-mode '(10 . 10))                         ; Disable/enable vertical fringes
(display-battery-mode 0)                         ; Hide/show battery level
(display-time-mode 1)                            ; Hide/Show time representation
(column-number-mode 0)                           ; Hide/show column number
(line-number-mode 1)                             ; Hide/show line number
(global-hl-line-mode 1)                          ; Hightlight current line
(set-frame-parameter nil 'fullscreen 'fullboth)  ; Enable pseudo fullscreen


;;=============================================================================
;; Check Emacs' version
;;=============================================================================


(when (version<= emacs-version "24")
  (unless (yes-or-no-p (concat "Your Emacs is getting old. Features may not work, continue? "))
    (kill-emacs)))


;;=============================================================================
;; Benchmark initialization
;;=============================================================================


(let ((benchmark-init.el "~/.emacs.d/elpa/benchmark-init-20150905.238/benchmark-init.el"))
  (when (file-exists-p benchmark-init.el)
    (load benchmark-init.el)))


;;=============================================================================
;; Load partials
;;=============================================================================


;; Load path of dependencies
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Load dependencies
(let ((file-name-handler-alist nil))

  ;; Initialize the core configuration
  (require 'init-constants)
  (require 'init-elpa)
  (require 'init-interface)
  (require 'init-palettes)
  (require 'init-theme)

  ;; Initialize the partials
  (require 'init-aliases)
  (require 'init-company)
  (require 'init-css)
  (require 'init-cursor)
  (require 'init-dired)
  (require 'init-docker)
  (require 'init-emmet)
  (require 'init-flycheck)
  (require 'init-git)
  (require 'init-golden-ratio)
  (require 'init-helm)
  (require 'init-html)
  (require 'init-javascript)
  (require 'init-lisp)
  (require 'init-markdown)
  (require 'init-mode-line)
  (require 'init-org)
  (require 'init-osx)
  (require 'init-parentheses)
  (require 'init-projectile)
  (require 'init-python)
  (require 'init-whitespace)
  (require 'init-yaml)
  ;; (require 'init-yasnippet)
  )


(provide 'init)
;;; init.el ends here
