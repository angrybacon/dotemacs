;;; init.el --- My Emacs configuration files         -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 24 Oct 2014
;; Keywords: convenience
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

;; To install, clone `dotemacs/' in your home folder `~/.emacs.d/'.
;;
;; If you have any issue using the following code, please refer to the package's README.

;;; Code:


;;─────────────────────────────────────────────────────────────────────────────
;; Bootstrap configuration
;;─────────────────────────────────────────────────────────────────────────────


;; Custom variables
;; FIXME: This should be removed after `mode-line' has been fixed.
(custom-set-variables
 '(custom-safe-themes '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))

;; Check Emacs' version
(when (version<= emacs-version "24")
  (unless (yes-or-no-p (concat "Your Emacs is getting old. Some functionalities may not work, continue ? "))
    (kill-emacs)))

;; Load dependency paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;─────────────────────────────────────────────────────────────────────────────
;; Load partials
;;─────────────────────────────────────────────────────────────────────────────


;; Initialize the core
(require 'init-constants)
(require 'init-interface)
(require 'init-elpa)
(require 'init-osx)
(require 'init-palettes)

;; Initialize the partials
(require 'init-aliases)
(require 'init-css)
(require 'init-emmet)
(require 'init-expand-region)
(require 'init-helm)
(require 'init-multiple-cursors)
(require 'init-projectile)

;; TODO: This needs cleaning.
(require 'init-javascript)
(require 'init-mode-line)
(require 'init-modes)
(require 'init-shortcuts)
(require 'init-theme)
(require 'init-smart-mode-line)


;;─────────────────────────────────────────────────────────────────────────────
;; End of init.el
;;─────────────────────────────────────────────────────────────────────────────


;; TODO: This needs cleaning

;; Company face customization
(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip)
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection))


;; Helm face customization
(with-eval-after-load 'helm
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'helm-header nil :font "Monaco-12")
    (set-face-attribute 'helm-source-header nil :font "Monaco-14"))
  (set-face-attribute 'helm-header nil :italic t)
  (set-face-attribute 'helm-source-header nil :foreground zenburn/blue :background zenburn/bg :box nil)
  (set-face-attribute 'helm-match nil :foreground "gold1"))
(with-eval-after-load 'helm-command
  (set-face-attribute 'helm-M-x-key nil :underline nil))


;; Magit face customization
(with-eval-after-load 'magit
  (when (member "Monaco" (font-family-list)) (set-face-attribute 'magit-section-title nil :font "Monaco-14"))
  (set-face-attribute 'magit-section-title nil :weight 'unspecified :foreground zenburn/blue))


;; Fringe face customization
(with-eval-after-load 'fringe
  (set-face-attribute 'fringe nil :background zenburn/bg :foreground zenburn/bg+2))


;; Whitespace face customization
(with-eval-after-load 'whitespace
  (set-face-attribute 'whitespace-empty nil :background zenburn/bg+1)
  (set-face-attribute 'whitespace-indentation nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-after-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space-before-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-tab nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-trailing nil :background zenburn/bg+1 :foreground zenburn/red)
  (set-face-attribute 'whitespace-space nil :background 'unspecified :foreground zenburn/bg+2))


(provide 'init)
