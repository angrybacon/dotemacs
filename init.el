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

;; Copy this file and its parent folder in your home.
;; A list of installed features:
;;
;;   - Multiple cursors
;;   - Expand region
;;   - Company (with Anaconda support)
;;   - Emmet
;;   - Indent guide
;;   - SCSS mode
;;   - Projectile
;;   - Helm
;;
;; Plus some neat improvements such as colorful highlight of hex color strings
;; and a cleaner mode-line.

;;; Code:


;; Initializing package manager with Melpa (Emacs > 24)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; Append path for binaries installed with Homebrew (Mac only)
(when (eq system-type 'darwin)
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; Loaded first so useless UI elements don't flicker
(load "~/.emacs.d/init-interface.el")


;; Loading configuration partials into current session
(load "~/.emacs.d/init-aliases.el")
(load "~/.emacs.d/init-modes.el")
(load "~/.emacs.d/init-shortcuts.el")
(load "~/.emacs.d/init-syntax.el")
(load "~/.emacs.d/init-theme.el")


;; Loaded last because of some elements being loaded above
(load "~/.emacs.d/init-mode-line.el")


;;; init.el ends here
