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

;; Please copy `.emacs.d/' folder in your home folder, `~/'.
;;
;; Major features:
;;
;; - `zenburn':           smooth palette and pastel feel
;; - `multiple-cursors':  dynamically add new cursors
;; - `projectile':        manage project files
;; - `helm':              improve interactive actions
;; - `company':           auto-completion
;; - `autopair':          automatically add closing parenthesis and others
;; - `org':               note taking made easier
;; - `smart-mode-line':   prettier mode-line
;;
;; If you have any issue using the following code, please refer to the package's README.

;;; Code:


;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))


;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Loaded first so useless UI elements don't make UI flicker
(load "~/.emacs.d/init-interface.el")


;; Initializing package manager with Melpa (Emacs > 24)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))


;; Append path for binaries installed with Homebrew (OS X)
(when (eq system-type 'darwin)
  (setq exec-path (append exec-path '("/usr/local/bin"))))


;; Loading partials into current session
(load "~/.emacs.d/init-aliases.el")
(load "~/.emacs.d/init-modes.el")
(load "~/.emacs.d/init-palettes.el")
(load "~/.emacs.d/init-shortcuts.el")
(load "~/.emacs.d/init-theme.el")


;; Loaded last because of some elements above being required
(load "~/.emacs.d/init-line.el")


;;; init.el ends here
