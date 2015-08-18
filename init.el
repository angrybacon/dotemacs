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
;; Following lines load several packages to configure my Emacs experience. I
;; work quite often with Python, HTML, SCSS and JavaScript code. My
;; configuration is stronlgy Web-oriented.
;;
;; Custom key binding conventions:
;; - C-c h: `helm'
;; - C-c g: `magit'
;; - C-c o: `org'
;; - C-c p: `projectile'
;;
;; See readme.md for more details.

;;; Code:


;;=============================================================================
;; Bootstrap configuration
;;=============================================================================


;; Custom variables
(custom-set-variables
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))


;; Check Emacs' version
(when (version<= emacs-version "24")
  (unless (yes-or-no-p (concat "Your Emacs is getting old. Some functionalities may not work, continue? "))
    (kill-emacs)))


;;=============================================================================
;; Load partials
;;=============================================================================


;; Load dependency paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Initialize the core configuration
(require 'init-constants)
(require 'init-interface)
(require 'init-elpa)
(require 'init-palettes)
(require 'init-theme)


;; Initialize the partials
(require 'init-aliases)
(require 'init-comments)
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
(require 'init-json)
(require 'init-markdown)
(require 'init-mode-line)
(require 'init-org)
(require 'init-osx)
(require 'init-parentheses)
(require 'init-projectile)
(require 'init-python)
(require 'init-semantic)
(require 'init-shell)
(require 'init-whitespace)
(require 'init-yaml)


(provide 'init)
;;; init.el ends here
