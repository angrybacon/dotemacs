;;; init.el --- My Emacs configuration files.        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Mathieu Marques

;; Author: Mathieu Marques <angrybacon@sandman.local>
;; Keywords: convenience, faces, lisp, tools

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

;; To install, clone `dotemacs/' into `~/.emacs.d/'.
;;
;; Following lines load several packages to configure my Emacs experience. I
;; work quite often with Python, HTML, SCSS and JavaScript code. My
;; configuration is stronlgy Web-oriented.
;;
;; See `readme.md' for more details.

;;; Code:


;;─────────────────────────────────────────────────────────────────────────────
;; Bootstrap configuration
;;─────────────────────────────────────────────────────────────────────────────


;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))


;; Check Emacs' version
(when (version<= emacs-version "24")
  (unless (yes-or-no-p (concat "Your Emacs is getting old. Some functionalities may not work, continue ? "))
    (kill-emacs)))


;; Load dependency paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;─────────────────────────────────────────────────────────────────────────────
;; Load partials
;;─────────────────────────────────────────────────────────────────────────────


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
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-git)
(require 'init-golden-ratio)
(require 'init-helm)
(require 'init-html)
(require 'init-javascript)
(require 'init-json)
(require 'init-markdown)
(require 'init-mode-line)
(require 'init-multiple-cursors)
(require 'init-org)
(require 'init-osx)
(require 'init-parentheses)
(require 'init-projectile)
(require 'init-python)
(require 'init-semantic)
(require 'init-shell)
(require 'init-utilities)
(require 'init-whitespace)


;;─────────────────────────────────────────────────────────────────────────────
;; End of init.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init)


;;; init.el ends here
