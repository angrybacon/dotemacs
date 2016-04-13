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
;; See README.md for more details.

;;; Code:

;; Mark `me/project-name' as safe
(defvar me/project-name nil)
(put 'me/project-name 'safe-local-variable #'stringp)

;; Tangle configuration
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
;;; init.el ends here
