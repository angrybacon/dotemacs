;;; init.el --- My Emacs configuration

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 Oct 2014
;; Keywords: abbrev, convenience, faces, maint, outlines, vc
;; Homepage: https://github.com/angrybacon/dotemacs

;; This program is free software. It comes without any warranty, to the extent
;; permitted by applicable law. You can redistribute it and/or modify it under the
;; terms of the Do What The Fuck You Want To Public License, Version 2, as published
;; by Sam Hocevar. See http://www.wtfpl.net/ for more details.

;;; Commentary:

;; To install, clone dotemacs/ into ~/.emacs.d/.
;;
;; Following lines load an Org file and build the configuration code out of it. This
;; process is known as tangling.
;; Using this configuration on a daily basis, I work quite often with Python, HTML,
;; SCSS and JavaScript code.
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
