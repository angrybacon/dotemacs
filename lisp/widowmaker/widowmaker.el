;;; widowmaker.el --- Manage windows -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: December 19, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/widowmaker
;; Package-Requires: ((emacs "29.1"))

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; Collection of window management features such as window navigation but also
;; pop-up management.

;;; Code:

(require 'menu-bar)
(require 'project)
(require 'window)
(require 'windmove)
(require 'winner)

(defgroup widowmaker nil
  "Manage windows."
  :group 'convenience
  :prefix "widowmaker-")

(provide 'widowmaker)

;;; widowmaker.el ends here
