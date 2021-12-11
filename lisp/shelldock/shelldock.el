;;; shelldock.el --- Hide away your cache files -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: December 11, 2021
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/shelldock

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

;; Small framework to prevent your cache files from littering in your Emacs
;; directory.
;;
;; Other packages with similar intent already exist but I've been using my own
;; for so long -- and it does exactly what I want -- so there's no real point in
;; trying to force another code to do that (yet).

;;; Code:

(defgroup shelldock nil
  "Hide away your cache files."
  :prefix "shelldock-")

(defcustom shelldock-directory (expand-file-name ".cache/" user-emacs-directory)
  "Directory where all cache files should be saved."
  :type 'directory)

(defun shelldock (file)
  "Return the absolute path of FILE under `shelldock-directory'."
  (let* ((directory (file-name-as-directory shelldock-directory))
         (path (convert-standard-filename (concat directory file))))
    (make-directory (file-name-directory path) t)
    path))

(with-eval-after-load 'request
  (setq-default request-storage-directory (shelldock "request/")))

(with-eval-after-load 'tramp
  (setq-default tramp-persistency-file-name (shelldock "tramp.eld")))

(with-eval-after-load 'url
  (setq-default url-configuration-directory (shelldock "url/")))

(provide 'shelldock)

;;; shelldock.el ends here
