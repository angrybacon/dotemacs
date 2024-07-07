;;; pendelhaven.el --- Tree-sitter facilities        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: August 26, 2023
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/pendelhaven
;; Package-Requires: ((emacs "30.0.60"))

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

;; Configure the Tree-sitter experience with pre-configured languages with their
;; respective major modes.

;;; Code:

(require 'treesit)
(declare-function cl-destructuring-bind "cl-macs")

(defgroup pendelhaven nil
  "Tree-sitter facilities."
  :group 'tools)

(defcustom pendelhaven-directory nil
  "Custom directory to contain the compiled grammars.
This directory is automatically added to `treesit-extra-load-path'."
  :type 'directory)

;;;###autoload
(defun pendelhaven-configure ()
  "Register language grammars to be used with the built-in major modes."
  (interactive)
  (when pendelhaven-directory
    (add-to-list 'treesit-extra-load-path pendelhaven-directory))
  (pendelhaven-remap-languages))

;;;###autoload
(defun pendelhaven-install ()
  "Install all languages in `pendelhaven-languages'."
  (interactive)
  (dolist (language (pendelhaven-languages))
    (treesit-install-language-grammar language pendelhaven-directory))
  (pendelhaven-remap-languages))

(defun pendelhaven-languages ()
  "Return all configured languages."
  (mapcar #'car treesit-language-source-alist))

(defvar pendelhaven--mode-alist
  '((javascript . (:regular javascript-mode :tree js-ts-mode))
    (json       . (:regular js-json-mode))
    (gomod      . (:tree go-mod-ts-mode))
    (toml       . (:regular conf-toml-mode)))
  "List of major mode pairs for languages where they cannot be guessed easily.
The values should be a plist of overrides containing the Tree-sitter version
and the regular version of the corresponding major mode.")

(defvar pendelhaven--pattern-alist
  `((go         . ,(rx ".go" eos))
    (gomod      . ,(rx "/go.mod" eos))
    (kotlin     . ,(rx ".kt" eos))
    (tsx        . ,(rx ".tsx" eos))
    (typescript . ,(rx ".ts" eos))
    (yaml       . ,(rx ".y" (? "a") "ml" eos)))
  "List of file patterns for languages that don't have a built-in mapping.")

(defun pendelhaven-remap-language (language)
  "Remap the appropriate Tree-sitter major mode for LANGUAGE.
For languages that don't have a built-in non-Tree-sitter major mode, register
them in `auto-mode-alist' directly. See `pendelhaven--pattern-alist'."
  (cl-destructuring-bind (&key regular tree)
      (alist-get language pendelhaven--mode-alist)
    (let ((mode (or regular (intern (format "%s-mode" language))))
          (tree-mode (or tree (intern (format "%s-ts-mode" language)))))
      (if (not (fboundp tree-mode))
          (message "[Pendelhaven] Unknown Tree-sitter mode `%s'" tree-mode)
        (if (not (treesit-ready-p language :quiet))
            (message "[Pendelhaven] Language `%s' isn't ready" language)
          (add-to-list 'major-mode-remap-alist `(,mode . ,tree-mode))
          (when-let ((pattern (alist-get language pendelhaven--pattern-alist)))
            (add-to-list 'auto-mode-alist `(,pattern . ,tree-mode))))))))

(defun pendelhaven-remap-languages ()
  "Remap Tree-sitter major mode for all languages in `pendelhaven-languages'."
  (mapc #'pendelhaven-remap-language (pendelhaven-languages)))

(provide 'pendelhaven)

;;; pendelhaven.el ends here
