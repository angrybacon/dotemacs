;;; pendelhaven.el --- Tree-sitter facilities        -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: August 26, 2023
;; Homepage: https://github.com/angrybacon/dotemacs/tree/master/lisp/pendelhaven
;; Package-Requires: ((emacs "29.0.60"))

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
;; (declare-function treesit-ready-p "treesit")

(defgroup pendelhaven nil
  "Tree-sitter facilities."
  :group 'tools)

(defcustom pendelhaven-directory nil
  "Custom directory to contain the compiled grammars.
This directory is automatically added to `treesit-extra-load-path'."
  :type 'directory)

(defun pendelhaven-configure ()
  "Register language grammars to be used with the built-in major modes.
Return `treesit-language-source-alist' once it is populated."
  (when pendelhaven-directory
    (add-to-list 'treesit-extra-load-path pendelhaven-directory))
  (setq
   treesit-language-source-alist
   '((css        . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
     (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
     (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
     (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src"))
     (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  (mapc #'pendelhaven-remap (pendelhaven-languages)))

;;;###autoload
(defun pendelhaven-install ()
  "Install all languages in `pendelhaven-languages'."
  (interactive)
  (let ((languages (pendelhaven-languages)))
    (advice-add #'treesit--install-language-grammar-1 :filter-args
      (lambda (rest) (append `(,pendelhaven-directory) (cdr rest)))
      '((name . pendelhaven--force-directory)))
    (unwind-protect
        (mapc #'treesit-install-language-grammar languages)
      (advice-remove
       #'treesit--install-language-grammar-1 'pendelhaven--force-directory))
    (mapc #'pendelhaven-remap languages)))

(defun pendelhaven-languages ()
  "Return all configured languages."
  (mapcar #'car treesit-language-source-alist))

(defvar pendelhaven--mode-alist
  '((javascript . (:regular javascript-mode :tree js-ts-mode))
    (json       . (:regular js-json-mode))
    (gomod      . (:tree go-mod-ts-mode))
    (toml       . (:regular conf-toml-mode)))
  "List of major mode pairs for languages where they cannot be guessed easily.
The values should be a plist of overwrites containing the Tree-sitter version
and the regular version of the corresponding major mode, both optional.")

(defvar pendelhaven--pattern-alist
  `((go         . ,(rx ".go" eos))
    (gomod      . ,(rx "/go.mod" eos))
    (tsx        . ,(rx ".tsx" eos))
    (typescript . ,(rx ".ts" eos))
    (yaml       . ,(rx ".yaml" eos)))
  "List of file patterns for languages that don't have a built-in mode.")

(defun pendelhaven-remap (language)
  "Remap Tree-sitter major mode for LANGUAGE.
For languages that don't have a built-in non-Tree-sitter major mode, register
them in `auto-mode-alist' directly. See `pendelhaven--pattern-alist'."
  (interactive)
  (cl-destructuring-bind (&key regular tree)
      (alist-get language pendelhaven--mode-alist)
    (let ((mode (or regular (intern (format "%s-mode" language))))
          (ts-mode (or tree (intern (format "%s-ts-mode" language)))))
      (if (not (fboundp ts-mode))
          (message "[Pendelhaven] Unknown mode `%s'" ts-mode)
        (if (not (treesit-ready-p language :quiet))
            (message "[Pendelhaven] Language `%s' isn't ready" language)
          (add-to-list 'major-mode-remap-alist `(,mode . ,ts-mode))
          (when-let ((pattern (alist-get language pendelhaven--pattern-alist)))
            (add-to-list 'auto-mode-alist `(,pattern . ,ts-mode))))))))

(provide 'pendelhaven)

;;; pendelhaven.el ends here
