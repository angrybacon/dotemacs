;;; init-comments.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 6 Jun 2015
;; Keywords: convenience, faces, outlines
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Change the way comments look.

;;; Code:


;;=============================================================================
;; Highlight todo-like comments
;;=============================================================================


(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIX\\(ME\\)?\\|HACK\\|NOTE\\|OPTIMIZE\\|TODO\\|XXX\\|WTF\\):"
                 1 font-lock-warning-face t)))))


(provide 'init-comments)
;;; init-comments.el ends here
