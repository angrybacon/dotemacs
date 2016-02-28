;;; init-comments.el --- Change the way comments look

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 6 June 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Highlight todo-like comments
;;=============================================================================


;; FIXME: Doesn't work everywhere
(add-hook 'change-major-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIX\\(ME\\)?\\|HACK\\|NOTE\\|OPTIMIZE\\|TODO\\|XXX\\|WTF\\) +?:"
                 1 font-lock-warning-face t)))))


(provide 'init-comments)
;;; init-comments.el ends here
