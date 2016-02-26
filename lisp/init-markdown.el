;;; init-markdown.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure Markdown mode.

;;; Code:


;;=============================================================================
;; Configure Markdown Mode
;;=============================================================================


;; Website: http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :delight markdown-mode "Markdown"
  :mode ("\\.markdown\\'" "\\.md\\'" "\\.txt\\'"))


(provide 'init-markdown)
;;; init-markdown.el ends here
