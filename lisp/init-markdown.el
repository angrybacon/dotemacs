;;; init-markdown.el --- All about Markdown

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure Markdown mode
;;=============================================================================


;; Website: http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :delight markdown-mode "Markdown"
  :mode ("\\.markdown\\'" "\\.md\\'" "\\.txt\\'"))


(provide 'init-markdown)
;;; init-markdown.el ends here
