;;; init-markdown.el --- All about Markdown

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure markdown-mode
;;=============================================================================


;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :delight markdown-mode "Markdown"
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'")
  :config (setq-default markdown-asymmetric-header t))


(provide 'init-markdown)
;;; init-markdown.el ends here
