;;; use-mouse.el --- Customize mouse support         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Mouse wheel

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(2 ((control) . 8)
                                 ((meta) . global-text-scale)
                                 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 8))

;;; use-mouse.el ends here
