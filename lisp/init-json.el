;;; init-json.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure JSON mode.

;;; Code:


;;=============================================================================
;; Silence the byte-compiler
;;=============================================================================


(require 'use-package)


;;=============================================================================
;; Configure JSON Mode
;;=============================================================================


;; Website: https://github.com/gongo/json-reformat
(use-package json-mode
  :delight json-mode "JSON"
  :ensure t

  :init
  (setq
   ;; FIXME: This will be fixed with https://github.com/joshwnj/json-mode/issues/32.
   auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist)
   auto-mode-alist (cons '("\\.jshintrc$" . json-mode) auto-mode-alist)
   auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist)
   json-reformat:indent-width 2))


(provide 'init-json)
;;; init-json.el ends here
