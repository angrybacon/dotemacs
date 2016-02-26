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
;; Configure JSON Mode
;;=============================================================================


;; Website: https://github.com/gongo/json-reformat
(use-package json-mode
  :delight json-mode "JSON"
  :mode "\\.json\\'"
  :init
  ;; FIXME: This will be fixed with https://github.com/joshwnj/json-mode/issues/32.
  (setq-default json-reformat:indent-width 2))


(provide 'init-json)
;;; init-json.el ends here
