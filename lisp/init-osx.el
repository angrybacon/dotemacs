;;; init-psx.el --- Enhance OSX experience

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Add OSX-specific settings
;;=============================================================================


(defvar ns-command-modifier)
(defvar ns-option-modifier)
(when (eq system-type 'darwin)
  (setq-default
   exec-path (append exec-path '("/usr/local/bin"))  ; Add path to binaries installed with Homebrew
   ns-command-modifier 'meta                         ; Map the Meta key to the `cmd' key
   ns-option-modifier nil))                          ; Disable the `alt' key


(provide 'init-osx)
;;; init-osx.el ends here
