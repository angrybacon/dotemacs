;;; init-psx.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Add OSX-specific settigns

;;; Code:


;;=============================================================================
;; Silence byte-compiler
;;=============================================================================


(defvar ns-command-modifier)
(defvar ns-option-modifier)


;;=============================================================================
;; Add OSX-specific settings
;;=============================================================================


(when (eq system-type 'darwin)
  (setq
   exec-path (append exec-path '("/usr/local/bin"))  ; Add path to binaries installed with Homebrew
   ns-command-modifier 'meta                         ; Map the Meta key to the `cmd' key
   ns-option-modifier nil))                          ; Disable the `alt' key


(provide 'init-osx)
;;; init-osx.el ends here
