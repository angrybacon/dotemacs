;;; init-connstants.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Customize these constants for further reference.

;;; Code:


;;=============================================================================
;; Set constants
;;=============================================================================


;; Set identity
(setq user-full-name "Mathieu Marques")
(setq user-mail-address "mathieumarques78@gmail.com")


;; Constants
;; NOTE: I don't think these should be constants.
(defconst me/font-family-default    "Monaco"  "The font to use for default text.")
(defconst me/font-family-header     "Monaco"  "The font to use for headers.")
(defconst me/font-family-mode-line  "Monaco"  "The font to use for the mode line.")
(defconst me/font-size-default      120       "The font size to use for default text.")
(defconst me/font-size-header       140       "The font size to use for headers.")
(defconst me/font-size-mode-line    120       "The font size to use for the mode line.")
(defconst me/initial-buffer         nil       "The buffer to load on start-up.")


(provide 'init-constants)
;;; init-constants.el ends here
