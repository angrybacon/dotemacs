;;; init-secret.el --- Customize these constants for further reference

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;; TODO: Make this optionnal.


;;=============================================================================
;; Set constants
;;=============================================================================


;; Set identity
(setq-default
 user-full-name "Mathieu Marques"
 user-mail-address "mathieumarques78@gmail.com")


;; Constants
;; NOTE: These probably shouldn't be constants, should they?
(defconst me/font-family            "Monaco"  "The font to use.")
(defconst me/font-size-default      120       "The font size to use for default text.")
(defconst me/font-size-header       140       "The font size to use for headers.")
(defconst me/font-size-mode-line    120       "The font size to use for the mode line.")
(defconst me/golden-ratio-factor    .8        "The factor to split windows with.")


(provide 'init-secret)
;;; init-secret.el ends here
