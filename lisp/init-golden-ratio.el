;;; init-golden-ratio.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Resize active window on focus.

;;; Code:


;;=============================================================================
;; Configure Golden Ratio
;;=============================================================================


;; Website: https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t

  :defines me/golden-ratio-factor

  :init
  (setq
   golden-ratio-adjust-factor me/golden-ratio-factor
   split-width-threshold nil)

  :config
  (golden-ratio-mode 1))


(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
