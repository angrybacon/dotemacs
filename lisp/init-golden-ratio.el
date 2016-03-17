;;; init-golden-ratio.el --- Resize active window on focus

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure golden-ratio
;;=============================================================================


;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :defer t
  :defines (me/golden-ratio-factor)
  :init (golden-ratio-mode 1)
  :config
  (setq-default
   ;; TODO: Find a sane way to determine the ratio factor.
   golden-ratio-adjust-factor me/golden-ratio-factor
   split-width-threshold nil))


(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
