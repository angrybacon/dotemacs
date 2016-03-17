;;; init-python.el --- All about Python

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure python
;;=============================================================================


;; Built-in
(use-package python
  :defer t
  :delight python-mode "Python")


;;=============================================================================
;; Configure pip-requirements
;;=============================================================================


;; https://github.com/Wilfred/pip-requirements.el
(use-package pip-requirements
  :defer t
  :delight pip-requirements-mode "PyPA Requirements"
  ;; FIXME: http://emacs.stackexchange.com/q/21019/2397
  )


(provide 'init-python)
;;; init-python.el ends here
