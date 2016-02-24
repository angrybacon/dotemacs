;;; init-css.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure CSS and SCSS modes.

;;; Code:


;;=============================================================================
;; Configure CSS Mode
;;=============================================================================


;; Built-in
(use-package css-mode
  :delight css-mode "CSS"

  :init
  (setq css-indent-offset 2))


;;=============================================================================
;; Configure SCSS Mode
;;=============================================================================


;; Website: https://github.com/antonj/scss-mode/
(use-package scss-mode
  :delight scss-mode "SCSS Test"
  :ensure t

  :init
  (setq scss-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))


;;=============================================================================
;; Configure Helm CSS SCSS
;;=============================================================================


;; Website: https://github.com/ShingoFukuyama/helm-css-scss
(use-package helm-css-scss
  :ensure t
  ;; TODO: https://github.com/bbatsov/zenburn-emacs/issues/220
  )


(provide 'init-css)
;;; init-css.el ends here
