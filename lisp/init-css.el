;;; init-css.el --- All about CSS

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 18 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure CSS mode
;;=============================================================================


;; Built-in
(use-package css-mode
  :delight css-mode "CSS"
  :init (setq-default css-indent-offset 2))


;;=============================================================================
;; Configure SCSS mode
;;=============================================================================


;; Website: https://github.com/antonj/scss-mode/
(use-package scss-mode
  :delight scss-mode "SCSS"
  :mode ("\\.css\\'" "\\.sass\\'" "\\.scss\\'")
  :init (setq-default scss-compile-at-save nil))


;;=============================================================================
;; Configure Helm CSS SCSS
;;=============================================================================


;; Website: https://github.com/ShingoFukuyama/helm-css-scss
(use-package helm-css-scss
  ;; TODO: https://github.com/bbatsov/zenburn-emacs/issues/220
  )


(provide 'init-css)
;;; init-css.el ends here
