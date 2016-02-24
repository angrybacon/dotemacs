;;; init-yaml.el --- My Emacs configuration

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 13 Jul 2015
;; Keywords: convenience
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Commentary:

;; Configure YAML mode.

;;; Code:


;;=============================================================================
;; Configure YAML Mode
;;=============================================================================


;; Website: https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :delight yaml-mode "YAML"
  :ensure t

  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(provide 'init-yaml)
;;; init-yaml.el ends here
