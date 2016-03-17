;;; init-yaml.el --- All about YAML

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 13 July 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure yaml-mode
;;=============================================================================


;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode "\\.yml\\'"
  :delight yaml-mode "YAML")


(provide 'init-yaml)
;;; init-yaml.el ends here
