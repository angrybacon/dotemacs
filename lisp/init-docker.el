;;; init-docker.el --- All about Docker

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure dockerfile-mode
;;=============================================================================


;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :delight dockerfile-mode "Dockerfile"
  :mode "Dockerfile\\'")


(provide 'init-docker)
;;; init-docker.el ends here
