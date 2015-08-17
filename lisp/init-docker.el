(require 'use-package)


;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :delight dockerfile-mode "Dockerfile"
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


(provide 'init-docker)
