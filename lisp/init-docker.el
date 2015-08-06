;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-docker.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)


;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Docker buffers
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :delight dockerfile-mode "Dockerfile"
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-docker.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-docker)
