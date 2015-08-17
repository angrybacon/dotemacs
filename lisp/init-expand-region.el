(require 'use-package)


;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :init
  (pending-delete-mode t)
  :bind
  ("C-=" . er/expand-region))


(provide 'init-expand-region)
