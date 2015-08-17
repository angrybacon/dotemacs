(require 'use-package)


;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t
  :init
  (setq golden-ratio-adjust-factor .9)
  :config
  (golden-ratio-mode 1))


(provide 'init-golden-ratio)
