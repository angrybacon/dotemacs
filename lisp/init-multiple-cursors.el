(require 'use-package)


;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/list-file "~/.emacs.d/elpa/multiple-cursors/mc-lists.el")
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)))


(provide 'init-multiple-cursors)
