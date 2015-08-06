;;─────────────────────────────────────────────────────────────────────────────
;; Beginning of init-multiple-cursors.el
;;─────────────────────────────────────────────────────────────────────────────


(require 'use-package)


;;─────────────────────────────────────────────────────────────────────────────
;; Add support for multiple cursors
;;─────────────────────────────────────────────────────────────────────────────


;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/list-file "~/.emacs.d/elpa/multiple-cursors/mc-lists.el")
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)))


;;─────────────────────────────────────────────────────────────────────────────
;; End of init-multiple-cursors.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-multiple-cursors)
