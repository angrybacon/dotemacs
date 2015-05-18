;;─────────────────────────────────────────────────────────────────────────────
;; Add support for multiple cursors
;;─────────────────────────────────────────────────────────────────────────────


;; Configure `multiple-cursors' (https://github.com/magnars/multiple-cursors.el)

;; (with-eval-after-load 'multiple-cursors
  (setq mc/list-file "~/.emacs.d/elpa/multiple-cursors/mc-lists.el")
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; )


;;─────────────────────────────────────────────────────────────────────────────
;; End init-multiple-cursors.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-multiple-cursors)
