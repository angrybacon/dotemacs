;; Inhibit the mouse events (coding on a laptop sucks)
;; (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
;;              [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
;;              [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]))
;;   (global-unset-key k))


;; Shortcut to jump to previous buffer
(defun goto-left-buffer ()
  (interactive)
  (other-window -1))
(define-key global-map (kbd "C-x p") 'goto-left-buffer)

;; Shortcut for Helm
(define-key global-map (kbd "C-c m") 'helm-imenu)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)


;; Consistent behavior when jumping paragraphs backward and forward
(defun backward-block ()
  (interactive)
  (skip-chars-backward "\n")
  (when (not (search-backward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-min)))
  (skip-chars-forward "\n"))
(global-set-key (kbd "<C-up>") 'backward-block)
(defun forward-block ()
  (interactive)
  (skip-chars-forward "\n")
  (when (not (search-forward-regexp "\n[[:blank:]]*\n" nil t)) (goto-char (point-max)))
  (skip-chars-forward "\n"))
(global-set-key (kbd "<C-down>") 'forward-block)

;; Map cmd key to Meta (Mac only)
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier nil))
