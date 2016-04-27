;;; selected.el --- Keymap for when region is active

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/selected.el
;; Version: 1.00
;; Keywords: convenience
;; Package-Requires: ()

;;; Commentary:

;; When `selected-minor-mode' is active, the keybindings in `selected-keymap'
;; will be enabled when the region is active.  This is useful for commands that
;; operates on the region, which you only want keybound when the region is
;; active.
;;
;; `selected-keymap' has no default bindings.  Bind it yourself:
;; 
;; (define-key selected-keymap (kbd "u") #'upcase-region)
;;
;; You can also bind keys specific to a major mode, by creating a keymap named
;; selected-<major-mode-name>-map:
;; 
;; (setq selected-org-mode-map (make-sparse-keymap))
;; (define-key selected-org-mode-map (kbd "t") #'org-table-convert-region)

;;; Code:
(defvar selected-keymap (make-sparse-keymap)
  "Keymap for `selected-minor-mode'.  Add keys here that should be active when region is active.")

(define-minor-mode selected-region-active
  "Meant to activate when region becomes active.  Not intended for the user.  Use `selected-minor-mode'."
  :keymap selected-keymap
  (when selected-region-active
    (let ((major-selected-map
           (intern-soft (concat "selected-" (symbol-name major-mode) "-map"))))
      (if major-selected-map
          (setf (cdr (assoc 'selected-region-active minor-mode-map-alist))
                (let ((map (eval major-selected-map)))
                  (set-keymap-parent map selected-keymap)
                  map))
        (setf (cdr (assoc 'selected-region-active minor-mode-map-alist))
              selected-keymap)))))

(defun selected--on ()
  (selected-region-active 1))

(defun selected-off ()
  "Disable bindings in `selected-keymap' temporarily."
  (interactive)
  (selected-region-active -1))

;;;###autoload
(define-minor-mode selected-minor-mode
  "If enabled activates the `selected-keymap' when the region is active."
  :lighter " sel"
  (if selected-minor-mode
      (progn
        (add-hook 'activate-mark-hook #'selected--on)
        (add-hook 'deactivate-mark-hook #'selected-off))
    (remove-hook 'activate-mark-hook #'selected--on)
    (remove-hook 'deactivate-mark-hook #'selected-off)
    (selected-off)))

(provide 'selected)
;;; selected.el ends here
