;;; init-dired.el --- All about Dired

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 27 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure dired
;;=============================================================================


;; Built-in
(use-package dired

  :ensure nil
  :delight dired-mode "Dired"

  :config

  (defadvice dired-readin (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (setq-default
   dired-auto-revert-buffer t
   dired-listing-switches "-alh"
   dired-ls-F-marks-symlinks nil
   dired-recursive-copies 'always))


(provide 'init-dired)
;;; init-dired.el ends here
