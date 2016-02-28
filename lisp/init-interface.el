;;; init-interface.el --- Enable or disable several interface components

;; Copyright (C) 2015 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: 16 May 2015
;; Homepage: https://bitbucket.org/angrybacon/dotemacs

;;; Code:


;;=============================================================================
;; Configure interface components
;;=============================================================================


;; Better default
(setq-default
 ad-redefinition-action 'accept                  ; Silence warnings for redefined functions
 display-time-default-load-average nil           ; Hide the time load
 display-time-format "%H:%M"                     ; Format the time string
 indent-tabs-mode nil                            ; Stop using tabs to indent
 indicate-empty-lines t                          ; Indicate unused lines in the fringe
 inhibit-startup-screen t                        ; Remove start-up screen
 initial-scratch-message ""                      ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                           ; Yank at mouse cursor rather than click location
 require-final-newline 'visit                    ; Add a newline at EOF on visit
 scroll-step 1                                   ; Line by line scrolling
 show-trailing-whitespace nil                    ; Display trailing whitespaces
 tab-width 4                                     ; Set width for tabs
 x-select-enable-clipboard t)                    ; Merge both system's and Emacs' clipboard


;; Goodies
(global-subword-mode 1)                          ; Iterate through CamelCase words
(mouse-avoidance-mode 'animate)                  ; Move pointer when point reaches cursor location


;;=============================================================================
;; Define helpers
;;=============================================================================


(add-hook 'prog-mode-hook 'me/highlight-hex-strings)


;; FIXME: Merge?
(defun me/list-directories-first ()
  "Sort by directory first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))


(defadvice dired-readin (after dired-after-updating-hook first () activate)
  "List directories first in `dired-mode'."
  (me/list-directories-first))


(defun me/highlight-hex-strings ()
  "Find and highlight hexadecimal color strings with a colored background."
  (interactive)
  (font-lock-add-keywords nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))


;;=============================================================================
;; Configure Uniquify
;;=============================================================================


;; Built-in
(use-package uniquify
  :ensure nil
  :init (setq-default uniquify-buffer-name-style 'forward))



(provide 'init-interface)
;;; init-interface.el ends here
