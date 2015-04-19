;; Global default settings
(setq-default show-trailing-whitespace 1         ; Display trailing whitespaces
      indent-tabs-mode nil                       ; No tabs
      indicate-empty-lines t)                    ; Indicate empty line (require left fringe)


;; Buffer local settings
(setq inhibit-startup-screen 1                   ; Remove start-up screen
      scroll-step 1                              ; Line by line scrolling
      default-tab-width 4                        ; Set width for tabs
      x-select-enable-clipboard 1                ; Merge both system's and Emacs' clipboard
      sgml-basic-offset 4                        ; Set indent to 4 spaces
      mouse-yank-at-point 1)                     ; Yank at mouse cursor rather than click location


;; Toggle Emacs' interface elements
(scroll-bar-mode -1)                             ; Remove scroll bar
(menu-bar-mode -1)                               ; Remove menu bar
(tool-bar-mode -1)                               ; Remove the toolbar
(line-number-mode )                              ; Display line number of the cursor current position
(column-number-mode 1)                           ; Display column number of the cursor current position
(fringe-mode '(8 . 0))                           ; Display left fringe
(global-hl-line-mode 1)                          ; Hightlight current line
(global-subword-mode 1)                          ; Iterate through CamelCase words
(mouse-avoidance-mode 'animate)                  ; Move the cursor to the corner when typing
(set-frame-parameter nil 'fullscreen 'fullboth)  ; Maxmimize the window
(display-battery-mode t)                         ; Display battery level in the mode-line
(display-time-mode t)                            ; Display time clock in the mode-line


;; Sort directories first while in dired mode
(defun me/list-directories-first ()
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))
(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  (me/list-directories-first))


;; Colorful highlight for hex color strings
(defun me/highlight-hex-strings ()
  (interactive)
  (font-lock-add-keywords nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))
(add-hook 'emacs-lisp-mode-hook 'me/highlight-hex-strings)
(add-hook 'emmet-mode-hook 'me/highlight-hex-strings)
(add-hook 'python-mode-hook 'me/highlight-hex-strings)
