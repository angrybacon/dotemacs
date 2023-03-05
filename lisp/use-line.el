;;; use-line.el --- Customize the mode-line          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :functions doom-modeline-def-modeline doom-modeline-def-segment
  :config
  (doom-modeline-def-segment me/buffer
    "The buffer description and major mode icon."
    (concat
     (doom-modeline-spc) (doom-modeline--buffer-name) (doom-modeline-spc)))
  (doom-modeline-def-segment me/position
    "The buffer position."
    (let* ((active (doom-modeline--active))
           (face (if active 'mode-line 'mode-line-inactive)))
      (propertize (concat (doom-modeline-spc)
                          (format-mode-line "%l:%c")
                          (doom-modeline-spc))
                  'face face)))
  (doom-modeline-def-segment me/buffer-simple
    "The buffer name but simpler."
    (let* ((active (doom-modeline--active))
           (face (cond ((and buffer-file-name (buffer-modified-p))
                        'doom-modeline-buffer-modified)
                       (active 'doom-modeline-buffer-file)
                       (t 'mode-line-inactive))))
      (concat (doom-modeline-spc)
              (propertize "%b" 'face face)
              (doom-modeline-spc))))
  (doom-modeline-def-segment me/default-directory
    "The buffer directory."
    (let* ((active (doom-modeline--active))
           (face (if active 'doom-modeline-buffer-path 'mode-line-inactive)))
      (concat (doom-modeline-spc)
              (propertize (abbreviate-file-name default-directory) 'face face)
              (doom-modeline-spc))))
  (doom-modeline-def-segment me/evil
    "The current Evil state."
    (doom-modeline--evil))
  (doom-modeline-def-segment me/flymake
    "The error status with color codes and icons."
    (when (bound-and-true-p flymake-mode)
      (let ((active (doom-modeline--active))
            (icon doom-modeline--flymake-icon)
            (text doom-modeline--flymake-text))
        (concat
         (when icon
           (concat (doom-modeline-spc)
                   (if active
                       icon
                     (doom-modeline-propertize-icon icon 'mode-line-inactive))))
         (when text
           (concat (if icon (doom-modeline-vspc) (doom-modeline-spc))
                   (if active
                       text
                     (propertize text 'face 'mode-line-inactive))))
         (when (or icon text)
           (doom-modeline-spc))))))
  (doom-modeline-def-segment me/info
    "The topic and nodes in Info buffers."
    (let ((active (doom-modeline--active)))
      (concat
       (propertize " (" 'face (if active 'mode-line 'mode-line-inactive))
       (propertize (if (stringp Info-current-file)
                       (replace-regexp-in-string
                        "%" "%%" (file-name-sans-extension
                                  (file-name-nondirectory Info-current-file)))
                     (format "*%S*" Info-current-file))
                   'face (if active 'doom-modeline-info 'mode-line-inactive))
       (propertize ") " 'face (if active 'mode-line 'mode-line-inactive))
       (when Info-current-node
         (propertize (concat
                      (replace-regexp-in-string "%" "%%" Info-current-node)
                      (doom-modeline-spc))
                     'face (if active
                               'doom-modeline-buffer-path
                             'mode-line-inactive))))))
  (doom-modeline-def-segment me/major
    "The current major mode, including environment information."
    (let* ((active (doom-modeline--active))
           (face (if active
                     'doom-modeline-buffer-major-mode
                   'mode-line-inactive)))
      (concat (doom-modeline-spc)
              (propertize (format-mode-line mode-name) 'face face)
              (doom-modeline-spc))))
  (doom-modeline-def-segment me/vcs
    "The version control system information."
    (when-let ((branch doom-modeline--vcs-text))
      (let ((active (doom-modeline--active))
            (text (concat ":" branch)))
        (concat (doom-modeline-spc)
                (if active text (propertize text 'face 'mode-line-inactive))
                (doom-modeline-spc)))))
  (doom-modeline-def-modeline 'info
    '(bar me/evil me/buffer me/info me/position selection-info)
    '(irc-buffers matches process debug me/major workspace-name))
  (doom-modeline-def-modeline 'main
    '(bar me/evil me/buffer remote-host me/position me/flymake selection-info)
    '(irc-buffers matches process me/vcs debug me/major workspace-name))
  (doom-modeline-def-modeline 'message
    '(bar me/evil me/buffer-simple me/position selection-info)
    '(irc-buffers matches process me/major workspace-name))
  (doom-modeline-def-modeline 'org-src
    '(bar me/evil me/buffer-simple me/position me/flymake selection-info)
    '(irc-buffers matches process debug me/major workspace-name))
  (doom-modeline-def-modeline 'project
    '(bar me/evil me/default-directory)
    '(irc-buffers matches process debug me/major workspace-name))
  (doom-modeline-def-modeline 'special
    '(bar me/evil me/buffer me/position selection-info)
    '(irc-buffers matches process debug me/major workspace-name))
  (doom-modeline-def-modeline 'vcs
    '(bar me/evil me/buffer remote-host me/position selection-info)
    '(irc-buffers matches process debug me/major workspace-name))
  :custom
  (doom-modeline-bar-width (szadek-get 'mode-line-bar 8))
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-height (szadek-get 'mode-line-height 36))
  (doom-modeline-enable-word-count t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-vcs-max-length 28)
  :hook
  (after-init . doom-modeline-mode))

(use-package leyline
  :load-path "lisp/leyline"
  :hook
  (after-change-major-mode . leyline-rename))

;;; use-line.el ends here
