;;; rich-minority-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rich-minority" "rich-minority.el" (21761 59216
;;;;;;  0 0))
;;; Generated autoloads from rich-minority.el

(autoload 'rm--mode-list-as-string-list "rich-minority" "\
Return `minor-mode-list' as a simple list of strings.

\(fn)" nil nil)

(defvar rich-minority-mode nil "\
Non-nil if Rich minority mode is enabled.
See the command `rich-minority-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `rich-minority-mode'.")

(custom-autoload 'rich-minority-mode "rich-minority" nil)

(autoload 'rich-minority-mode "rich-minority" "\
Toggle Rich minority mode on or off.
With a prefix argument ARG, enable Rich minority mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{rich-minority-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rich-minority-autoloads.el ends here
