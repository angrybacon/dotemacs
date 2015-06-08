;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for JavaScript buffers
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package js
  :delight js-mode "JS")


;;─────────────────────────────────────────────────────────────────────────────
;; Create `imenu' entries for AngularJS elements
;;─────────────────────────────────────────────────────────────────────────────


(setq javascript-common-imenu-regex-list
      '(("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
        ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
        ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
        ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
        ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
        ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
        ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))
        ;; ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
        ;; ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
        ;; ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
        ;; ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)))


(defun me/make-imenu-index-for-js ()
  "Make `imenu' entries for AngularJS elements."
  (save-excursion (imenu--generic-function javascript-common-imenu-regex-list)))


(defun me/js-mode-hook ()
  "Initialize custom `imenu' entries for AngularJS elements."
  (setq imenu-create-index-function 'me/make-imenu-index-for-js))


(add-hook 'js-mode-hook 'me/js-mode-hook)


;;─────────────────────────────────────────────────────────────────────────────
;; End init-javascript.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-javascript)
