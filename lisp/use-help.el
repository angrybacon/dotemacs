;;; use-help.el --- Customize how help is provided   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eldoc
  ;; NOTE Pending https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47109 we would
  ;;      be able to join pieces of documentation with horizontal rules.
  :ensure nil
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0))

(use-package help
  :ensure nil
  :config
  (define-key help-map (kbd "C-h") nil :remove)
  (define-key help-map (kbd "C") #'find-function-on-key)
  (define-key help-map (kbd "F") #'describe-face)
  (define-key help-map (kbd "K") #'describe-keymap))

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 2))

;;; use-help.el ends here
