;;; use-help.el --- Customize how help is provided   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eldoc
  :ensure nil
  :custom
  ;; NOTE Pending https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47109 we would
  ;;      be able to join pieces of documentation with horizontal rules.
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0))

(use-package help-mode
  :ensure nil
  :bind
  ("C-h K" . describe-keymap))

(use-package helpful
  :defines helpful-mode-map
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-h C" . find-function-on-key)
  ("C-h F" . helpful-function)
  :config
  (with-eval-after-load 'evil
    (evil-define-key* 'normal helpful-mode-map
      (kbd "<") #'help-go-back
      (kbd ">") #'help-go-forward))
  :custom
  (helpful-max-buffers 2))

;;; use-help.el ends here
