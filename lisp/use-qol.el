;;; use-qol.el --- Quality of life customizations    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Auto-Fill

(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

;;;; Embark

(use-package embark
  :bind
  ([remap describe-bindings] . embark-bindings)
  ("C-;" . embark-act)
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Logs

(advice-add 'message :after
  (defun me/message-tail (&rest _)
    "Automatically scroll the message buffer to the bottom on new messages.
The behavior is ignored when the message buffer is active."
    (let* ((name "*Messages*")
           (windows (get-buffer-window-list name nil :all-frames)))
      (unless (string-equal name (buffer-name))
        (dolist (window windows)
          (with-selected-window window
            (goto-char (point-max))))))))

;;;; Paste

(use-package webpaste
  :custom
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org")))

;;;; Rainbow

(use-package rainbow-mode
  :hook
  (css-base-mode . rainbow-mode)
  (help-mode . rainbow-mode)
  :init
  (add-to-list 'safe-local-eval-forms
               '(when (require 'rainbow-mode nil :noerror) (rainbow-mode 1)))
  :custom
  (rainbow-x-colors nil))

;;; use-qol.el ends here
