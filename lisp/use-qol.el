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
  (lambda (&rest _)
    "Automatically scroll the message buffer to the bottom on new messages."
    (let* ((name "*Messages*"))
      (when (not (string= name (buffer-name)))
        (dolist (window (get-buffer-window-list name nil :all-frames))
          (with-selected-window window
            (goto-char (point-max)))))))
  '((name . me/message-tail)))

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
