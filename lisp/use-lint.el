;;; use-lint.el --- Configure linters                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :ensure nil
  :config
  (with-eval-after-load 'evil
    (evil-global-set-key 'motion "g'" #'flymake-goto-next-error)
    (evil-global-set-key 'motion "g\"" #'flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position nil))

(use-package flymake-eslint
  :functions flymake-eslint-enable
  :preface
  (defun me/flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

(use-package prettier
  :init
  (add-to-list 'safe-local-eval-forms '(prettier-mode)))

;;; use-lint.el ends here
