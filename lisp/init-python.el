;;─────────────────────────────────────────────────────────────────────────────
;; Configure Emacs for Python buffers
;;─────────────────────────────────────────────────────────────────────────────


;; Built-in
(use-package python
  :config
  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "--colors=Linux"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))


;;─────────────────────────────────────────────────────────────────────────────
;; End init-python.el
;;─────────────────────────────────────────────────────────────────────────────


(provide 'init-python)
