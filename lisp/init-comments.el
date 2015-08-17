;; Highlight TODO-like comments
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIX\\(ME\\)?\\|HACK\\|NOTE\\|OPTIMIZE\\|TODO\\|XXX\\|WTF\\):"
                 1 font-lock-warning-face t)))))


(provide 'init-comments)
