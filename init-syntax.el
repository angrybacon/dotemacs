;; Colorful highlight the hex color strings
(defun highlight-hex-strings ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))
(add-hook 'emacs-lisp-mode-hook 'highlight-hex-strings)
(add-hook 'emmet-mode-hook 'highlight-hex-strings)
(add-hook 'python-mode-hook 'highlight-hex-strings)
