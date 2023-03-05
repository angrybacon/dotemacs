;;; use-http.el --- Emacs as a HTTP client           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package restclient
  :defines restclient-mode-map
  :bind
  (:map restclient-mode-map
   ([remap restclient-http-send-current]
    . restclient-http-send-current-stay-in-window)
   ("C-n" . restclient-jump-next)
   ("C-p" . restclient-jump-prev))
  :hook
  (restclient-mode . display-line-numbers-mode)
  :mode ((rx ".http" eos) . restclient-mode))

;;; use-http.el ends here
