;;; use-http.el --- Emacs as a HTTP client           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package restclient
  :hook
  (restclient-mode . display-line-numbers-mode)
  :mode ((rx ".http" eos) . restclient-mode))

;;; use-http.el ends here
