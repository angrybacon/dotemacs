;;; use-packages.el --- Third-party machinery        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default
 package-install-upgrade-built-in t
 package-native-compile t
 use-package-always-defer t
 use-package-always-ensure t
 use-package-enable-imenu-support t)

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

;;; use-packages.el ends here
