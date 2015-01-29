;; Customize the major and minor modes strings
(delight 'css-mode                "css"     :major)
(delight 'emacs-lisp-mode         "elisp"   :major)
(delight 'html-mode               "html"    :major)
(delight 'js-mode                 "js"      :major)
(delight 'lisp-interaction-mode   "lisp"    :major)
(delight 'python-mode             "python"  :major)
(delight 'scss-mode               "scss"    :major)
(delight 'anaconda-mode           nil       "anaconda-mode")
(delight 'company-mode            nil       "company")
(delight 'emmet-mode              nil       "emmet-mode")
(delight 'magit-auto-revert-mode  nil       "magit")
(delight 'smooth-scroll-mode      nil       "smooth-scroll")
(delight 'subword-mode            nil       "subword")


;; Nyan mode (https://github.com/TeMPOraL/nyan-mode/)
(add-to-list 'load-path "~/.emacs.d/vendor/nyan-mode/")
(require 'nyan-mode)
(setq nyan-bar-length 16)

;; Customize mode line faces
(set-face-attribute 'mode-line nil :box `(:line-width 4 :color ,zenburn/bg-1))
(set-face-attribute 'mode-line-inactive nil :box `(:line-width 4 :color ,zenburn/bg-05))
