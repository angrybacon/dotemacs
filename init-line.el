;; Customize the major mode strings
(delight 'css-mode                    "css"           :major)
(delight 'dired-mode                  "dired"         :major)
(delight 'emacs-lisp-mode             "elisp"         :major)
(delight 'eshell-mode                 "eshell"        :major)
(delight 'fundamental-mode            "fundamental"   :major)
(delight 'help-mode                   "help"          :major)
(delight 'html-mode                   "html"          :major)
(delight 'js-mode                     "js"            :major)
(delight 'lisp-interaction-mode       "lisp"          :major)
(delight 'magit-commit-mode           "magit-commit"  :major)
(delight 'magit-log-mode              "magit-log"     :major)
(delight 'magit-status-mode           "magit-status"  :major)
(delight 'org-mode                    "org"           :major)
(delight 'python-mode                 "python"        :major)
(delight 'scss-mode                   "scss"          :major)
(delight 'text-mode                   "text"          :major)
(delight 'web-mode                    "web"           :major)


;; Customize the minor mode strings
(delight 'anaconda-mode               nil  "anaconda-mode")
(delight 'autopair-mode               nil  "autopair")
(delight 'company-mode                nil  "company")
(delight 'eldoc-mode                  nil  "eldoc-mode")
(delight 'emmet-mode                  nil  "emmet-mode")
(delight 'highlight-parentheses-mode  nil  "highlight-parentheses")
(delight 'magit-auto-revert-mode      nil  "magit")
(delight 'smooth-scroll-mode          nil  "smooth-scroll")
(delight 'subword-mode                nil  "subword")


;; Nyan mode (https://github.com/TeMPOraL/nyan-mode/)
(setq nyan-bar-length 16)

;; Customize mode line faces
(set-face-attribute 'mode-line nil :box `(:line-width 4 :color ,zenburn/bg-1))
(set-face-attribute 'mode-line-inactive nil :box `(:line-width 4 :color ,zenburn/bg-05))
