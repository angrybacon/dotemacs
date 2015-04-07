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
(delight 'autopair-mode               " autopair"               "autopair")
(delight 'company-mode                "company"                 "company")
(delight 'eldoc-mode                  " eldoc"                  "eldoc-mode")
(delight 'emmet-mode                  " emmet"                  "emmet-mode")
(delight 'golden-ratio-mode           " golden-ratio"           "golden-ratio")
(delight 'highlight-parentheses-mode  " highlight-parentheses"  "highlight-parentheses")
(delight 'magit-auto-revert-mode      " magit"                  "magit")
(delight 'smooth-scroll-mode          " smooth-scroll"          "smooth-scroll")
(delight 'subword-mode                " subword"                "subword")


;; Nyan mode (https://github.com/TeMPOraL/nyan-mode/)
;; (setq nyan-bar-length 16)


;; Smart Mode Line (https://github.com/Malabarba/smart-mode-line)
(display-battery-mode)
(setq rm-whitelist '("company"))
;; (setq sml/position-percentage-format "%p")
(setq sml/projectile-replacement-format "[%s]")
(setq sml/theme 'powerline)
(sml/setup)


;; Customize mode line faces
(defgroup me/sml-numbers '('sml/line-number 'sml/numbers-separator 'sml/col-number)
  "Faces for numbers within `smart-mode-line'.")
(set-face-attribute 'mode-line nil :background zenburn/bg-1 :box `(:line-width 1 :color ,zenburn/bg-1))
(set-face-attribute 'mode-line-inactive nil :background zenburn/bg-1 :slant 'unspecified :box `(:line-width 1 :color ,zenburn/bg-1))
;; Numbers
(set-face-attribute 'sml/global nil :foreground zenburn/bg+3)
(set-face-attribute 'sml/line-number nil :background zenburn/bg-1 :foreground zenburn/fg :weight 'unspecified)
(set-face-attribute 'sml/col-number nil :background zenburn/bg-1)
(set-face-attribute 'sml/numbers-separator nil :background zenburn/bg-1)
;; File info
(set-face-attribute 'sml/mule-info nil :background zenburn/bg-1)
(set-face-attribute 'sml/not-modified nil :background zenburn/bg-1)
(set-face-attribute 'sml/modified nil :foreground zenburn/red :weight 'unspecified)
(set-face-attribute 'sml/read-only nil :foreground zenburn/blue)
(set-face-attribute 'sml/remote nil :background zenburn/bg-1)
;; Battery
(set-face-attribute 'sml/discharging nil :background zenburn/bg-1 :foreground zenburn/red)
