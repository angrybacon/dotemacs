;; Change the major mode strings
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


;; Change the minor mode strings
(delight 'autopair-mode               " autopair"               "autopair")
(delight 'company-mode                " company"                "company")
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
(setq display-time-format "%I:%M%p")
(setq display-time-load-average-threshold 1.5)
(setq rm-whitelist '(""))
(setq sml/projectile-replacement-format "[%s] ")
(setq sml/use-projectile-p 'before-prefixes)
(setq sml/battery-format "%p%% ")
(setq sml/show-remote nil)
(setq sml/vc-mode-show-backend nil)
(setq sml/theme 'automatic)
(sml/setup)

;; Customize mode line faces
(set-face-attribute 'mode-line nil :background zenburn/bg-1 :box `(:line-width 4 :color ,zenburn/bg-1))
(set-face-attribute 'mode-line-inactive nil :background zenburn/bg-0 :slant 'unspecified :box `(:line-width 4 :color ,zenburn/bg-0))
;; Global
(set-face-attribute 'sml/global nil :foreground zenburn/bg+3)
(set-face-attribute 'sml/prefix nil :foreground zenburn/orange)
;; Numbers
(set-face-attribute 'sml/line-number nil :foreground zenburn/fg :weight 'unspecified)
;; Buffer status
(set-face-attribute 'sml/modified nil :foreground zenburn/red :weight 'unspecified)
(set-face-attribute 'sml/read-only nil :foreground zenburn/blue)
;; Projectile
(set-face-attribute 'sml/git nil :foreground zenburn/blue-1)
;; Filename
(set-face-attribute 'sml/filename nil :foreground zenburn/yellow)
;; Remote
(set-face-attribute 'sml/vc-edited nil :foreground zenburn/yellow)
;; Modes
(set-face-attribute 'sml/modes nil :foreground 'unspecified)
;; Battery
(set-face-attribute 'sml/charging nil :foreground zenburn/green)
(set-face-attribute 'sml/discharging nil :foreground zenburn/red)
