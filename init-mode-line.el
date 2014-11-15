;; Customize the major and minor modes strings
(add-to-list 'load-path "~/.emacs.d/vendor/delight/")
(require 'delight)
(delight 'css-mode               "css"     :major)
(delight 'emacs-lisp-mode        "elisp"   :major)
(delight 'html-mode              "html"    :major)
(delight 'js-mode                "js"      :major)
(delight 'lisp-interaction-mode  "lisp"    :major)
(delight 'python-mode            "python"  :major)
(delight 'scss-mode              "scss"    :major)
(delight 'autopair-mode          ""        "autopair")
(delight 'company-mode           ""        "company")
(delight 'emmet-mode             ""        "emmet-mode")
(delight 'indent-guide-mode      ""        "indent-guide")
(delight 'magit-auto-revert-mode ""        "magit")
(delight 'smooth-scroll-mode     ""        "smooth-scroll")
(delight 'subword-mode           ""        "subword")


;; Nyan mode (https://github.com/TeMPOraL/nyan-mode/)
(add-to-list 'load-path "~/.emacs.d/vendor/nyan-mode/")
(require 'nyan-mode)


;; Mode line setup (http://amitp.blogspot.fr/2011/08/emacs-custom-mode-line.html)
(setq-default mode-line-format
              ;; Position, including warning for 80 columns
              '((:propertize "%4l:" face mode-line-position-face)
                (:eval (propertize "%3c" 'face
                                   (if (>= (current-column) 80)
                                       'mode-line-80col-face
                                     'mode-line-position-face)))

                ;; emacsclient [default -- keep?]
                mode-line-client
                "  "

                ;; Read-only or modified status
                (:eval (cond (buffer-read-only
                              (propertize "  RO  " 'face 'mode-line-read-only-face))
                             ((buffer-modified-p)
                              (propertize "  **  " 'face 'mode-line-modified-face))
                             (t "  --  ")))

                ;; Position in file
                (:propertize "  %p" face mode-line-position-relative-face)
                (:propertize "/%I  " face mode-line-buffer-size-face)

                ;; Directory and buffer/file name
                (:propertize (:eval (concat "  " (shorten-directory default-directory 20)))
                             face mode-line-folder-face)
                (:propertize "%b  " face mode-line-filename-face)

                ;; narrow [default -- keep?]
                ;; "  %n  "
                ;; Mode indicators: vc, recursive edit, major mode, minor modes, process, global
                ;; (vc-mode vc-mode)
                "  %["
                (:propertize mode-name face mode-line-mode-face)
                "%]"
                (:eval (propertize (format-mode-line minor-mode-alist)
                                   'face 'mode-line-minor-mode-face))
                (:propertize mode-line-process face mode-line-process-face)
                (global-mode-string global-mode-string)
                "  "

                ;; nyan-mode uses nyan cat as an alternative to %p
                (:eval (when nyan-mode (list (nyan-create))))
                ))


;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to 'max-length' characters of a directory name 'dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-position-relative-face)
(make-face 'mode-line-buffer-size-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)


(set-face-attribute 'vertical-border nil :foreground "#073642")

;; Customize mode line faces
(set-face-attribute 'mode-line nil
                    :foreground "#586E75"
                    :background "#073642"
                    :inverse-video nil
                    :underline nil
                    :overline nil
                    :box '(:line-width 2 :color "#073642" :style nil))

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#586E75"
                    :background "#073642"
                    :inverse-video nil
                    :underline nil
                    :overline nil
                    :box '(:line-width 2 :color "#073642" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#CB4B16"
                    :weight 'bold)

(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#CB4B16"
                    :weight 'bold)

(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#93A1A1"
                    :weight 'bold)

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :foreground "#93A1A1"
                    :weight 'bold)

(set-face-attribute 'mode-line-position-relative-face nil
                    :inherit 'mode-line-face
                    :foreground "#93A1A1"
                    :weight 'bold)

(set-face-attribute 'mode-line-buffer-size-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "#93A1A1"
                    :weight 'bold)

(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718C00")

(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "#CB4B16")
