;;; use-hydra.el --- Spawn Hydra heads               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-c d")
                (let ((map (make-sparse-keymap "Dates")))
                  (define-key map "d" '("date" . barrinalo-date-short))
                  (define-key map "i" '("iso" . barrinalo-date-iso))
                  (define-key map "l" '("long" . barrinalo-date-long))
                  map))

(global-set-key (kbd "C-c g")
                (let ((map (make-sparse-keymap "Git")))
                  (define-key map "b" '("blame" . magit-blame))
                  (define-key map "c" '("clone" . magit-clone))
                  (define-key map "r" '("revert" . diff-hl-revert-hunk))
                  (define-key map "s" '("stage" . diff-hl-stage-current-hunk))
                  map))

(global-set-key (kbd "C-c s")
                (let ((map (make-sparse-keymap "System")))
                  (define-key map "d" '("clear desktop" . desktop-remove))
                  (define-key map "g" '("toggle debug" . toggle-debug-on-error))
                  (define-key map "l" '("processes" . list-processes))
                  (define-key map "p" '("packages" . package-list-packages))
                  (define-key map "Q" '("clear kill" . manticore-kill-terminal))
                  map))

;; TODO Migrate Hydra to simple maps

(defun hercules-heading (&rest headings)
  "Format HEADINGS to look pretty in a hydra docstring."
  (concat "\n "
          (mapconcat (lambda (heading)
                       (propertize (format "%-18s" heading) 'face 'shadow))
                     headings)))

(use-package hydra
  :functions defhydra
  :bind
  ("C-c i" . hydra-interface/body)
  ("C-c v" . hydra-visit/body)
  :config
  (defhydra hydra-interface (:color pink :idle 1.0 :pre (require 'morophon))
    (concat (hercules-heading "Do" "Toggles") "
 _m_ maximize frame  _a_ / _A_ alpha: %s`morophon--alpha
 _M_ cycle displays  _n_ line numbers: %s`display-line-numbers
 _t_ cycle theme     _o_ olivetti: %s`widowmaker-olivetti-automatic
 ^^                  _O_ olivetti width: %s`olivetti-body-width")
    ("q" nil)
    ("a" morophon-alpha-less)
    ("A" morophon-alpha-more)
    ("m" toggle-frame-maximized)
    ("M" widowmaker-placement-cycle)
    ("n" ruric-toggle-line-numbers)
    ("o" widowmaker-olivetti-automatic-toggle :color blue)
    ("O" widowmaker-olivetti-body-reset :color blue)
    ("<" widowmaker-olivetti-body-less)
    (">" widowmaker-olivetti-body-more)
    ("t" morophon-cycle :color blue)
    ("T" morophon-cycle))
  (defhydra hydra-visit (:color teal :idle 1.0)
    (concat (hercules-heading "Visit") "
 _._ secrets         _e_ emacs           _s_ zsh
 _`_ scratch         _l_ linux           _t_ kitty
 _c_ picom           _m_ macos           _v_ vim
 _d_ qtile           _n_ dunst           ^^")
    ("q" nil)
    ("`" (scratch-buffer))
    ("." (find-file "~/.config/emacs/.cache/szadek.eld"))
    ("c" (find-file "~/Workspace/dot/config/picom.org"))
    ("d" (find-file "~/Workspace/dot/config/qtile.org"))
    ("e" (find-file user-init-file))
    ("l" (find-file "~/Workspace/dot/LINUX.org"))
    ("m" (find-file "~/Workspace/dot/MACOS.org"))
    ("n" (find-file "~/Workspace/dot/config/dunst.org"))
    ("s" (find-file "~/Workspace/dot/config/zsh.org"))
    ("t" (find-file "~/Workspace/dot/config/kitty.org"))
    ("v" (find-file "~/Workspace/dot/config/vim.org")))
  :hook
  (emacs-lisp-mode . hydra-add-imenu))

;;; use-hydra.el ends here
