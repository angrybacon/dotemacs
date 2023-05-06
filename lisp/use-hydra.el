;;; use-hydra.el --- Spawn Hydra heads               -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  ("C-c d" . hydra-dates/body)
  ("C-c g" . hydra-git/body)
  ("C-c i" . hydra-interface/body)
  ("C-c s" . hydra-system/body)
  ("C-c v" . hydra-visit/body)
  :custom
  (hydra-default-hint nil)
  :config

  (defhydra hydra-dates (:color teal :idle 1.0)
    (concat (hercules-heading "Insert" "Insert with Time") "
 _d_ short           _D_ short           ^^
 _i_ iso             _I_ iso             ^^
 _l_ long            _L_ long            ^^")
    ("q" nil)
    ("d" barrinalo-date-short)
    ("D" barrinalo-date-short-with-time)
    ("i" barrinalo-date-iso)
    ("I" barrinalo-date-iso-with-time)
    ("l" barrinalo-date-long)
    ("L" barrinalo-date-long-with-time))

  (defhydra hydra-git (:color teal :idle 1.0)
    (concat (hercules-heading "Do" "Gutter") "
 _b_ blame           _p_ previous        ^^
 _c_ clone           _n_ next            ^^
 _g_ status          _r_ revert          ^^
 _m_ smerge...       _s_ stage           ^^")
    ("q" nil)
    ("b" magit-blame)
    ("c" magit-clone)
    ("g" magit-status)
    ("m" (progn (require 'smerge-mode) (hydra-git--smerge/body)))
    ("n" git-gutter:next-hunk :color red)
    ("p" git-gutter:previous-hunk :color red)
    ("r" git-gutter:revert-hunk)
    ("s" git-gutter:stage-hunk :color red))

  (defhydra hydra-git--smerge (:color pink
                               :idle 1.0
                               :pre (if (not smerge-mode) (smerge-mode 1))
                               :post (smerge-auto-leave))
    (concat (hercules-heading "Move" "Keep" "Diff") "
 _g_ first           _RET_ current       _<_ upper / base
 _G_ last            _a_ all             _=_ upper / lower
 _j_ next            _b_ base            _>_ base / lower
 _k_ previous        _l_ lower           _E_ ediff
 ^^                  _u_ upper           _H_ highlight")
    ("q" nil :color blue)
    ("j" smerge-next)
    ("k" smerge-prev)
    ("<" smerge-diff-base-upper :color blue)
    ("=" smerge-diff-upper-lower :color blue)
    (">" smerge-diff-base-lower :color blue)
    ("RET" smerge-keep-current)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("E" smerge-ediff :color blue)
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("H" smerge-refine)
    ("l" smerge-keep-lower)
    ("u" smerge-keep-upper))

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

  (defhydra hydra-system (:color teal :idle 1.0)
    (concat (hercules-heading "Do" "Packages" "Toggles") "
 _d_ clear compiled  _p_ update          _g_ debug: %-3s`debug-on-error
 _D_ clear desktop   _P_ prune           ^^
 _l_ processes       ^^                  ^^
 _Q_ clear and kill  ^^                  ^^")
    ("q" nil)
    ("d" manticore-delete-compiled)
    ("D" desktop-remove)
    ("g" (setq debug-on-error (not debug-on-error)))
    ("l" list-processes)
    ("p" package-update-all)
    ("P" package-autoremove)
    ("Q" (let ((desktop-save nil))
           (manticore-delete-compiled)
           (desktop-remove)
           (save-buffers-kill-terminal))))

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
