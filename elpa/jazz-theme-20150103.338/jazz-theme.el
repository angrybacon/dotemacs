;;; jazz-theme.el --- A warm color theme for Emacs 24.

;; Copyright (C) 2012-2014 Roman Parykin, Bozhidar Batsov

;; Author: Roman Parykin <donderom@ymail.com>
;; URL: https://github.com/donderom/jazz-theme
;; Version: 20150103.338
;; X-Original-Version: 1.0

;; Based on zenburn-theme.el
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 1.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;   Drop the theme in a folder that is on `custom-theme-load-path'
;; and enjoy
;;
;; or
;;
;; (load-file "path/to/jazz-theme.el")
;; (load-theme 'jazz t)
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Credits
;;
;; Jani Nurminen (zenburn theme for vim)
;; Bozhidar Batsov (zenburn theme port for Emacs),
;; Christian Brassat <crshd@mail.com> (jazz theme initial colors inspiration)
;;
;;; Code:
(deftheme jazz "The Jazz color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Jazz palette
      ;; colors with +x are lighter, colors with -x are darker
      (jazz-fg       "#c6a57b")
      (jazz-fg-1     "#c6a57b")

      (jazz-bg-1     "#101010")
      (jazz-bg-05    "#151515")
      (jazz-bg       "#151515")
      (jazz-bg+1     "#202020")
      (jazz-bg+2     "#505050")
      (jazz-bg+3     "#606060")

      (jazz-red+1    "#8d4a4a")
      (jazz-red      "#953331")
      (jazz-red-1    "#953331")
      (jazz-red-2    "#953331")
      (jazz-red-3    "#953331")
      (jazz-red-4    "#953331")

      (jazz-orange   "#ba5b34")

      (jazz-yellow+1 "#96a62d")
      (jazz-yellow   "#909737")
      (jazz-yellow-1 "#909737")
      (jazz-yellow-2 "#909737")

      (jazz-green-1  "#546a29")
      (jazz-green    "#546a29")
      (jazz-green+1  "#7e9960")
      (jazz-green+2  "#7e9960")
      (jazz-green+3  "#7e9960")
      (jazz-green+4  "#7e9960")

      (jazz-cyan     "#34676f")

      (jazz-blue+1   "#5c737c")
      (jazz-blue     "#385e6b")
      (jazz-blue-1   "#385e6b")
      (jazz-blue-2   "#385e6b")
      (jazz-blue-3   "#385e6b")
      (jazz-blue-4   "#385e6b")
      (jazz-blue-5   "#385e6b")

      (jazz-magenta  "#7f355e"))
  (custom-theme-set-faces
   'jazz
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,jazz-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,jazz-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,jazz-fg :background ,jazz-bg))))
   `(cursor ((,class (:foreground ,jazz-fg :background ,jazz-fg))))
   `(escape-glyph-face ((,class (:foreground ,jazz-red))))
   `(fringe ((,class (:foreground "#303030" :background ,jazz-bg))))
   `(header-line ((,class (:foreground ,jazz-yellow
                                       :background ,jazz-bg-1
                                       :box (:line-width -1 :color ,jazz-bg :style released-button)))))
   `(highlight ((,class (:background ,jazz-bg+1))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,jazz-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,jazz-green))))
   `(compilation-error-face ((,class (:foreground ,jazz-red-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,jazz-fg))))
   `(compilation-info-face ((,class (:foreground ,jazz-blue))))
   `(compilation-info ((,class (:foreground ,jazz-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,jazz-green))))
   `(compilation-line-face ((,class (:foreground ,jazz-yellow))))
   `(compilation-line-number ((,class (:foreground ,jazz-yellow))))
   `(compilation-message-face ((,class (:foreground ,jazz-blue))))
   `(compilation-warning-face ((,class (:foreground ,jazz-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,jazz-fg))))
   `(grep-error-face ((,class (:foreground ,jazz-red-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,jazz-blue))))
   `(grep-match-face ((,class (:foreground ,jazz-orange :weight bold))))
   `(match ((,class (:background ,jazz-bg-1 :foreground ,jazz-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,jazz-yellow :background ,jazz-bg-1))))
   `(isearch-fail ((,class (:foreground ,jazz-fg :background ,jazz-red-4))))
   `(lazy-highlight ((,class (:foreground ,jazz-yellow :background ,jazz-bg+2))))

   `(menu ((,class (:foreground ,jazz-fg :background ,jazz-bg))))
   `(minibuffer-prompt ((,class (:foreground ,jazz-blue))))
   `(mode-line
     ((,class (:foreground ,jazz-fg
                           :background ,jazz-bg+1
                           :box (:line-width 5 :color ,jazz-bg+1)))))
   `(mode-line-buffer-id ((,class (:foreground ,jazz-yellow :weight bold))))
   `(mode-line-highlight ((,class (:inverse-video t))))
   `(mode-line-inactive
     ((,class (:inherit mode-line :foreground ,jazz-green-1
                        :background ,jazz-bg-1
                        :box (:line-width 5 :color ,jazz-bg-1)))))
   `(mode-line-folder-face ((,class (:foreground ,jazz-bg+2))))
   `(mode-line-modified-face ((,class (:foreground ,jazz-red))))
   `(mode-line-ro-modified-face ((,class (:foreground ,jazz-blue))))
   `(mode-line-buffer-name ((,class (:foreground ,jazz-yellow))))
   `(mode-line-mode-name ((,class (:foreground ,jazz-blue))))
   `(mode-line-mode-string ((,class (:foreground ,jazz-bg+3))))
   `(mode-line-vc-mode ((,class (:foreground ,jazz-magenta))))
   `(mode-line-minor-mode-face ((,class (:foreground ,jazz-bg+2 :height 96))))

   `(region ((,class (:background ,jazz-fg :foreground ,jazz-blue))))
   `(secondary-selection ((,class (:background ,jazz-bg+2))))
   `(trailing-whitespace ((,class (:background ,jazz-red))))
   `(vertical-border ((,class (:foreground ,jazz-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,jazz-blue))))
   `(font-lock-comment-face ((,class (:foreground ,jazz-bg+2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,jazz-bg+2))))
   `(font-lock-constant-face ((,class (:foreground ,jazz-magenta))))
   `(font-lock-doc-face ((,class (:foreground ,jazz-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,jazz-blue+1))))
   `(font-lock-function-name-face ((,class (:foreground ,jazz-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,jazz-yellow))))
   `(font-lock-negation-char-face ((,class (:foreground ,jazz-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,jazz-blue))))
   `(font-lock-string-face ((,class (:foreground ,jazz-red))))
   `(font-lock-type-face ((,class (:foreground ,jazz-yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,jazz-orange))))
   `(font-lock-warning-face ((,class (:foreground ,jazz-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,jazz-fg))))
   `(ack-file ((,class (:foreground ,jazz-blue))))
   `(ack-line ((,class (:foreground ,jazz-yellow))))
   `(ack-match ((,class (:foreground ,jazz-orange :background ,jazz-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,jazz-yellow :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,jazz-bg+3 :foreground "black"))))
   `(ac-selection-face ((,class (:background ,jazz-blue-4 :foreground ,jazz-fg))))
   `(popup-tip-face ((,class (:background ,jazz-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,jazz-blue-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,jazz-bg-1))))
   `(popup-isearch-match ((,class (:background ,jazz-bg :foreground ,jazz-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,jazz-green+4))))
   `(diff-changed ((,class (:foreground ,jazz-yellow))))
   `(diff-removed ((,class (:foreground ,jazz-red))))
   `(diff-header ((,class (:background ,jazz-bg+1))))
   `(diff-file-header
     ((,class (:background ,jazz-bg+2 :foreground ,jazz-fg :bold t))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,jazz-green+4 :background ,jazz-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,jazz-red :background ,jazz-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,jazz-yellow))))
   `(eshell-ls-archive ((,class (:foreground ,jazz-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,jazz-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,jazz-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,jazz-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,jazz-yellow))))
   `(eshell-ls-symlink ((,class (:foreground ,jazz-cyan :weight bold))))

   ;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,jazz-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,jazz-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-blue)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,jazz-blue :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-yellow-1) :inherit unspecified))
      (t (:foreground ,jazz-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-red-1) :inherit unspecified))
      (t (:foreground ,jazz-red-1 :weight bold :underline t))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,jazz-red :weight bold :underline t))))
   `(flycheck-fringe-error ((,class (:foreground ,jazz-red :background ,jazz-bg))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,jazz-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,jazz-orange :weight bold :underline t))))
   `(flycheck-fringe-warning ((,class (:foreground ,jazz-orange :background ,jazz-bg))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,jazz-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,jazz-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,jazz-yellow))))
   `(erc-keyword-face ((,class (:foreground ,jazz-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,jazz-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,jazz-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,jazz-green))))
   `(erc-pal-face ((,class (:foreground ,jazz-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,jazz-orange :background ,jazz-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,jazz-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,jazz-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,jazz-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,jazz-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,jazz-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,jazz-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,jazz-blue))))
   `(gnus-summary-low-read ((t (:foreground ,jazz-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,jazz-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,jazz-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,jazz-blue))))
   `(gnus-summary-normal-read ((,class (:foreground ,jazz-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,jazz-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,jazz-fg))))
   `(gnus-summary-selected ((,class (:foreground ,jazz-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,jazz-blue))))
   `(gnus-cite-10 ((,class (:foreground ,jazz-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,jazz-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,jazz-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,jazz-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,jazz-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,jazz-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,jazz-green))))
   `(gnus-cite-7 ((,class (:foreground ,jazz-red))))
   `(gnus-cite-8 ((,class (:foreground ,jazz-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,jazz-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,jazz-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,jazz-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,jazz-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,jazz-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,jazz-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,jazz-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,jazz-bg+2))))
   `(gnus-signature ((,class (:foreground ,jazz-yellow))))
   `(gnus-x ((,class (:background ,jazz-fg :foreground ,jazz-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,jazz-green
                           :background ,jazz-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,jazz-yellow
                           :background ,jazz-bg-1
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,jazz-bg-1))))
   `(helm-selection-line ((,class (:background ,jazz-bg-1))))
   `(helm-visible-mark ((,class (:foreground ,jazz-bg :background ,jazz-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,jazz-green+4 :background ,jazz-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,jazz-bg-1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,jazz-yellow :weight normal))))
   `(ido-only-match ((,class (:foreground ,jazz-orange :weight normal))))
   `(ido-subdir ((,class (:foreground ,jazz-red))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,jazz-orange))))
   `(js2-error-face ((,class (:foreground ,jazz-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,jazz-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,jazz-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,jazz-green+3))))
   `(js2-function-param-face ((,class (:foreground, jazz-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,jazz-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,jazz-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,jazz-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,jazz-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,jazz-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,jazz-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,jazz-red+1))))
   `(jabber-activity-face((,class (:foreground ,jazz-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,jazz-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,jazz-bg+2 :background ,jazz-bg-1))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,jazz-yellow :weight bold :box nil :background ,jazz-bg))))
   `(magit-branch ((,class (:foreground ,jazz-orange :weight bold :box nil :background ,jazz-bg))))
   `(magit-item-highlight ((t (:background ,jazz-bg+1))))

   ;; markdown
   `(markdown-header-face ((,class (:inherit variable-pitch))))
   `(markdown-header-face-1 ((,class (:height 200 :inherit markdown-header-face))))
   `(markdown-header-face-2 ((,class (:height 150 :inherit markdown-header-face))))
   `(markdown-header-face-3 ((,class (:height 100 :inherit markdown-header-face))))
   `(markdown-header-face-4 ((,class (:height 90  :inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,jazz-green+1))))
   `(message-header-other ((,class (:foreground ,jazz-green))))
   `(message-header-to ((,class (:foreground ,jazz-yellow :weight bold))))
   `(message-header-from ((,class (:foreground ,jazz-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,jazz-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,jazz-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,jazz-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,jazz-green))))
   `(message-mml ((,class (:foreground ,jazz-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,jazz-orange))))
   `(mew-face-header-from ((,class (:foreground ,jazz-yellow))))
   `(mew-face-header-date ((,class (:foreground ,jazz-green))))
   `(mew-face-header-to ((,class (:foreground ,jazz-red))))
   `(mew-face-header-key ((,class (:foreground ,jazz-green))))
   `(mew-face-header-private ((,class (:foreground ,jazz-green))))
   `(mew-face-header-important ((,class (:foreground ,jazz-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,jazz-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,jazz-red))))
   `(mew-face-header-xmew ((,class (:foreground ,jazz-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,jazz-red))))
   `(mew-face-body-url ((,class (:foreground ,jazz-orange))))
   `(mew-face-body-comment ((,class (:foreground ,jazz-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,jazz-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,jazz-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,jazz-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,jazz-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,jazz-red))))
   `(mew-face-mark-review ((,class (:foreground ,jazz-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,jazz-green))))
   `(mew-face-mark-delete ((,class (:foreground ,jazz-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,jazz-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,jazz-green))))
   `(mew-face-mark-unread ((,class (:foreground ,jazz-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,jazz-green))))
   `(mew-face-eof-part ((,class (:foreground ,jazz-yellow))))

   ;; minimap
   `(minimap-font-face ((default (:height 30 :family "Anka/Coder")) (nil nil)))
   `(minimap-semantic-function-face ((((background dark)) (:inherit (font-lock-function-name-face minimap-font-face) :background "gray10"))))
   `(minimap-semantic-type-face ((,class (:inherit (font-lock-type-face minimap-font-face) :background "gray10"))))
   `(minimap-semantic-variable-face ((,class (:inherit (font-lock-variable-name-face minimap-font-face) :background "gray10"))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,jazz-cyan :background ,jazz-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,jazz-bg :background ,jazz-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,jazz-bg :background ,jazz-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,jazz-yellow))))
   `(nav-face-button-num ((,class (:foreground ,jazz-cyan))))
   `(nav-face-dir ((,class (:foreground ,jazz-green))))
   `(nav-face-hdir ((,class (:foreground ,jazz-red))))
   `(nav-face-file ((,class (:foreground ,jazz-fg))))
   `(nav-face-hfile ((,class (:foreground ,jazz-red-4))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,jazz-fg :weight bold))))
   `(org-checkbox ((,class (:background ,jazz-bg+2 :foreground "white"
                                        :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,jazz-blue :underline t))))
   `(org-deadline-announce ((,class (:foreground ,jazz-red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,jazz-green+3))))
   `(org-formula ((,class (:foreground ,jazz-yellow-2))))
   `(org-headline-done ((,class (:foreground ,jazz-green+3))))
   `(org-hide ((,class (:foreground ,jazz-bg-1))))
   `(org-level-1 ((,class (:foreground ,jazz-orange))))
   `(org-level-2 ((,class (:foreground ,jazz-green+1))))
   `(org-level-3 ((,class (:foreground ,jazz-blue-1))))
   `(org-level-4 ((,class (:foreground ,jazz-yellow-2))))
   `(org-level-5 ((,class (:foreground ,jazz-cyan))))
   `(org-level-6 ((,class (:foreground ,jazz-green-1))))
   `(org-level-7 ((,class (:foreground ,jazz-red-4))))
   `(org-level-8 ((,class (:foreground ,jazz-blue-4))))
   `(org-link ((,class (:foreground ,jazz-yellow-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,jazz-green+4))))
   `(org-scheduled-previously ((,class (:foreground ,jazz-red-4))))
   `(org-scheduled-today ((,class (:foreground ,jazz-blue+1))))
   `(org-special-keyword ((,class (:foreground ,jazz-yellow-1))))
   `(org-table ((,class (:foreground ,jazz-green+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,jazz-orange))))
   `(org-todo ((,class (:bold t :foreground ,jazz-red :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,jazz-red :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,jazz-cyan))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,jazz-yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,jazz-blue+1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,jazz-red+1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,jazz-orange))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,jazz-blue-1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,jazz-green+4))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,jazz-red-3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,jazz-yellow-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,jazz-green+2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,jazz-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,jazz-red-4))))

   ;; rcirc
   `(rcirc-my-nick ((,class (:foreground ,jazz-blue))))
   `(rcirc-other-nick ((,class (:foreground ,jazz-orange))))
   `(rcirc-bright-nick ((,class (:foreground ,jazz-blue+1))))
   `(rcirc-dim-nick ((,class (:foreground ,jazz-blue-2))))
   `(rcirc-server ((,class (:foreground ,jazz-green))))
   `(rcirc-server-prefix ((,class (:foreground ,jazz-green+1))))
   `(rcirc-timestamp ((,class (:foreground ,jazz-green+2))))
   `(rcirc-nick-in-message ((,class (:foreground ,jazz-yellow))))
   `(rcirc-nick-in-message-full-line ((,class (:bold t))))
   `(rcirc-prompt ((,class (:foreground ,jazz-yellow :bold t))))
   `(rcirc-track-nick ((,class (:inverse-video t))))
   `(rcirc-track-keyword ((,class (:bold t))))
   `(rcirc-url ((,class (:bold t))))
   `(rcirc-keyword ((,class (:foreground ,jazz-yellow :bold t))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,jazz-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,jazz-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,jazz-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,jazz-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,jazz-red))))
   `(rpm-spec-package-face ((,class (:foreground ,jazz-red))))
   `(rpm-spec-section-face ((,class (:foreground ,jazz-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,jazz-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,jazz-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,jazz-orange))))
   `(rst-level-2-face ((,class (:foreground ,jazz-green+1))))
   `(rst-level-3-face ((,class (:foreground ,jazz-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,jazz-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,jazz-cyan))))
   `(rst-level-6-face ((,class (:foreground ,jazz-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,jazz-red-3 :background ,jazz-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,jazz-blue-1 :background ,jazz-bg :weight bold))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,jazz-red))))

   ;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,jazz-blue))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,jazz-red :weight bold))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,jazz-fg))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,jazz-yellow))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground, jazz-fg))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,jazz-bg :foreground ,jazz-bg+1))))
   `(whitespace-hspace ((,class (:background ,jazz-bg :foreground ,jazz-bg+1))))
   `(whitespace-tab ((,class (:background ,jazz-bg :foreground ,jazz-red))))
   `(whitespace-newline ((,class (:foreground ,jazz-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,jazz-red :background ,jazz-bg))))
   `(whitespace-line ((,class (:background ,jazz-bg-05 :foreground ,jazz-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,jazz-orange :foreground ,jazz-orange))))
   `(whitespace-indentation ((,class (:background ,jazz-yellow :foreground ,jazz-red))))
   `(whitespace-empty ((,class (:background ,jazz-yellow :foreground ,jazz-red))))
   `(whitespace-space-after-tab ((,class (:background ,jazz-yellow :foreground ,jazz-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,jazz-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,jazz-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,jazz-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,jazz-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,jazz-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,jazz-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,jazz-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,jazz-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,jazz-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,jazz-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,jazz-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,jazz-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,jazz-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,jazz-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,jazz-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,jazz-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,jazz-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,jazz-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,jazz-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,jazz-fg
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,jazz-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,jazz-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,jazz-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,jazz-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,jazz-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; ensime
   `(ensime-errline-highlight ((,class (:background ,jazz-red :foreground ,jazz-fg))))
   `(ensime-warnline ((,class (:background ,jazz-bg+1))))
   
   ;; coffee-mode
   `(coffee-mode-function-param ((,class (:foreground ,jazz-blue+1))))
   `(coffee-mode-class-name ((,class (:foreground ,jazz-blue))))
   
   ;; which-func-mode
   `(which-func ((,class (:foreground ,jazz-green+4)))))

  ;;; custom theme variables
  (custom-theme-set-variables
   'jazz
   `(ansi-color-names-vector [,jazz-bg ,jazz-red ,jazz-green ,jazz-yellow
                                       ,jazz-blue ,jazz-magenta ,jazz-cyan ,jazz-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,jazz-bg-05)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jazz)

;;; jazz-theme.el ends here.
