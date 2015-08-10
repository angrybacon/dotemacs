company-mode backend providing autocompletion for emoji. 🆒💦

### setup

Add `company-emoji.el` to your load-path, then add something like the
following to your init file (`company-emoji-init` doesn’t start
company-mode):

(require 'company-emoji)
(add-hook 'markdown-mode-hook 'company-mode)
(add-hook 'markdown-mode-hook 'company-emoji-init)

After selecting an emoji-word from the completion-list, it will be
replaced by the real unicode emoji (`:cactus:` becomes 🌵, `:cat:`
becomes 🐱, etc.)

### custom variables

#### aliases

You can add shortcode aliases by modifying `company-emoji-aliases`.
Run `M-x customize-variable [RET] company-emoji-aliases` to bring up
company-emoji’s customization pane, then add or remove aliases to your
taste:

Hide Company Emoji Aliases:
[INS] [DEL] Symbol: :man-woman-boy:
            String: :family:
[INS] [DEL] Symbol: :woman-kiss-man:
            String: :couplekiss:
[INS] [DEL] Symbol: :woman_man_holding_hands:
            String: :couple:
[INS] [DEL] Symbol: :woman-heart-man:
            String: :couple_with_heart:
[INS] [DEL] Symbol: :oop:
            String: :speak_no_evil:
[INS]

(“Symbol” designates the user-defined alias, and “string” designates
the original shortcode you want your alias to mimick.)

#### unicode replacement

By default, `:cat:` is replaced with 🐱 upon completion, but that can
be turned off by setting the variable `company-emoji-insert-unicode`
to `nil`.

### cocoa/ns emacs

If you’re using the cocoa version of Emacs (i.e., if built
 `‐-with-ns`, or `--with-cocoa` using Homebrew), you’ll need to add
 something like this to your init file:

(defun darwin-set-emoji-font (frame)
"Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
  (if (eq system-type 'darwin)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)))
For when emacs is started with Emacs.app
(darwin-set-emoji-font nil)
Hook for when a cocoa frame is created with emacsclient
see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 'darwin-set-emoji-font)
