;;; autopair.el --- Automagically pair braces and quotes like TextMate

;; Copyright (C) 2009,2010 Joao Tavora

;; Author: Joao Tavora <joaotavora [at] gmail.com>
;; Keywords: convenience, emulations
;; X-URL: http://autopair.googlecode.com
;; URL: http://autopair.googlecode.com
;; EmacsWiki: AutoPairs
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 20140825.427
;; X-Original-Version: 0.6.1

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
;; Another stab at making braces and quotes pair like in
;; TextMate:
;;
;; * Opening braces/quotes are autopaired;
;; * Closing braces/quotes are autoskipped;
;; * Backspacing an opening brace/quote autodeletes its adjacent pair.
;; * Newline between newly-opened brace pairs open an extra indented line.
;;
;; Autopair deduces from the current syntax table which characters to
;; pair, skip or delete.
;;
;;; Installation:
;;
;;   (require 'autopair)
;;   (autopair-global-mode) ;; to enable in all buffers
;;
;; To enable autopair in just some types of buffers, comment out the
;; `autopair-global-mode' and put autopair-mode in some major-mode
;; hook, like:
;;
;; (add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
;;
;; Alternatively, do use `autopair-global-mode' and create
;; *exceptions* using the `autopair-dont-activate' local variable (for
;; emacs < 24), or just using (autopair-mode -1) (for emacs >= 24)
;; like:
;;
;; (add-hook 'c-mode-common-hook
;;           #'(lambda ()
;;             (setq autopair-dont-activate t)
;;             (autopair-mode -1)))
;;
;;
;;; Use:
;;
;; The extension works by rebinding the braces and quotes keys, but
;; can still be minimally intrusive, since the original binding is
;; always called as if autopair did not exist.
;;
;; The decision of which keys to actually rebind is taken at
;; minor-mode activation time, based on the current major mode's
;; syntax tables. To achieve this kind of behaviour, an emacs
;; variable `emulation-mode-map-alists' was used.
;;
;; If you set `autopair-pair-criteria' and `autopair-skip-criteria' to
;; 'help-balance (which, by the way, is the default), braces are not
;; autopaired/autoskiped in all situations; the decision to autopair
;; or autoskip a brace is taken according to the following table:
;;
;;  +---------+------------+-----------+-------------------+
;;  | 1234567 | autopair?  | autoskip? | notes             |
;;  +---------+------------+-----------+-------------------+
;;  |  (())   |  yyyyyyy   |  ---yy--  | balanced          |
;;  +---------+------------+-----------+-------------------+
;;  |  (()))  |  ------y   |  ---yyy-  | too many closings |
;;  +---------+------------+-----------+-------------------+
;;  |  ((())  |  yyyyyyy   |  -------  | too many openings |
;;  +---------+------------+-----------+-------------------+
;;
;; The table is read like this: in a buffer with 7 characters laid out
;; like the first column, an "y" marks points where an opening brace
;; is autopaired and in which places would a closing brace be
;; autoskipped.
;;
;; Quote pairing tries to support similar "intelligence", but is less
;; deterministic. Some inside-string or inside-comment situations may
;; not always behave how you intend them to.
;;
;; The variable `autopair-autowrap' tells autopair to automatically
;; wrap the selection region with the delimiters you're trying to
;; insert. This is done conditionally based of syntaxes of the two
;; ends of the selection region. It is compatible with `cua-mode's
;; typing-deletes-selection behaviour.
;;
;; If you find the paren-blinking annoying, turn `autopair-blink' to
;; nil.
;;
;; For lisp-programming you might also like `autopair-skip-whitespace'.
;;
;; For further customization have a look at `autopair-dont-pair',
;; `autopair-handle-action-fns' and `autopair-extra-pairs'.
;;
;; `autopair-dont-pair' lets you define special cases of characters
;; you don't want paired.  Its default value skips pairing
;; single-quote characters when inside a comment literal, even if the
;; language syntax tables does pair these characters.
;;
;; (defvar autopair-dont-pair `(:string (?') :comment  (?'))
;;
;; As a further example, to also prevent the '{' (opening brace)
;; character from being autopaired in C++ comments use this in your
;; .emacs.
;;
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;                (push ?{
;;                      (cl-getf autopair-dont-pair :comment))))
;;
;; `autopair-handle-action-fns' lets you override/extend the actions
;; taken by autopair after it decides something must be paired,skipped
;; or deleted. To work with triple quoting in python mode, you can use
;; this for example:
;;
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))
;;
;; It's also useful to deal with latex's mode use of the "paired
;; delimiter" syntax class.
;;
;; (add-hook 'latex-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'autopair-handle-action-fns)
;;                    (list #'autopair-default-handle-action
;;                          #'autopair-latex-mode-paired-delimiter-action))))
;;
;; `autopair-extra-pairs' lets you define extra pairing and skipping
;; behaviour for pairs not programmed into the syntax table. Watch
;; out, this is work-in-progress, a little unstable and does not help
;; balancing at all. To have '<' and '>' pair in c++-mode buffers, but
;; only in code, use:
;;
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (cl-getf autopair-extra-pairs :code))))
;;
;; if you program in emacs-lisp you might also like the following to
;; pair backtick and quote
;;
;; (add-hook 'emacs-lisp-mode-hook
;;           #'(lambda ()
;;               (push '(?` . ?')
;;                     (cl-getf autopair-extra-pairs :comment))
;;               (push '(?` . ?')
;;                     (cl-getf autopair-extra-pairs :string))))
;;
;;; Bugs:
;;
;; * Quote pairing/skipping inside comments is not perfect...
;;
;; * See the last section on monkey-patching for the `defadvice'
;;   tricks used to make `autopair-autowrap' work with `cua-mode' and
;;   `delete-selection-mode'.
;;
;;; Credit:
;;
;; Thanks Ed Singleton for early testing.
;;
;;; Code:

;; requires
(require 'cl-lib)
(require 'paren)

(defgroup autopair nil
  "Automagically pair braces and quotes"
  :group 'convenience)

;; variables
(defcustom autopair-pair-criteria 'help-balance
  "How to decide whether to pair opening brackets or quotes.

Set this to 'always to always pair, or 'help-balance to be more
criterious when pairing."
  :group 'autopair
  :type '(choice (const :tag "Help balance" help-balance)
                 (const :tag "Always pair" always)))

(defcustom autopair-skip-criteria 'help-balance
  "How to decide whether to skip closing brackets or quotes.

Set this to 'always to always skip, or 'help-balance to be more
criterious when skipping."
  :group 'autopair
  :type '(choice (const :tag "Help balance" help-balance)
                 (const :tag "Always skip" always)))

(defcustom autopair-autowrap 'help-balance
  "If non-nil autopair attempts to wrap the selected region.

This is also done in an optimistic \"try-to-balance\" fashion.
Set this to to 'help-balance to be more criterious when
wrapping."
  :group 'autopair
  :type '(choice (const :tag "Do wrap" t)
                 (const :tag "Do not wrap" nil)
                 (const :tag "Help Balance" 'help-balance)))

(defvar autopair--emulation-alist nil
  "A keymap alist for adding to `emulation-mode-map-alists'.

The alist contains single (t MAP) association, where MAP is a
dynamic keymap set mostly from the major mode's syntax table.")

(unless (eval-when-compile (> emacs-major-version 23))
  (defvar autopair-dont-activate nil
    "Control activation of `autopair-global-mode'.

Set this to a non-nil value to skip activation of `autopair-mode'
in certain contexts.  If however the value satisfies `functionp'
and is a function of no arguments, the function is called and it is
the return value that decides.")
  (make-variable-buffer-local 'autopair-dont-activate))

(defvar autopair-extra-pairs nil
  "Extra pairs for which to use pairing.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , PAIRS). PAIRS is a mixed
list whose elements are cons cells, which look like cells look
like (OPENING . CLOSING). Autopair pairs these like
parenthesis.

TYPE can be one of:

:string : whereby PAIRS will be considered only when inside a
          string literal

:comment : whereby PAIRS will be considered only when inside a comment

:code : whereby PAIRS will be considered only when outisde a
        string and a comment.

:everywhere : whereby PAIRS will be considered in all situations

In Emacs-lisp, this might be useful

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq autopair-extra-pairs `(:comment ((?`. ?'))))))


Note that this does *not* work for single characters,
e.x. characters you want to behave as quotes.  See the
docs/source comments for more details.")

(make-variable-buffer-local 'autopair-extra-pairs)

(defvar autopair-dont-pair `(:string (?') :comment  (?'))
  "Characters for which to skip any pairing behaviour.

This variable overrides `autopair-pair-criteria' and
`autopair-extra-pairs'. It does not
  (currently) affect the skipping behaviour.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , CHARS). CHARS is a list of
characters and TYPE can be one of:

:string : whereby characters in CHARS will not be autopaired when
          inside a string literal

:comment : whereby characters in CHARS will not be autopaired when
          inside a comment

:never : whereby characters in CHARS won't even have their
         bindings replaced by autopair's. This particular option
         should be used for troubleshooting and requires
         `autopair-mode' to be restarted to have any effect.")
(make-variable-buffer-local 'autopair-dont-pair)

(defvar autopair-action nil
  "Autopair action decided on by last interactive autopair command, or nil.

When autopair decides on an action this is a list whose first
three elements are (ACTION PAIR POS-BEFORE).

ACTION is one of `opening', `insert-quote', `skip-quote',
`backspace', `newline' or `paired-delimiter'. PAIR is the pair of
the `autopair--inserted' character, if applicable. POS-BEFORE is
value of point before action command took place .")


(defvar autopair-wrap-action nil
  "Autowrap action decided on by autopair, if any.

When autopair decides on an action this is a list whose first
three elements are (ACTION PAIR POS-BEFORE REGION-BEFORE).

ACTION can only be `wrap' currently. PAIR and POS-BEFORE
delimiter are as in `autopair-action'. REGION-BEFORE is a cons
cell with the bounds of the region before the command takes
place")

(defvar autopair-handle-action-fns '()
  "Autopair handlers to run *instead* of the default handler.

Each element is a function taking three arguments (ACTION, PAIR
and POS-BEFORE), which are the three elements of the
`autopair-action' variable, which see.

If non-nil, these functions are called *instead* of the single
function `autopair-default-handle-action', so use this variable
to specify special behaviour. To also run the default behaviour,
be sure to include `autopair-default-handle-action' in the
list, or call it from your handlers.")
(make-variable-buffer-local 'autopair-handle-action-fns)

(defvar autopair-handle-wrap-action-fns '()
  "Autopair wrap handlers to run *instead* of the default handler.

Each element is a function taking four arguments (ACTION, PAIR,
POS-BEFORE and REGION-BEFORE), which are the three elements of the
`autopair-wrap-action' variable, which see.

If non-nil, these functions are called *instead* of the single
function `autopair-default-handle-wrap-action', so use this
variable to specify special behaviour. To also run the default
behaviour, be sure to include `autopair-default-handle-wrap-action' in
the list, or call it in your handlers.")
(make-variable-buffer-local 'autopair-handle-wrap-action-fns)

(defvar autopair-inserted nil
  "Delimiter inserted by last interactive autopair command.

This is calculated with `autopair-calculate-inserted', which see.")

(defun autopair--calculate-inserted ()
  "Attempts to guess the delimiter the current command is inserting.

For now, simply returns `last-command-event'"
  last-command-event)

;; minor mode and global mode
;;
;;;###autoload
(define-minor-mode autopair-mode
  "Automagically pair braces and quotes like in TextMate."
  nil " pair" nil
  (cond (autopair-mode
         ;; Setup the dynamic emulation keymap, i.e. sets `autopair--emulation-alist'
         ;;
         (autopair--set-emulation-bindings)
         (add-to-list 'emulation-mode-map-alists 'autopair--emulation-alist 'append)
         ;; Init important vars
         ;;
         (setq autopair-action nil)
         (setq autopair-wrap-action nil)
         ;; Add the post command handler
         ;;
         (add-hook 'post-command-hook 'autopair--post-command-handler nil 'local))
        (t
         (set (make-local-variable 'autopair--emulation-alist) nil)
         (remove-hook 'post-command-hook         'autopair--post-command-handler 'local))))

;;;###autoload
(define-globalized-minor-mode autopair-global-mode autopair-mode autopair-on)

(when (eval-when-compile (>= emacs-major-version 24))
  (defvar autopair--global-mode-emacs24-hack-flag nil)
  (defadvice autopair-global-mode-enable-in-buffers (before autopairs-global-mode-emacs24-hack activate)
    "Monkey patch for recent emacsen 24.

It's impossible for a globalized minor-mode to see variables set
by major-mode-hooks. However, the auto-generated
`autopair-global-mode-enable-in-buffers' does run after the
major-mode-hooks.

This advice makes sure the emulation keybindings are (re)set
there. It relies on the fact that
`autopair-global-mode-enable-in-buffers' is still called again in
`after-change-major-mode-hook' (but the autopair-mode has already
been turned on before the major-mode hooks kicked in).

We want this advice to only kick in the *second* call to
`autopair-global-mode-enable-in-buffers'."
    (dolist (buf autopair-global-mode-buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and autopair-mode
                     (not autopair--global-mode-emacs24-hack-flag))
            (autopair--set-emulation-bindings)
            (set (make-local-variable 'autopair--global-mode-emacs24-hack-flag) t)))))))

(defun autopair-on ()
  (unless (or buffer-read-only
              (and (not (minibufferp))
                   (string-match "^ \\*" (buffer-name)))
              (eq major-mode 'sldb-mode)
              (and (eval-when-compile (< emacs-major-version 24))
                   (boundp 'autopair-dont-activate)
                   autopair-dont-activate)
              (autopair-mode 1))))

(defun autopair--set-emulation-bindings ()
  "Setup keymap MAP with keybindings based on the major-mode's
syntax table and the local value of `autopair-extra-pairs'."
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-backward-char] 'autopair-backspace)
    (define-key map [remap backward-delete-char-untabify] 'autopair-backspace)
    (define-key map "\177" 'autopair-backspace)
    (define-key map "\r" 'autopair-newline)
    (dotimes (char 256) ;; only searches the first 256 chars,
      ;; TODO: is this enough/toomuch/stupid?
      (unless (member char
                      (cl-getf autopair-dont-pair :never))
        (let* ((syntax-entry (aref (syntax-table) char))
               (class (and syntax-entry
                           (syntax-class syntax-entry)))
               (pair (and syntax-entry
                          (cdr syntax-entry))))
          (cond ((and (eq class (car (string-to-syntax "(")))
                      pair)
                 ;; syntax classes "opening parens" and "close parens"
                 (define-key map (string char) 'autopair-insert-opening)
                 (define-key map (string pair) 'autopair-skip-close-maybe))
                ((eq class (car (string-to-syntax "\"")))
                 ;; syntax class "string quote
                 (define-key map (string char) 'autopair-insert-or-skip-quote))
                ((eq class (car (string-to-syntax "$")))
                 ;; syntax class "paired-delimiter"
                 ;;
                 ;; Apropos this class, see Issues 18, 25 and
                 ;; elisp info node "35.2.1 Table of Syntax
                 ;; Classes". The fact that it supresses
                 ;; syntatic properties in the delimited region
                 ;; dictates that deciding to autopair/autoskip
                 ;; can't really be as clean as the string
                 ;; delimiter.
                 ;;
                 ;; Apparently, only `TeX-mode' uses this, so
                 ;; the best is to bind this to
                 ;; `autopair-insert-or-skip-paired-delimiter'
                 ;; which defers any decision making to
                 ;; mode-specific post-command handler
                 ;; functions.
                 ;;
                 (define-key map (string char) 'autopair-insert-or-skip-paired-delimiter))))))
    ;; read `autopair-extra-pairs'
    ;;
    (dolist (pairs-list (cl-remove-if-not #'listp autopair-extra-pairs))
      (dolist (pair pairs-list)
        (define-key map (string (car pair)) 'autopair-extra-insert-opening)
        (define-key map (string (cdr pair)) 'autopair-extra-skip-close-maybe)))

    (set (make-local-variable 'autopair--emulation-alist) (list (cons t map)))))

;; helper functions
;;
(defun autopair--syntax-ppss ()
  "Calculate syntax info relevant to autopair.

A list of four elements is returned:

- SYNTAX-INFO is either the result `syntax-ppss' or the result of
  calling `parse-partial-sexp' with the appropriate
  bounds (previously calculated with `syntax-ppss'.

- WHERE-SYM can be one of the symbols :string, :comment or :code.

- QUICK-SYNTAX-INFO is always the result returned by `syntax-ppss'.

- BOUNDS are the boudaries of the current string or comment if
  we're currently inside one."
  (let* ((quick-syntax-info (syntax-ppss))
         (string-or-comment-start (nth 8 quick-syntax-info)))
    (cond (;; inside a string, recalculate
           (nth 3 quick-syntax-info)
           (list (parse-partial-sexp (1+ string-or-comment-start) (point))
                 :string
                 quick-syntax-info
                 (cons string-or-comment-start
                       (condition-case nil
                           (scan-sexps string-or-comment-start 1)
                         (scan-error nil)))))
          ((nth 4 quick-syntax-info)
           (list (parse-partial-sexp (1+ (nth 8 quick-syntax-info)) (point))
                 :comment
                 quick-syntax-info))
          (t
           (list quick-syntax-info
                 :code
                 quick-syntax-info)))))

(defun autopair--pair-of (delim &optional closing)
  (when (and delim
             (integerp delim))
    (let ((syntax-entry (aref (syntax-table) delim)))
      (cond ((eq (syntax-class syntax-entry) (car (string-to-syntax "(")))
             (cdr syntax-entry))
            ((or (eq (syntax-class syntax-entry) (car (string-to-syntax "\"")))
                 (eq (syntax-class syntax-entry) (car (string-to-syntax "$"))))
             delim)
            ((and (not closing)
                  (eq (syntax-class syntax-entry) (car (string-to-syntax ")"))))
             (cdr syntax-entry))
            (autopair-extra-pairs
             (cl-some #'(lambda (pair-list)
                          (cl-some #'(lambda (pair)
                                       (cond ((eq (cdr pair) delim) (car pair))
                                             ((eq (car pair) delim) (cdr pair))))
                                   pair-list))
                      (cl-remove-if-not #'listp autopair-extra-pairs)))))))

(defun autopair--calculate-wrap-action ()
  (when (and transient-mark-mode mark-active)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (save-excursion
      (let* ((region-before (cons (region-beginning)
                                  (region-end)))
             (point-before (point))
             (start-syntax (syntax-ppss (car region-before)))
             (end-syntax   (syntax-ppss (cdr region-before))))
        (when (or (not (eq autopair-autowrap 'help-balance))
                  (and (eq (nth 0 start-syntax) (nth 0 end-syntax))
                       (eq (nth 3 start-syntax) (nth 3 end-syntax))))
          (list 'wrap (or (cl-second autopair-action)
                          (autopair--pair-of autopair-inserted))
                point-before
                region-before))))))

(defun autopair--original-binding (fallback-keys)
  (or (key-binding `[,autopair-inserted])
      (key-binding (this-single-command-keys))
      (key-binding fallback-keys)))

(defvar autopair--this-command nil)
(defun autopair--fallback (&optional fallback-keys)
  (let* ((autopair--emulation-alist nil)
         (beyond-cua (let ((cua--keymap-alist nil))
                       (autopair--original-binding fallback-keys)))
         (beyond-autopair (autopair--original-binding fallback-keys)))
    (when autopair-autowrap
      (setq autopair-wrap-action (autopair--calculate-wrap-action)))

    (setq autopair--this-command this-command)
    (setq this-original-command beyond-cua)
    ;; defer to "paredit-mode" if that is installed and running
    (when (and (featurep 'paredit)
               (symbolp beyond-cua)
               (string-match "paredit" (symbol-name beyond-cua)))
      (setq autopair-action nil))
    (let ((cua-delete-selection (not autopair-autowrap))
          (blink-matching-paren (not autopair-action)))
      (call-interactively beyond-autopair))))

(defcustom autopair-skip-whitespace nil
  "If non-nil also skip over whitespace when skipping closing delimiters.

If set to 'chomp, this will be most useful in lisp-like languages
where you want lots of )))))...."
  :group 'autopair
  :type 'boolean)

(defcustom autopair-blink (if (boundp 'blink-matching-paren)
                              blink-matching-paren
                            t)
  "If non-nil autopair blinks matching delimiters."
  :group 'autopair
  :type 'boolean)

(defcustom autopair-blink-delay 0.1
  "Autopair's blink-the-delimiter delay."
  :group 'autopair
  :type 'float)

(defun autopair--document-bindings (&optional fallback-keys)
  (concat
   "Works by scheduling possible autopair behaviour, then calls
original command as if autopair didn't exist"
   (when (eq this-command 'describe-key)
     (let* ((autopair--emulation-alist nil)
            (command (or (key-binding (this-single-command-keys))
                         (key-binding fallback-keys))))
       (when command
         (format ", which in this case is `%s'" command))))
   "."))

(defun autopair--escaped-p (syntax-info)
  (nth 5 syntax-info))

(defun autopair--exception-p (where-sym exception-where-sym blacklist &optional fn)
  (and (or (eq exception-where-sym :everywhere)
           (eq exception-where-sym where-sym))
       (member autopair-inserted
               (if fn
                   (mapcar fn (cl-getf blacklist exception-where-sym))
                 (cl-getf blacklist exception-where-sym)))))

(defun autopair--forward-sexp (arg)
  (forward-sexp arg)
  (cond ((cl-plusp arg)
	 (skip-syntax-backward "'"))
	(t
	 (skip-syntax-forward "'"))))

(defun autopair--find-pair (direction)
  "Compute (MATCHED START END) for the pair of the delimiter at point.

With positive DIRECTION consider the delimiter after point and
travel forward, otherwise consider the delimiter is just before
point and travel backward."
  (let* ((show-paren-data (and nil
                               (funcall show-paren-data-function)))
         (here (point)))
    (cond (show-paren-data
           (cl-destructuring-bind (here-beg here-end there-beg there-end mismatch)
               show-paren-data
             (if (cl-plusp direction)
                 (list (not mismatch) there-end here-beg)
               (list (not mismatch) there-beg here-end))))
          (t
           (condition-case move-err
               (save-excursion
                 (autopair--forward-sexp (if (cl-plusp direction) 1 -1))
                 (list (if (cl-plusp direction)
                           (eq (char-after here)
                               (autopair--pair-of (char-before (point))))
                         (eq (char-before here)
                             (autopair--pair-of (char-after (point)))))
                       (point) here))
             (scan-error
              (list nil (nth 2 move-err) here)))))))

(defun autopair--up-list (&optional n)
  "Try to up-list forward as much as N lists.

With negative N, up-list backward.

Return a cons of two descritions (MATCHED START END) for the
innermost and outermost lists that enclose point. The outermost
list enclosing point is either the first top-level or mismatched
list found by uplisting."
  (save-excursion
    (cl-loop with n = (or n (point-max))
             for i from 0 below (abs n)
             with outermost
             with innermost
             until outermost
             do
             (condition-case forward-err
                 (progn
                   (scan-sexps (point) (if (cl-plusp n)
                                           (point-max)
                                         (- (point-max))))
                   (unless innermost
                     (setq innermost (list t)))
                   (setq outermost (list t)))
               (scan-error
                (goto-char
                 (if (cl-plusp n)
                     ;; HACK: the reason for this `max' is that some
                     ;; modes like ruby-mode sometimes mis-report the
                     ;; scan error when `forward-sexp'eeing too-much, its
                     ;; (nth 3) should at least one greater than its (nth
                     ;; 2). We really need to move out of the sexp so
                     ;; detect this and add 1. If this were fixed we
                     ;; could move to (nth 3 forward-err) in all
                     ;; situations.
                     ;;
                     (max (1+ (nth 2 forward-err))
                          (nth 3 forward-err))
                   (nth 3 forward-err)))
                (let ((pair-data (autopair--find-pair (- n))))
                  (unless innermost
                    (setq innermost pair-data))
                  (unless (cl-first pair-data)
                    (setq outermost pair-data)))))
             finally (cl-return (cons innermost outermost)))))

;; interactive commands and their associated predicates
;;
(defun autopair-insert-or-skip-quote ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (let* ((syntax-triplet (autopair--syntax-ppss))
         (syntax-info (cl-first syntax-triplet))
         (where-sym (cl-second syntax-triplet))
         (orig-info (cl-third syntax-triplet))
         ;; inside-string may the quote character itself or t if this
         ;; is a "generically terminated string"
         (inside-string (and (eq where-sym :string)
                             (cl-fourth orig-info)))
         (escaped-p (autopair--escaped-p syntax-info))

         )
    (cond (;; decides whether to skip the quote...
           ;;
           (and (not escaped-p)
                (eq autopair-inserted (char-after (point)))
                (or
                 ;; ... if we're already inside a string and the
                 ;; string starts with the character just inserted,
                 ;; or it's a generically terminated string
                 (and inside-string
                      (or (eq inside-string t)
                          (eq autopair-inserted inside-string)))
                 ;; ... if we're in a comment and ending a string
                 ;; (the inside-string criteria does not work
                 ;; here...)
                 (and (eq where-sym :comment)
                      (condition-case nil
                          (eq autopair-inserted (char-after (scan-sexps (1+ (point)) -1)))
                        (scan-error nil)))))
           (setq autopair-action (list 'skip-quote autopair-inserted (point))))
          (;; decides whether to pair, i.e do *not* pair the quote if...
           ;;
           (not
            (or
             escaped-p
             ;; ... inside a generic string
             (eq inside-string t)
             ;; ... inside an unterminated string started by this char 
             (autopair--in-unterminated-string-p syntax-triplet)
             ;; ... the position at the end of buffer is inside an
             ;; unterminated string
             (autopair--in-unterminated-string-p (save-excursion
                                                   (goto-char (point-max))
                                                   (autopair--syntax-ppss)))
             ;; ... comment-disable or string-disable are true at
             ;; point.  The latter is only useful if we're in a string
             ;; terminated by a character other than
             ;; `autopair-inserted'.
             (cl-some #'(lambda (sym)
                          (autopair--exception-p where-sym sym autopair-dont-pair))
                      '(:comment :string))))
           (setq autopair-action (list 'insert-quote autopair-inserted (point)))))
    (autopair--fallback)))

(put 'autopair-insert-or-skip-quote 'function-documentation
     '(concat "Insert or possibly skip over a quoting character.\n\n"
              (autopair--document-bindings)))

(defun autopair--in-unterminated-string-p (autopair-triplet)
  (let* ((relevant-ppss (cl-third autopair-triplet))
         (string-delim (cl-fourth relevant-ppss)))
    (and (or (eq t string-delim)
             (eq autopair-inserted string-delim))
         (condition-case nil (progn (scan-sexps (cl-ninth relevant-ppss) 1) nil) (scan-error t)))))


(defun autopair-insert-opening ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (when (autopair--pair-p)
    (setq autopair-action (list 'opening (autopair--pair-of autopair-inserted) (point))))
  (autopair--fallback))
(put 'autopair-insert-opening 'function-documentation
     '(concat "Insert opening delimiter and possibly automatically close it.\n\n"
              (autopair--document-bindings)))

(defun autopair-skip-close-maybe ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (when (autopair--skip-p)
    (setq autopair-action (list 'closing (autopair--pair-of autopair-inserted) (point))))
  (autopair--fallback))
(put 'autopair-skip-close-maybe 'function-documentation
     '(concat "Insert or possibly skip over a closing delimiter.\n\n"
              (autopair--document-bindings)))

(defun autopair-backspace ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (when (char-before)
    (setq autopair-action (list 'backspace (autopair--pair-of (char-before) 'closing) (point))))
  (autopair--fallback (kbd "DEL")))
(put 'autopair-backspace 'function-documentation
     '(concat "Possibly delete a pair of paired delimiters.\n\n"
              (autopair--document-bindings (kbd "DEL"))))

(defun autopair-newline ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (let ((pair (autopair--pair-of (char-before))))
    (when (and pair
               (eq (char-syntax pair) ?\))
               (eq (char-after) pair))
      (setq autopair-action (list 'newline pair (point))))
    (autopair--fallback (kbd "RET"))))
(put 'autopair-newline 'function-documentation
     '(concat "Do a smart newline when right between parenthesis.\n
In other words, insert an extra newline along with the one inserted normally
by this command. Then place point after the first, indented.\n\n"
              (autopair--document-bindings (kbd "RET"))))

(defun autopair--skip-p ()
  (let* ((syntax-triplet (autopair--syntax-ppss))
         (syntax-info (cl-first syntax-triplet))
         (orig-point (point)))
    (cond ((eq autopair-skip-criteria 'help-balance)
           (cl-destructuring-bind (innermost . outermost)
               (autopair--up-list (- (point-max)))
             (cond ((cl-first outermost)
                    (cl-first innermost))
                   ((cl-first innermost)
                    (not (eq (autopair--pair-of (char-after (cl-third outermost)))
                             autopair-inserted))))))
          ((eq autopair-skip-criteria 'need-opening)
           (save-excursion
             (condition-case err
                 (progn
                   (backward-list)
                   t)
               (scan-error nil))))
          (t
           t))))

(defun autopair--pair-p ()
  (let* ((syntax-triplet (autopair--syntax-ppss))
         (syntax-info (cl-first syntax-triplet))
         (where-sym (cl-second syntax-triplet))
         (orig-point (point)))
    (and (not (cl-some #'(lambda (sym)
                           (autopair--exception-p where-sym sym autopair-dont-pair))
                       '(:string :comment :code :everywhere)))
         (cond ((eq autopair-pair-criteria 'help-balance)
                (and (not (autopair--escaped-p syntax-info))
                     (cl-destructuring-bind (innermost . outermost)
                         (autopair--up-list (point-max))
                       (cond ((cl-first outermost)
                              t)
                             ((not (cl-first innermost))
                              (not (eq (autopair--pair-of (char-before (cl-third outermost)))
                                       autopair-inserted)))))))
               ((eq autopair-pair-criteria 'always)
                t)
               (t
                (not (autopair--escaped-p syntax-info)))))))

;; post-command-hook stuff
;;
(defun autopair--post-command-handler ()
  "Performs pairing and wrapping based on `autopair-action' and
`autopair-wrap-action'. "
  (when (and autopair-wrap-action
             (cl-notany #'null autopair-wrap-action))

    (if autopair-handle-wrap-action-fns
        (condition-case err
            (mapc #'(lambda (fn)
                      (apply fn autopair-wrap-action))
                  autopair-handle-wrap-action-fns)
          (scan-error (progn
                        (message "[autopair] error running custom `autopair-handle-wrap-action-fns', switching autopair off")
                        (autopair-mode -1))))
      (apply #'autopair-default-handle-wrap-action autopair-wrap-action))
    (setq autopair-wrap-action nil))

  (when (and autopair-action
             (cl-notany #'null autopair-action))
    (if autopair-handle-action-fns
        (condition-case err
            (mapc #'(lambda (fn)
                      (funcall fn (cl-first autopair-action) (cl-second autopair-action) (cl-third autopair-action)))
                  autopair-handle-action-fns)
          (scan-error (progn
                        (message "[autopair] error running custom `autopair-handle-action-fns', switching autopair off")
                        (autopair-mode -1))))
      (apply #'autopair-default-handle-action autopair-action))
    (setq autopair-action nil)))

(defun autopair--blink-matching-open ()
  (let ((blink-matching-paren autopair-blink)
        (show-paren-mode nil)
        (blink-matching-delay autopair-blink-delay))
    (blink-matching-open)))

(defun autopair--blink (&optional pos)
  (when autopair-blink
    (if pos
        (save-excursion
          (goto-char pos)
          (sit-for autopair-blink-delay))
      (sit-for autopair-blink-delay))))

(defun autopair-default-handle-action (action pair pos-before)
  ;;(message "action is %s" action)
  (condition-case err
      (cond (;; automatically insert closing delimiter
             (and (eq 'opening action)
                  (not (eq pair (char-before))))
             (insert pair)
             (autopair--blink)
             (backward-char 1))
            (;; automatically insert closing quote delimiter
             (eq 'insert-quote action)
             (insert pair)
             (autopair--blink)
             (backward-char 1))
            (;; automatically skip oper closer quote delimiter
             (and (eq 'skip-quote action)
                  (eq pair (char-after (point))))
             (delete-char 1)
             (autopair--blink-matching-open))
            (;; skip over newly-inserted-but-existing closing delimiter
             ;; (normal case)
             (eq 'closing action)
             (let ((skipped 0))
               (when autopair-skip-whitespace
                 (setq skipped (save-excursion (skip-chars-forward "\s\n\t"))))
               (when (eq autopair-inserted (char-after (+ (point) skipped)))
                 (backward-delete-char 1)
                 (unless (zerop skipped) (autopair--blink (+ (point) skipped)))
                 (if (eq autopair-skip-whitespace 'chomp)
                     (delete-char skipped)
                   (forward-char skipped))
                 (forward-char))
               (autopair--blink-matching-open)))
            (;; autodelete closing delimiter
             (and (eq 'backspace action)
                  (eq pair (char-after (point))))
             (delete-char 1))
            (;; opens an extra line after point, then indents
             (and (eq 'newline action)
                  (eq pair (char-after (point))))
             (save-excursion
               (newline-and-indent))
             (indent-according-to-mode)))
    (error
     (message "[autopair] Ignored error in `autopair-default-handle-action'"))))


(defun autopair-default-handle-wrap-action (action pair pos-before region-before)
  "Default handler for the wrapping action in `autopair-wrap'"
  (condition-case err
      (when (eq 'wrap action)
        (let ((delete-active-region nil))
          (cond
           ((member autopair--this-command '(autopair-insert-opening
                                             autopair-extra-insert-opening))
            (goto-char (1+ (cdr region-before)))
            (insert pair)
            (autopair--blink)
            (goto-char (1+ (car region-before))))
           (;; wraps
            (member autopair--this-command '(autopair-skip-close-maybe
                                             autopair-extra-skip-close-maybe))
            (delete-char -1)
            (insert pair)
            (goto-char (1+ (cdr region-before)))
            (insert autopair-inserted))
           ((member autopair--this-command '(autopair-insert-or-skip-quote
                                             autopair-insert-or-skip-paired-delimiter))
            (goto-char (1+ (cdr region-before)))
            (insert pair)
            (autopair--blink))
           (t
            (delete-char -1)
            (goto-char (cdr region-before))
            (insert autopair-inserted)))
          (setq autopair-action nil)))
    (error
     (message "[autopair] Ignored error in `autopair-default-handle-wrap-action'"))))


;; example python triple quote helper
;;
(defun autopair-python-triple-quote-action (action pair pos-before)
  (cond ((and (eq 'insert-quote action)
              (>= (point) 3)
              (string= (buffer-substring (- (point) 3)
                                         (point))
                       (make-string 3 pair)))
         (save-excursion (insert (make-string 2 pair))))
        ((and (eq 'backspace action)
              (>= (point) 2)
              (<= (point) (- (point-max) 2))
              (string= (buffer-substring (- (point) 2)
                                         (+ (point) 2))
                       (make-string 4 pair)))
         (delete-region (- (point) 2)
                        (+ (point) 2)))
        ((and (eq 'skip-quote action)
              (<= (point) (- (point-max) 2))
              (string= (buffer-substring (point)
                                         (+ (point) 2))
                       (make-string 2 pair)))
         (forward-char 2))
        (t
         t)))

;; example latex paired-delimiter helper
;;
(defun autopair-latex-mode-paired-delimiter-action (action pair pos-before)
  "Pair or skip latex's \"paired delimiter\" syntax in math mode. Added AucText support, thanks Massimo Lauria"
  (when (eq action 'paired-delimiter)
    (when (eq (char-before) pair)
      (if (and (or
                (eq (get-text-property pos-before 'face) 'tex-math)
                (eq (get-text-property (- pos-before 1) 'face) 'font-latex-math-face)
                (member 'font-latex-math-face (get-text-property (- pos-before 1) 'face)))
               (eq (char-after) pair))
          (cond ((and (eq (char-after) pair)
                      (eq (char-after (1+ (point))) pair))
                 ;; double skip
                 (delete-char 1)
                 (forward-char))
                ((eq (char-before pos-before) pair)
                 ;; doube insert
                 (insert pair)
                 (backward-char))
                (t
                 ;; simple skip
                 (delete-char 1)))
        (insert pair)
        (backward-char)))))

;; Commands and predicates for the autopair-extra* feature
;;

(defun autopair-extra-insert-opening ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (when (autopair--extra-pair-p)
    (setq autopair-action (list 'opening (autopair--pair-of autopair-inserted) (point))))
  (autopair--fallback))
(put 'autopair-extra-insert-opening 'function-documentation
     '(concat "Insert (an extra) opening delimiter and possibly automatically close it.\n\n"
              (autopair--document-bindings)))

(defun autopair-extra-skip-close-maybe ()
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (when (autopair--extra-skip-p)
    (setq autopair-action (list 'closing autopair-inserted (point))))
  (autopair--fallback))
(put 'autopair-extra-skip-close-maybe 'function-documentation
     '(concat "Insert or possibly skip over a (and extra) closing delimiter.\n\n"
              (autopair--document-bindings)))

(defun autopair--extra-pair-p ()
  (let* ((syntax-triplet (autopair--syntax-ppss))
         (syntax-info (cl-first syntax-triplet))
         (where-sym (cl-second syntax-triplet)))
    (cl-some #'(lambda (sym)
                 (autopair--exception-p where-sym sym autopair-extra-pairs #'car))
             '(:everywhere :comment :string :code))))

(defun autopair--extra-skip-p ()
  (let* ((syntax-triplet (autopair--syntax-ppss))
         (syntax-info (cl-first syntax-triplet))
         (where-sym (cl-second syntax-triplet))
         (orig-point (point)))
    (and (eq (char-after (point)) autopair-inserted)
         (cl-some #'(lambda (sym)
                      (autopair--exception-p where-sym sym autopair-extra-pairs #'cdr))
                  '(:comment :string :code :everywhere))
         (save-excursion
           (condition-case err
               (backward-sexp (point-max))
             (scan-error
              (goto-char (cl-third err))))
           (search-forward (make-string 1 (autopair--pair-of autopair-inserted))
                           orig-point
                           'noerror)))))

;; Commands and tex-mode specific handler functions for the "paired
;; delimiter" syntax class.
;;
(defun autopair-insert-or-skip-paired-delimiter ()
  " insert or skip a character paired delimiter"
  (interactive)
  (setq autopair-inserted (autopair--calculate-inserted))
  (setq autopair-action (list 'paired-delimiter autopair-inserted (point)))
  (autopair--fallback))

(put 'autopair-insert-or-skip-paired-delimiter 'function-documentation
     '(concat "Insert or possibly skip over a character with a syntax-class of \"paired delimiter\"."
              (autopair--document-bindings)))



;; monkey-patching: Compatibility with delete-selection-mode and cua-mode
;;
;; Ideally one would be able to use functions as the value of the
;; 'delete-selection properties of the autopair commands. The function
;; would return non-nil when no wrapping should/could be performed.
;;
;; Until then use some `defadvice' i.e. monkey-patching, which relies
;; on these features' implementation details.
;;
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

(defun autopair--should-autowrap ()
  (and autopair-mode
       (not (eq this-command 'autopair-backspace))
       (symbolp this-command)
       (string-match "^autopair" (symbol-name this-command))
       (autopair--calculate-wrap-action)))

(defadvice cua--pre-command-handler-1 (around autopair-override activate)
  "Don't actually do anything if autopair is about to autowrap. "
  (unless (autopair--should-autowrap) ad-do-it))

(defadvice delete-selection-pre-hook (around autopair-override activate)
  "Don't actually do anything if autopair is  about to autowrap. "
  (unless (autopair--should-autowrap) ad-do-it))

(provide 'autopair)


;; Local Variables:
;; coding: utf-8
;; End:
;;; autopair.el ends here
