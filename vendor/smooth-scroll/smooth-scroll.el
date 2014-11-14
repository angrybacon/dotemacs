;;; smooth-scroll.el --- Minor mode for smooth scrolling and in-place scrolling.

;; Copyright (C) 2010, 2012-2013 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: 14 March 2010 AM 03:36 JST
;; Keywords: convenience emulations frames
;; Revision: $Id: eef2ee2c490109779f0e5928df89f7a1a5e20234 $
;; URL: http://www.emacswiki.org/emacs/download/smooth-scroll.el
;; GitHub: http://github.com/k-talo/smooth-scroll.el
;; Version: 1.2

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; NOTE
;;
;; This library is just tested on Emacs 23.2.1 on Ubuntu 10.04
;; and Mac OS X 10.6.3, and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;;
;; This library provides minor mode `smooth-scroll-mode' which adds
;; smooth scrolling feature to Emacs.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'smooth-scroll)
;;    (smooth-scroll-mode t)
;;
;;
;; USING
;; =====
;; To toggle smooth scrolling feature, just type:
;;
;;   `M-x smooth-scroll-mode RET'
;;
;; while smooth scrolling feature is enabled, the string "SScr" will
;; be displayed on mode line.
;;
;; Also check out the customization group
;;
;;   `M-x customize-group RET smooth-scroll RET'
;;
;;
;; Additional commands provided by `smooth-scroll.el'.
;; ===================================================
;;
;; This library provides commands that brings `in place scrolling'
;; feature, listed below:
;;
;;    `scroll-up-1'
;;    `scroll-down-1'
;;    `scroll-left-1'
;;    `scroll-right-1'
;;
;; Bind these commands to any key you like for your convenience.
;;
;;    Keymap example:
;;
;;      (global-set-key [(control  down)]  'scroll-up-1)
;;      (global-set-key [(control  up)]    'scroll-down-1)
;;      (global-set-key [(control  left)]  'scroll-right-1)
;;      (global-set-key [(control  right)] 'scroll-left-1)
;;
;;      NOTE: Keys described above won't work on non window-system.
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; - The speed of smooth scrolling is very slow on `Carbon Emacs'
;;   and `Cocoa Emacs' on Mac OS X. If you want to use smooth scrolling
;;   feature comfortably on these Emacsen, set large number
;;   (e.g. 4, 6 or 8) to the variable `smooth-scroll/vscroll-step-size' 
;;   and `smooth-scroll/hscroll-step-size'.
;;
;; - `scroll-left-1' and `scroll-right-1' may not work properly
;;   when the `smooth-scroll-mode' is turned off, due to the behavior
;;   of original `scroll-left' and `scroll-right' functions.

;;; Change Log:
;; 
;; v1.2 Mon Mar 18 12:55:10 2013 JST
;;   - Fixed a bug that smooth scroll feature won't work with Emacs 24.
;; v1.1 Thu Feb  2 16:10:09 2012 JST
;;   - Supress compiler warnings.

;;; Code:

(defconst smooth-scroll/version "1.1")

(eval-when-compile
  (require 'cl)
  (require 'easy-mmode))

 
;;; ===========================================================================
;;;
;;;  User customizable things.
;;;
;;; ===========================================================================

(defgroup smooth-scroll nil
  "Scroll window smoothly."
  :group 'editing)

(defcustom smooth-scroll/vscroll-step-size 2
  "Amount of lines, which determines quality of smooth vertical scrolling.
The small number makes it better, the large number makes it rough.

This value must be a positive number, otherwise `1' will be used.

Default value is `2'."
  :type 'integer
  :group 'smooth-scroll)

(defcustom smooth-scroll/hscroll-step-size 2
  "Amount of columns, which determines quality of smooth horizontal scrolling.
The small number makes it better, the large number makes it rough.

This value must be a positive number, otherwise `1' will be used.

Default value is `2'."
  :type 'integer
  :group 'smooth-scroll)

 
;;;============================================================================
;;;
;;; Additional commands provided by `smooth-scroll.el'.
;;;
;;;============================================================================

(defun scroll-up-1 (&optional arg)
  "Scroll text of selected window upward ARG lines.
If ARG is omitted or nil, scroll upward by a line.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by a line.
When calling from a program, supply as argument a number, nil, or `-'.

After scrolling, position of the cursor will be kept when possible."
  (interactive "P")
  (let ((amount (case arg ((-) -1) ((nil) 1) (t arg))))
    (scroll-up amount)))

(defun scroll-down-1 (&optional arg)
  "Scroll text of selected window down ARG lines.
If ARG is omitted or nil, scroll down by a line.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by a line.
When calling from a program, supply as argument a number, nil, or `-'.

After scrolling, position of the cursor will be kept when possible."
  (interactive "P")
  (let ((amount (case arg ((-) -1) ((nil) 1) (t arg))))
    (scroll-down amount)))

(defun scroll-left-1 (&optional arg)
  "Scroll selected window display ARG columns left.
If ARG is omitted or nil, scroll left by a column.
Negative ARG means scroll rightward.
If ARG is the atom `-', scroll right by a column.
When calling from a program, supply as argument a number, nil, or `-'.

After scrolling, position of the cursor will be kept when possible."
  (interactive "P")
  (let ((amount (case arg ((-) -1) ((nil) 1) (t arg))))
    (scroll-left amount)))

(defun scroll-right-1 (&optional arg)
  "Scroll selected window display ARG columns right.
If ARG is omitted or nil, scroll right by a column.
Negative ARG means scroll leftward.
If ARG is the atom `-', scroll left by a column.
When calling from a program, supply as argument a number, nil, or `-'.

After scrolling, position of the cursor will be kept when possible."
  (interactive "P")
  (let ((amount (case arg ((-) -1) ((nil) 1) (t arg))))
    (scroll-right amount)))

 
;;;============================================================================
;;;
;;; Variables.
;;;
;;;============================================================================
(defvar smooth-scroll/.scrolling-p nil
  "Private variable used in `smooth-scroll-mode'.")

(defvar smooth-scroll/.debugging-p nil
  "Private variable used in `smooth-scroll-mode'.")

 
;;;============================================================================
;;;
;;; Macros and Utility functions.
;;;
;;;============================================================================
(defmacro smooth-scroll/.run-without-recursive-call (&rest body)
  `(when (and (not smooth-scroll/.scrolling-p)
              (not (smooth-scroll/.drop-events)))
     (prog2 (setq smooth-scroll/.scrolling-p t)
       (unwind-protect
         (progn ,@body)
         (setq smooth-scroll/.scrolling-p nil)))))
    
(defun smooth-scroll/.debug-msg (str)
  (when smooth-scroll/.debugging-p
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (insert str)
      (goto-char (point-max))
      ;;(recenter -3)
      )))
    
(defun smooth-scroll/.drop-events ()
  ;; To avid queuing so much scrolling events to the event
  ;; queue of Emacs, drop them while scrolling smoothly.
  (when (input-pending-p)
    (smooth-scroll/.debug-msg "smooth-scroll/.drop-events: dropped event.\n")
    (let (ev)
      (while (and (input-pending-p) (setq ev (read-event)))
        (smooth-scroll/.debug-msg
         (format "\"%s\"\n" ev))))
    t))

(defvar smooth-scroll/redisplay-interval (when (not window-system) 0.005)
  "Private variable used in `smooth-scroll-mode'.")

(defun smooth-scroll/.force-redisplay ()
  (while (and (not executing-kbd-macro)
              (not (redisplay t)))
    (smooth-scroll/.debug-msg "Redisplay was not run."))
  (when (numberp smooth-scroll/redisplay-interval)
    ;; Required by emacs running with '-nw' option.
    (sleep-for smooth-scroll/redisplay-interval)))

 
;;;============================================================================
;;;
;;; Vertical scrolling.
;;;
;;;============================================================================

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;;; Commands
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun smooth-scroll/scroll-up (&optional arg)
       "Scroll text of selected window upward ARG lines.
If ARG is omitted or nil, scroll upward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (smooth-scroll/.vscroll-aux arg t))


(defun smooth-scroll/scroll-down (&optional arg)
       "Scroll text of selected window down ARG lines.
If ARG is omitted or nil, scroll down by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (smooth-scroll/.vscroll-aux arg nil))


(defun smooth-scroll/scroll-other-window (&optional arg)
       "Scroll next window upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
The next window is the one below the current one; or the one at the top
if the current one is at the bottom.  Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'.

If `other-window-scroll-buffer' is non-nil, scroll the window
showing that buffer, popping the buffer up if necessary.
If in the minibuffer, `minibuffer-scroll-window' if non-nil
specifies the window to scroll.  This takes precedence over
`other-window-scroll-buffer'."
  (interactive "P")
  (let ((orig-fn (symbol-function 'smooth-scroll/orig-scroll-up)))
    (unwind-protect
        (progn
          (setf (symbol-function 'smooth-scroll/orig-scroll-up)
                (symbol-function 'smooth-scroll/orig-scroll-other-window))
          (smooth-scroll/.vscroll-aux arg t))
      (setf (symbol-function 'smooth-scroll/orig-scroll-up)
            orig-fn))))


(defun smooth-scroll/scroll-other-window-down (&optional arg)
       "Scroll the \"other window\" down.
For more details, see the documentation for
`smooth-scroll/scroll-other-window'."
  (interactive "P")
  (let ((orig-fn (symbol-function 'smooth-scroll/orig-scroll-up)))
    (unwind-protect
        (progn
          (setf (symbol-function 'smooth-scroll/orig-scroll-up)
                (symbol-function 'smooth-scroll/orig-scroll-other-window))
          (smooth-scroll/.vscroll-aux arg t t))
      (setf (symbol-function 'smooth-scroll/orig-scroll-up)
            orig-fn))))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;;; Functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun smooth-scroll/.vscroll-aux (amount up-p &optional inverse)
  "Private function used in `smooth-scroll-mode'."
  (smooth-scroll/.run-without-recursive-call
     ;; First argument is a list, typically prefix arguments with no value.
     (when (listp amount)
       (setq amount (first amount)))
     
   ;; Arrange direction.
   ;;
   (when (eq amount '-)
     (setq amount nil)
     (setq up-p (not up-p)))
   (when (minusp (or amount 0))
     (setq amount (- amount))
     (setq up-p (not up-p)))

   ;; Scroll by a near full screen, when scroll amount
   ;; is not specified.
   (or amount
       (setq amount (- (window-height)
                       1                ;current line
                       (if mode-line-format 1 0)
                       (if header-line-format 1 0)
                       (or next-screen-context-lines 0))))

   ;; Do smooth scrolling.
   ;;
   (while (> amount 0)
     (let ((delta (min amount (max smooth-scroll/vscroll-step-size 1))))
       ;; inverse is for `smooth-scroll/scroll-other-window-down'.
       (if up-p
         (if (fboundp 'smooth-scroll/orig-scroll-up) ;; For compiler warnings.
             (smooth-scroll/orig-scroll-up (if inverse (- delta) delta)))
         (if (fboundp 'smooth-scroll/orig-scroll-down) ;; For compiler warnings.
             (smooth-scroll/orig-scroll-down (if inverse (- delta) delta))))
       (smooth-scroll/.force-redisplay)
       (setq amount (- amount delta))))

   amount))

 
;;;============================================================================
;;;
;;; Horizontal scrolling.
;;;
;;;============================================================================

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;;; Commands
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun smooth-scroll/scroll-left (&optional arg set-minimum)
  "Scroll selected window display ARG columns left.
Default for ARG is window width minus 2.
Value is the total amount of leftward horizontal scrolling in
effect after the change.
If SET-MINIMUM is non-nil, the new scroll amount becomes the
lower bound for automatic scrolling, i.e. automatic scrolling
will not scroll a window to a column less than the value returned
by this function.  This happens in an interactive call."
  (interactive "P")
  ;; XXX: Process set-minimum properly.
  ;;
  (smooth-scroll/.hscroll-aux arg t))


(defun smooth-scroll/scroll-right (&optional arg set-minimum)
  "Scroll selected window display ARG columns right.
Default for ARG is window width minus 2.
Value is the total amount of leftward horizontal scrolling in
effect after the change.
If SET-MINIMUM is non-nil, the new scroll amount becomes the
lower bound for automatic scrolling, i.e. automatic scrolling
will not scroll a window to a column less than the value returned
by this function.  This happens in an interactive call."
  (interactive "P")
  ;; XXX: Process set-minimum properly.
  ;;
  (smooth-scroll/.hscroll-aux arg nil))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;;; Functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun smooth-scroll/.keep-cursor-within-new-hscroll-margin (delta left-p)
  ;; Relations of values used in this function are:
  ;;
  ;;         col-num
  ;;             \
  ;;  |<-------------------->|
  ;;  |          left-margin |     right-margin
  ;;  |        |<----------->|<-------------------->|
  ;;                                                      BUFFER
  ;;           +------------------------------------+      /
  ;;  +--------|             (point)                |-----+
  ;;  |        |             /                      |     |
  ;;  |text text text text.. I text text text text text   |
  ;;  |\       |                                    | /   |
  ;;  |(point-at-bol)                       (poitn-at-eol)|
  ;;  |        |                                    |     |
  ;;  +--------|                                    |-----+
  ;;           +------------------------------------+ <--\
  ;;                                                     WINDOW
  ;;           |<--->|                        |<--->|
  ;;           |   \                            /   |
  ;;           | required-margin    required-margin |
  ;;  |<------>|<---------------------------------->|
  ;;       \                       \
  ;;       hscroll                (window-width)
  ;;
  (let* ((col-num (- (point) (point-at-bol)))
         (hscroll (window-hscroll))
         (required-margin (+ hscroll-margin delta 1)))
    ;; Retains required margin if necessary.
    (if left-p 
      ;; When scroll to left:
      (let ((left-margin (- col-num hscroll)))
        (when (< left-margin required-margin)
          ;; Move cursor to forward.
          (forward-char (min (- required-margin left-margin)
                             ;; Don't move forward over (point-at-eol).
                             (- (point-at-eol) (point))))))
      ;; When scroll to right:
      (let ((right-margin (- (+ (window-width) hscroll) col-num)))
        (when (< right-margin required-margin)
          (backward-char (min (- required-margin right-margin)
                              ;; Don't move back over (point-at-bol).
                              col-num)))))))


(defun smooth-scroll/.do-primitive-hscroll (delta left-p)
  (if left-p
      (if (fboundp 'smooth-scroll/orig-scroll-left) ;; For compiler warnings.
          (smooth-scroll/orig-scroll-left delta))
    (if (fboundp 'smooth-scroll/orig-scroll-right) ;; For compiler warnings.
        (smooth-scroll/orig-scroll-right delta))))


;; These two variables are used in `smooth-scroll/.restore-auto-hscroll-mode'.
(defvar smooth-scroll/.pre-command-hook nil)
(defvar smooth-scroll/.orig-auto-hscroll-mode nil)

(defun smooth-scroll/.restore-auto-hscroll-mode (orig-auto-hscroll-mode)
  (when (not smooth-scroll/.pre-command-hook)
    (setq smooth-scroll/.orig-auto-hscroll-mode orig-auto-hscroll-mode)
    (setq smooth-scroll/.pre-command-hook
            #'(lambda ()
                (when (or (not (symbolp this-command))
                          (not (get this-command 'scroll-command-p)))
                  (progn
                    (smooth-scroll/.debug-msg
                     (format "this-command: %s\n" this-command))
                    (setq auto-hscroll-mode smooth-scroll/.orig-auto-hscroll-mode)
                    (remove-hook 'pre-command-hook smooth-scroll/.pre-command-hook)
                    (setq smooth-scroll/.pre-command-hook nil)))))
    (add-hook 'pre-command-hook smooth-scroll/.pre-command-hook)))


(defun smooth-scroll/.hscroll-aux (amount left-p)
  (smooth-scroll/.run-without-recursive-call
   (if (not truncate-lines)
       (message "[smooth-scroll] hscroll won't work when the value of `truncate-line' is `nil'.")
     ;; First argument is a list, typically prefix arguments with no value.
     (when (listp amount)
       (setq amount (first amount)))
     
     ;; Arrange direction.
     ;;
     (when (eq amount '-)
       (setq amount nil)
       (setq left-p (not left-p)))
     (when (minusp (or amount 0))
       (setq amount (- amount))
       (setq left-p (not left-p)))

     ;; Scroll by a near full screen, when scroll amount
     ;; is not specified.
     (or amount
         (setq amount (- (window-width)
                         2)))

     (let ((orig-auto-p auto-hscroll-mode))
       ;; 
       (setq auto-hscroll-mode nil)
       ;; Do smooth scrolling.
       (while (> amount 0)
         (let ((delta (min amount
                           (max smooth-scroll/hscroll-step-size 1))))
           (smooth-scroll/.keep-cursor-within-new-hscroll-margin delta left-p)
           (smooth-scroll/.do-primitive-hscroll delta left-p)
           (smooth-scroll/.force-redisplay)
           (decf amount delta)))
       ;;
       (smooth-scroll/.restore-auto-hscroll-mode orig-auto-p)
       
       amount))))

 
;;;============================================================================
;;;
;;; Define minor mode `smooth-scroll-mode'.
;;;
;;;============================================================================

(easy-mmode-define-minor-mode
 smooth-scroll-mode "Minor mode for smooth scrolling and in-place scrolling."
 :global t
 :init-value nil
 :lighter " SScr"
 (if (or (not (boundp 'smooth-scroll-mode))
         smooth-scroll-mode)
   (progn
     ;; Override embedded functions. (Evil hack)
     (setf (symbol-function 'scroll-up)
             (symbol-function 'smooth-scroll/scroll-up))
     (setf (symbol-function 'scroll-down)
             (symbol-function 'smooth-scroll/scroll-down))
     (setf (symbol-function 'scroll-other-window)
             (symbol-function 'smooth-scroll/scroll-other-window))
     (setf (symbol-function 'scroll-other-window-down)
             (symbol-function 'smooth-scroll/scroll-other-window-down))
     (setf (symbol-function 'scroll-left)
             (symbol-function 'smooth-scroll/scroll-left))
     (setf (symbol-function 'scroll-right)
             (symbol-function 'smooth-scroll/scroll-right))

     ;; Initialize variables
     (setq smooth-scroll/.scrolling-p nil)
     (when smooth-scroll/.pre-command-hook
       (remove-hook 'pre-command-hook smooth-scroll/.pre-command-hook)
       (setq smooth-scroll/.pre-command-hook nil)))
   (progn
     ;; Restore original scrolling functions.
     (setf (symbol-function 'scroll-up)
             (symbol-function 'smooth-scroll/orig-scroll-up))
     (setf (symbol-function 'scroll-down)
             (symbol-function 'smooth-scroll/orig-scroll-down))
     (setf (symbol-function 'scroll-other-window)
             (symbol-function 'smooth-scroll/orig-scroll-other-window))
     (setf (symbol-function 'scroll-other-window-down)
             (symbol-function 'smooth-scroll/orig-scroll-other-window-down))
     (setf (symbol-function 'scroll-left)
             (symbol-function 'smooth-scroll/orig-scroll-left))
     (setf (symbol-function 'scroll-right)
             (symbol-function 'smooth-scroll/orig-scroll-right)))))

 
;;;============================================================================
;;;
;;; Initialization.
;;;
;;;============================================================================

(when (not (featurep 'smooth-scroll))
  ;; Save original scrolling functions.
  (when (not (fboundp 'smooth-scroll/orig-scroll-up))
    (setf (symbol-function 'smooth-scroll/orig-scroll-up)
            (symbol-function 'scroll-up)))
  (when (not (fboundp 'smooth-scroll/orig-scroll-down))
    (setf (symbol-function 'smooth-scroll/orig-scroll-down)
            (symbol-function 'scroll-down)))
  (when (not (fboundp 'smooth-scroll/orig-scroll-other-window))
    (setf (symbol-function 'smooth-scroll/orig-scroll-other-window)
            (symbol-function 'scroll-other-window)))
  (require 'simple)
  (when (not (fboundp 'smooth-scroll/orig-scroll-other-window-down))
    (setf (symbol-function 'smooth-scroll/orig-scroll-other-window-down)
            (symbol-function 'scroll-other-window-down)))
  (when (not (fboundp 'smooth-scroll/orig-scroll-left))
    (setf (symbol-function 'smooth-scroll/orig-scroll-left)
            (symbol-function 'scroll-left)))
  (when (not (fboundp 'smooth-scroll/orig-scroll-right))
    (setf (symbol-function 'smooth-scroll/orig-scroll-right)
            (symbol-function 'scroll-right)))
  
  ;; Mark scroll commands.
  (put 'scroll-up                'scroll-command-p t)
  (put 'scroll-down              'scroll-command-p t)
  (put 'scroll-other-window      'scroll-command-p t)
  (put 'scroll-other-window-down 'scroll-command-p t)
  (put 'scroll-left              'scroll-command-p t)
  (put 'scroll-right             'scroll-command-p t)
  (put 'scroll-up-1              'scroll-command-p t)
  (put 'scroll-down-1            'scroll-command-p t)
  (put 'scroll-left-1            'scroll-command-p t)
  (put 'scroll-right-1           'scroll-command-p t))

(provide 'smooth-scroll)

;;; smooth-scroll.el ends here
