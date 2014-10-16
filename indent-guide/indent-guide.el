;;; indent-guide.el --- show vertical lines to guide indentation

;; Copyright (C) 2013-2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 2.1.6

;;; Commentary:

;; Require this script
;;
;;   (require 'indent-guide)
;;
;; and call command "M-x indent-guide-mode".

;; If you want to enable indent-guide-mode automatically,
;; call "indent-guide-global-mode" function.
;;
;;   (indent-guide-global-mode)

;; Column lines are propertized with "indent-guide-face". So you may
;; configure this face to make guides more pretty in your colorscheme.
;;
;;   (set-face-background 'indent-guide-face "dimgray")
;;
;; You may also change the character for guides.
;;
;;   (setq indent-guide-char ":")

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 cleaned and optimized code
;;       works better for the file without trailing-whitespaces
;; 1.0.2 modified behavior for lines with only whitespaces
;; 1.0.3 Allow custom indent guide char
;; 1.0.4 disabled in org-indent-mode
;; 1.0.5 faster update of indent-guide (especially for huge files)
;; 1.1.0 work with tab-indented files
;; 1.1.1 turned into minor-mode
;; 1.1.2 an infinite-loop bug fix
;; 1.1.3 changed behavior for blank lines
;; 2.0.0 rewrite almost everything
;; 2.0.1 improve blank-line and tab handling
;; 2.0.2 fixed bug that sometimes newline gets invisible
;; 2.0.3 added indent-guide-global-mode
;; 2.1.0 now lines are not drawn over the cursor
;; 2.1.1 work better with blank lines
;; 2.1.2 fixed bug in empty files
;; 2.1.3 better bob and eob handling
;; 2.1.4 use "display" property instead of "before-string"
;;       (now works better with hl-line and linum)
;; 2.1.5 add "indent-guide-inhibit-modes"
;; 2.1.6 add option "indent-guide-recursive"

;;; Code:

(defconst indent-guide-version "2.1.6")

;; * customs

(defgroup indent-guide nil
  "Show vertical lines to guide indentation."
  :group 'emacs)

(defcustom indent-guide-char "|"
  "Character used as vertical line."
  :type 'string
  :group 'indent-guide)

(defcustom indent-guide-inhibit-modes '(dired-mode)
  "List of major-modes in which indent-guide should be turned off."
  :type '(repeat symbol)
  :group 'indent-guide)

(defcustom indent-guide-recursive nil
  "When non-nil, draw multiple guide lines recursively."
  :type 'boolean
  :group 'indent-guide)

(defcustom indent-guide-delay nil
  "When a positive number, rendering guide lines is delayed DELAY
  seconds."
  :type 'number
  :group 'indent-guide)

(defface indent-guide-face '((t (:foreground "#535353")))
  "Face used to indent guide lines."
  :group 'indent-guide)

;; * variables

(defvar indent-guide--timer-object nil)

;; * utilities

(defun indent-guide--active-overlays ()
  "Return the list of all overlays created by indent-guide."
  (delq nil
        (mapcar
         (lambda (ov)
           (and (eq (overlay-get ov 'category) 'indent-guide) ov))
         (overlays-in (point-min) (point-max)))))

(defun indent-guide--beginning-of-level ()
  "Move to the beginning of current indentation level and returns
the point."
  (let ((base-level (if (progn (back-to-indentation)
                               (not (eolp)))
                        (current-column)
                      (max (save-excursion
                             (skip-chars-forward "\s\t\n")
                             (back-to-indentation)
                             (current-column))
                           (save-excursion
                             (skip-chars-backward "\s\t\n")
                             (back-to-indentation)
                             (current-column))))))
    (if (zerop base-level)
        (point)
      (catch 'fail
        (while (progn
                 (when (= (forward-line -1) -1)
                   (throw 'fail nil))
                 (back-to-indentation)
                 (or (eolp)
                     (>= (current-column) base-level))))
        (point)))))

;; * generate guides

(defun indent-guide--make-overlay (line col)
  "draw line at (line, col)"
  (let ((original-pos (point))
        diff string ov prop)
    (save-excursion
      ;; try to goto (line, col)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)
      ;; calculate difference from the actual col
      (setq diff (- col (current-column)))
      ;; make overlay or not
      (cond ((and (eolp) (<= 0 diff))   ; the line is too short
             ;; <-line-width->  <-diff->
             ;;               []        |
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-guide)
                               ov))
                           (overlays-in (point) (point))))
                 ;; we already have an overlay here => append to the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'before-string)))
                                (concat str
                                        (make-string (- diff (length str)) ?\s)
                                        indent-guide-char))
                       prop   'before-string)
               (setq string (concat (make-string diff ?\s) indent-guide-char)
                     prop   'before-string
                     ov     (make-overlay (point) (point)))))
            ((< diff 0)                 ; the column is inside a tab
             ;;  <---tab-width-->
             ;;      <-(- diff)->
             ;;     |            []
             (if (setq ov (cl-some
                           (lambda (ov)
                             (when (eq (overlay-get ov 'category) 'indent-guide)
                               ov))
                           (overlays-in (1- (point)) (point))))
                 ;; we already have an overlay here => modify the existing overlay
                 ;; (important when "recursive" is enabled)
                 (setq string (let ((str (overlay-get ov 'display)))
                                (aset str (+ 1 tab-width diff) ?|)
                                str)
                       prop   'display)
               (setq string (concat (make-string (+ tab-width diff) ?\s)
                                    indent-guide-char
                                    (make-string (1- (- diff)) ?\s))
                     prop   'display
                     ov     (make-overlay (point) (1- (point))))))
            ((looking-at "\t")          ; okay but looking at tab
             ;;    <-tab-width->
             ;; [|]
             (setq string (concat indent-guide-char
                                  (make-string (1- tab-width) ?\s))
                   prop   'display
                   ov     (make-overlay (point) (1+ (point)))))
            (t                          ; no problem
             (setq string indent-guide-char
                   prop   'display
                   ov     (make-overlay (point) (1+ (point))))))
      (when ov
        (overlay-put ov 'category 'indent-guide)
        (overlay-put ov prop
                     (propertize string 'face 'indent-guide-face))))))

(defun indent-guide-show ()
  (interactive)
  (unless (or (indent-guide--active-overlays)
              (active-minibuffer-window))
    (let ((win-start (window-start))
          (win-end (window-end nil t))
          line-col line-start line-end)
      ;; decide line-col, line-start
      (save-excursion
        (if (not (indent-guide--beginning-of-level))
            ;; we couldn't find the beginning of this level, so assume it 0
            (setq line-col 0
                  line-start 1)
          (setq line-col (current-column)
                line-start (max (1+ (line-number-at-pos))
                                (line-number-at-pos win-start))))
        (when (and indent-guide-recursive (> line-col 0))
          (indent-guide-show)))
      ;; decide line-end
      (save-excursion
        (while (and (progn (back-to-indentation)
                           (or (< line-col (current-column)) (eolp)))
                    (forward-line 1)
                    (not (eobp))
                    (<= (point) win-end)))
        (if (>= line-col (current-column))
            (forward-line -1))
        (setq line-end (line-number-at-pos)))
      ;; draw line
      (dotimes (tmp (- (1+ line-end) line-start))
        (indent-guide--make-overlay (+ line-start tmp) line-col))
      (remove-overlays (point) (point) 'category 'indent-guide))))

(defun indent-guide-remove ()
  (dolist (ov (indent-guide--active-overlays))
    (delete-overlay ov)))

;; * minor-mode

(defun indent-guide-post-command-hook ()
  (if (null indent-guide-delay)
      (indent-guide-show)
    (when (null indent-guide--timer-object)
      (setq indent-guide--timer-object
            (run-with-idle-timer indent-guide-delay nil
                                 (lambda ()
                                   (indent-guide-show)
                                   (setq indent-guide--timer-object nil)))))))

(defun indent-guide-pre-command-hook ()
  (indent-guide-remove))

;;;###autoload
(define-minor-mode indent-guide-mode
  "show vertical lines to guide indentation"
  :init-value nil
  :lighter " ing"
  :global nil
  (if indent-guide-mode
      (progn
        (add-hook 'pre-command-hook 'indent-guide-pre-command-hook nil t)
        (add-hook 'post-command-hook 'indent-guide-post-command-hook nil t))
    (remove-hook 'pre-command-hook 'indent-guide-pre-command-hook t)
    (remove-hook 'post-command-hook 'indent-guide-post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode indent-guide-global-mode
  indent-guide-mode
  (lambda ()
    (unless (memq major-mode indent-guide-inhibit-modes)
      (indent-guide-mode 1))))

;; * provide

(provide 'indent-guide)

;;; indent-guide.el ends here
