;;; stickyfunc-enhance.el --- An enhancement to stock `semantic-stickyfunc-mode'
;;
;; Filename: stickyfunc-enhance.el
;; Description: An enhancement to `semantic-stickyfunc-mode'
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-stickyfunc-enhance
;; Package-Version: 20150429.1114
;; Maintainer: Tu, Do Hoang
;; Created: Friday March 13
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: c, languages, tools
;; Compatibility: GNU Emacs: 24.3+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; When enable, `semantic-stickyfunc-mode' shows the function point is
;; currently in at the first line of the current buffer. This is
;; useful when you have a very long function that spreads more than a
;; screen, and you don't have to scroll up to read the function name
;; and then scroll down to original position.
;;
;; However, one of the problem with current semantic-stickyfunc-mode
;; is that it does not display all parameters that are scattered on
;; multiple lines. To solve this problem, we need to redefine
;; `semantic-stickyfunc-fetch-stickyline' function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'cl-lib)
(require 'cc-mode)
(require 'semantic)
(if (not (version< emacs-version "24.4"))
    (require 'subr-x)
  (defsubst string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (string-trim-left (string-trim-right string)))

  (defsubst string-empty-p (string)
    "Check whether STRING is empty."
    (string= string ""))

  (defsubst string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string))

  (defsubst string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string)))

;;;###autoload
(defun semantic-stickyfunc-fetch-stickyline ()
  "Make the function at the top of the current window sticky.
Capture its function declaration, and place it in the header line.
If there is no function, disable the header line."
  (save-excursion
    (goto-char (window-start (selected-window)))
    (let* ((noshow (bobp))
           (str
            (progn
              (forward-line -1)
              (end-of-line)
              ;; Capture this function
              (let* ((tag (semantic-stickyfunc-tag-to-stick))
                     param-tags filtered-tags tmp-str)
                ;; TAG is nil if there was nothing of the appropriate type there.
                (if (not tag)
                    ;; Set it to be the text under the header line
                    (if noshow
                        ""
                      (if semantic-stickyfunc-show-only-functions-p ""
                        (buffer-substring (point-at-bol) (point-at-eol))))
                  (setq param-tags (semantic-tag-function-arguments tag))
                  (setq filtered-tags (stickyfunc-enhance--tags-out-of-screen param-tags tag)) ;
                  (setq tmp-str (semantic-format-tag-prototype tag nil t))
                  (if (and (= (length param-tags) (length filtered-tags))
                           (not (eq major-mode 'python-mode)))
                      tmp-str
                    (if (not (eq (semantic-tag-class tag) 'function))
                        tmp-str
                      (string-match (stickyfunc-enhance--parameters-regexp tag) tmp-str)
                      (setq tmp-str (replace-match (stickyfunc-enhance--text-to-replace tag) t t tmp-str 0))
                      (if filtered-tags
                          (dolist (v filtered-tags)
                            (setq tmp-str (concat tmp-str
                                                  (stickyfunc-enhance--function-parameter-string v)
                                                  (stickyfunc-enhance--function-argument-separator))))
                        (setq tmp-str (concat tmp-str ")"))))
                    tmp-str)))))
           (start 0))
      (while (string-match "%" str start)
        (setq str (replace-match "%%" t t str 0)
              start (1+ (match-end 0))))
      ;; In 21.4 (or 22.1) the header doesn't expand tabs.  Hmmmm.
      ;; We should replace them here.
      ;;
      ;; This hack assumes that tabs are kept smartly at tab boundaries
      ;; instead of in a tab boundary where it might only represent 4 spaces.
      (while (string-match "\t" str start)
        (setq str (replace-match "        " t t str 0)))
      str)))

(defun stickyfunc-enhance--function-parameter-string (tag)
  "Return a string of a parameter TAG to be displayed.

It handles Python specifically along with other modes, because
the stock Semantic formate functions do not display assigned
values to parameters if there is any.

Also handles a case if tag is stored a string, not a list, as
returned by `semantic-tag-function-arguments' in Emacs Lisp mode."
  (cond
   ((eq major-mode 'python-mode)
    (save-excursion
      (let* ((tag-start (semantic-tag-start tag))
             (next-tag (save-excursion
                         (goto-char tag-start)
                         (semantic-find-tag-by-overlay-next)))
             (next-tag-start (if (not next-tag)
                                 (search-forward ":")
                               (semantic-tag-start next-tag))))
        (string-trim
         (replace-regexp-in-string "\\Ca.*"
                                   ""
                                   (buffer-substring tag-start
                                                     next-tag-start))))))
   (t
    (if (listp tag)
        (semantic-format-tag-prototype tag nil t)
      (propertize tag 'face 'font-lock-variable-name-face)))))

(defun stickyfunc-enhance--function-argument-separator ()
  "Return a proper separator between parameter tags."
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    ",")
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'python-mode))
    " ")
   (t ",")))

(defun stickyfunc-enhance--text-to-replace (tag)
  "Text to replace a matched text of a TAG.

To be used with `stickyfunc-enhance--parameters-regexp'"
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(")
   ((eq major-mode 'emacs-lisp-mode)
    (concat "(" (propertize (semantic-tag-name tag) 'face 'font-lock-function-name-face) " "))
   (t "(")))

(defun stickyfunc-enhance--parameters-regexp (tag)
  "Return parameter regexp of a function TAG.

To be used with `stickyfunc-enhance--text-to-replace'"
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(.*)")
   ((eq major-mode 'emacs-lisp-mode)
    "(.*)")
   (t "(.*)")))

(defun stickyfunc-enhance--tags-out-of-screen (tags parent-tag)
  "Return a list of tags that are out of current visible screen.

TAGS are a list of tags that are function parameters of PARENT-TAG.

PARENT-TAG is a function."
  (let ((start-line (line-number-at-pos (window-start))))
    (cl-remove-if (lambda (tag)
                    (>= (line-number-at-pos (if (listp tag)
                                                (semantic-tag-start tag)
                                              (save-excursion
                                                (goto-char (semantic-tag-start parent-tag))
                                                (search-forward tag)
                                                (point))))
                        start-line))
                  tags)))

(provide 'stickyfunc-enhance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stickyfunc-enhance.el ends here
;; Local Variables:
;; byte-compile-warnings: t
;; End:
