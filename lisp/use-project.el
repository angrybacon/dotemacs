;;; use-project.el --- Project-centric features      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Directory Variables

(declare-function szadek-get "szadek")
(declare-function szadek-register "szadek")

;; Create and set a class for Prettier-enabled projects
(dir-locals-set-class-variables 'prettier
 '((css-base-mode . ((eval . (prettier-mode))))
   (js-base-mode . ((eval . (prettier-mode))))
   (json-ts-mode . ((eval . (prettier-mode))))
   (markdown-mode . ((eval . (prettier-mode))))
   (typescript-ts-base-mode . ((eval . (prettier-mode))))
   (yaml-ts-mode . ((eval . (prettier-mode))))))

(defun me/dir-locals-reset-directory-cache ()
  "Reset the class directory cache."
  (interactive)
  (setq dir-locals-directory-cache nil))

(defun me/dir-locals-set-directories ()
  "Apply directory-local class variables to the appropriate project paths."
  (interactive)
  (mapc (lambda (it) (dir-locals-set-directory-class it 'prettier))
        (szadek-get 'projects-prettier '())))

(szadek-register #'me/dir-locals-set-directories :immediate)

;;;; Project

;;;;; Core

(declare-function consult-grep "consult")
(declare-function consult-ripgrep "consult")
(declare-function project-root "project")

(defun me/project-find-file ()
  "Find a file under the current project.
If not in a project, fallback to `find-file-at-point' instead."
  (interactive)
  (if (project-current)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file-at-point)))

(defun me/project-kill-buffer-p (buffer)
  "Return whether BUFFER is safe to kill with `project-kill-buffers'."
  ;; (when (not (eq buffer (current-buffer)))
  ;;   (message "%s" buffer))
  (not (eq buffer (current-buffer))))

(defun me/project-kill-path ()
  "Save the current absolute path in the kill ring."
  (interactive)
  ;; TODO Provide a variant that starts at project root
  (let ((path (buffer-file-name)))
    (kill-new path)
    (message (format "[Project] Copied `%s'" path))))

(defun me/project-name (&optional project)
  "Return the name for PROJECT.
If PROJECT is not specified, assume current project root."
  (when-let (root (or project (me/project-root)))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory root)))))

(defun me/project-root ()
  "Return the current project root."
  (when-let (project (project-current))
    (project-root project)))

(defun me/project-save (&rest _)
  "Save file-visiting buffers under the current project root."
  (interactive)
  (save-some-buffers :noconfirm #'save-some-buffers-root))

(defun me/project-search ()
  "Run ripgrep against project root.
If ripgrep is not installed, use grep instead."
  (interactive)
  (let ((root (me/project-root)))
    (if (executable-find "rg")
        (consult-ripgrep root)
      (message "[Project] Could not find `rg', using `grep' instead")
      (consult-grep root))))

(defun me/project-todo ()
  "Visit the todo file for the current project."
  (interactive)
  (if-let (root (me/project-root))
      (find-file (expand-file-name "TODO.org" root))
    (user-error "[Project] Not in a project")))

;;;;; Lint

(defun me/project-lint-command (path)
  "Return the lint command for the provided PATH."
  (format "npx eslint %s --fix" path))

(defun me/project-prettify-command (path)
  "Return the prettify command for the provided PATH."
  (format "npx prettier %s --write" path))

(defun me/project-lint-dwim ()
  "Run lint for the current file."
  (interactive)
  (let ((path (buffer-file-name)))
    (shell-command (format "%s && %s"
                           (me/project-lint-command path)
                           (me/project-prettify-command path)))))

;;;;; Test

(declare-function vterm-send-return "vterm")
(declare-function vterm-send-string "vterm")
(declare-function widowmaker-terminal-window "widowmaker")

(defvar me/project-test-file-pattern (rx ".test." (1+ alphabetic) eos)
  "Pattern describing a supported test file.")

(defun me/project-test-command (path &optional watch)
  "Return the test command for the provided PATH as per project configuration.
With WATCH optional parameter, return a command that watch for file changes."
  (concat "yarn test" (and watch " --watch") " " path))

(defun me/project-test-path (path)
  "Run test suite at PATH location."
  (if-let ((window (widowmaker-terminal-window)))
      (with-current-buffer (window-buffer window)
        (vterm-send-string (me/project-test-command path))
        (vterm-send-return))
    (message "[Project] Could not find terminal window")))

(defun me/project-test-dwim ()
  "Run test suite for the current file."
  (interactive)
  (let* ((path (buffer-file-name))
         (match (string-match me/project-test-file-pattern path)))
    (if (and match (> match 0))
        (me/project-test-path path)
      ;; TODO Current file is not a test file, find nearest that matches
      (message "[Project] Not a test file"))))

;;;;; Project.el

(declare-function cl-nsubstitute "cl-seq")
(declare-function shelldock "shelldock")

(use-package project
  :ensure nil
  :bind
  (([remap project-vc-dir] . magit-project-status)
   ([remap project-find-regexp] . me/project-search)
   ([remap project-shell] . widowmaker-terminal-dwim)
   :map project-prefix-map
   ("l" . me/project-lint-dwim)
   ("P" . me/project-todo)
   ("S" . me/project-save)
   ("t" . me/project-test-dwim))
  :config
  (cl-nsubstitute
   #'me/project-kill-buffer-p #'buffer-file-name project-kill-buffer-conditions)
  :custom
  ;; TODO Should reuse the same keys from the avove remaps
  (project-switch-commands
   '((project-find-file        "File")
     (magit-project-status     "Git" ?v)
     (me/project-todo          "Todo")
     (me/project-search        "Search" ?g)
     (widowmaker-terminal-dwim "Terminal" ?s)))
  :init
  (setq-default project-list-file (shelldock "projects.eld")))

;;; use-project.el ends here
