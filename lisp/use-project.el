;;; use-project.el --- Project-centric features      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Directory Variables

;; Create and set a class for Prettier-enabled projects
(dir-locals-set-class-variables 'prettier
 '((css-base-mode . ((eval . (prettier-mode))))
   (js-base-mode . ((eval . (prettier-mode))))
   (json-ts-mode . ((eval . (prettier-mode))))
   (markdown-mode . ((eval . (prettier-mode))))
   (typescript-ts-base-mode . ((eval . (prettier-mode))))
   (yaml-ts-mode . ((eval . (prettier-mode))))))

(defun me/dir-locals-set-directories ()
  "Apply directory-local class variables to the appropriate project paths."
  (interactive)
  (mapc (lambda (it) (dir-locals-set-directory-class it 'prettier))
        (szadek-get 'projects-prettier '())))

(szadek-register #'me/dir-locals-set-directories :immediate)

;;;; Project

(defun me/project-find-file ()
  "Find a file under the current project.
If not in a project, fallback to `find-file-at-point' instead."
  (interactive)
  (if (project-current)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file-at-point)))

(defun me/project-name (&optional project)
  "Return the name for PROJECT.
If PROJECT is not specified, assume current project root."
  (when-let (root (or project (me/project-root)))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory root)))))

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

(defun me/project-root ()
  "Return the current project root."
  (when-let (project (project-current))
    (project-root project)))

(defun me/project-todo ()
  "Visit the todo file for the current project."
  (interactive)
  (if-let (root (me/project-root))
      (find-file (expand-file-name "TODO.org" root))
    (user-error "[Project] Not in a project")))

(defun me/project-kill-path ()
  "Save the current absolute path in the kill ring."
  (interactive)
  ;; TODO Provide a variant that starts at project root
  (let ((path (buffer-file-name)))
    (kill-new path)
    (message (format "[Project] Copied `%s'" path))))

(use-package project
  :ensure nil
  :custom
  (project-switch-commands
   '((project-dired "Root" ?D)
     (project-find-file "File" ?f)
     (magit-project-status "Git" ?g)
     (me/project-todo "Todo" ?o)
     (me/project-search "Search" ?s)
     (widowmaker-terminal-dwim "Terminal" ?t)))
  :init
  (setq-default project-list-file (shelldock "projects.eld")))

;;; use-project.el ends here
