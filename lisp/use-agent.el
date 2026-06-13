;;; use-agent.el --- LLM integrations                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun me/claude-get ()
  "Return the active Claude configuration name."
  (when-let* ((configuration (getenv "CLAUDE_CONFIG_DIR"))
              ((string-match "claude-\\(.+\\)$" configuration)))
    (match-string 1 configuration)))

(defun me/claude--use (configuration)
  "Switch to the provided CONFIGURATION."
  (let ((path (pcase configuration
                ('korumite "~/.config/claude-korumite")
                ('manomano "~/.config/claude-manomano")
                (_ (error "Unknown configuration `%s'" configuration)))))
    (setenv "CLAUDE_CONFIG_DIR" (expand-file-name path))
    (message "Switched to `%s' Claude configuration" configuration)))

(defun me/claude-use-korumite ()
  "Switch to the personal Claude configuration."
  (interactive)
  (me/claude--use 'korumite))

(defun me/claude-use-manomano ()
  "Switch to the work Claude configuration."
  (interactive)
  (me/claude--use 'manomano))

(use-package agent-shell
  :commands
  agent-shell-help-menu
  :config
  (define-advice agent-shell--make-header-model
      (:filter-return (model) add-configuration-name)
    (when-let* ((configuration (me/claude-get))
                (name (map-elt model :buffer-name)))
      (setf (map-elt model :buffer-name) (format "%s (%s)" name configuration)))
    model)
  (define-advice agent-shell--start
      (:before (&rest _) require-configuration)
    (unless (me/claude-get)
      (user-error "No Claude configuration set")))
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-busy-indicator-frames 'dots-block)
  (agent-shell-header-style 'text)
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-session-strategy 'latest)
  (agent-shell-show-usage-at-turn-end t)
  (agent-shell-show-welcome-message nil)
  ;; NOTE See <https://github.com/xenodium/agent-shell/issues/273>
  (agent-shell-status-kind-label-function #'agent-shell--inverse-label-status-kind-label)
  :hook
  (agent-shell-mode . hl-line-mode))

(use-package shell-maker
  :custom
  (shell-maker-root-path shelldock-directory))

;;; use-agent.el ends here
