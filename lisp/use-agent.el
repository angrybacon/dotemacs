;;; use-agent.el --- LLM integrations                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package agent-shell
  :commands
  agent-shell-help-menu
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :api-key (szadek-get 'anthropic-key)))
  (agent-shell-busy-indicator-frames 'dots-block)
  (agent-shell-header-style 'graphical)
  (agent-shell-header-style 'text)
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-session-strategy 'latest)
  (agent-shell-show-welcome-message nil)
  (agent-shell-show-usage-at-turn-end t))

(use-package shell-maker
  :custom
  (shell-maker-root-path shelldock-directory))

;;; use-agent.el ends here
