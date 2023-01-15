;;; lisp/chidori-workspaces.el --- Workspaces configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use tabspaces to manage workspaces

;;; Code:
(package! tabspaces :auto
  :defer-incrementally t
  :hook (doom-first-buffer . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :config
  (setq tabspaces-use-filtered-buffers-as-default t)
  (setq tabspaces-remove-to-default t)
  (setq tabspaces-include-buffers '("*scratch*"))
;;;; Auto save session
  (setq tabspaces-session t)
  (setq tabspaces-session-auto-restore t)
  )


;; Make sure project is initialized
(project--ensure-read-project-list)

(provide 'chidori-workspaces)
;;; chidori-workspaces.el ends here
