;;; -*- lexical-binding: t; -*-
;;; lisp/chidori-project.el

;;; Commentary
;; Provides default settings for project management with project.el

;;; Code:

(package! project :builtin
  :init
;;;; Set our own list of actions on `project-switch-project'
  (setq project-switch-commands '((project-find-file "Find file" "f")
                                  (magit-status "Magit status" "g")
                                  (project-eshell "Eshell" "e")
                                  (consult-ripgrep "Ripgrep" "r"))))

(package! chidori-conjecture :builtin
  :after project
  :config
  (add-hook 'project-find-functions #'project-explicit-recognizer -90 ))

(provide 'chidori-project)
;;; chidori-project.el ends here
