;;; core/autoload/doom/git.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun doom-git-toplevel (&rest segments)
  "Return the path to the current repo's root."
  (cl-destructuring-bind (code . output)
      (doom-call-process "git" "rev-parse" "--show-toplevel")
    (if (zerop code)
        (apply #'file-name-concat output segments)
      ;; TODO throw stderr as error
      (user-error "Not in a git repo: %s" default-directory))))

;;; git.el ends here
