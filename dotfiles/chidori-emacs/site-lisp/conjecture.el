;;; lisp/chidori-conjecture.el --- Extensions for project.el -*- lexical-binding: t; -*-


(require 'project)
(require 'cl-lib)

;;; Code:

(defvar-local project-name nil
  "A user-facing name for the current buffer's project.

If set, it will be used as the default return value for the
`project-name' function, before querying the project.")

(defvar-local project-manifest-file nil
  "A file containing the project description or configuration.

This should be a path relative to the project root. If set, it
will be used as the default return value for the
`project-manifest-file' project method. Individual project types
may use this value or ignore it as appropriate.")

(defvar project--this-action nil
  "The name of the active compile action as a string.

It can be used in the name of the compilation buffer when running
a project method defined via `def-project-compile-action'.

This is not intended to be given a value by user code; instead it
will be dynamically set just for the duration of the compilation
action.")

;;;; Project methods

;;;###autoload
(cl-defgeneric project-manifest-file (project)
  "A file that describes the project configuration.

This should return the file name relative to the project root.
The default implementation returns the value of
`project-manifest-file' if set, or \"Makefile\" if that exists in
the project root, otherwise nil."
  (or project-manifest-file
      (when (file-exists-p (project-expand-file "Makefile" project))
        "Makefile")))

;;;###autoload
(cl-defgeneric project-display-name (project)
  "A name for the project that can be presented to the user.

This should be a simple string appropriate for use in buffer
names, user messages, labels, and the like."
  (file-name-nondirectory (directory-file-name (project-root project))))

(defmacro def-project-compile-action (verb &optional default-cmd fndoc)
  "Declare a new project.el method and config variable to perform VERB.

Generic documentation for both will be generated. If provided,
FNDOC will also be inserted into the method's docstring.

DEFAULT-CMD, if provided, will be used with `compile' if no value
is set for the configuration variable.

In addition, a command will be defined that invokes the new
project method, passing the current project. If the buffer is not
part of a project, the command falls back to `compile'."
  (declare (indent 1))
  (let ((fn (intern (concat "project-" (symbol-name verb))))
        (var (intern (concat "project-" (symbol-name verb) "-command"))))
    `(progn
       (defvar-local ,var nil
         ,(string-join `(,(format "Configuration variable for `%s'." fn)
                         ""
                         "Intended to be set as part of project configuration,"
                         "most likely in a .dir-locals file."
                         ""
                         "Its value may take one of several forms:"
                         ""
                         "STRING            A shell command to use with `compile'."
                         "'(render . FUNC)  FUNC must be a function returning a string."
                         "That string should be a shell command to use with `compile'."
                         "The function may use the current buffer or other information"
                         "to decide what to return."
                         "'(exec . FUNC)    FUNC must be a function that does whatever"
                         ,(format "is required to %s the project itself." verb)
                         ""
                         "Specific project types are also free to interpret this"
                         "in their own fashion or recognize different forms. The base"
                         ,(format "`%s' implementation will fall back" fn)
                         "to `compile' if the value is unrecognized or nil.")
                       "\n"))

       (cl-defgeneric ,fn (project arg)
         ,(string-join `(,(format "%s the current project." (capitalize (symbol-name verb)))
                         ,(if fndoc
                              (concat "\n" fndoc "\n")
                            "")
                         ,(format "The base implementation depends on the value of `%s'." var)
                         "If that resolves to a string, the string is passed to `compile'."
                         "With non-nil ARG, the string is instead bound to `compile-command'"
                         "before calling `compile', which will prompt for input."
                         ""
                         "If it is the \\(exec . FUNC) form, then FUNC is simply called."
                         "It will be called interactively if it is a command."
                         ""
                         "As a fallback, if the value is nil or another unrecognized form,"
                         "`compile' is invoked with no arguments."
                         ""
                         "In all cases the `default-directory' will be the root"
                         "of the project.")
                       "\n")
         (let ((default-directory (project-root project))
               (project--this-action ,(symbol-name verb))
               (parsed
                (pcase ,var
                  ((and (pred stringp) cmd) cmd)
                  ((and `(render . ,renderer)
                        (let (pred
                              functionp)
                          renderer))
                   (funcall renderer))
                  ((and `(exec . ,executor)
                        (let (pred
                              functionp)
                          executor))
                   executor)
                  ((or 'nil _) ,default-cmd))))
           (cond
            ((stringp parsed)
             (if arg
                 (let ((compile-command parsed))
                   (call-interactively #'compile))
               (funcall-interactively #'compile parsed)))
            ((commandp parsed)
             (call-interactively parsed))
            ((functionp parsed)
             (funcall parsed))
            ;; Fallback for no configured value and no `default-cmd'
            (t
             (call-interactively #'compile)))))

       (defun ,(intern (concat (symbol-name verb) "-buffer-or-project")) (&optional arg)
         ,(string-join `(,(format "Execute either `compile' or `%s', depending on the buffer." fn)
                         "If the buffer is visiting a file and part of a project, use"
                         ,(format "`%s'; otherwise just compile the buffer alone." fn))
                       "\n")
         (interactive "P")
         (if-let ((proj (project-current))
                  ((buffer-file-name)))
           (funcall-interactively #',fn proj arg)
           (call-interactively #'compile))))))

;;;; Utilities

;;;###autoload
(defun project-expand-file (file &optional project)
  "Convert path FILE to an absolute path under PROJECT.

PROJ defaults to the current project; its root directory is used
as the DEFAULT-DIRECTORY in `expand-file-name', q.v."
  (if-let ((proj (or project (project-current))))
    (expand-file-name file (project-root proj))
    (error "No project available")))

;;;###autoload
(defun project-name ()
  "The user-facing name of the current project.

If there is no current project, returns `nil'. If the
buffer-local variable `project-name' has a value, that is
returned; otherwise the value of `project-display-name'."
  (or project-name
      (when-let ((project (project-current)))
        (project-display-name project))))

;;;;; Project types and overrides
;;
;;;###autoload
(cl-defmethod project-files :around
  (project &optional dirs)
  "Use 'fd' if available to find all files for PROJECT in DIRS."
  (if-let ((fd (executable-find "fd")))
    (let* ((search-dirs (string-join (or dirs (list (project-root project))) " "))
           (command (format "%s --hidden --type f --print0 '.*' %s" fd search-dirs)))
      (split-string (shell-command-to-string command) "\0" t))
    (cl-call-next-method project dirs)))

;;;; Explicit project
;;
;;;###autoload
(cl-defmethod project-root ((project (head explicit)))
  "Definition for a project whose root is defined by an
explicit marker file.

The file may be named either '.project' or '.projectile'."
  (cdr project))

(cl-defmethod project-manifest-file ((project (head explicit)))
  "The project file for a explicitly-marked project.

This is the marker file itself: either \".project\" or
\".projectile\"."
  (or project-manifest-file
      (cond
       ((file-exists-p (project-expand-file ".project" project))
        ".project")
       ((file-exists-p (project-expand-file ".projectile" project))
        ".projectile"))))

;;;###autoload
(defun project-explicit-recognizer (dir)
  "Search up the directory tree for an explicit project marker.

The marker is a file named either '.project' or '.projectile';
the file's directory is considered the root directory of
the project. Only the file's name is significant. Its
contents are irrelevant."
  (when-let ((explicit-root
              (condition-case nil
                  (locate-dominating-file
                   dir
                   (lambda (search-dir)
                     (directory-files search-dir
                                      nil
                                      (rx
                                       string-start
                                       (or ".project"
                                           ".projectile")
                                       string-end))))
                (file-missing nil))))
    (cons 'explicit explicit-root)))

(provide 'conjecture)
;; chidori-conjecture ends here
