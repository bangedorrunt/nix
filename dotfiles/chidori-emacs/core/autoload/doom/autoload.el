;;; core/autoload/doom/autoload.el -*- lexical-binding: t; -*-

(defvar generated-autoload-file nil
  "This is neccessary, otherwise raise error.
`Defining as dynamic an already lexical var`.")

;; NOTE don't store autoload file in nested dir and `user-emacs-directory'
;; this might cause file-missing issue
(defvar chidori-autoload--file (expand-file-name "autoload.el" chidori-cache-dir)
  "Autoload file.")

(defvar chidori-autoload--dirs
  `(,chidori-autoload-dir
    ,chidori-doom-dir
    ,chidori-org-dir
    ,chidori-site-lisp-dir
    ,chidori-lisp-dir
    )
  "Autoload file.")

(defun chidori-autoload--generate-define (loaddef &rest DIRS)
  "LOADDEF DIRS."
  (let ((generated-autoload-file loaddef))
    (unless (file-exists-p generated-autoload-file)
      ;; Prevent `update-directory-autoloads' from running hooks
      ;; (for example, adding to `recentf') when visiting the
      ;; autoload file.
      (let ((find-file-hook nil)
            (write-file-functions nil)
            ;; Apparently fixes a bug in Emacs 27, see
            ;; <https://github.com/radian-software/straight.el/issues/434>.
            (debug-on-error nil)
            ;; Non-nil interferes with autoload generation in Emacs < 29, see
            ;; <https://github.com/radian-software/straight.el/issues/904>.
            (left-margin 0))
        ;; Actually generate the autoload file. Emacs 28.1 replaces
        ;; `update-directory-autoloads' with
        ;; `make-directory-autoloads', and Emacs 29 with
        ;; `loaddefs-generate’
        (cond
         ((fboundp 'loaddefs-generate)
          (loaddefs-generate DIRS generated-autoload-file))
         ((fboundp 'update-directory-autoloads)
          (apply 'update-directory-autoloads DIRS))))
      ;; And for some reason Emacs leaves a newly created buffer
      ;; lying around. Let's kill it.
      (when-let ((buf (find-buffer-visiting generated-autoload-file)))
        (kill-buffer buf)))))

;;;###autoload
(defun chidori-autoload/reload ()
  "Generate autoload file from `core/autoload'."
  (interactive)
  (when (file-exists-p chidori-autoload--file)
    (delete-file chidori-autoload--file t)
    (message "delete old autoload file: %s" chidori-autoload--file))

  (apply 'chidori-autoload--generate-define chidori-autoload--file chidori-autoload--dirs)
  (load chidori-autoload--file nil 'nomessage)
  (message "generate autoload file: %s done." chidori-autoload--file))

(unless (file-exists-p chidori-autoload--file)
  (chidori-autoload/reload))

(load chidori-autoload--file nil 'nomessage)

(provide 'doom '(autoload))
;; autoload.el ends here
