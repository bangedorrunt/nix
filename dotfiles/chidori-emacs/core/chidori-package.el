;;;; core/chidori-package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;;; Code:
;; https://github.com/hlissner/doom-emacs/blob/42a21dffddeee57d84e82a9f0b65d1b0cba2b2af/core/core.el#L353
(defvar doom--deferred-packages-alist '(t))
(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:
  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))
This is already done by the lang/org module, however.
If you want to disable incremental loading altogether, either remove
`doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
`doom-incremental-first-idle-timer' to nil.")

(defvar doom-incremental-first-idle-timer 2.0
  "How long (in idle seconds) until incremental loading starts.
Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar doom-incremental-load-immediately nil
  ;; (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.
If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
intervals."
  (if (not now)
      (appendq! doom-incremental-packages packages)
    (while packages
      (let ((req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (gc-cons-threshold most-positive-fixnum)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load '%s' package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer doom-incremental-idle-timer
                                 nil #'doom-load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (if doom-incremental-load-immediately
      (mapc #'require (cdr doom-incremental-packages))
    (when (numberp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))
;; TODO incremental loading should be reliable
(add-hook 'doom-after-module-config-hook #'doom-load-packages-incrementally-h)

(defun chidori-bootstrap ()
  "Bootstrap major components and set up for use"

  (with-eval-after-load 'gnutls
    (eval-when-compile
      (require 'gnutls))
    (setq gnutls-verify-error t) ; Do not allow insecure TLS connections.
    (setq gnutls-min-prime-bits 3072)) ; Make TLS use an acceptable modern value

  ;; Use the more-cutting-edge develop branch of straight, and don't allow it to check for
  ;; modifications in every repo on Emacs init, saving some startup time.
  (eval-when-compile
    (defvar straight-repository-branch)
    (defvar straight-check-for-modifications))
  (setq
   straight-repository-branch "develop"
   straight-cache-autoloads t
   straight-check-for-modifications nil
   straight-vc-git-default-clone-depth '(1 single-branch))

  ;; Tell straight that let-alist is a built-in package now, so it doesn't need to be checked if we
  ;; (or more likely any dependency) try to pull it in.
  (with-eval-after-load 'straight
    (setq straight--native-comp-available nil)
    (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent
           'inhibit-cookies)
        (setf (point) (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; install use-package
  ;; (straight-use-package 'use-package)

  ;; don't require `use-package' when loading compiled file; saves a millisecond
  ;; or 2; compiling now saves ~0.1s overall (maybe another 0.1s after general
  ;; rewrite)
  (eval-when-compile
    (require 'use-package)

    ;; don't actually need `eval-when-compile' for rest since currently loading
    ;; entire init file before compiling already
    (setq use-package-always-defer t))


  ;; Only expand minimally if we're byte-compiling, and only use verbose if we're in --debug-init.
  (eval-when-compile
    (defvar use-package-expand-minimally)
    (defvar use-package-verbose))
  (setq
   use-package-expand-minimally byte-compile-current-file
   use-package-verbose init-file-debug)

  ;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
  ;;
  ;;   :after-call SYMBOL|LIST
  ;;   :defer-incrementally SYMBOL|LIST|t
  ;;
  (eval-when-compile
    (dolist (keyword '(:defer-incrementally :after-call))
      (push keyword use-package-deferring-keywords)
      (setq use-package-keywords
            (use-package-list-insert keyword use-package-keywords :after)))

    (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
    (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
      (use-package-concat
       `((doom-load-packages-incrementally
          ',(if (equal targets '(t))
                (list name)
              (append targets (list name)))))
       (use-package-process-keywords name rest state)))

    (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
    (defun use-package-handler/:after-call (name _keyword hooks rest state)
      (if (plist-get state :demand)
          (use-package-process-keywords name rest state)
        (let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
          (use-package-concat
           `((fset ',fn
                   (lambda (&rest _)
                     (condition-case e
                         (let ((default-directory user-emacs-directory))
                           (require ',name))
                       ((debug error)
                        (message "Failed to load deferred package %s: %s" ',name e)))
                     (when-let (deferral-list (assq ',name doom--deferred-packages-alist))
                       (dolist (hook (cdr deferral-list))
                         (advice-remove hook #',fn)
                         (remove-hook hook #',fn))
                       (delq! deferral-list doom--deferred-packages-alist)
                       (unintern ',fn nil)))))
           (let (forms)
             (dolist (hook hooks forms)
               (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                         `(add-hook ',hook #',fn)
                       `(advice-add #',hook :before #',fn))
                     forms)))
           `((unless (assq ',name doom--deferred-packages-alist)
               (push '(,name) doom--deferred-packages-alist))
             (nconc (assq ',name doom--deferred-packages-alist)
                    '(,@hooks)))
           (use-package-process-keywords name rest state)))))
    )
  ;; A last ditch opportunity to undo dodgy optimizations or do extra
  ;; configuration before the session is complicated by user config and
  ;; packages.
  ;; REVIEW seem this fix `vterm-module' compilation
  (doom-run-hooks 'doom-before-init-hook)

  (and (doom-has-modules-p) (require 'treesit nil t))
  )

(chidori-bootstrap)


(provide 'chidori-package)
;;; chidori-package.el ends here
