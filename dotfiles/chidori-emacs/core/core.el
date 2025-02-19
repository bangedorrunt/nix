;;; core/core.el -*- lexical-binding: t; -*-
(defvar chidori-user "bangedorrunt")
(defvar chidori-mail "9uermpgz@duck.com")
(defvar chidori-gc-cons '((#x40000000 1.0) (1000000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high value to avoid collection,
temporarily trading space for cycles, but not so high that we require OS paging. During normal
execution, the normal value (cadr) is used, a bit above the default of 800 KiB, to reverse the trade
so we use more cycles but less space, but not too little space.")


(defun chidori-init ()
  "Perform startup initialization, including all comilation and loading"
  (setq
   user-full-name chidori-user
   user-mail-address chidori-mail)

  ;; Avoid garbage collection during startup by increasing thresholds.
  ;; Also disable some other crap which would just slow us down.
  (setq
   gc-cons-threshold (car (cadr chidori-gc-cons))
   gc-cons-percentage (cadr (cadr chidori-gc-cons)))
  ;; NOTE to future self, Doom has an optimization where `file-name-handler-alist' is set to nil
  ;; during startup because many IO functions consult it needlessly. However, this stops Emacs from
  ;; falling back to *.el.gz files if it can't find native- or byte-compiled versions of a package.
  ;; This breaks often enough that it's not worth it to copy this behavior.

  ;; BUT, trade memory for less cycles when using the minibuffer
  (add-hook
   'minibuffer-setup-hook
   (lambda () (setq gc-cons-threshold (car (car chidori-gc-cons)))))
  (add-hook
   'minibuffer-exit-hook
   (lambda () (setq gc-cons-threshold (car (cadr chidori-gc-cons)))))

  ;; Do garbage collection when I'm not actively doing anything
  (run-with-idle-timer 7 t 'garbage-collect)

  ;; (require 'cl-lib)
  (require 'doom)
  (doom-require 'doom 'autoload)
  (doom-require 'doom 'hooks)
  (doom-require 'doom 'package)

  (defvar chidori-modules-packages
    '(chidori-startup
      chidori-prelude
      chidori-evil
      chidori-popup
      chidori-ligatures
      chidori-ui
      chidori-vertico
      chidori-corfu
      chidori-project
      ;; chidori-workspaces
      chidori-vterm
      chidori-git
      chidori-eglot
      chidori-backend
      chidori-org
      chidori-denote
      chidori-lisp
      chidori-tty
      chidori-binding
      ))

  (apply 'doom-requires chidori-modules-packages)

  ;; Don't use customization system
  (setq-default custom-file "/dev/null")

  (with-no-warnings
    (doom-run-hooks 'doom-after-module-config-hook)
    (add-hook 'emacs-startup-hook #'chidori-finish)))


(defun chidori-finish ()
  "Restore default values after init."
  ;; Expand GC parameters so we use fewer, larger collections instead of more, smaller ones. On
  ;; modern systems with plenty of RAM, this should speed up Emacs slightly, at least in theory.
  ;; This is controversial, but I figure it makes enough sense to keep in here.
  (eval-when-compile
    (defvar chidori-gc-cons))
  (setq
   gc-cons-threshold (car (cadr chidori-gc-cons))
   gc-cons-percentage (cadr (cadr chidori-gc-cons)))
  (setq read-process-output-max #x1000000))

(provide 'core)
;; core.el ends here
