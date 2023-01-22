;;; init.el -*- lexical-binding: t; -*-

;; Define customization group for Chidori Emacs.
(defgroup chidori nil
  "A sensible starting point for hacking your own Emacs configuration."
  :tag "Chidori Emacs"
  :group 'chidori-emacs
  :prefix 'chidori)

(setq debug-on-error t) ; verifier les erreurs dans ce fichier
(setq ad-redefinition-action 'accept) ; Accept advice redefinition without complaining

;; (require 'chidori-earlyinit (expand-file-name "core/chidori-earlyinit.el" user-emacs-directory))

(require 'core)
(chidori-init)

(setq warning-minimum-level :error) ; Log warnings but don't let them pop up
(setq debug-on-error nil) ; Disable debug for normal runtime
(setq load-prefer-newer nil)

;;; init.el ends here
