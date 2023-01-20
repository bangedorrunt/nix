;; lisp/chidori-eglot.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(package! eglot :auto
  :hook ((scss-mode
          css-mode
          clojure-mode
          ;; java-mode
          sh-mode
          json-mode
          json-ts-mode
          js2-mode
          typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          nix-mode
          rust-mode)
         . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil) ; don't ask to edit file immediately after I told it to
  (eglot-autoshutdown t) ; shutdown server after killing last managed buffer
  :config
  (add-hook! 'eglot-managed-mode-hook
    (defun +eglot-flymake--disable-overlay ()
      (put 'eglot-note 'flymake-overlay-control nil)
      (put 'eglot-warning 'flymake-overlay-control nil)
      (put 'eglot-error 'flymake-overlay-control nil)))

  (map!
   :leader
   "ca"  #'eglot-code-actions
   "cf"  '(:ignore t :wk "find")
   "cfr" #'xref-find-references
   "cfd" #'eglot-find-declaration
   "cfi" #'eglot-find-implementation
   "cft" #'eglot-find-typeDefinition
   "cr"  '(:ignore t :wk "refactor")
   "crr" #'eglot-rename
   "crf" #'eglot-format
   "cro" #'eglot-code-action-organize-imports))

(package! flymake-languagetool :auto
  ;; :disabled
  :after-call doom-first-buffer-hook
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :init
  ;; Remote server config with LanguageTool's free API
  ;; (setq flymake-languagetool-url "https://api.languagetool.org")
  ;; (setq flymake-languagetool-server-port nil)
  ;; (setq flymake-languagetool-server-jar nil)

  ;; Local server config
  (setq flymake-languagetool-server-jar "~/Downloads/Language-Tool-6.0/languagetool-server.jar"))

(package! eglot-grammarly (:host github :repo "emacs-grammarly/eglot-grammarly")
  :disabled
  :hook ((text-mode markdown-mode). (lambda ()
                                      (require 'eglot-grammarly)
                                      (eglot-ensure))))
(after! xref
  (defun noct-xref-find-definition ()
    "Call `xref-find-definitions' but prompt for identifier.
Don't immediately jump to the symbol at the point."
    (interactive)
    (call-interactively #'xref-find-definitions))
  (map!
   :map help-map
   "x" #'noct-xref-find-definition))

(provide 'chidori-eglot)
;;; chidori-eglot.el ends here
