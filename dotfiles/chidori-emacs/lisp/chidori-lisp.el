;;;; lisp/chidori-lisp.el --- Lisp development configuration  -*- lexical-binding: t; -*-

;;; Code:
(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars ; don't complain about unknown variables
        noruntime ; don't complain about unknown function calls
        unresolved) ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and scripts, which are
so stateful that the deluge of false positives (from the byte-compiler,
package-lint, and checkdoc) can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")


;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)

;; Global defaults
(package! eldoc :builtin :blackout t)

(package! elisp-mode :builtin
 :mode ("\\.Cask\\'" . emacs-lisp-mode)
 :config
 (set-repl-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp/open-repl)
 (set-ligatures! 'emacs-lisp-mode :lambda "lambda")
 (setq-hook!
  'emacs-lisp-mode-hook
  ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
  ;; with a tab width of 8. Any smaller and the indentation will be
  ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
  ;; safe to ignore this setting otherwise.
  tab-width 8
  ;; Don't treat autoloads or sexp openers as outline headers, we have
  ;; hideshow for that.
  outline-regexp +emacs-lisp-outline-regexp
  outline-level #'+emacs-lisp-outline-level)

 ;; Fixed indenter that intends plists sensibly.
 (advice-add
  #'calculate-lisp-indent
  :override #'+emacs-lisp--calculate-lisp-indent-a)

 ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
 ;; and `editorconfig' would force fixed indentation on elisp.
 (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode)

 (add-hook!
  'emacs-lisp-mode-hook
  ;; Allow folding of outlines in comments
  #'outline-minor-mode
  ;; Make parenthesis depth easier to distinguish at a glance
  #'rainbow-delimiters-mode
  ;; Make quoted symbols easier to distinguish from free variables
  #'highlight-quoted-mode
  ;; Ensure straight sees modifications to installed packages
  #'+emacs-lisp-init-straight-maybe-h)

 ;; Enhance elisp syntax highlighting, by highlighting Doom-specific
 ;; constructs, defined symbols, and truncating :pin's in `package!' calls.
 (font-lock-add-keywords
  'emacs-lisp-mode
  (append
   `( ;; custom Doom cookies
     ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
   ;; highlight defined, special variables & functions
   (when +emacs-lisp-enable-extra-fontification
     `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

 ;; Recenter window after following definition
 (advice-add #'elisp-def :after #'doom-recenter-a)

 (defadvice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
  "Display variable value next to documentation in eldoc."
  :around #'elisp-get-var-docstring
  (when-let (ret
             (funcall fn sym))
    (if (boundp sym)
        (concat
         ret " "
         (let* ((truncated " [...]")
                (print-escape-newlines t)
                (str (symbol-value sym))
                (str (prin1-to-string str))
                (limit (- (frame-width) (length ret) (length truncated) 1)))
           (format (format "%%0.%ds%%s" (max limit 0))
                   (propertize str 'face 'warning)
                   (if (< (length str) limit)
                       ""
                     truncated))))
      ret)))

 (map!
  :localleader
  :map (emacs-lisp-mode-map lisp-interaction-mode-map)
  :desc
  "Expand macro"
  "m"  #'macrostep-expand
  "d"   '(:ignore t :wk "debug")
  "df" #'+emacs-lisp/edebug-instrument-defun-on
  "dF" #'+emacs-lisp/edebug-instrument-defun-off
  "e"   '(:ignore t :wk "eval")
  "eb" #'eval-buffer
  "ed" #'eval-defun
  "ee" #'eval-last-sexp
  "er" #'eval-region
  "el" #'load-library))

(package! elisp-demos :auto
 :defer t
 :init
 (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
 (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(package! highlight-quoted :auto)

(package! elisp-autofmt :auto
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 ;; Use system python
 :custom (elisp-autofmt-python-bin "/usr/bin/python3")
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(autoload 'overseer-test "overseer" nil t)
;; Properly lazy load overseer by not loading it so early:
(remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode)

(provide 'chidori-lisp)
;;; chidori-lisp.el ends here
