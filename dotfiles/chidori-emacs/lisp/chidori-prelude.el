;;; lisp/chidori-prelude.el -*- lexical-binding: t; -*-
;;; Code:
;;;

;; Set up core packages. The ELPA keyring sometimes gets screwed up, but this package lets us fix
;; it easily.
(package! gnu-elpa-keyring-update :auto)

(package! popup :auto)

;; Mostly only required for MacOS, we need to grab environment variables from the default shell.
;; This lets us use TRAMP more easily and connects us with some tools.
(package! exec-path-from-shell :auto
  :disabled t
  :defer-incrementally t
  :config
  (when (or IS-MAC (daemonp))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(package! blackout :auto :demand t)

(package! no-littering :auto
  :demand t
  :config
  (setq
    backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup-dir/"))))
  (setq
   auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(package! tramp (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :defer t
  :functions tramp-cleanup-all-connection
  :config
  (setq tramp-auto-save-directory "~/.cache/emacs/backups")
  (setq tramp-persistency-file-name "~/.config/emacs/data/tramp")
  (setq tramp-use-ssh-controlmaster-options nil)  ; use system settings instead
  (setq tramp-default-method "ssh") ; faster than default scp
  (setq tramp-terminal-type "tramp")
  (setq remote-file-name-inhibit-cache 60)
  (setq tramp-completion-reread-directory-timeout 60)
  (setq tramp-verbose 1)
  (setq vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))
(package! winner :builtin
  :hook (doom-first-buffer . winner-mode)
  :defines winner-boring-buffers
  :config
  ;; list of buffers that winner-undo won't restore
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*")))

(package! dtrt-indent :auto
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `doom-detect-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . doom-detect-indentation-h)
  :config
  (defun doom-detect-indentation-h ()
    (unless (or (not after-init-time)
                doom-inhibit-indent-detection
                doom-large-file-p
                (memq major-mode doom-detect-indentation-excluded-modes)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! doom--fix-broken-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg)))))

(package! better-jumper :auto
  :hook (doom-first-input . better-jumper-mode)
  :commands doom-set-jump-a doom-set-jump-maybe-a doom-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun doom-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))

;; Use `so-long-revert' in a buffer to get back to what it would otherwise have loaded as.
(package! so-long :builtin
  :hook (doom-first-file . global-so-long-mode)
  :config
  ;; Emacs 29 introduced faster long-line detection, so they can afford a much
  ;; larger `so-long-threshold' and its default `so-long-predicate'.
  (if (fboundp 'buffer-line-statistics)
      (unless (featurep 'native-compile)
        (setq so-long-threshold 5000))
    ;; reduce false positives w/ larger threshold
    (setq so-long-threshold 400)

    (defun doom-buffer-has-long-lines-p ()
      (unless (bound-and-true-p visual-line-mode)
        (let ((so-long-skip-leading-comments
               ;; HACK Fix #2183: `so-long-detected-long-line-p' calls
               ;;   `comment-forward' which tries to use comment syntax, which
               ;;   throws an error if comment state isn't initialized, leading
               ;;   to a wrong-type-argument: stringp error.
               ;; DEPRECATED Fixed in Emacs 28.
               (bound-and-true-p comment-use-syntax)))
          (so-long-detected-long-line-p))))
    (setq so-long-predicate #'doom-buffer-has-long-lines-p))
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode)))


;; Turn on recentf mode
(package! recentf :builtin
  :defer-incrementally easymenu tree-widget timer
  :after-call after-find-file
  :config
  ;; No littering
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (setq recentf-save-file (expand-file-name ".recentf" chidori-var-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 17)

  (defun doom--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))

  ;; from doom
  (setq
   recentf-filename-handlers
   '(;; Text properties inflate the size of recentf's files, and there is
     ;; no purpose in persisting them, so we strip them out.
     substring-no-properties
     ;; Resolve symlinks of local files. Otherwise we get duplicate
     ;; entries opening symlinks.
     doom--recent-file-truename
     ;; Replace $HOME with ~, which is more portable, and reduces how much
     ;; horizontal space the recentf listing uses to list recent files.
     abbreviate-file-name)
   recentf-auto-cleanup 'never)
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (add-hook!
   '(doom-switch-window-hook write-file-functions)
   (defun doom--recentf-touch-buffer-h ()
     "Bump file in recent file list when it is switched or written to."
     (when buffer-file-name
       (recentf-add-file buffer-file-name))
     ;; Return nil for `write-file-functions'
     nil))

  ;; save recent after 10 seconds of idle time
  ;; if not idle, save every 5 minutes
  (defun +recentf-save-list-quiet ()
    (quiet! (when recentf-mode
              (recentf-save-list))))
  (run-at-time 60 (* 5 60) #'+recentf-save-list-quiet))

(package! savehist :builtin
  :defer-incrementally custom
  :hook (doom-first-input . savehist-mode)
  :custom (savehist-file (concat chidori-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (add-hook! 'savehist-save-hook
    (defun doom-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))
    (defun doom-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

(package! saveplace :builtin
  ;; persistent point location in buffers
  :hook (doom-first-file . save-place-mode)
  :custom (save-place-file (concat chidori-cache-dir "saveplace"))
  :config
  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! doom--inhibit-saveplace-in-long-files-a (fn &rest args)
    :around #'save-place-to-alist
    (unless doom-large-file-p
      (apply fn args)))

  (defadvice! doom--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so this replace calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn))))

;; detects when the buffer matches what's on disk and marks it unmodified. If, for example, you
;; visit a file, change something, then undo the change, this package ensures the buffer doesn't
;; think its still modified.
(package! unmodified-buffer (:host github
                             :repo "arthurcgusmao/unmodified-buffer")
  :hook ((prog-mode text-mode) . unmodified-buffer-mode))

(package! server :builtin
  :when (display-graphic-p)
  :after-call doom-first-input-hook doom-first-file-hook focus-out-hook
  :defer 1
  :config
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))



(provide 'chidori-prelude)
;;; chidori-prelude.el ends here
