;;; core/autoload/doom/hooks.el --- Hooks for faster startup -*- lexical-binding: t; -*-

;; SEE: https://github.com/ajgrf/on.el

;;;###autoload
(defvar doom-theme nil
  "A symbol representing the Emacs theme to load at startup.
Set to `nil' to load no theme at all. This variable is changed by
`load-theme'.")

;;;###autoload
(defvar doom-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`doom/reload-theme'.")

;;;###autoload
(defvar doom-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")
(put 'doom-init-ui-hook 'permanent-local t)

;;;###autoload
(defvar doom-after-init-hook nil
  "A hook run after UI initialized")
(put 'doom-after-init-hook 'permanent-local t)

;;;###autoload
(defvar doom-after-module-config-hook nil
  "A hook run after config chidori modules")
(put 'doom-after-module-config-hook 'permanent-local t)

;;;###autoload
(defvar doom-before-init-hook nil
  "A hook run before UI initialized")
(put 'doom-before-init-hook 'permanent-local t)

;;;###autoload
(defvar doom-first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'doom-first-input-hook 'permanent-local t)

;;;###autoload
(defvar doom-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'doom-first-file-hook 'permanent-local t)

;;;###autoload
(defvar doom-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'doom-first-buffer-hook 'permanent-local t)

;;;###autoload
(defvar doom-switch-buffer-hook nil
  "A hook run after changing the current buffer.")

;;;###autoload
(defvar doom-switch-window-hook nil
  "A hook run after changing the focused windows.")

;;;###autoload
(defvar doom-switch-frame-hook nil
  "A hook run after changing the focused frame.")

;;;###autoload
(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (doom-run-hooks 'doom-switch-buffer-hook)))

(defun doom-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (doom-run-hooks 'doom-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (doom-run-hooks 'doom-switch-window-hook))))

;;;###autoload
(defun doom-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))

;;;###autoload
(defun doom-highlight-non-default-indentation-h ()
  "Highlight whitespace at odds with `indent-tabs-mode'.
That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (cl-union (if indent-tabs-mode
                       '(indentation)
                     '(tabs tab-mark))
                   (when whitespace-mode
                     (remq 'face whitespace-active-style))))
    (cl-pushnew 'face whitespace-style) ; must be first
    (whitespace-mode +1)))


(defun doom-before-init-h (&optional _)
  "Begin the startup process."
  ;; Remember these variables' initial values, so we can safely reset them at
  ;; a later time, or consult them without fear of contamination.
  (dolist (var '(exec-path load-path process-environment))
    (put var 'initial-value (default-toplevel-value var))))

(defadvice! doom--load-theme-a (fn theme &optional no-confirm no-enable)
  " Disable old themes, and trigger `doom-load-theme-hook'."
  :around #'load-theme
  ;; Run `load-theme' from an estranged buffer, where we can ensure that
  ;; buffer-local face remaps (by `mixed-pitch-mode', for instance) won't
  ;; interfere with recalculating faces in new themes.
  (with-temp-buffer
    (let ((last-themes (copy-sequence custom-enabled-themes)))
      ;; Disable previous themes so there are no conflicts. If you truly want
      ;; multiple themes enabled, then use `enable-theme' instead.
      (mapc #'disable-theme custom-enabled-themes)
      (prog1 (funcall fn theme no-confirm no-enable)
        (when (and (not no-enable) (custom-theme-enabled-p theme))
          ;; DEPRECATED Hook into `enable-theme-functions' when we target 29
          (doom-run-hooks 'doom-load-theme-hook)
          )))))

(defun doom-init-ui-h (&optional _)
  "Initialize user interface by applying its hooks.
These should be done as late as possible, as to avoid/minimize prematurely
triggering hooks during startup."
  (doom-run-hooks 'doom-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'doom-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (push '(buffer-predicate . doom-buffer-frame-predicate) default-frame-alist)

  ;; Initialize `doom-switch-window-hook' and `doom-switch-frame-hook'
  (add-hook 'window-selection-change-functions #'doom-run-switch-window-or-frame-hooks-h)
  ;; Initialize `doom-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'doom-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'doom-run-switch-buffer-hooks-h)

  ;; Only execute this function once.
  (remove-hook 'window-buffer-change-functions #'doom-init-ui-h)
  )

;; Initialize UI as late as possible. `window-buffer-change-functions' runs
;; once, when the scratch/dashboard buffer is first displayed.
;; NOTE: `window-setup-hook' seem more reliable
(add-hook 'window-buffer-change-functions #'doom-init-ui-h -100)

(unless noninteractive
  (doom-run-hook-on 'doom-first-buffer-hook '(find-file-hook doom-switch-buffer-hook))
  (doom-run-hook-on 'doom-first-file-hook   '(find-file-hook dired-initial-position-hook))
  (doom-run-hook-on 'doom-first-input-hook  '(pre-command-hook))
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (doom-run-hooks 'doom-after-init-hook))
  )

(provide 'doom '(hooks))
;;; hooks.el ends here
